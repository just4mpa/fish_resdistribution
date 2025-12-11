# title: "Download aquamaps data"
# author: "Brunno F Oliveira"

# Load libraries ----

rm(list=ls())
gc()

library(here)

computer = Sys.info()["nodename"]
if(computer == "BRUNNO-THINKPAD" | computer == "just"){
  wd <- here()
} else {
  wd <- "/users/boliveira/fish_redistribution"
}

source(here(wd,"R/source_code.R"))

# remotes::install_github("cboettig/duckdbfs")
# remotes::install_github("fisw10/BHPMF")
list.of.packages <- c("stringr", "crul", "tidyr", "here", "pbapply","purrr","dplyr",
                      "pbapply","dplyr","httr","parallel","duckdbfs",
                      "data.table", "ggplot2", "knitr", "kableExtra",
                      "raster", "terra","rnaturalearthdata","tidyterra",
                      "rgbif","CoordinateCleaner","rfishbase")

load_packages(list.of.packages)

cores <- parallel::detectCores()

# Load global sea shp file ----
globe <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sv")


# Load Nutrient data ----
if(file.exists(here(dir_data,"nutri_data.csv"))){
  nutri_data <- read.csv(here(dir_data,"nutri_data.csv"))
} else {
  nutri_data <- read.csv(here(dir_data,"Hicks_etal_2019/Species_Nutrient_Predictions.csv"))
  
  # Clean species name (remove sub species and taxa identified to the genus level)
  nutri_data$name_clean <- sapply(nutri_data$species, function(x){
    tmp <- gsub("_"," ",x)
    tmp <- strsplit(tmp," ")[[1]]
    if(length(tmp)==1){
      return(NA)
    }
    if(length(tmp)==2){
      return(paste(str_to_title(tmp[1]),tolower(tmp[2])))
    }
    if(length(tmp)>2){
      return(NA)
    }
  })
  rem <- which(is.na(nutri_data$name_clean))
  nutri_data <- nutri_data[-rem,]
  
  write.csv(nutri_data,
            here(dir_data,"nutri_data.csv"))
}


# Get GBIF accepted names ----
sps_list <- data.frame(species = nutri_data$species,
                       FB_ID = nutri_data$spec_code)
nrow(sps_list)

if(file.exists(here::here(dir_data,"sps_list_w_accepted_names.csv"))){
  
  sps_list <- read.csv(here(dir_data,"sps_list_w_accepted_names.csv"))
  
} else {
  cl <- parallel::makeCluster(cores) 
  
  sps_names_accepted <- pblapply(sps_list$name_clean, function(x){
    data.frame(requested_name = x, 
               rgbif::name_backbone(x))
  }, cl = cl)
  
  parallel::stopCluster(cl)
  
  sps_names_accepted <- rbindlist(sps_names_accepted, fill=TRUE)
  sps_list$AcceptedName <- sps_names_accepted$species
  sps_list$GBIF_ID <- sps_names_accepted$speciesKey
  
  write.csv(sps_list,
            here(dir_data,"sps_list_w_accepted_names.csv"),
            row.names = FALSE)
  
}

# Get taxonomy from fishbase & GBIF ----
if(file.exists(here(dir_data,"sp_taxonomy.csv"))){
  sps_list <- read.csv(here(dir_data,"sp_taxonomy.csv"))
} else {
  fb_taxonomy <- load_taxa()
  sps_list <- merge(sps_list, 
                    fb_taxonomy[,c(1,3:8)],
                    by.x = "FB_ID",
                    by.y = "SpecCode",
                    all.x = TRUE) 
  
  # use GBIF to get taxonomy info for species without taxonomic info from fishbase
  taxo_2get <- sps_list[which(is.na(sps_list$Class)),]
  taxo_2get <- lapply(na.omit(taxo_2get$AcceptedName), rgbif::name_backbone)
  taxo_2get <- rbindlist(taxo_2get,fill = TRUE)
  taxo_2get <- taxo_2get %>%
    mutate(GBIF_ID = speciesKey,
           Genus = genus,
           Family = family,
           Order = order,
           Class = class) %>% 
    select(GBIF_ID, Genus, Family, Order, Class)
  for(i in 1:nrow(taxo_2get)){
    pos <- which(sps_list$GBIF_ID == taxo_2get$GBIF_ID[i])
    sps_list$Genus[pos] <- taxo_2get$Genus
    sps_list$Family[pos] <- taxo_2get$Family
    sps_list$Order[pos] <- taxo_2get$Order
    sps_list$Class[pos] <- taxo_2get$Class
  }
  
  write.csv(sps_list,
            here(dir_data,"sp_taxonomy.csv"),
            row.names = FALSE)
}





# Get habitat data from fishbase ----

if(file.exists(here(dir_data,"habitat_traits_raw.csv"))){
  
  fs_habitat_class <- read.csv(here(dir_data,"habitat_traits_raw.csv"))
  
} else {
  # refer to the manual https://www.fishbase.se/manual/fishbasethe_ecology_table.htm
  all_sps <- sps_list$species # as we interact directly with fishbase, here we use the species name provided in Hicks et al. (not the accepted name from GBIF)
  fs_habitat <- ecology(all_sps)
  fs_habitat <- fs_habitat[,c(1,2,6:28)]
  names(fs_habitat)
  # Define habitat trait columns
  trait_cols <- names(fs_habitat)[4:25]
  
  # summarise habitat traits >>> get one row per species
  fs_habitat_unique <- fs_habitat %>%
    group_by(SpecCode, Species) %>%
    summarise(
      HabitatsRef = paste(unique(HabitatsRef), collapse = ","),
      across(all_of(trait_cols), ~ ifelse(any(. == -1, na.rm = TRUE), -1, 0)),
      .groups = "drop"
    )
  
  ## More ecology 
  fs_ecology <- species(all_sps)
  # select traits
  fs_ecology <- fs_ecology %>%
    select(SpecCode, DemersPelag)
  
  # classify these traits in three main categories:
  # 1) Benthic (ocean floor / substrate)
  # 2) Pelagic (water column / open ocean)
  # 3) Surface / coastal / nearshore
  
  # Surface / coastal / nearshore
  superficial <- c(
    "Neritic","SupraLittoralZone","Saltmarshes","LittoralZone",
    "TidePools","Estuaries","Mangroves","MarshesSwamps","Stream","Lakes"
  )
  
  # Ocean floor / substrate
  benthic <- c(
    "Intertidal","SubLittoral","Caves","CaveAnchialine","Cave","Cave2"
  )
  
  # Water column / open ocean
  pelagic <- c(
    "Oceanic","Epipelagic","Mesopelagic","Bathypelagic","Abyssopelagic","Hadopelagic"
  )
  
  # Create three new columns classifying species
  fs_habitat_class <- fs_habitat_unique %>%
    mutate(
      Benthic = ifelse(rowSums(select(., all_of(benthic)) == -1) > 0, 1, 0),
      Pelagic = ifelse(rowSums(select(., all_of(pelagic)) == -1) > 0, 1, 0),
      Superficial = ifelse(rowSums(select(., all_of(superficial)) == -1) > 0, 1, 0)
    ) %>%
    select(Species, SpecCode, Benthic, Pelagic, Superficial)
  
  # write NA if has no info on habitat
  rem <- which(rowSums(fs_habitat_class[,c(3:5)]) == 0)
  fs_habitat_class[rem,c(3:5)] <- NA
  
  # plug in reference for habitat
  fs_habitat_class <- merge(fs_habitat_class, 
                            fs_habitat_unique[,c(1,3)],
                            by = "SpecCode",
                            all.x = TRUE)
  
  write.csv(fs_habitat_class,
            here(dir_data,"habitat_traits_raw.csv"),
            row.names = FALSE)
  
}


# Download GBIF occurrences ----
cat("Download GBIF occurrences\n")
dir_sp_occ_raw <- here(dir_sp_occ,"raw")
if(!dir.exists(dir_sp_occ_raw)){
  dir.create(dir_sp_occ_raw,recursive = TRUE)
}

# columns of interest
col_interest <- c("species","decimalLatitude","decimalLongitude",
                  "basisOfRecord","speciesKey","iucnRedListCategory",
                  "kingdom", "phylum", "order", "family")

all_keys <- na.omit(sps_list$GBIF_ID)

cl <- parallel::makeCluster(cores) 
parallel::clusterExport(cl, c("all_keys","sps_list","dir_sp_occ_raw","col_interest"))

download_GBIF_verbose <- pbsapply(1:length(all_keys), function(i){
  
  key_i <- all_keys[i]
  
  file_to_save <- here::here(dir_sp_occ_raw,paste0(key_i,".csv"))
  
  if(file.exists(file_to_save)){
    return("worked")
  } else {
    tmp <- try({
      rgbif::occ_search(key_i,
                        hasCoordinate = TRUE,
                        hasGeospatialIssue = FALSE,
                        occurrenceStatus = "PRESENT")$data
    }, silent = TRUE)
    
    if(any(class(tmp)=="try-error") | is.null(tmp)){
      
      return("error")
      
    } else {
      
      if(nrow(tmp) >= 30){
        
        tmp <- tmp[,which(names(tmp)%in%col_interest)]
        write.csv(tmp, file_to_save) 
        
        return("worked")
        
      } else {
        
        return("< 30 occurrences")
        
      }
    }
  } 
  
}, cl = cl)

parallel::stopCluster(cl)

download_GBIF_verbose <- data.frame(int = 1:length(all_keys),
                                    key = all_keys,
                                    verb = download_GBIF_verbose)
# head(download_GBIF_verbose)
write.csv(download_GBIF_verbose, 
          here(dir_status,"GBIF_status.csv"),
          row.names = FALSE)

# Clean occurrences ----
# This removes duplicates and other potentially erroneous coordinates
cat("Clean occurrences using CoordinateCleaner\n")

ocean <- rast(here(dir_layers_pres,"BO_sstmean.tif"))

sps_occ_list <- list.files(dir_sp_occ_raw)

CoordinateCleaner_verbose <- c()

for(i in 1:length(sps_occ_list)){
  
  cat("\r cleaning sps", i ,"from", length(sps_occ_list))
  
  x = sps_occ_list[i]
  
  file_to_save <- here::here(dir_sp_occ,x)
  
  if(!file.exists(file_to_save)){
    
    sps_occ <- read.csv(here::here(dir_sp_occ_raw,x))
    
    if(nrow(sps_occ) >= 30){
      
      # use CoordinateCleaner to remove potential mistakes
      sps_occ <- CoordinateCleaner::clean_coordinates(
        x = sps_occ,
        lon = "decimalLongitude",
        lat = "decimalLatitude",
        tests = c("centroids", "duplicates", "equal","zeros"),
        value = "clean",
        verbose = FALSE)
      
      if(nrow(sps_occ) >= 30){
        # remove occurrences falling on land
        test <- terra::vect(data.frame(sps_occ),
                            geom = c("decimalLongitude","decimalLatitude"))
        test.seas <- terra::extract(ocean, test, cells = TRUE)
        
        # get only coordinates in the ocean
        test <- which(is.na(test.seas))
        if(length(test>0)){
          sps_occ <- sps_occ[-test,]
        }
        
        # test <- terra::vect(data.frame(sps_occ),
        #              geom = c("decimalLongitude","decimalLatitude"))
        # plot(globe);plot(test,add=TRUE,col="red")
        
        if(nrow(sps_occ) >= 30){
          
          # One occurrence per grid-cell
          all_cells <- test.seas %>%
            filter(!is.na(BO_sstmean)) %>%
            select(cell) %>%
            unique() %>% pull()
          
          sps_occ <- xyFromCell(ocean, all_cells)
          # test <- terra::vect(data.frame(sps_occ),
          #              geom = c("x","y"))
          # plot(globe);plot(test,add=TRUE,col="red")
          
          if(nrow(sps_occ) >= 30){
            
            # save clean data
            write.csv(sps_occ,
                      file_to_save,
                      row.names = FALSE)
            
            CoordinateCleaner_verbose[i] <- "worked"
          }
          CoordinateCleaner_verbose[i] <- "< 30 occurrences after cleaning + keeping only ocean"
        }
        CoordinateCleaner_verbose[i] <- "< 30 occurrences after cleaning + keeping only ocean"
      }
      CoordinateCleaner_verbose[i] <- "< 30 occurrences after cleaning (potentially has land occurrences)"
    }
    CoordinateCleaner_verbose[i] <- "< 30 occurrences raw"
  }
  CoordinateCleaner_verbose[i] <- "worked"
}

CoordinateCleaner_verbose <- data.frame(int = 1:length(CoordinateCleaner_verbose),
                                        verb = CoordinateCleaner_verbose)
head(CoordinateCleaner_verbose)

write.csv(CoordinateCleaner_verbose, 
          here(dir_status,"CoordinateCleaner_status.csv"),
          row.names = FALSE)

cat("\nDone!")

