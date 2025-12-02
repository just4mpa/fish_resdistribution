library(here)

slurm_job_singularity <- function(jobdir, logdir, sptogo, args,
                                  N_Nodes = 1, tasks_per_core = 1, cores = 1, time = "24:00:00", memory = "8G", partition = "normal",
                                  singularity_image,Rscript_file){
  
  # Start writing to this file
  sink(here::here(jobdir,paste0(sptogo,'.sh')))
  
  # the basic job submission script is a bash script
  cat("#!/bin/bash\n")
  
  cat("#SBATCH -N",N_Nodes,"\n")
  cat("#SBATCH -n",tasks_per_core,"\n")
  cat("#SBATCH -c",cores,"\n")
  cat("#SBATCH --partition",partition,"\n")
  cat("#SBATCH --mem=",memory,"\n", sep="")
  cat("#SBATCH --time=",time,"\n", sep="")
  
  cat("#SBATCH --job-name=",sptogo,"\n", sep="")
  cat("#SBATCH --output=",here::here(logdir,paste0(sptogo,".out")),"\n", sep="")
  cat("#SBATCH --error=",here::here(logdir,paste0(sptogo,".err")),"\n", sep="")
  # cat("#SBATCH --mail-type=ALL\n")
  # cat("#SBATCH --mail-user=brunno.oliveira@fondationbiodiversite.fr\n")
  
  cat(paste0("IMG_DIR='",singularity_image,"'\n"))
  
  cat("module purge\n")
  cat("module load utils/apptainer\n")
  cat("module load gcc\n")
  cat("module load math/r\n")
  
  cat("apptainer exec", singularity_image, "Rscript", Rscript_file, args,"\n", sep=" ")
  
  # Close the sink!
  sink()
  
  # Submit to run on cluster
  system(paste("sbatch", here::here(jobdir, paste0(sptogo,'.sh'))))
  
}

## Select the best partition for job submission
ter_partitions <- c("normal-amd","normal","bigmem-amd","bigmem")
limits <- data.frame(partition = ter_partitions,
                     max_mem_node = c(250, 125, 1000, 500),
                     max_mem_user = c(2000, 2000, 2000, 2000),
                     max_cpu = c(512, 448, 128, 112))

select_partition_inset <- function(request_mem, request_cpu, limits){
  
  # filter by node constraints
  limits <- limits %>% filter(max_mem_node >= request_mem)
  
  # get running partitions for this user
  test_partition <- system("squeue --format='%.50P' --me", intern = TRUE)[-1]
  
  if(length(test_partition) == 0){
    return(limits[1, , drop=FALSE])
  }
  
  test_partition <- gsub(" ", "", test_partition)
  using <- data.frame(table(test_partition))
  colnames(using)[1] <- "partition"
  
  using <- merge(limits, using, by="partition", all.x=TRUE)
  
  # replace NA counts with zero
  using$Freq[is.na(using$Freq)] <- 0
  
  using$using_mem <- request_mem * using$Freq
  using$using_cpu <- request_cpu * using$Freq
  
  tmp <- using %>% 
    filter(using_mem <= max_mem_user & 
             using_cpu <= max_cpu)
  
  return(tmp)
}

select_partition <- function(request_mem, request_cpu, limits){
  repeat {
    test <- select_partition_inset(request_mem, request_cpu, limits)
    if(nrow(test) > 0){
      return(as.character(test$partition[1]))
    }
    
    cat("\nNo resources available, waiting 10s...\n")
    Sys.sleep(10)
  }
}


get_meow_shp <- function(varsdir){
  # create dir to save ecoregion 
  if(!dir.exists(here::here(wd,varsdir,"MEOW"))){
    dir.create(here::here(wd,varsdir,"MEOW"),recursive = TRUE)
  }
  
  # check if shape was downloaded and if not download it
  if(file.exists(here::here(wd,varsdir,"MEOW","MEOW.zip"))){
    ecoreg_shp <- terra::vect(here::here(wd,varsdir,"MEOW", "DataPack-14_001_WCMC036_MEOW_PPOW_2007_2012_v1/01_Data/WCMC-036-MEOW-PPOW-2007-2012-NoCoast.shp"))
    return(ecoreg_shp)
  } else {
    cat("Downloading MEOW shape file")
    download.file(url = "https://datadownload-production.s3.amazonaws.com/WCMC036_MEOW_PPOW_2007_2012_v1.zip",
                  destfile = here::here(wd,varsdir,"MEOW","MEOW.zip"))
    ## Unzip
    unzip(zipfile = here::here(wd,varsdir,"MEOW","MEOW.zip"), 
          exdir = here::here(wd,varsdir,"MEOW"))
    ecoreg_shp <- terra::vect(here::here(wd,varsdir,"MEOW", "DataPack-14_001_WCMC036_MEOW_PPOW_2007_2012_v1/01_Data/WCMC-036-MEOW-PPOW-2007-2012-NoCoast.shp"))
    return(ecoreg_shp)
  }
}

get_BA_shp <- function(varsdir,PresAbs){
  
  meow_shp <- get_meow_shp(varsdir)
  # At which ecoregions/realms the species occur?
  subdata <- terra::extract(meow_shp, PresAbs)
  # Subset shape file to the ecoregions
  ecoreg <- na.omit(unique(subdata$ECOREGION))
  ecoreg_shp <- terra::subset(meow_shp, meow_shp$ECOREGION %in% ecoreg)
  if(any(is.na(subdata$ECOREGION))){
    prov <- na.omit(unique(subdata$PROVINC))
    prov_shp <- terra::subset(meow_shp, meow_shp$PROVINC %in% prov)
    ecoreg_shp <- rbind(ecoreg_shp, prov_shp)
  } 
  # plot(ecoreg_shp,col="black")
  # plot(vect(PresAbs,geom=c("x","y")),col="red",add=TRUE)
  # dev.off()
  
  
  return(ecoreg_shp)
}


# Impute traits based on taxonomy hierarchy
impute_traits_taxonomy <- function(df, 
                                   traits = c("Benthic", "Pelagic", "Superficial"),
                                   taxonomy_order = c("Genus", "Family", "Order", "Class", "SuperClass")) {
  
  df_out <- df
  
  for (trait in traits) {
    # Loop rows where trait is NA
    na_idx <- which(is.na(df_out[[trait]]))
    
    for (i in na_idx) {
      # Try hierarchy
      for (level in taxonomy_order) {
        group_val <- df_out[i, level]
        
        if (!is.na(group_val)) {
          # Compute mean in taxonomic group
          group_mean <- mean(df_out[df_out[[level]] == group_val, trait], na.rm = TRUE)
          
          # If mean exists, replace & stop
          if (!is.nan(group_mean)) {
            df_out[i, trait] <- group_mean
            break
          }
        }
      }
    }
  }
  
  return(df_out)
}


# only use this if in the terminal


if(computer=="buca"){
  my_system <- Sys.info()[["sysname"]]
  if (my_system == "Linux") {
    my_lib <- "/home/just/R/x86_64-pc-linux-gnu-library/4.2"
    
    # Create the directory if it doesn't exist
    if (!dir.exists(my_lib)) dir.create(my_lib, recursive = TRUE)
    
    # Prepend to .libPaths
    .libPaths(c(my_lib, .libPaths()))
    wd <- "/home/just/fish_redistribution"
  }
}
if(computer=="matrics"){
  if(Sys.info()[["sysname"]] == "Linux"){
    if(Sys.getenv("APPTAINER_NAME") == "brunnospatial.sif"){
      .libPaths("/usr/local/lib/R/site-library")
    } else {
      .libPaths("/users/boliveira/R/libs")
    }
  }
  wd <- "/users/boliveira/fish_redistribution"
}


dir_data <- here(wd,"Data")
dir_code <- here(wd,"R")

dir_layers_pres <- here(dir_data,"ocean_layers/present")
dir.create(dir_layers_pres, recursive = TRUE, showWarnings = FALSE)

dir_layers_fut <- here(dir_data,"ocean_layers/future")
dir.create(dir_layers_fut, recursive = TRUE, showWarnings = FALSE)

dir_sp_occ <- here(dir_data,"sp_occ_dir")
dir.create(dir_sp_occ, recursive = TRUE, showWarnings = FALSE)

dir_status <- here(dir_data,"status")
dir.create(dir_status, recursive = TRUE, showWarnings = FALSE)

dir_sdms <- here(dir_data,"SDM")
dir.create(dir_sdms, recursive = TRUE, showWarnings = FALSE)

dir_temp <- here(dir_data,"temp")
dir.create(dir_temp, recursive = TRUE, showWarnings = FALSE)

dir.create(dir_code, recursive = TRUE, showWarnings = FALSE)
