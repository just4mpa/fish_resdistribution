library(here)


# make bivariate maps
bivariate.map <- function(rasterx, rastery, colormatrix = colmat) {
  
  require(terra)
  require(classInt)
  
  colormatrix <- colormatrix[[1]]
  
  quanx <- rasterx[]
  tempx <- data.frame(quanx, quantile = rep(NA, length(quanx)))
  brks <- with(tempx, classIntervals(quanx,
                                     n = attr(colormatrix, "nbreaks"),
                                     na.rm=TRUE,
                                     style = attr(colormatrix, "breakstyle"))$brks)
  ## Add (very) small amount of noise to all but the first break
  ## https://stackoverflow.com/a/19846365/1710632
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r1 <- within(tempx, quantile <- base::cut(quanx,
                                            breaks = brks,
                                            labels = 2:length(brks),
                                            include.lowest = TRUE))
  quantr <- data.frame(r1[, 2])
  quany <- rastery[]
  tempy <- data.frame(quany, quantile = rep(NA, length(quany)))
  brks <- with(tempy, classIntervals(quany,
                                     n = attr(colormatrix, "nbreaks"),
                                     na.rm=TRUE,
                                     style = attr(colormatrix, "breakstyle"))$brks)
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r2 <- within(tempy, quantile <- base::cut(quany,
                                            breaks = brks,
                                            labels = 2:length(brks),
                                            include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colormatrix
  cn <- unique(colormatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1, col.matrix2[i] <- which(
             col.matrix2[i] == cn
           )[1]
    )
  }
  v1 <- as.numeric.factor(quantr[, 1])
  v2 <- as.numeric.factor(quantr2[, 1])
  cols <- pbapply::pbsapply(1:length(v1), function(i) {
    as.numeric(col.matrix2[v2[i],v1[i]])
  })
  r <- rasterx
  r[] <- cols
  return(r)
}


# Make legend for bivariate map
bivariate.map <- function(rasterx, rastery, colormatrix = colmat) {
  
  require(terra)
  require(classInt)
  
  colormatrix <- colormatrix[[1]]
  
  quanx <- rasterx[]
  tempx <- data.frame(quanx, quantile = rep(NA, length(quanx)))
  brks <- with(tempx, classIntervals(quanx,
                                     n = attr(colormatrix, "nbreaks"),
                                     na.rm=TRUE,
                                     style = attr(colormatrix, "breakstyle"))$brks)
  ## Add (very) small amount of noise to all but the first break
  ## https://stackoverflow.com/a/19846365/1710632
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r1 <- within(tempx, quantile <- base::cut(quanx,
                                            breaks = brks,
                                            labels = 2:length(brks),
                                            include.lowest = TRUE))
  quantr <- data.frame(r1[, 2])
  quany <- rastery[]
  tempy <- data.frame(quany, quantile = rep(NA, length(quany)))
  brks <- with(tempy, classIntervals(quany,
                                     n = attr(colormatrix, "nbreaks"),
                                     na.rm=TRUE,
                                     style = attr(colormatrix, "breakstyle"))$brks)
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r2 <- within(tempy, quantile <- base::cut(quany,
                                            breaks = brks,
                                            labels = 2:length(brks),
                                            include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colormatrix
  cn <- unique(colormatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1, col.matrix2[i] <- which(
             col.matrix2[i] == cn
           )[1]
    )
  }
  v1 <- as.numeric.factor(quantr[, 1])
  v2 <- as.numeric.factor(quantr2[, 1])
  cols <- pbapply::pbsapply(1:length(v1), function(i) {
    as.numeric(col.matrix2[v2[i],v1[i]])
  })
  r <- rasterx
  r[] <- cols
  return(r)
}


# Make legend for bivariate map
colmat <- function(nbreaks = 3, breakstyle = "quantile",
                   upperleft = "#0096EB", upperright = "#820050",
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", 
                   x_breaks = NULL, y_breaks = NULL,
                   plotLeg = TRUE,
                   saveLeg = TRUE) {
  library(tidyverse)
  require(ggplot2)
  require(classInt)
  if (breakstyle == "sd") {
    warning("SD breaks style cannot be used.\nWill not always return the correct number of breaks.\nSee classInt::classIntervals() for details.\nResetting to quantile",
            call. = FALSE, immediate. = FALSE)
    breakstyle <- "quantile"}
  # The colours can be changed by changing the HEX codes for:
  # upperleft, upperright, bottomleft, bottomright
  # From http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  # upperleft = "#64ACBE", upperright = "#574249", bottomleft = "#E8E8E8", bottomright = "#C85A5A",
  # upperleft = "#BE64AC", upperright = "#3B4994", bottomleft = "#E8E8E8", bottomright = "#5AC8C8",
  # upperleft = "#73AE80", upperright = "#2A5A5B", bottomleft = "#E8E8E8", bottomright = "#6C83B5",
  # upperleft = "#9972AF", upperright = "#804D36", bottomleft = "#E8E8E8", bottomright = "#C8B35A",
  # upperleft = "#DA8DC8", upperright = "#697AA2", bottomleft = "#E8E8E8", bottomright = "#73BCA0",
  # Similar to Teuling, Stockli, Seneviratnea (2011) [https://doi.org/10.1002/joc.2153]
  # upperleft = "#F7900A", upperright = "#993A65", bottomleft = "#44B360", bottomright = "#3A88B5",
  # Viridis style
  # upperleft = "#FEF287", upperright = "#21908D", bottomleft = "#E8F4F3", bottomright = "#9874A1",
  # Similar to Fjeldsa, Bowie, Rahbek 2012
  # upperleft = "#34C21B", upperright = "#FFFFFF", bottomleft = "#595757",  bottomright = "#A874B8",
  # Default from original source
  # upperleft = "#0096EB", upperright = "#820050", bottomleft= "#BEBEBE", bottomright = "#FFE60F",
  my.data <- seq(0, 1, .01)
  # Default uses terciles (Lucchesi and Wikle [2017] doi: 10.1002/sta4.150)
  my.class <- classInt::classIntervals(my.data,
                                       n = nbreaks,
                                       style = breakstyle)
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- classInt::findColours(my.class, my.col)
  }
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>%
    mutate("Y" = row_number()) %>%
    mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>%
    pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>%
    mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(.$Y)) %>%
    dplyr::select(-c(4)) %>%
    mutate("Y" = rep(seq(from = 1, to = nbreaks, by = 1), each = nbreaks),
           "X" = rep(seq(from = 1, to = nbreaks, by = 1), times = nbreaks)) %>%
    mutate("UID" = row_number())
  # Use plotLeg if you want a preview of the legend
  if (plotLeg | saveLeg) {
    p <- ggplot(col.matrix.plot, aes((X*2)/10, (Y*2)/10, fill = HEXCode)) +
      geom_raster() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, 1, by=0.2), labels = NULL) +
      scale_y_continuous(breaks = seq(0, 1, by=0.2),labels = NULL)+
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 10, colour = "black",hjust = 0.5,
                                      vjust = 1),
            axis.text = element_text(size = 8),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
  }
  if (plotLeg) {
    print(p)
  }
  seqs <- seq(0, 100, (100 / nbreaks))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  attr(col.matrix, "breakstyle") <- breakstyle
  attr(col.matrix, "nbreaks") <- nbreaks
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    return(list(col.matrix = col.matrix,
                plot = p))
  } else {
    return(list(col.matrix = col.matrix,
                plot = NULL))
  }
}




# Function to compute Nutrient Quality Score (NQ)
compute_NQ <- function(nutrient_df, rda, weights = NULL, species_col = NULL) {
  # nutrient_df: tibble, rows = species, cols = nutrients
  # rda: named numeric vector, RDA per nutrient
  # weights: optional named numeric vector; if NULL, equal weights are used
  # species_col: optional column name in nutrient_df with species names
  
  # Extract species names if provided
  if(!is.null(species_col)) {
    species <- nutrient_df[[species_col]]
    nutrient_df <- nutrient_df %>% select(-all_of(species_col))
  } else {
    species <- seq_len(nrow(nutrient_df))
  }
  
  # Convert nutrient data to numeric matrix
  nutrient_mat <- as.matrix(nutrient_df)
  
  # Check names
  if(!all(colnames(nutrient_mat) %in% names(rda))) {
    stop("Column names of nutrient_df must match names of RDA vector")
  }
  
  # Use equal weights if not provided
  if(is.null(weights)) {
    weights <- rep(1 / ncol(nutrient_mat), ncol(nutrient_mat))
    names(weights) <- colnames(nutrient_mat)
  }
  
  # Reorder weights to match nutrient columns
  weights <- weights[colnames(nutrient_mat)]
  
  # Compute proportion of RDA per nutrient
  prop_RDA <- sweep(nutrient_mat, 2, rda, FUN = function(x, y) x / 100 / y)
  
  # Weighted sum to get NQ
  NQ <- as.numeric(prop_RDA %*% weights)
  
  # Return tibble with species and NQ
  tibble(
    sps = species,
    NQ = NQ
  )
}



# Definir CRS Mollweide
moll_crs <- "+proj=moll +datum=WGS84"

# Function that loads a vector of packages. It installs packages if not already installed.
load_packages <- function(list_of_packages){
  new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  sapply(list_of_packages, require, character.only = TRUE)
}


# Submits jobs in slurm computer
slurm_job_singularity <- function(jobdir, logdir, jobname, args,
                                  N_Nodes = 1, tasks_per_core = 1, cores = 1, time = "24:00:00", memory = "8G", partition = "normal",
                                  singularity_image,Rscript_file){
  
  # Start writing to this file
  sink(here::here(jobdir,paste0(jobname,'.sh')))
  
  # the basic job submission script is a bash script
  cat("#!/bin/bash\n")
  
  cat("#SBATCH -N",N_Nodes,"\n")
  cat("#SBATCH -n",tasks_per_core,"\n")
  cat("#SBATCH -c",cores,"\n")
  cat("#SBATCH --partition",partition,"\n")
  cat("#SBATCH --mem=",memory,"\n", sep="")
  cat("#SBATCH --time=",time,"\n", sep="")
  
  cat("#SBATCH --job-name=",jobname,"\n", sep="")
  cat("#SBATCH --output=",here::here(logdir,paste0(jobname,".out")),"\n", sep="")
  cat("#SBATCH --error=",here::here(logdir,paste0(jobname,".err")),"\n", sep="")
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
  system(paste("sbatch", here::here(jobdir, paste0(jobname,'.sh'))))
  
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
  if(!dir.exists(here::here(varsdir,"MEOW"))){
    dir.create(here::here(varsdir,"MEOW"),recursive = TRUE)
  }
  
  # check if shape was downloaded and if not download it
  if(file.exists(here::here(varsdir,"MEOW","MEOW.zip"))){
    ecoreg_shp <- terra::vect(here::here(varsdir,"MEOW", "DataPack-14_001_WCMC036_MEOW_PPOW_2007_2012_v1/01_Data/WCMC-036-MEOW-PPOW-2007-2012-NoCoast.shp"))
    return(ecoreg_shp)
  } else {
    cat("Downloading MEOW shape file")
    download.file(url = "https://datadownload-production.s3.amazonaws.com/WCMC036_MEOW_PPOW_2007_2012_v1.zip",
                  destfile = here::here(varsdir,"MEOW","MEOW.zip"))
    ## Unzip
    unzip(zipfile = here::here(varsdir,"MEOW","MEOW.zip"), 
          exdir = here::here(varsdir,"MEOW"))
    ecoreg_shp <- terra::vect(here::here(varsdir,"MEOW", "DataPack-14_001_WCMC036_MEOW_PPOW_2007_2012_v1/01_Data/WCMC-036-MEOW-PPOW-2007-2012-NoCoast.shp"))
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


computer = Sys.info()["nodename"]
if(computer=="just"){
  my_lib <- "/home/just/R/x86_64-pc-linux-gnu-library/4.2"
  # Create the directory if it doesn't exist
  if (!dir.exists(my_lib)) dir.create(my_lib, recursive = TRUE)
  # Prepend to .libPaths
  .libPaths(c(my_lib, .libPaths()))
  wd <- "/home/just/fish_redistribution"
}

if(computer=="login02"){
  if(Sys.getenv("APPTAINER_NAME") == "brunnospatial.sif"){
    .libPaths("/usr/local/lib/R/site-library")
  } else {
    .libPaths("/users/boliveira/R/libs")
  }
  wd <- "/users/boliveira/fish_redistribution"
}

dir_data <- here(wd,"Data")
dir_code <- here(wd,"R")

dir_layers_pres <- here(dir_data,"ocean_layers","present")
dir.create(dir_layers_pres, recursive = TRUE, showWarnings = FALSE)

dir_layers_fut <- here(dir_data,"ocean_layers","future")
dir.create(dir_layers_fut, recursive = TRUE, showWarnings = FALSE)

dir_sp_occ <- here(dir_data,"sp_occ")
dir.create(dir_sp_occ, recursive = TRUE, showWarnings = FALSE)

dir_status <- here(dir_data,"status")
dir.create(dir_status, recursive = TRUE, showWarnings = FALSE)

dir_sdms <- here(dir_data,"SDM")
dir.create(dir_sdms, recursive = TRUE, showWarnings = FALSE)

dir_sp_range_pres <- here(dir_data,"sp_range","present")
dir.create(dir_sp_range_pres, recursive = TRUE, showWarnings = FALSE)

dir_sp_range_fut <- here(dir_data,"sp_range","future")
dir.create(dir_sp_range_fut, recursive = TRUE, showWarnings = FALSE)

dir_temp <- here(dir_data,"temp")
dir.create(dir_temp, recursive = TRUE, showWarnings = FALSE)

dir.create(dir_code, recursive = TRUE, showWarnings = FALSE)
