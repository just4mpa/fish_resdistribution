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

# install.packages("biomod2", dependencies = TRUE)

list.of.packages <- c("terra","rnaturalearthdata","rnaturalearth",
                      "biomod2","gbm","maxnet","gam",
                      "tidyverse","tictoc","tidyterra","dismo",
                      "ggplot2","data.table",
                      "parallelly","dplyr","foreach","doParallel")

load_packages(list.of.packages)

# Get world shapefile
globe <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sv")

# Species list
args <- commandArgs(trailingOnly = TRUE)
sp_i_ID <- as.character(args[1])  # species name passed by SLURM
# sp_i_ID <- as.character(2340972)

# Register parallel backend
N_cpus <- parallelly::availableCores()

out_verbose <- data.frame(PCA_env = NA,
                          SDM_fit = NA,
                          SDM_proj_pres = NA,
                          SDM_proj_fut = NA)

out_verbose  <- try({
  
  verbose_file <- here(dir_sdms,sp_i_ID,paste0(sp_i_ID,"_SDM_verbose.csv"))
  
  run <- TRUE
  if(file.exists(verbose_file)){
    test <- read.csv(verbose_file)
    if(all(test[1,] == "Done")) run <- FALSE
  }
  
  if(run){
    
    output_dir <- here(dir_sdms,sp_i_ID)
    dir.create(output_dir, showWarnings = FALSE)
    
    # Load species occurrence data ----
    sp_occ_file <- here(dir_sp_occ,paste0(sp_i_ID,'.csv'))
    sp_occ <- read.csv(sp_occ_file)
    sp_occ_vect <- vect(sp_occ, geom = c("x","y"))
    
    # Generate background (calibration) area ----
    # Load marine ecoregions (MEOW)
    BA_area <- get_BA_shp(dir_data,sp_occ_vect)
    # plot(globe);plot(BA_area,add=TRUE);plot(sp_occ_vect, add = TRUE)
    
    # Load and crop the environmental layers ----
    ## Present ----
    pres_layers <- list.files(dir_layers_pres,full.names = TRUE) %>% rast
    window(pres_layers) <- ext(BA_area)
    pres_layers <- mask(pres_layers,BA_area)
    names(pres_layers) <- gsub("BO_|BO22_","",names(pres_layers))
    # plot(pres_layers[[1]]);plot(sp_occ_vect, add = TRUE)
    
    ## Future ----
    fut_layers <- list.files(dir_layers_fut,full.names = TRUE) %>% rast
    window(fut_layers) <- ext(BA_area)
    fut_layers <- mask(fut_layers,BA_area)
    names(fut_layers) <- gsub("BO_A2_2100_|BO22_RCP45_2100_|BO22_","",names(fut_layers))
    # plot(fut_layers[[1]])
    
    # Simplify environmental layers ----
    
    env_data <- as.data.frame(pres_layers) %>% na.omit()
    env_PCA <- prcomp(env_data, retx = TRUE, scale. = TRUE, center = TRUE)
    cumvar <- cumsum(env_PCA$sdev^2) / sum(env_PCA$sdev^2)
    num_pc <- which(cumvar >= 0.9)[1]
    env_PCA <- prcomp(env_data, retx = TRUE, scale. = TRUE, center = TRUE, rank. = num_pc)
    
    # Create new raster for the present layers
    pres_layers_pca <- predict(pres_layers, env_PCA)
    fut_layers_pca <- predict(fut_layers, env_PCA)
    names(pres_layers_pca) <- names(fut_layers_pca) <- paste0("PC", 1:num_pc)
    
    out_verbose$PCA_env <- "Done"
    
    # Generate pseudo-absence data ----
    n_pres <- nrow(sp_occ)
    n_pa <- max(100, n_pres)
    
    # 1) get cells at the BA
    BA <- ifel(!is.na(pres_layers[[1]]), 1, 0)
    cells_BA <- terra::cells(BA, 1)[[1]]
    # 2) remove cells with occurrences
    cells_occ <- terra::cellFromXY(object = BA, xy = sp_occ)
    cells_BA <- setdiff(cells_BA, cells_occ)
    # 3) sample random cells for pseudo-absences
    n_pa <- min(n_pa, length(cells_BA))
    set.seed(666)
    PA <- xyFromCell(BA, sample(cells_BA, n_pa)) %>% data.frame()
    
    # Generate pres/abs data ---
    PresAbs <- rbind(data.frame(pa = 1, sp_occ),
                     data.frame(pa = 0, PA))
    PresAbs_env <- terra::extract(pres_layers_pca,PresAbs[,-1])
    PresAbs <- cbind(PresAbs,PresAbs_env[,-1]) %>% na.omit()
    
    # save basic data
    write.csv(data.frame(Species = sp_i_ID,
                         N_occ = length(which(PresAbs$pa==1)),
                         N_background = length(which(PresAbs$pa==0))),
              here::here(output_dir,paste0(sp_i_ID,"_SDM_info.csv")),
              row.names = FALSE)
    
    # Fit SDMs ----
    cat("\nFitting SDMs\n")
    
    file.out <- here(dir_sdms,sp_i_ID,paste0(sp_i_ID, ".AllModels.ensemble.models.out"))
    if(file.exists(file.out)) {
      ens_model_sp <- get(load(file.out))
      out_verbose$SDM_fit <- "Done"
    } else {
      
      resp <- as.numeric(PresAbs$pa)
      resp[resp==0] <- NA
      
      # Format Data
      data_sp <- BIOMOD_FormatingData(
        resp.name = sp_i_ID, 
        resp.var = resp, 
        expl.var = data.frame(PresAbs[,-1:-3]), 
        resp.xy = data.frame(PresAbs[,2:3]),
        dir.name = dir_sdms,
        na.rm = TRUE,
        PA.strategy = "random",
        PA.nb.rep = 1,
        PA.nb.absences = length(which(is.na(resp))))
      
      # Fit sdms
      # c("GLM","GBM","GAM","MAXNET")
      
      # Model single models
      model_sp <- BIOMOD_Modeling(
        bm.format = data_sp, 
        modeling.id = 'AllModels',
        models = "MAXNET",
        CV.perc = 0.7,
        CV.strategy = "kfold",
        CV.k = 3,
        CV.do.full.models = FALSE,
        prevalence = 0.5,
        metric.eval = "TSS",
        scale.models = TRUE,
        nb.cpu = N_cpus
      )
      
      # Model ensemble models
      ens_model_sp <- BIOMOD_EnsembleModeling(
        bm.mod = model_sp, 
        models.chosen = 'all',
        em.by = 'all',
        metric.select = "TSS",
        metric.eval = "TSS")
      
      # Evaluate models ----
      write.csv(get_evaluations(ens_model_sp), 
                here::here(output_dir,paste0(sp_i_ID,"_ens_mod_eval.csv")),
                row.names = FALSE)
      
      write.csv(get_evaluations(model_sp), 
                here::here(output_dir,paste0(sp_i_ID,"_mod_eval.csv")),
                row.names = FALSE)
      
      out_verbose$SDM_fit <- "Done"
      
    }
    
    # Project to the present ----
    file.out <- here(dir_sdms,
                     sp_i_ID,
                     paste0("proj_",sp_i_ID, "_fut_proj"),
                     paste0(sp_i_ID,".",sp_i_ID, "_fut_proj.ensemble.projection.out"))
    if(!file.exists(file.out)) {
      
      projname <- paste0(sp_i_ID,"_pres_proj")
      
      pres_proj_m <- BIOMOD_EnsembleForecasting(
        bm.em = ens_model_sp, 
        new.env = pres_layers_pca,
        proj.name = projname,
        nb.cpu = N_cpus,
        keep.in.memory = FALSE
      )
      # plot(pres_proj_m)
      
      out_verbose$SDM_proj_pres <- "Done"
      
    }
    
    # Project to the future ----
    file.out <- here(dir_sdms,
                     sp_i_ID,
                     paste0("proj_",sp_i_ID, "_fut_proj"),
                     paste0(sp_i_ID,".",sp_i_ID, "_fut_proj.ensemble.projection.out"))
    if(!file.exists(file.out)) {
      
      projname <- paste0(sp_i_ID,"_fut_proj")
      
      fut_proj_m <- BIOMOD_EnsembleForecasting(
        bm.em = ens_model_sp, 
        new.env = fut_layers_pca,
        proj.name = projname,
        nb.cpu = N_cpus,
        keep.in.memory = FALSE
      )
      # plot(fut_proj_m)
      
      out_verbose$SDM_proj_fut <- "Done"
      
    }
    
    write.csv(out_verbose,
              verbose_file,
              row.names = FALSE)
    
  }
  
})


cat("Done!")

##################### -

