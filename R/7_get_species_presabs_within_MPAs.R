# Load libraries ----

rm(list=ls())
gc()

# install.packages("qs")

library(here)

computer = Sys.info()["nodename"]
if(computer == "BRUNNO-THINKPAD" | computer == "just"){
  wd <- here()
} else {
  wd <- "/users/boliveira/fish_redistribution"
}

source(here(wd,"R/source_code.R"))

library(terra)
library(here)
library(sf)
library(pbapply)
library(qs)
library(parallel)

# Load MPA data ----
MPA_cells <- read.csv(here(dir_data,"MPA_cells.csv"))
MPA_cells <- unique(MPA_cells$cell)

# Make species presabs long format ----
all_sps <- list.files(dir_sdms)

N_cpus <- parallelly::availableCores(logical = FALSE)
cl <- makeCluster(N_cpus)

project_terra <- terra::project
cells_terra <- terra::cells
clusterExport(cl, c("dir_sdms","dir_layers_pres","dir_layers_fut",
                    "dir_sp_range_pres","dir_sp_range_fut",
                    "here","rast","ifel","project_terra","cells_terra"))

## Present ----

species_pres_cells <- pblapply(all_sps, function(x){
  try({
    file_to_save <- here(dir_sp_range_pres,paste0(x,".csv"))
    if(file.exists(file_to_save)){
      ocean <- rast(here(dir_layers_pres,"BO_sstmean.tif"))
      sp_i_rast_file <- here(dir_sdms,x,
                             paste0("proj_",x,"_pres_proj"),
                             paste0("proj_",x,"_pres_proj_",x,"_ensemble.tif"))
      sps_i <- rast(sp_i_rast_file)
      # make raster binary (pres-abs)
      sp_i_threshold_file <- here(dir_sdms,x,paste0(x,"_ens_mod_eval.csv"))
      sps_i_threshold <- read.csv(sp_i_threshold_file)
      sps_i_threshold <- sps_i_threshold$cutoff
      sps_i[sps_i < sps_i_threshold] <- NA
      sps_i[sps_i >= sps_i_threshold] <- 1
      # project to the extent of ocean so that cells will overlap
      sps_i_bin <- project_terra(sps_i,ocean)
      sps_i_cells <- cells_terra(sps_i_bin,1)[[1]]
      sps_i_cells <- data.frame(sps = x, cell = sps_i_cells)
      write.csv(sps_i_cells,
                file_to_save)
    }
  })
}, cl = cl)

sps_cells_pres_file <- here::here(dir_data,"sps_cells_pres.qs")
if(file.exists(sps_cells_pres_file)){
  
  sps_cells_pres <- qread(sps_cells_pres_file)
  
} else {
  
  sps_cells_pres <- list.files(dir_sp_range_pres,full.names = TRUE)
  sps_cells_pres <- lapply(sps_cells_pres,read.csv)
  sps_cells_pres <- do.call(rbind,sps_cells_pres)
  sps_cells_pres <- sps_cells_pres[,c("sps","cell")]
  sps_cells_pres <- unique(sps_cells_pres)
  # save
  qs::qsave(sps_cells_pres, here::here(dir_data,"sps_cells_pres.qs"))
  
}

head(sps_cells_pres)
length(unique(sps_cells_pres$sps))

# get species within MPAs
sps_cells_pres_MPAs <- sps_cells_pres %>%
  dplyr::filter(cell %in% MPA_cells)

qs::qsave(sps_cells_pres_MPAs, here::here(dir_data,"sps_cells_pres_MPA.qs"))

gc()

## Future ----

species_fut_cells <- pblapply(all_sps, function(x){
  try({
    file_to_save <- here(dir_sp_range_pres,paste0(x,".csv"))
    if(file.exists(file_to_save)){
      ocean <- rast(here(dir_layers_pres,"BO_sstmean.tif"))
      sp_i_rast_file <- here(dir_sdms,x,
                             paste0("proj_",x,"_fut_proj"),
                             paste0("proj_",x,"_fut_proj_",x,"_ensemble.tif"))
      sps_i <- rast(sp_i_rast_file)
      # make raster binary (fut-abs)
      sp_i_threshold_file <- here(dir_sdms,x,paste0(x,"_ens_mod_eval.csv"))
      sps_i_threshold <- read.csv(sp_i_threshold_file)
      sps_i_threshold <- sps_i_threshold$cutoff
      sps_i[sps_i < sps_i_threshold] <- NA
      sps_i[sps_i >= sps_i_threshold] <- 1
      # project to the extent of ocean so that cells will overlap
      sps_i_bin <- project_terra(sps_i,ocean)
      sps_i_cells <- cells_terra(sps_i_bin,1)[[1]]
      sps_i_cells <- data.frame(sps = x, cell = sps_i_cells)
      write.csv(sps_i_cells,
                file_to_save)
    }
  }, silent = TRUE)
}, cl = cl)


stopCluster(cl)

sps_cells_fut_file <- here::here(dir_data,"sps_cells_fut.qs")
if(file.exists(sps_cells_fut_file)){
  
  sps_cells_fut <- qread(sps_cells_fut_file)
  
} else {
  
  sps_cells_fut <- list.files(dir_sp_range_fut,full.names = TRUE)
  sps_cells_fut <- lapply(sps_cells_fut,read.csv)
  sps_cells_fut <- do.call(rbind,sps_cells_fut)
  sps_cells_fut <- sps_cells_fut[,c("sps","cell")]
  sps_cells_fut <- unique(sps_cells_fut)
  # save
  qs::qsave(sps_cells_fut, here::here(dir_data,"sps_cells_fut.qs"))
  
}

head(sps_cells_fut)
length(unique(sps_cells_fut$sps))

# get species within MPAs
sps_cells_fut_MPAs <- sps_cells_fut %>%
  dplyr::filter(cell %in% MPA_cells)

qs::qsave(sps_cells_fut_MPAs, here::here(dir_data,"sps_cells_fut_MPA.qs"))

gc()

cat("Done!")

