# Load libraries ----

rm(list=ls())
gc()

library(here)

computer = "buca"
if(computer == "matrics"){
  wd <- "/users/boliveira/fish_redistribution"
} 
if(computer == "buca"){
  wd <- here()
} 

source(here(wd,"R/source_code.R"))

library(wdpar)
library(terra)
library(here)
library(sf)
library(pbapply)

# Make species presabs long format
all_sps <- list.files(dir_sdms)

ocean <- rast(here(dir_layers_pres,"BO_sstmean.tif"))

N_cpus <- parallelly::availableCores()

species_pres_cells <- pblapply(all_sps, function(x){
  try({
    sp_i_rast_file <- here(dir_sdms,x,
                           paste0("proj_",x,"_pres_proj"),
                           paste0("proj_",x,"_pres_proj_",x,"_ensemble.tif"))
    sps_i <- rast(sp_i_rast_file)
    # make raster binary (pres-abs)
    sp_i_threshold_file <- here(dir_sdms,x,paste0(x,"_mod_eval.csv"))
    sps_i_threshold <- read.csv(sp_i_threshold_file)
    sps_i_threshold <- weighted.mean(sps_i_threshold$cutoff, 
                                     w = sps_i_threshold$validation)
    sps_i_bin <- ifel(sps_i >= sps_i_threshold, 1, NA)
    # project to the extent of ocean so that cells will overlap
    sps_i_bin <- project(sps_i_bin,ocean)
    sps_i_cells <- cells(sps_i_bin,1)[[1]]
    data.frame(sps = x, cell = sps_i_cells)
  }, silent = TRUE)
}, cl = N_cpus)

rem <- sapply(species_pres_cells,class)
table(rem)
rem <- which(rem=="try-error")
species_pres_cells <- species_pres_cells[-rem]

species_fut_cells <- pblapply(all_sps, function(x){
  try({
    sp_i_rast_file <- here(dir_sdms,x,
                           paste0("proj_",x,"_fut_proj"),
                           paste0("proj_",x,"_fut_proj_",x,"_ensemble.tif"))
    sps_i <- rast(sp_i_rast_file)
    # make raster binary (pres-abs)
    sp_i_threshold_file <- here(dir_sdms,x,paste0(x,"_mod_eval.csv"))
    sps_i_threshold <- read.csv(sp_i_threshold_file)
    sps_i_threshold <- weighted.mean(sps_i_threshold$cutoff, 
                                     w = sps_i_threshold$validation)
    sps_i_bin <- ifel(sps_i >= sps_i_threshold, 1, NA)
    # project to the extent of ocean so that cells will overlap
    sps_i_bin <- project(sps_i_bin,ocean)
    sps_i_cells <- cells(sps_i_bin,1)[[1]]
    data.frame(sps = x, cell = sps_i_cells)
  }, silent = TRUE)
}, cl = N_cpus)
rem <- sapply(species_fut_cells,class)
table(rem)
rem <- which(rem=="try-error")
species_fut_cells <- species_fut_cells[-rem]

sps_cells_pres <- do.call(rbind,species_pres_cells)
sps_cells_pres <- unique(sps_cells_pres)
head(sps_cells_pres)
length(unique(sps_cells_pres$sps))

MPA_cells_fut <- do.call(rbind,species_fut_cells)
MPA_cells_fut <- unique(MPA_cells_fut)
head(MPA_cells_fut)
length(unique(MPA_cells_fut$sps))
