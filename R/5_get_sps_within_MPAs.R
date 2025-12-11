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

list.of.packages <- c("terra","wdpar","here","sf","pbapply","dplyr")

load_packages(list.of.packages)



sp_list_file <- list.files(dir_sp_occ, full.names = TRUE, pattern = ".csv")
sp_list <- list.files(dir_sp_occ, pattern = ".csv")
sp_list <- gsub(".csv","",sp_list)
length(unique(sp_list))

ocean <- rast(here(dir_layers_pres,"BO_sstmean.tif"))
sps_cells <- pblapply(1:length(sp_list_file), function(i){
  sps_i <- read.csv(sp_list_file[i])
  sps_i <- vect(sps_i, geom = c("x","y"))
  sps_i_cells <- cells(ocean,sps_i)
  data.frame(sps = sp_list[i], cell = sps_i_cells[,2])
})
sps_cells <- do.call(rbind,sps_cells)
head(sps_cells)

MPA_cells <- read.csv(here(dir_data,"MPA_cells.csv"))
head(MPA_cells)
MPA_cells <- unique(MPA_cells$cell)

sps_cells <- sps_cells %>%
  filter(cell %in% MPA_cells)
head(sps_cells)
length(unique(sps_cells$sps))

cat("Done!")

