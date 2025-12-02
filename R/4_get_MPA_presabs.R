# Load libraries ----

rm(list=ls())
gc()

library(here)

computer = "matrics"
if(computer == "matrics"){
  wd <- "/users/boliveira/fish_redistribution"
} 
if(computer == "buca"){
  wd <- here()
}

source(here(wd,"R/source_code.R"))

list.of.packages <- c("terra","wdpar","here","sf","pbapply", "tidyterra")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)

sapply(list.of.packages, require, character.only = TRUE)

# increase timeout to 10 minutes (600 seconds)
options(timeout = 600)

# Download WDPA data ----
url <- "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Dec2025_Public_shp.zip"
destfile <- file.path(dir_temp, "WDPA_Dec2025_Public.gdb.zip")
download.file(url, destfile, mode = "wb")

# Unzip file
zipfile <- list.files(dir_temp, pattern = '.zip',full.names = TRUE)
lapply(zipfile, function(x){
  unzip(zipfile = x,
      exdir = dir_temp)
})

unlink(zipfile)

# Get only marine protected areas ----
gdb_path <- here(dir_temp, "WDPA_Dec2025_Public_shp-polygons.shp")
MPA_poly <- vect(gdb_path)
# Filter for marine or coastal areas
MPA_poly <- MPA_poly %>%
  filter(REALM != "Terrestrial")
unique(MPA_poly$REALM)

gdb_path <- here(dir_temp, "WDPA_Dec2025_Public_shp-points.shp")
MPA_point <- vect(gdb_path)
# Filter for marine or coastal areas
MPA_point <- MPA_point %>%
  filter(REALM != "Terrestrial")
unique(MPA_point$REALM)

# Get cells at which each MPA occurs 
# Load one environmental data to base the rasterization
ocean <- rast(here(dir_layers_pres,"BO_sstmean.tif"))
MPA_poly_cells <- pblapply(1:nrow(MPA_poly), function(x){
  mpa_i <- MPA_poly[x,]
  mpa_i_cells <- cells(ocean,mpa_i)[,2]
  data.frame(MPA = mpa_i$SITE_ID, cell = mpa_i_cells)
})

MPA_point_cells <- pblapply(1:nrow(MPA_point), function(x){
  mpa_i <- MPA_point[x,]
  mpa_i_cells <- try({ cells(ocean,mpa_i)[,2] })
  if(class(mpa_i_cells)=="try-error"){
    mpa_i_cells <- extract(ocean,mpa_i,cell = TRUE)$cell
  }
  data.frame(MPA = mpa_i$SITE_ID, cell = mpa_i_cells)
})

MPA_cells <- rbind(do.call(rbind,MPA_poly_cells),
                   do.call(rbind,MPA_point_cells))
MPA_cells <- unique(MPA_cells)
head(MPA_cells)

write.csv(MPA_cells,
          here(dir_data,"MPA_cells.csv"),
          row.names = FALSE)

