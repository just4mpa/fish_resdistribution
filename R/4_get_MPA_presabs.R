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

list.of.packages <- c("terra","wdpar","here","sf","pbapply", "tidyterra")

load_packages(list.of.packages)

# increase timeout to 10 minutes (600 seconds)
options(timeout = 600)

MPA_dir <- here(dir_data, "WDPA")
dir.create(MPA_dir, showWarnings = FALSE)

# Download WDPA data ----
shp_poly_path <- here(MPA_dir, "WDPA_Dec2025_Public_shp-polygons.shp")
shp_point_path <- here(MPA_dir, "WDPA_Dec2025_Public_shp-points.shp")

if(!file.exists(shp_poly_path) & !file.exists(shp_point_path)){
  url <- "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Dec2025_Public_shp.zip"
  destfile <- file.path(MPA_dir, "WDPA_Dec2025_Public.gdb.zip")
  download.file(url, destfile, mode = "wb")
  
  # Unzip file
  zipfile <- list.files(MPA_dir, pattern = '.zip',full.names = TRUE)
  lapply(zipfile, function(x){
    unzip(zipfile = x,
          exdir = MPA_dir)
  })
  
  unlink(zipfile)
}

# Get only marine protected areas ----
MPA_poly <- vect(shp_poly_path)
# Filter for marine or coastal areas
MPA_poly <- MPA_poly %>%
  filter(REALM != "Terrestrial")
unique(MPA_poly$REALM)

MPA_point <- vect(shp_point_path)
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
    mpa_i_cells <- terra::extract(ocean,mpa_i,cell = TRUE)$cell
  }
  data.frame(MPA = mpa_i$SITE_ID, cell = mpa_i_cells)
})

MPA_cells <- rbind(do.call(rbind,MPA_poly_cells),
                   do.call(rbind,MPA_point_cells))
MPA_cells <- unique(MPA_cells)
head(MPA_cells)

write.csv(MPA_cells,
          here::here(dir_data,"MPA_cells.csv"),
          row.names = FALSE)

