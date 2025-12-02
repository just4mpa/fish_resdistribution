# title: "Download aquamaps data"
# author: "Brunno F Oliveira"

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

list.of.packages <- c("stringr", "crul", "tidyr", "here", "pbapply","purrr",
                      "pbapply","dplyr","httr",
                      "sdmpredictors", # get biooracle data
                      "data.table", "ggplot2",
                      "raster", "terra","rnaturalearthdata","tidyterra")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(
  new.packages)

sapply(list.of.packages, require, character.only = TRUE)

# Load global sea shp file -----

# Get world shapefile
globe <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sv")

# Select variables -----
datasets <- list_datasets(terrestrial = FALSE, marine = TRUE)

# Present
layers <- list_layers(datasets)

layercodes <- c(
  "MS_bathy_5m", 
  "BO_sstrange",
  "BO_sstmean", 
  "BO_salinity",
  "BO22_chlomean_ss",
  "BO22_ph",
  "BO22_dissoxmean_bdmean",
  "BO22_ppmean_ss"
)

# Future
layers_fut <- list_layers_future(datasets)

# Load equal area rasters 
layercodes_fut <- layers_fut |>
  filter(current_layer_code %in% layercodes,
         scenario %in% c("A2","RCP45"),
         year == "2100") |>
  select(current_layer_code, layer_code)

layercodes_fut <- merge(layercodes_fut,
                        data.frame(current_layer_code = layercodes),
                        all = TRUE)
pos <- which(is.na(layercodes_fut$layer_code))
layercodes_fut$layer_code[pos] <- layercodes_fut$current_layer_code[pos]
layercodes_fut <- layercodes_fut[match(layercodes,layercodes_fut$current_layer_code),]


# Download predictor variables ----
# Get bioclimatic layers from the package `sdmpredictors` at 10km at the equator

## Present ----

# Download
env_pres <- load_layers(layercodes, 
                        datadir = dir_layers_pres, 
                        rasterstack = FALSE)
env_pres$MS_bathy_5m <- abs(env_pres$MS_bathy_5m)

# Harmonize extent
tmp <- env_pres[[1]]
env_pres <- lapply(env_pres, function(x){
  x <- rast(x)
  if(!ext(x)==ext(tmp)){
    x <- extend(x, tmp)
  }
  x
})
env_pres <- rast(env_pres)

plot(env_pres$BO_sstrange)

# Save rasters
for(i in 1:nlyr(env_pres)){
  rast_i <- env_pres[[i]]
  rast_i_name <- names(rast_i)
  rast_i_name_file <- paste0(rast_i_name,".tif")
  
  writeRaster(rast_i, here(dir_layers_pres,rast_i_name_file))
}

# remove zip files
rem <- list.files(dir_layers_pres, pattern = ".zip", full.names = TRUE)
unlink(rem)

## Future ----
# Here using the "midterm" A2 scenario. From https://archive.ipcc.ch/ipccreports/sres/emission/index.php?idp=03:
# A2. The A2 storyline and scenario family describes a very heterogeneous world. The underlying theme is self reliance and preservation of local identities. Fertility patterns across regions converge very slowly, which results in continuously increasing population. Economic development is primarily regionally oriented and per capita economic growth and technological change more fragmented and slower than other storylines.

env_future <- try({
  load_layers(layercodes = layercodes_fut$layer_code, 
                          datadir = dir_layers_fut, 
                          rasterstack = FALSE)
  }, 
  silent = TRUE)
env_future$MS_bathy_5m <- abs(env_future$MS_bathy_5m)

tmp <- env_future[[1]]
env_future <- lapply(env_future, function(x){
  x <- rast(x)
  if(!ext(x)==ext(tmp)){
    x <- extend(x, tmp)
  }
  x
})
env_future <- rast(env_future)

plot(env_future$BO_A2_2100_sstrange)

# Save rasters
for(i in 1:nlyr(env_future)){
  rast_i <- env_future[[i]]
  rast_i_name <- names(rast_i)
  rast_i_name_file <- paste0(rast_i_name,".tif")
  
  writeRaster(rast_i, here(dir_layers_fut,rast_i_name_file))
}

# remove zip files
rem <- list.files(dir_layers_fut, pattern = ".zip", full.names = TRUE)
unlink(rem)

cat("Done!")


