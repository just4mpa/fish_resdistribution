#' Fish redistribution: Nutritional impacts
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Brunno F Oliveira \email{brunno.oliveira@me.com}
#'
#' @date 2025/09/22

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

# 1) Download predictor variables from BioOracle ----
# Get bioclimatic layers from the package `sdmpredictors` at 10km resolution
source(here(wd,"R/1_download_predictors.R"))

# 2) Download species traits and occurrence records from GBIF ----
source(here(wd,"R/2_fixnames_download_traits_and_occurrences.R"))

# 3) Trait imputation ----
source(here(wd,"R/3_trait_imputation.R"))

# 4) Get MPA pres/abs data ----
source(here(wd,"R/4_get_MPA_presabs.R"))

# 5) Get species list within MPAs ----
source(here(wd,"R/5_get_sps_within_MPAs.R"))

# 6) Fit SDMs for species that occur within MPAs ----
# Run this is in a normal computer:
source(here(wd,"R/6_sdms.R"))
# Or run this is a HPC:
source(here(wd,"R/6_sdms_job.R"))

# Read verbose file to check for errors
all_sdms <- list.files(dir_sdms)
verbose_sdms <- lapply(all_sdms, function(x){
  read.csv(here(dir_sdms,x,paste0(x,"_SDM_verbose.csv")))
})
verbose_sdms <- do.call(rbind,verbose_sdms)
table(verbose_sdms$PCA_env)
table(verbose_sdms$SDM_fit)
table(verbose_sdms$SDM_proj_pres)
table(verbose_sdms$SDM_proj_fut)

# 7) Get species pres/abs within MPAs ----
source(here(wd,"R/7_get_species_within_MPAs.R"))

# 8) Measure species gain/loss inside protected  areas


# 9)  