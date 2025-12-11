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
library(pbapply)

computer = Sys.info()["nodename"]
if(computer == "login02" | computer == "login01"){
  wd <- "/users/boliveira/fish_redistribution"
} 
if(computer == "just"){
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
# source(here(wd,"R/6_sdms.R"))
# Or run this is a HPC (P.S.: The underlying code must be adjust to reflect HPC architecture):
source(here(wd,"R/6_sdms_job.R"))

# 7) Get species pres/abs within MPAs ----
# It's memory intense. Run this is a HPC (P.S.: The underlying code must be adjust to reflect HPC architecture):
slurm_job_singularity(jobdir = here(dir_code,"jobs"),
                      logdir = here(dir_code,"logs"), 
                      jobname = "presabs", 
                      args = "",
                      N_Nodes = 1, 
                      tasks_per_core = 1, 
                      cores = 10, 
                      time = "3:00:00", 
                      memory = "100G", 
                      partition = "bigmem", 
                      singularity_image = "/users/boliveira/geospatial_4.5.1.sif", 
                      Rscript_file = here(wd,"R/7_get_species_presabs_within_MPAs.R"))

# 8) Quantify species gain/loss inside protected areas


# 9)  