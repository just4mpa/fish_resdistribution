rm(list=ls())
gc()

library(here)
library(dplyr)

computer = "matrics"
if(computer == "matrics"){
  wd <- "/users/boliveira/fish_redistribution"
} 
if(computer == "buca"){
  wd <- here()
}

source(here(wd,"R/source_code.R"))

singularity_image <- "/users/boliveira/geospatial_4.5.1.sif"
Rscript_file <- here(dir_code,"6_sdms_single_species.R")

sp_list <- list.files(dir_sp_occ, pattern = ".csv")
sp_list <- gsub(".csv","",sp_list)

job_dir <- here(dir_code,"jobs")
dir.create(job_dir, showWarnings = FALSE)

log_dir <- here(dir_code,"logs")
dir.create(log_dir, showWarnings = FALSE)

for(i in 1:length(sp_list)){
  
  cat("\nRunning sps",i,"from",length(sp_list),"\n")
  
  sptogo <- sp_list[i]
  
  # check if job for this species is running
  test_run <- system("squeue --format='%.50j' --me", intern = TRUE)
  test_run <- gsub(" ","",test_run)
  
  args = sptogo
  
  # Submit job
  if(!sptogo %in% test_run){
    
    N_jobs_at_a_time = 100
    N_Nodes = 1
    tasks_per_core = 1
    cores = 3 
    memory = "16G"
    time = "1:00:00"
    partition = select_partition(request_mem = as.numeric(gsub("[^0-9.-]", "", memory)), 
                                 request_cpu = cores, 
                                 limits=limits)
    
    slurm_job_singularity(jobdir = job_dir,
                          logdir = log_dir, 
                          sptogo = sptogo, 
                          args = args,
                          N_Nodes = N_Nodes, 
                          tasks_per_core = tasks_per_core, 
                          cores = cores, 
                          time = time, 
                          memory = memory, 
                          partition = partition, 
                          singularity_image = singularity_image, 
                          Rscript_file = Rscript_file)
    
    # check how many jobs in progress
    tmp <- system("squeue -u $USER",intern = T)
    
    while(length(tmp)>N_jobs_at_a_time){
      
      Sys.sleep(10)
      tmp <- system("squeue -u $USER",intern = T)
      
    }
  }
}

