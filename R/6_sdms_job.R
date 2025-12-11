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

library(dplyr)

singularity_image <- "/users/boliveira/geospatial_4.5.1.sif"
Rscript_file <- here(dir_code,"6_sdms_single_species.R")

sp_list <- list.files(dir_sp_occ, pattern = ".csv")
sp_list <- gsub(".csv","",sp_list)
length(sp_list)

job_dir <- here(dir_code,"jobs")
dir.create(job_dir, showWarnings = FALSE)

log_dir <- here(dir_code,"logs")
dir.create(log_dir, showWarnings = FALSE)

# See if there are missing species
verbose_sdms <- lapply(sp_list, function(x){
  try({
    read.csv(here(dir_sdms,x,paste0(x,"_SDM_verbose.csv")))
  }, silent = TRUE)
})
table(sapply(verbose_sdms,class))
sp_list <- sp_list[which(sapply(verbose_sdms,class)=="try-error")]

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
    cores = 3 # 3 k-fold and 1 model (Maxnet)
    memory = "30G"
    time = "1:00:00"
    partition = select_partition(request_mem = as.numeric(gsub("[^0-9.-]", "", memory)), 
                                 request_cpu = cores, 
                                 limits=limits)
    
    slurm_job_singularity(jobdir = job_dir,
                          logdir = log_dir, 
                          jobname = sptogo, 
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

