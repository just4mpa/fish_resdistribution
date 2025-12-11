# title: "Download aquamaps data"
# author: "Brunno F Oliveira"

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

list.of.packages <- c("tidyr","here", "dplyr")

load_packages(list.of.packages)

# Load taxonomy ----
sps_list <- read.csv(here(dir_data,"sp_taxonomy.csv"))
sps_list <- sps_list %>%
  dplyr::select(FB_ID, SuperClass, Class, Order, Family, Genus)

# Load habitat traits ----
fs_habitat_class <- read.csv(here(dir_data,"habitat_traits_raw.csv"))
fs_habitat_class <- fs_habitat_class %>%
  mutate(FB_ID = SpecCode) %>%
  select(FB_ID, Benthic,Pelagic,Superficial)

# add in taxonomy
fs_habitat_class <- fs_habitat_class %>%
  left_join(sps_list, by = "FB_ID") %>%
  unique()

any(!fs_habitat_class$FB_ID %in% sps_list$FB_ID)

head(fs_habitat_class)
# Imput missing habitats based on taxonomy ----

trait_matrix <- fs_habitat_class %>% dplyr::select(my_traits)
taxo_matrix <- fs_habitat_class %>% dplyr::select(taxo_rank)

head(trait_matrix)
head(taxo_matrix)

trait_matrix$Genus <- taxo_matrix$Genus

trait_matrix <- trait_matrix %>% as.matrix()
taxo_matrix <- taxo_matrix %>% as.matrix()

# Filling the gaps in X
fs_habitat_imputed <- impute_traits_taxonomy(fs_habitat_class, 
                                             traits = c("Benthic","Pelagic","Superficial"),
                                             taxonomy_order = c("Genus", "Family", "Order"))

fs_habitat_imputed[,c("Benthic","Pelagic","Superficial")]<- round(fs_habitat_imputed[,c("Benthic","Pelagic","Superficial")])

head(fs_habitat_imputed)

write.csv(fs_habitat_class[,1:4],
          here(dir_data,"habitat_trait.csv"), 
          row.names = FALSE)


cat("Done!")

