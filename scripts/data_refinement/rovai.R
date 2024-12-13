## import data from Rovai et al. 2018

library(dplyr)
library(magrittr)
library(tidyverse)


input_data01 <- read.csv("data/raw_data/Rovai2018.csv")


input_data02 <- input_data01  %>% 
mutate(Method = "EA", 
       U_depth_m = as.numeric(subsmple_up_depth_cm)/100 , #cm to m
       L_depth_m = as.numeric(subsmple_lw_depth_cm)/100, # cm to m
N_perc = as.numeric(N_pcent), OC_perc = as.numeric(C_pcent), Habitat_type = "mangrove", Year_collected = 2015,
Latitude = LAT, Longitude = LON, BD_reported_g_cm3 = BD_gcc , 
study_id = "Rovai_et_al_2018", site_id = paste(REGION,ENVSETT,SITE, sep ="_"), core_id = CORE, DOI ="https://doi.org/10.1038/s41558-018-0162-5" )

##### add informational  
source_name <- "Rovai2018"
author_initials <- "AR"



#### export ####

export_data01 <- input_data02 %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3,DOI)


## export

path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)