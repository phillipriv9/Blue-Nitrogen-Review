## import data from Weston and Langley

library(dplyr)
library(magrittr)
library(tidyverse)




input_data01 <- read.csv("data/raw_data/WestonLangleywMetadata.csv")

input_data02 <- input_data01  %>% 
mutate(Method = "EA", 
       U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
       L_depth_m = as.numeric(L_depth_cm)/100, #cm to m
Year_collected = 2014, DOI ="https://doi.org/10.1038/s41558-018-0162-5",
study_id=Source, site_id=Site, core_id=Plot)

##### add informational  
source_name <- "WestonLangley"
author_initials <- "NW"


#### export ####
export_data01 <- input_data02 %>% 
  dplyr::select(Database, study_id, site_id, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3, DOI)


## export
path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)