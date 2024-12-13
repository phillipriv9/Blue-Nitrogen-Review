## import data from Rovai et al. 2018

library(dplyr)
library(magrittr)
library(tidyverse)


input_data01 <- read.csv("data/raw_data/carnero.csv")

##### add informational  
#Ask Adam if it's carnero or ruiz-fernandes
source_name <- "carnera-bravo"
author_initials <- "cb"



#### export ####

export_data01 <- input_data01 %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3,DOI)


## export

path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)
