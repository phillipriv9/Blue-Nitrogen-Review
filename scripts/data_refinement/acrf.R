## import data from Rovai et al. 2018

library(magrittr)
library(dplyr)
library(tidyverse)


input_data01 <- read.csv("data/raw_data/acrf.csv") #raw data was provided by author

#Removing empty row
input_data01 <- input_data01[-c(48, 99), ]

#Changing column to numeric data
input_data01$N_perc <- as.numeric(input_data01$N_perc)

##### add informational  
source_name <- "ruiz-fernadez"
author_initials <- "rf"



#### export ####

export_data01 <- input_data01 %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3,DOI)


## export

path_out = 'data/refined/'

export_file <- paste(path_out, "ruiz-fernadez", ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)

