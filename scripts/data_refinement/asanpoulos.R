#Import data from Asanpoulos et al. 2021

library(magrittr)
library(dplyr)
library(tidyverse)

#Reading in raw data provided by author
input_data01 <- read.csv('data/raw_data/asanopoulos.csv')

#remove row one
input_data01 <- input_data01[-1,]

#### Add informational
source_name <- "Asanopoulos"
author_initials <- "ca"

#### export ####

#Preping for export
df <- input_data01  %>% 
  mutate(Method = "EA", Latitude = -1*Latitude,
         
         study_id = "Asanopouos_et_al_2021", Database="individual" )

export_data01 <- df %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3,DOI)


## export

path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)
