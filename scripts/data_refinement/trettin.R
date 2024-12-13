## import data from Trettin et al. from Ruffiji

library(tidyverse)

input_data01 <- read.csv("data/raw_data/Trettin_et_al_2020.csv")



export_data01 <- input_data01 %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected,
                Latitude, Longitude,
                U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3)



## export

path_out = 'data/refined/'

export_file <- paste(path_out, "Trettin", ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)
