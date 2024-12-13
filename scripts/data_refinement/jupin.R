#read in data from csv in email from Johanna Jupin

library(dplyr)

df <- read.csv("data/raw_data/jupin.csv")


df <- df  %>% 
mutate(Method = "EA", 
       U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
       L_depth_m = as.numeric(L_depth_cm)/100, # cm to m
       study_id = "jupin_et_al_unpublished",  DOI ="in_submission", Database="individual" )

export_data01 <- df %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3,DOI)

##### add informational  
source_name <- "jupin"
author_initials <- "jj"


#Creating CSV file
path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)

