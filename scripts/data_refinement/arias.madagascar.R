#import data from Arias-Ortiz unpublsihed madagascar study from email

library(tidyverse)


df1 <- read.csv("data/raw_data/AriasOrtiz_et_al_Madagascar_2021.csv")

df2 <- df1 %>%
  mutate(Habitat_type= "mangrove")


##### add informational  
source_name <- "arias_ortiz_madagascar"
author_initials <- "AAO"


#### export ####

export_data01 <- df2 %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected,
                Latitude, Longitude,
                U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3, DOI)


## export
path_out = 'data/refined/'
export_file <- paste(path_out, "arias_ortiz_madagascar", ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)

