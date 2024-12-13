## import data from Kelleway Rogers

library(magrittr)
library(dplyr)
library(tidyverse)


input_data01 <- read_csv("data/raw_data/KellewayRogers.csv")



##### add informational  
source_name <- "KellewayRogers"
author_initials <- "KR"

# Assuming 'column_name' is the name of the column you want to modify
# and 'value_to_replace' is the specific value you want to replace with NA
# Also, assuming your data frame is named 'data'

# Mark the rows where replacements will be made
replaced_rows <- !is.na(input_data01$N_perc) & input_data01$N_perc == '<0.001'

# Create a new column to indicate replacements
input_data01$replacement_indicator <- ifelse(replaced_rows, "Replaced", "Original")

# Replace specific values with NA, only if the value is not already NA
input_data01$N_perc[replaced_rows] <- NA


#### export ####

export_data01 <- input_data01 %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3,DOI)


export_data01$N_perc <- as.numeric(export_data01$N_perc)

## export

path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)

