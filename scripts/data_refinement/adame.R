# Function to convert DMS coordinates to decimal degrees
dms_to_decimal <- function(dms) {
  parts <- strsplit(dms, "[dmsNWE]", perl = TRUE)[[1]]
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  seconds <- as.numeric(parts[3])
  direction <- substr(dms, nchar(dms), nchar(dms))
  sign <- ifelse(direction %in% c("S", "W"), -1, 1)
  decimal_degrees <- sign * (degrees + minutes/60 + seconds/3600)
  return(decimal_degrees)
}

# Read in data from CSV
library(measurements)
library(dplyr)

# Phill: I moved the location of the data to tidy up the repository
#read in data in email from Fernanda
df <- read_csv("data/raw_data/Adame.csv")

# Convert DMS coordinates to decimal degrees
df <- df  %>% 
  mutate(Method = "EA", 
         U_depth_m = as.numeric(U_depth_cm)/100, # cm to m
         L_depth_m = as.numeric(L_depth_cm)/100, # cm to m
         Database="individual", Latitude = as.character(Latitude),
         Longitude = as.character(Longitude),
  )

export_data01 <- df %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3, DOI)

# Add informational
source_name <- "adame"
author_initials <- "fern"
path_out <- 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

# Write to CSV
write.csv(export_df, export_file)
