# Install and load necessary packages if not already installed
#install.packages(c("dplyr", "purrr"))
library(dplyr)
library(purrr)

# Set the path to the folder containing your CSV files
folder_path <- "data/CCN/"

# Get a list of all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
list_of_dfs <- map(csv_files, ~{
  df <- read.csv(.)
  columns_to_convert <- c("Latitude", "Longitude", "Plot", "core_id", "fraction_nitrogen", "Habitat_type")
  for (col in columns_to_convert) {
    if (col %in% colnames(df)) {
      df <- mutate(df, !!col := as.character(!!sym(col)))
    }
  }
  return(df)
})

# Check the structure of each data frame
walk(list_of_dfs, ~str(.))

# Combine all data frames into one
combined_data <- bind_rows(list_of_dfs, .id = "file_id")

# Add a new column with the value "Maxwell" for every row
combined_data <- mutate(combined_data, Database = "CCN") %>%
  mutate(study_site_core = paste(study_id, site_id, core_id, sep = "_"))

# Reorganize columns to make "Database" the first column
combined_data <- select(combined_data, Database, everything())


# Select only certain columns
selected_columns <- c("Database", "study_id", "study_site_core" , "site_id", "file_id", "core_latitude", "core_longitude", "core_id", "fraction_nitrogen", "fraction_total_nitrogen", 
                      "depth_min","depth_max","dry_bulk_density","core_year","salinity_class", "Habitat_type",
                      "fraction_carbon","fraction_carbon_measured", "fraction_carbon_modeled", "fraction_organic_carbon" )
combined_data <- select(combined_data, all_of(selected_columns))

# Rename selected columns
combined_data <- rename(combined_data,
                        BD_reported_g_cm3=dry_bulk_density 
                      )

#convert fractions to percents
combined_data2 <- combined_data%>% 
  mutate(OC_perc = as.numeric(fraction_carbon) * 100,
         OC_perc = ifelse(is.na(OC_perc), as.numeric(fraction_organic_carbon) * 100, OC_perc),
         OC_perc = ifelse(is.na(OC_perc), as.numeric(fraction_carbon_measured) * 100, OC_perc),
         N_perc = as.numeric(fraction_nitrogen) * 100,
         N_perc = ifelse(is.na(N_perc), as.numeric(fraction_total_nitrogen) * 100, N_perc),
         U_depth_m = as.numeric(depth_min/100),
         L_depth_m = as.numeric(depth_max/100), 
         Latitude = core_latitude, 
         Longitude = core_longitude)

# Specify the folder for saving the combined data
output_folder <- "data/combined"

# Create the output folder if it doesn't exist
if (!dir.exists(output_folder)) dir.create(output_folder)

# Write the combined data frame to a new CSV file in the "Combined" folder
output_file <- file.path(output_folder, "CCN_combined_data.csv")
write.csv(combined_data2, output_file, row.names = FALSE)
