# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)

# You had the raw files in directory -PR
# URL for Morrissette_et_al_2023_plots.csv 
#csv_url1 <- "https://smithsonian.figshare.com/ndownloader/files/41472927"

# Read the CSV file off of Github
#data1 <- read.csv(url(csv_url1))

# URL for Morrissette_et_al_2023_depthseries.csv 
#csv_url2 <- "https://smithsonian.figshare.com/ndownloader/files/39158762"

# Read the CSV file off of Github
#data2 <- read_csv(url(csv_url2))



# Read the first CSV file from Respository
data1 <- read.csv("data/raw_data/Morrissette_et_al_2023_plots.csv")

# Read the second CSV file from Repository
data2 <- read.csv("data/raw_data/Morrissette_et_al_2023_depthseries.csv")

# Merge the two data frames based on common columns
merged_data <- left_join(data1, data2, by = c("study_id", "site_id", "transect_id", "plot_id", "core_id"))

# Rename selected columns
merged_data1 <- merged_data %>%
mutate(fraction_carbon = as.numeric(soil_organic_carbon)/ 100,
       fraction_nitrogen = as.numeric(soil_nitrogen)/ 100, 
       core_latitude = latitude,
       core_longitude = longitude,
       Habitat_type= "mangrove")

# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data1, "data/CCN/Morrissette.merged.csv", row.names = FALSE)
