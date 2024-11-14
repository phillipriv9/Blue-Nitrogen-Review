#Loading packages
library(dplyr)


# Read the first CSV file from Github
data1 <- read.csv( "https://smithsonian.figshare.com/ndownloader/files/21251496")

# Read the second CSV file from Github
data2 <- read.csv("https://smithsonian.figshare.com/ndownloader/files/21251499")

# Merge the two data frames based on common columns
merged_data <- left_join(data1, data2, by = c("study_id", "site_id", "core_id"))

# Create a new column Habitat_type based on the rightmost character of site_id
merged_data <- merged_data %>%
  mutate(
    Habitat_type = ifelse(substr(site_id, nchar(site_id), nchar(site_id)) == "g", "mangrove", "marsh"))

# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data, "data/CCN/breithaupt.merged.csv", row.names = FALSE)

