# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)


# Read the first CSV 
data1 <- read.csv("data/raw_data/piazza_et_al_2020_cores.csv")

# Read the second CSV file 
data2 <- read.csv("data/raw_data/piazza_et_al_2020_depthseries.csv")

# Merge the two data frames based on common columns
merged_data <- left_join(data1, data2, by = c("study_id", "site_id", "core_id")) 

merged_data2 <- merged_data %>% 
  mutate(Habitat_type = "saltmarsh") #should be marsh


# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data2, "data/CCN/Piazza.merged.csv", row.names = FALSE)

