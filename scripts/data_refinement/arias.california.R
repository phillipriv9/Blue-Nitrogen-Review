# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)

# URL for arias_ortiz_et_al_2021_cores.csv
csv_url1 <- "https://smithsonian.figshare.com/ndownloader/files/31290997"

# URL for arias_ortiz_et_al_2021_depthseries

csv_url2 <- "https://smithsonian.figshare.com/ndownloader/files/31291000"

# Read the first CSV file from GitHub
data1 <- read.csv(url(csv_url1))

# Read the second CSV file from GitHub
data2 <- read.csv(url(csv_url2))

# Merge the two data frames based on common columns
merged_data <- left_join(data1, data2, by = c("study_id", "site_id", "core_id") ) %>%
                           mutate(Habitat_type = "marsh") 

# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data, "data/CCN/Arias.merged.csv", row.names = FALSE)
