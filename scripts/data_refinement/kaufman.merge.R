# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)

# Read the first CSV file from GitHub Sanborne  kauffman_et_al_2020_cores.csv
data1 <- read.csv("https://smithsonian.figshare.com/ndownloader/files/24198395")

# Read the second CSV file from GitHub kauffman_et_al_2020_depthseries.csv
data2 <- read.csv("https://smithsonian.figshare.com/ndownloader/files/24198401")

# Merge the two data frames based on common columns
merged_data <- left_join(data1, data2, by = c("study_id", "site_id", "core_id"))

# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data, "data/CCN/Kauffman.merged.csv", row.names = FALSE)

