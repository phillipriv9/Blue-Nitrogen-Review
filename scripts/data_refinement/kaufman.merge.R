# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)

# URL for Sanborne  kauffman_et_al_2020_cores.csv
csv_url1 <- "https://smithsonian.figshare.com/ndownloader/files/24198395"

# URL for  kauffman_et_al_2020_depthseries.csv
csv_url2 <- "https://smithsonian.figshare.com/ndownloader/files/24198401"

# Read the first CSV file from GitHub
data1 <- read.csv(url(csv_url1))

# Read the second CSV file from GitHub
data2 <- read.csv(url(csv_url2))

# Merge the two data frames based on common columns
merged_data <- left_join(data1, data2, by = c("study_id", "site_id", "core_id"))

# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data, "CCN/Kauffman.merged.csv", row.names = FALSE)
