#need coordinates for Sanborne


# install.packages("dplyr")
library(dplyr)

# URL for Sanborne  cores.csv on 
csv_url1 <- "https://smithsonian.figshare.com/ndownloader/files/23147168"

# URL for  depthseries.csv 
csv_url2 <- "https://smithsonian.figshare.com/ndownloader/files/23147162"

# Read the first CSV file 
data1 <- read.csv(url(csv_url1))

# Read the second CSV file 
data2 <- read.csv(url(csv_url2))

# Merge the two data frames based on common columns
merged_data <- left_join(data1, data2, by = c("study_id", "site_id"))

merged_data2 <- merged_data %>%
  mutate(Habitat_type= "saltmarsh", Latitude= "54.187753", Longitude = "-130.268095") #approximate coordinates of Skeena estuary: 54.187753, -130.268095


# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data2, "data/CCN/Sanborne.merged.csv", row.names = FALSE)
