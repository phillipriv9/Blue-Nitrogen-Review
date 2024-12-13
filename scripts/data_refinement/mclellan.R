# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)

# Read McLellan data from CSV file
data1 <- read.csv("data/raw_data/mcclellan2021.csv")

# Mutate and transform columns
data1 <- data1 %>%
  mutate(study_id = "McClellan_et_al_2021",
         site_id = Site,
         core_id = Core_id,
         Longitude = ifelse(Longitude > 0, Longitude * (-1), Longitude)) 
#for some sites, longitude was expressed without minus sign

# Save the modified data frame to a CSV file in the working directory
write.csv(data1, "data/refined/mclellan.csv", row.names = FALSE)

