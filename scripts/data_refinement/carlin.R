# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)

# URL for carlin_et_al_2021_cores.csv
csv_url1 <- "https://smithsonian.figshare.com/ndownloader/files/30770221"

# URL for carlin_et_al_2021_depthseries.csv

csv_url2 <- "https://smithsonian.figshare.com/ndownloader/files/30770224"

# Read the first CSV file from GitHub
data1 <- read.csv(url(csv_url1))

# Read the second CSV file from GitHub
data2 <- read.csv(url(csv_url2))

# Merge the two data frames based on common columns
merged_data <- left_join(data1, data2, by = c("study_id", "site_id", "core_id"))


#from paper: mudflat (ELM1812-MFA1)  cordgrass-dominated (ELM1812-SPA2), and pickleweed-dominated (ELM1812-PWA1).

merged_data <- merged_data %>%
  mutate(Habitat_type = case_when(
    core_id == "ELM1812-MFA1" ~ "mudflat",
    core_id %in% c("ELM1812-SPA2", "ELM1812-PWA1") ~ "marsh",
    TRUE ~ "Other"
  ))

# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data, "data/CCN/Carlin.merged.csv", row.names = FALSE)

