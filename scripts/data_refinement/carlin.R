# Install and load the dplyr package if you haven't already
# install.packages("dplyr")
library(dplyr)



# Read the first CSV file from GitHub
data1 <- read.csv("https://smithsonian.figshare.com/ndownloader/files/30770221")

# Read the second CSV file from GitHub
data2 <- read.csv("https://smithsonian.figshare.com/ndownloader/files/30770224")

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

