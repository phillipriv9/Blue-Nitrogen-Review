#Finalized version with N Merged

# URL for Fell depth series
csv_url1 <- "https://smithsonian.figshare.com/ndownloader/files/30451638"

# URL for Fell cores
csv_url2 <- "https://smithsonian.figshare.com/ndownloader/files/30451635"


# Function to remove numbers after the last letter in a string
remove_numbers_after_last_letter <- function(string) {
  sub("[0-9]+$", "", string)
}

# Read the first CSV file from GitHub
# Phil Notes: I think it's using the URL for the carlin data here, and there isn't another Github url in this script files
data1 <- read.csv(url(csv_url1))

# Convert DMS coordinates to decimal degrees
data1 <- data1 %>% 
  mutate(Method = "EA", 
         U_depth_m = as.numeric(depth_min)/100, # cm to m
         L_depth_m = as.numeric(depth_max)/100, # cm to m
         Database="individual",
         core_id = remove_numbers_after_last_letter(core_id)  # Remove numbers after last letter in core_id
  )

# Read the second CSV file from GitHub
data2 <- read.csv(url(csv_url2))

#read the N data in from my files becaue they aren't on server
data3 <-read.csv("data/raw_data/Fell_N.csv")

# Merge the two data frames based on common columns
data1_2 <- left_join(data1, data2, by = c("study_id", "site_id", "core_id")) %>%
  mutate(Habitat_type = "marsh", Latitude = core_latitude,
         Longitude = core_longitude,) 

merged_data <-left_join(data1_2, data3, by = 'sample_id')

# Save the merged data frame to a CSV file in the working directory
write.csv(merged_data, "data/CCN/Fell.merged.csv", row.names = FALSE)
