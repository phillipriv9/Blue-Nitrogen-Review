library(dplyr)
library(readr)
df1 <- read.csv("data/combined/Maxwell_combined_data.csv")
df2 <- read.csv("data/combined/CCN_combined_data.csv")

# make coordinates characters
df1 <- df1 %>% mutate(Latitude = as.character(Latitude), Longitude = as.character(Longitude))
df2 <- df2 %>% mutate(Latitude = as.character(Latitude), Longitude = as.character(Longitude))
df3 <- bind_rows(df1, df2, .id=NULL)

df4 <- df3 %>% 
  dplyr::select(Database, Source, study_id, study_site_core, site_id, Site, Plot, core_id, Habitat_type, Year_collected, 
                Latitude, Longitude, U_depth_m, L_depth_m, OC_perc, N_perc, BD_reported_g_cm3, DOI)

# Recategorize groups and create a new variable 'Recategorized_Habitat'
df4 <- df4 %>%
  mutate(Habitat = case_when(
    grepl("saltmarsh", Habitat_type, ignore.case = TRUE) ~ "marsh", 
    grepl("salt marsh", Habitat_type, ignore.case = TRUE) ~ "marsh",
    grepl("marsh", Habitat_type, ignore.case = TRUE) ~ "marsh",
    grepl("mangrove", Habitat_type, ignore.case = TRUE) ~ "mangrove",
    grepl("seagrass", Habitat_type, ignore.case = TRUE) ~ "seagrass",
    grepl("mudflat", Habitat_type, ignore.case = TRUE) ~ "mudflat",
    grepl("tidal flat", Habitat_type, ignore.case = TRUE) ~ "mudflat",
    grepl("high sabkha", Habitat_type, ignore.case = TRUE) ~ "mudflat",
    grepl("low sabkha", Habitat_type, ignore.case = TRUE) ~ "mudflat",
    grepl("microbial mat", Habitat_type, ignore.case = TRUE) ~ "mudflat",
    grepl("peat", Habitat_type, ignore.case = TRUE) ~ "marsh",
    TRUE ~ "Other"
  ))

#calculate C:N, centerdepth and Ndensity
df5 <- df4 %>%
  mutate(CN = OC_perc/N_perc, centerdepth = (U_depth_m + L_depth_m)/2, Ndensity = 1000* BD_reported_g_cm3 * N_perc/100, BDclass= ifelse(BD_reported_g_cm3>.5,"BD>0.5", "BD<0.5"))

# Specify the folder for saving the recategorized data
output_folder_recategorized <- "data/Combined"

# Create the output folder if it doesn't exist
if (!dir.exists(output_folder_recategorized)) dir.create(output_folder_recategorized)

# Eliminate rows with NA in 'Source' or 'N_perc', or CN>100 or <5
df6 <- df5 %>%
  filter(!is.na(N_perc), 
         !is.na(site_id), 
         centerdepth < 1,
         !Habitat %in% c("mudflat", "seagrass", "Other"))


# Eliminate rows with NA in 'Source' or 'N_perc', or CN>100 or <5
df7 <- df6 %>%
  filter( 
         CN > 5, 
         CN < 100)


# This version is not filtered by C:N ratio
output_file_unfiltered <- file.path(output_folder_recategorized, "AllCombinedUnfiltered.csv")
write.csv(df6, output_file_unfiltered, row.names = FALSE)


# This file is filteres so that CN is between 5 and 100
output_file_filtered <- file.path(output_folder_recategorized, "AllCombined.csv")
write.csv(df7, output_file_filtered, row.names = FALSE)

