# Load the necessary library
library(readr)
library(dplyr)

# Read the CSV file into a data frame
#df <- read_csv("data/combined/CCN_combined_data.csv")
#df<- read_csv("data/combined/Maxwell_combined_data.csv")

#total segments for CCN
unique_combinations <- df %>%
  filter(Database %in% ("CCN"))%>%
  distinct(study_site_core, U_depth_m) %>%
  summarise(unique_combinations_count = n()) 
  # Print the result
  print(unique_combinations)
  
#for MAxwell
  unique_combinations <- df %>%
    filter(Database %in% ("Maxwell"))%>%
    distinct(study_site_core, U_depth_m) %>%
    summarise(unique_combinations_count = n()) 
  # Print the result
  print(unique_combinations)

 
  count_by_variable <- df %>%
    count(study_id)
  # Print the result
  print(count_by_variable)

  
print(unique(df$study_id))
print(unique(df$site_id))
print(unique(df$core_id))


#number of cores
CCN <- df %>%
filter(Database %in% ("CCN"))
print(unique(CCN$study_site_core))

unique_combinations <- CCN %>%
  distinct(study_site_core, U_depth_m) %>%
  summarise(CCN_count = n()) 
print(unique_combinations)

204+527
2666+3525


#number of cores
Maxwell <- df %>%
  filter(Database %in% ("Maxwell"))
print(unique(Maxwell$study_site_core))

#number of Segments
unique_combinations <- Maxwell  %>%
  distinct(study_site_core, U_depth_m) %>%
  summarise(Maxwell_count = n()) 
print(unique_combinations)



print(unique(df$Plot))
print(unique(df$porewater_salinity))




# Print the number of rows in the data frame
cat("Number of rows:", nrow(df), "\n")

# Count the number of non-missing values in the "salinity_class" column
non_missing_salinity_rows <- sum(!is.na(df$OC_perc))
cat("Number of rows with values for 'N':", non_missing_salinity_rows, "\n")


# Display the first few rows of the data frame
head(df)
head(df2)


#estimate mean N density
mean(df$N_perc/100*df$BD_reported_g_cm3, na.rm = TRUE)

mangrove_data <- df %>% filter(Habitat=="mangrove")
marsh_data <- df %>% filter(Habitat=="marsh")
mean(marsh_data$N_perc/100*marsh_data$BD_reported_g_cm3, na.rm = TRUE) 
mean(mangrove_data$N_perc/100*mangrove_data$BD_reported_g_cm3, na.rm = TRUE)
sd(marsh_data$N_perc/100*marsh_data$BD_reported_g_cm3, na.rm = TRUE) 
sd(mangrove_data$N_perc/100*mangrove_data$BD_reported_g_cm3, na.rm = TRUE)




