## import data from Miller et al 2022, Marine Scotland dataset
## from Scottish soils (narrow vs wide sampling)
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 11.08.22


library(tidyverse)




input_data_narrow <- read.csv("data/raw_data/Physical_geochemical_properties_narrow_cores.csv")

narrow_cores <- input_data_narrow %>% 
  mutate(Core_type = "Narrow")


input_data_wide <- read.csv("data/raw_data/Physical_geochemical_properties_wide_cores.csv")

wide_cores <- input_data_wide %>% 
  mutate(Core_type = "Wide") %>% 
  rename( Sample_depth_cm = Sample_depth_interval_cm, #to match the narrow core dataset
         Mid_Point_depth_cm = Mid_point_depth_cm,
         Dry_bulk_density_g_cm_3 = Dry_Bulk_Density_g_cm_3)


## merge datasets
input_data01 <- rbind(narrow_cores, wide_cores)


# PR Tryng to pipe here?
input_data02 <- input_data01 #%>% 
  #filter(Marsh_zone != "Mudflat" )


##### add informational  
source_name <- "Miller et al 2022"
author_initials <- "LCM"



input_data03 <- input_data02 %>%
  rename(State = Local_authority,
     ) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Habitat_type = "Salt marsh",
         Country = "UK") 


#### reformat data ####

input_data04 <- input_data03 %>% 
  rename(Latitude = Lat_dec_deg,
         Longitude = Long_dec_deg,
         Year_collected = Sampling_year,
         BD_reported_g_cm3 = Dry_bulk_density_g_cm_3) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA",
         DOI = "https://doi.org/10.7489/12422-1")


## edit depth

input_data05 <- input_data04 %>% 
  filter(Sample_depth_cm != "Surface") %>% 
  separate(Sample_depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '_') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100, # cm to m
        study_id= "Miller_et_al_2022", Core_type = "Wide", site_id= paste(Marsh_ID, Marsh_zone, sep="_"), core_id=Core_ID)

#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Country, State, Year_collected,
                Latitude, Longitude,
                U_depth_m, L_depth_m, Method, OC_perc, N_perc, BD_reported_g_cm3, DOI)


## export

path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)




