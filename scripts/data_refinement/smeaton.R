## import data from Smeaton et al unpublished C-side dataset
## from sampling sites across UK saltmarshes
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 01.09.22
# edit 20.02.23 - separating into 2 datasets wide and narrow
# wide: https://doi.org/10.5285/279558cd-20fb-4f19-8077-4400817a4482



library(tidyverse)

input_data01 <- read.csv("data/raw_data/Smeaton_Physical_geochemical_properties_wide_coresX.csv")

### combine 


##### add informational  
source_name <- "Smeaton et al 2022b"
author_initials <- "CS"


input_data02 <- input_data01 %>% 
  dplyr::rename(Site = Saltmarsh,
                Soil_type = Substrate) %>% 
  mutate(Source = source_name,
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Core_ID),
         Habitat_type = "Saltmarsh",
         Country = "UK") %>% 
  # some core_IDs are not unique! need to pate with the site id
  mutate(Core = case_when(Core_ID == "1"|Core_ID == "10"|Core_ID == "11"|Core_ID == "12"|
                            Core_ID == "13"|Core_ID == "14"|Core_ID == "15"|Core_ID == "16"|
                            Core_ID == "17"|Core_ID == "18"|Core_ID == "19"|Core_ID == "2"|
                            Core_ID == "20"|Core_ID == "3"|Core_ID == "4"|Core_ID == "5"|
                            Core_ID == "6"|Core_ID == "7"|Core_ID == "8"|Core_ID == "9" 
                          ~ paste(Site, Core_ID),
                          TRUE ~ Core_ID)) 


#### reformat data ####

input_data03 <- input_data02 %>% 
  dplyr::rename(Latitude = Latitude_Dec_Degree,
                Longitude = Longitude_Dec_Degree,
                OC_perc = OC_Perc, 
                N_perc = N_Perc,
                BD_reported_g_cm3 = Dry_bulk_density_g_cm_3,
                Year_collected = Collection_Year) %>% 
  mutate(accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA")


## edit depth

input_data04 <- input_data03 %>% 
  mutate(Core_depth_cm = gsub("-", "_", Core_depth_cm)) %>% #some core depths were separated by - instead of _
  separate(Core_depth_cm, c("U_depth_cm", "L_depth_cm"), sep = '_') %>%   #separate upper and lower depth
  mutate(U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100,
         study_id="Smeaton_et_al", site_id=Site_name, core_id=Core_ID, DOI = "unpublished")

#note: one row miss-written

input_data05 <- input_data04 %>% 
  mutate(L_depth_cm = case_when(U_depth_cm == "2022" ~ "22",
                                TRUE ~ L_depth_cm),
         U_depth_cm = case_when(U_depth_cm == "2022" ~ "20",
                                TRUE ~ U_depth_cm))

#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(study_id, site_id, core_id, Habitat_type, Soil_type, Country, Nation, Year_collected,
                Latitude, Longitude,
                U_depth_m, L_depth_m, Method, OC_perc, N_perc, BD_reported_g_cm3)

#test <-  export_data01 %>% 
# mutate(GPS_combined = paste(Latitude, Longitude)) %>% 
 # group_by(Source, Site_name, Core)  %>% 
  #dplyr::summarise(distinct_location = n_distinct(GPS_combined))


## export

path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)
