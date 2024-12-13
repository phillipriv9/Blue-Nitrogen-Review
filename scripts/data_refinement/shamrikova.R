## import data from Shamrikova et al 2019, EURASIAN SOIL SCIENCE
## from Barents Sea Coastal Area (Khaypudyrskaya Bay)
## export for marsh soil C
# contact Tania Maxwell, tlgm2@cam.ac.uk
# 05.08.22
# 15.05.23 including soil_type column

library(tidyverse)


input_data01 <- read.csv("data/raw_data/Shamrikova_2019_Table1.csv")



##### add informational  
source_name <- "Shamrikova et al 2019"
author_initials <- "EVS"


input_data02 <- input_data01 %>% 
  dplyr::rename(Plot = Site) %>% 
  mutate(Source = source_name,
         Site = "Khaypudyrskaya Bay",
         Source_abbr = author_initials,
         Site_name = paste(Source_abbr, Plot),
         Country = "Russia") 


#### reformat data ####

input_data03 <- input_data02 %>% 
  mutate(Year_collected = NA,
         accuracy_flag = "direct from dataset",
         accuracy_code = "1") %>% 
  mutate(Method = "EA", 
         U_depth_m = as.numeric(U_depth_cm)/100 , #cm to m
         L_depth_m = as.numeric(L_depth_cm)/100)# cm to m

### replace NAs from depth and calculate Corg 

input_data04 <- input_data03 %>% 
  mutate(L_depth_m = case_when(Plot == "Plot V" & U_depth_cm == 38 ~ 55/100,
                               Plot == "Plot V" & L_depth_cm == 76 ~ 76/100,
                               TRUE ~ L_depth_m)) %>% 
  mutate(U_depth_m = case_when(Plot == "Plot V" & L_depth_m == 0.76 ~ 55/100,
                               TRUE ~ U_depth_m))

### DATA IS IN PER MILLE (g / kg), so need to divide by 10 to get percent

input_data05 <- input_data04 %>% 
  mutate(Ctot_perc = Ctot_perc/10,
         Cinorg_perc = Cinorg_perc/10,
         N_perc = Ntot_perc/10) %>% 
  mutate(OC_perc = case_when(is.na(Cinorg_perc) == F ~ Ctot_perc - Cinorg_perc,
                             TRUE ~ Ctot_perc), study_id="Shamrikova_et_al_2019", site_id=Site, core_id="1")


#### export ####

export_data01 <- input_data05 %>% 
  dplyr::select(Source, Site_name, Site, Plot, Habitat_type, Soil_type, Country, Year_collected,
                Latitude, Longitude, accuracy_flag, accuracy_code,
                U_depth_m, L_depth_m, Method, OC_perc, Ctot_perc, Cinorg_perc, N_perc) %>% 
  mutate(DOI = "https://doi.org/10.1134/S1064229319030098")

#No export_data02 object


#AL: don't wnat to filter
#export_data03 <- export_data02 %>% 
 # filter(Habitat_type == "Marsh")

## export

path_out = 'data/refined/'

export_file <- paste(path_out, source_name, ".csv", sep = '') 
export_df <- export_data01

write.csv(export_df, export_file)



