library(ggplot2)
library(dplyr)
library(tidyr)

setwd("~/Villanova Dropbox/Adam Langley/Manuscripts/Blue nitrogen/BlueNApril")
# Read the CSV files into data frames
df1 <- read.csv("data/carbon/Wang_et_al_2021_world_car.csv")
regions <- read.csv("data/carbon/all.csv")

# Perform a left join to combine the data frames based on the country codes
df2 <- left_join(df1, regions)

# Use gather to transform the data frame into long format
df_long <- df2 %>%
  gather(key = "category", value = "value", starts_with("nar"))



# Other code...

# Other code...

# Create the bar plot with regions on the x-axis, faceted by region
ggplot(df_long, aes(x = category, y = value, fill = category)) +
  geom_bar(stat = "identity") +
  
  labs(title = NULL,
       x = NULL,
       y = expression("N accumulation rate (Gg yr-1)")) +  # Superscript -1
  scale_fill_manual(values = c("nar_marsh" = "#00BFC4", 
                               "nar_mangrove" = "#F8766D", 
                               "nar_total" = "slateblue4")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_blank()) +  # Remove vertical gridlines
  scale_y_continuous(breaks = seq(0, 2700, by = 300))  # Set tick marks at every 300 units, up to 3000


# Calculate the mean value of area_tidal_marsh_km2
mean_area_tidal_marsh <- sum(df2$area_mangrove_km2, na.rm = TRUE)

# Print the mean value in the console
print(paste("Mean area_mangrove_km2:", round(mean_area_tidal_marsh, 2)))

