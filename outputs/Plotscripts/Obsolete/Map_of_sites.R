# Install and load necessary libraries
# install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)



df1 <- read.csv("data/combined/AllCombined.csv")

df1_concatenated <- df1 %>%
  mutate(site_plot = paste(study_id, site_id, core_id, sep = "_"))

# Convert Latitude and Longitude to numeric if needed
df1_concatenated$Latitude <- as.numeric(df1_concatenated$Latitude)
df1_concatenated$Longitude <- as.numeric(df1_concatenated$Longitude)

# summarize
df1_summarized <- df1_concatenated %>%
  group_by(site_plot) %>%
  summarize(
    MeanLatitude = mean(Latitude, na.rm = TRUE),
    MeanLongitude = mean(Longitude, na.rm = TRUE),
    Habitat = Habitat # Add other summary statistics or variables as needed
  )

# Load world map data from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a scatter plot of latitude vs longitude with a world map background
ggplot() +
  geom_sf(data = world, fill = "white", color = "gray") +
  geom_point(data = df1_summarized, aes(x = MeanLongitude, y = MeanLatitude, color = Habitat), shape = 1, size = 1.5, alpha=.5) +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none"  # Remove legend
  ) +
  scale_color_manual(values = c("marsh" = "#00BFC4", "mangrove" = "#F8765D")) + # Specify colors for each level
  coord_sf(lims_method = "geometry_bbox")  # Set lims_method to resolve the issue


