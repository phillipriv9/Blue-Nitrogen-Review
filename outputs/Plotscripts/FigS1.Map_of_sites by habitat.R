# Install and load necessary libraries
# install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"))
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)

# Read your data (replace "your_data.csv" with your actual file name)
df1 <- read.csv("data/combined/AllCombined.csv")

df1_concatenated <- df1 %>%
  mutate(site_plot = paste(study_id, site_id, core_id, sep = "_"))

# Convert Latitude and Longitude to numeric if needed
df1_concatenated$Latitude <- as.numeric(df1_concatenated$Latitude)
df1_concatenated$Longitude <- as.numeric(df1_concatenated$Longitude)

# Filter dataset to include only "marsh" and "mangrove" habitats
df_filtered <- df1_concatenated %>%
  filter(Habitat %in% c("marsh", "mangrove"))

# Summarize
df_summarized <- df_filtered %>%
  group_by(site_plot) %>%
  summarize(
    MeanLatitude = mean(Latitude, na.rm = TRUE),
    MeanLongitude = mean(Longitude, na.rm = TRUE),
    Habitat = unique(Habitat)  # Capture all unique habitat values for a given site_plot
    # Add other summary statistics or variables as needed
  )

# Load world map data from rnaturalearth
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a scatter plot of latitude vs longitude with a world map background
map<-ggplot() +
  geom_sf(data = world, fill = "white", color = "gray") +
  geom_point(data = df_summarized, aes(x = MeanLongitude, y = MeanLatitude, color = Habitat), shape = 1, size = 2, alpha = 0.7) +
  labs(title = NULL,
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )

# Save the combined plots
ggsave("outputs/S1.Mapofsites.pdf", plot = map, width = 6, height = 4)  # Adjust overall dimensions as needed
ggsave("outputs/S1.Mapofsites.tif", plot = map, width = 6, height = 4, dpi=300)  # Adjust overall dimensions as needed

print(map)
