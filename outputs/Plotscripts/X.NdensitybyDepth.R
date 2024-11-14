library(ggplot2)
library(ggExtra)  # Make sure you have this for ggMarginal


df1 <- read.csv("data/combined/AllcombinedUnfiltered.csv")
df2 <- read.csv("data/combined/Allcombined.csv")


# Create a scatter plot with open circles and color by Habitat
Ndensitybydepth <- ggplot(df2, aes(x = centerdepth, y = Ndensity, color = Habitat)) +
  geom_point(shape = 1, alpha = 0.3) +  # Use shape = 1 for open circle and set transparency
  labs(title = NULL,
       x = "Centerdepth (m)",
       y = "Soil N density (mg cm-3)") +
  scale_y_continuous(limits = c(0, 10)) +  # Set y-axis limits to 0-50
  theme_minimal() +  # Adjusted to remove the gray background
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "lightgray", linewidth = .3),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Add a single linear best fit line for all data
  theme(legend.position = "none") +  # Eliminate legend
  guides(color = guide_legend(title.position = "top"))   # Adjust legend title position


# Create a scatter plot with open circles and color by Habitat
Ndensitybystudy <- ggplot(df2, aes(x = centerdepth, y = Ndensity, color = study_id)) +
  geom_point(shape = 1, alpha = 0.3) +  # Use shape = 1 for open circle and set transparency
  labs(title = NULL,
       x = "Centerdepth (m)",
       y = "Soil C:N ratio") +
  scale_y_continuous(limits = c(0, 10)) +  # Set y-axis limits to 0-50
  theme_minimal() +  # Adjusted to remove the gray background
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "lightgray", linewidth = .3),   axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank()) +
  geom_smooth(aes(group = study_id), method = "lm", se = FALSE) +  # Add linear best fit lines
  theme(legend.position = "none") +  # Eliminate legend
  guides(color = guide_legend(title.position = "top"))   # Adjust legend title position


Ndensity <- ( Ndensitybydepth + Ndensitybystudy)
print(CNfigs)

# Save the plot
ggsave("outputs/X.Ndensity2panel.pdf", plot = Ndensity, width = 8, height = 5)
