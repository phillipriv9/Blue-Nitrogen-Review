
library(ggplot2)
library(ggExtra)
library(dplyr)


df1 <- read.csv("data/combined/Allcombined.csv")

# Create a scatter plot with open circles and color by Habitat
plot <- ggplot(df1, aes(x = BD_reported_g_cm3, y = CN, color = Habitat)) +
  geom_point(shape = 1, alpha = 0.5) +  # Use shape = 1 for open circle and set transparency
  labs(title = NULL,
       x = "Bulk density g cm-3",
       y = "C:N ratio") +
  theme_minimal() +  # Adjusted to remove the gray background
  theme(panel.grid = element_blank(), 
        axis.line = element_line(color = "lightgray", linewidth = .3),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14)) +
  geom_smooth(aes(group = Habitat), method = "lm", se = FALSE) +  # Add linear best fit lines
  theme(legend.position = "none") +  # Eliminate legend
  guides(color = guide_legend(title.position = "top")) +
           scale_x_continuous(limits = c(0, 1))+
          scale_y_continuous(limits = c(0, 50))  # Set x-axis limits
          
# Create marginal histograms with white fill, aligned with scatterplot axes
histo <- ggMarginal(plot, type = "histogram", bins = 30, fill = "lightgray", 
                    margins = "both", size = 4) 


print(histo)

# Save the net N plots
ggsave("outputs/1.CN_depth_histo.pdf", plot = histo, width = 5, height = 5)
