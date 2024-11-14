library(dplyr)
library(tidyr)
library(ggplot2)

# ... (previous code remains unchanged)
df1 <- read_csv("data/combined/Allcombined.csv")
# Create a histogram faceted by Habitat
hist_plot <- ggplot(df1, aes(y = CN, fill = Habitat)) +
  geom_histogram(bins = 40, color = "white") +
  labs(
    title = NULL,
    y = expression("Soil CN ratio"),
    x = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 14),
    axis.line.x = element_line(linewidth = 0.2),
    axis.line.y = element_line(linewidth = 0.2),
    legend.position = c(0.4, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  ) +
  scale_y_continuous(limits = c(0, 50)) +
  #scale_y_log10() +  # Log-scale the y-axis
  facet_wrap(~Habitat, scales = "free_x")  # Facet by Habitat with free y-axes

print(hist_plot)


##################
#N density

# ... (previous code remains unchanged)
df1 <- read_csv("data/combined/Allcombined.csv")
# Create a histogram faceted by Habitat
hist_plot <- ggplot(df1, aes(y = Ndensity, fill = Habitat)) +
  geom_histogram(bins = 60, color = "white") +
  labs(
    title = NULL,
    y = expression("N density"),
    x = "Observations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 14),
    axis.line.x = element_line(linewidth = 0.2),
    axis.line.y = element_line(linewidth = 0.2),
    legend.position = c(0.4, 0.8),
    legend.title = element_blank(),
    legend.text = element_text(size = 11)
  ) +
  scale_y_continuous(limits = c(0, 10)) +
  #scale_y_log10() +  # Log-scale the y-axis
  facet_wrap(~Habitat, scales = "free_x")  # Facet by Habitat with free y-axes

print(hist_plot)
