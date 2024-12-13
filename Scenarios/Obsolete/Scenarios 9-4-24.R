
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# Define the sequence of years
years <- 2020:2100

# Create the data frame for each year
scenarios <- data.frame(
  scenario = rep(1:6, each = length(years)),
  scenarioname = rep(c('high loss/low slr', 'high loss/high slr', 'low loss/ low slr', 
                       'low loss/ high slr', 'high gain/low slr', 'high gain/high slr'), 
                     each = length(years)),
  R2020 = 3.5,
  R2100 = rep(c(4.1, 7.8, 4.1, 7.8, 4.1, 7.8), each = length(years)),
  A2020marsh = 52880,
  A2020mangrove = 200239,
  year = rep(years, times = 6)
)

# Calculate the exponential function (k) for each SLR scenario
scenarios$k <- log(scenarios$R2100 / scenarios$R2020) / 80

# Calculate the relative sea level rise (rSLR) for each year: rSLR = SLR2020 * e^(k*(year-2020))
scenarios$rSLR <- scenarios$R2020 * exp(scenarios$k * (scenarios$year - 2020))

# Calculate A2100marsh and A2100mangrove for high loss scenarios 1 and 2
scenarios$A2100marsh[scenarios$scenario %in% c(1, 2)] <- scenarios$A2020marsh[scenarios$scenario %in% c(1, 2)] - 20623
scenarios$A2100mangrove[scenarios$scenario %in% c(1, 2)] <- scenarios$A2020mangrove[scenarios$scenario %in% c(1, 2)] - 144412

# Calculate A2100marsh and A2100mangrove for low loss scenarios 3 and 4
scenarios$A2100marsh[scenarios$scenario %in% c(3, 4)] <- scenarios$A2020marsh[scenarios$scenario %in% c(3,4)] -1568
scenarios$A2100mangrove[scenarios$scenario %in% c(3, 4)] <- scenarios$A2020mangrove[scenarios$scenario %in% c(3,4)] -4421

# Calculate A2100marsh and A2100mangrove for high gain scenarios 5 and 6
scenarios$A2100marsh[scenarios$scenario %in% c(5, 6)] <- scenarios$A2020marsh[scenarios$scenario %in% c(5, 6)] + 20905
scenarios$A2100mangrove[scenarios$scenario %in% c(5, 6)] <- scenarios$A2020mangrove[scenarios$scenario %in% c(5, 6)] + 99434

# Calculate Areas for each year
scenarios$Amarsh <- (scenarios$A2100marsh - scenarios$A2020marsh) * (scenarios$year - 2020) / 80 + scenarios$A2020marsh
scenarios$Amangrove <- (scenarios$A2100mangrove - scenarios$A2020mangrove) * (scenarios$year - 2020) / 80 + scenarios$A2020mangrove
scenarios$Atotal <- scenarios$Amarsh + scenarios$Amangrove

# Calculate the cumulative sum of rSLR within each scenario
scenarios <- scenarios %>%
  group_by(scenario) %>%
  mutate(SL = cumsum(rSLR)) %>%
  ungroup()

# Calculate N gain using Ndensity for marsh and mangrove = 1.78 mg N cm-3
scenarios$Ngainmarsh <- ((scenarios$Amarsh) * (scenarios$SL/10) * (1.78/1000) *10^10)/10^12
scenarios$Ngainmangrove <- ((scenarios$Amangrove) * (scenarios$SL/10) * (1.78/1000) * 10^10)/10^12
scenarios$Ngaintotal <- scenarios$Ngainmarsh + scenarios$Ngainmangrove

# Calculate N loss using Ndensity for marsh, mangrove = 1.78 mg N cm-3, assuming 30 cm of erosive loss
scenarios$Nlossmarsh <- ifelse(   
  scenarios$Amarsh < scenarios$A2020marsh,
  (scenarios$Amarsh - scenarios$A2020marsh) * 30 * (1.78 / 1000) * 10^10 / 10^12,
  0)
scenarios$Nlossmangrove <- ifelse(
  scenarios$Amangrove < scenarios$A2020mangrove,
  (scenarios$Amangrove - scenarios$A2020mangrove) * 30 * (1.78 / 1000) * 10^10 / 10^12,
  0
)
scenarios$Nlosstotal <- scenarios$Nlossmarsh + scenarios$Nlossmangrove

# Calculate net N change
scenarios$netNmarsh <- scenarios$Ngainmarsh + scenarios$Nlossmarsh
scenarios$netNmangrove <- scenarios$Ngainmangrove + scenarios$Nlossmangrove
scenarios$netNtotal <- scenarios$netNmarsh + scenarios$netNmangrove

# Reorder the levels of scenarioname without duplication
scenarios$scenarioname <- factor(scenarios$scenarioname, levels = c('high gain/low slr', 'high gain/high slr', 'low loss/ low slr', 
                                                                    'low loss/ high slr', 'high loss/low slr', 'high loss/high slr'))

# Define plot colors
plot_colors <- brewer.pal(6, "Paired")

# Plot net N over time
plot_netN <- ggplot(scenarios, aes(x = year, y = netNtotal, color = scenarioname, group = scenario)) +
  geom_line() +
  scale_color_manual(values = plot_colors) +
  labs(title = NULL, x = "Year", y = "Net N Gain") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank())

# Create a common legend theme
legend_theme <- theme(
  legend.position = "right",
  legend.box = "vertical",
  legend.text = element_text(size = 8),
  legend.key.size = unit(0.5, 'cm'),      # Size of legend keys
  legend.spacing.x = unit(0.5, 'cm'),    # Horizontal spacing between legend items
  legend.spacing.y = unit(0.2, 'cm')     # Vertical spacing between legend items
)

# Plot net N over time
plot_netN <- ggplot(scenarios, aes(x = year, y = netNtotal, color = scenarioname, group = scenario)) +
  geom_line() +
  scale_color_manual(values = plot_colors) +
  labs(title = NULL, x = "Year", y = "Net N Gain") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(),   # Remove major gridlines
        panel.grid.minor = element_blank()) +
  legend_theme

# Plot gross N gain over time
plot_Ngain <- ggplot(scenarios, aes(x = year, y = Ngaintotal, color = scenarioname, group = scenario)) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = plot_colors) +
  labs(title = NULL, x = "Year", y = "Gross N gain") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),panel.grid.major = element_blank(),   # Remove major gridlines
        panel.grid.minor = element_blank()) +
  legend_theme

# Plot N loss over time
plot_Nloss <- ggplot(scenarios, aes(x = year, y = Nlosstotal, color = scenarioname, group = scenario)) +
  geom_line(show.legend = FALSE) +
  scale_color_manual(values = plot_colors) +
  labs(title = NULL, x = "Year", y = "GrossN Loss") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),   # Remove major gridlines
        panel.grid.minor = element_blank()) +
  legend_theme

# Combine the plots into a vertical layout
N_by_year <- plot_netN / plot_Ngain / plot_Nloss 

# Display the combined plot
print(N_by_year)


#Plot marsh elevation by area change

plot_elev_area <- ggplot(scenarios, aes(x = Atotal, y = rSLR, color = scenarioname, group = scenario)) +
  geom_line() +
  scale_color_manual(values = plot_colors) +
  labs(title = NULL, x = "Area", y = "Wetland elevation chagne") +
  theme_minimal() +
  theme( panel.grid.major = element_blank(),   # Remove major gridlines
        panel.grid.minor = element_blank()) +
  legend_theme

print(plot_elev_area)


####################################################################
###########################

#This version kinda works
#I want this to generate a smooth countour for the background of plot_elev_area, so i need to plot those lines over top of this countour.
plot_elev_area_contour <- ggplot() +
  # Use geom_raster for a smoother background
  geom_raster(data = contourdata, aes(x = area, y = SLRmmyr, fill = NetN)) +  
  scale_fill_viridis_c(option = "D", direction = -1) +  # Continuous color scale
  
  # Black lines with varying dash patterns for different scenarios
  geom_line(data = scenarios, aes(x = Atotal, y = rSLR, linetype = scenarioname, group = scenario), color = "black", "gray") +  
  
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) +  # Define different dash types
  
  labs(title = NULL, x = "Total Area", y = "Wetland Elevation Change") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, 'cm'),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.2, 'cm')
  ) +
  guides(fill = guide_colorbar(title = "Net N Gain"))  # Add a colorbar legend for the fill

print(plot_elev_area_contour)

############################################################
plot_elev_area_contour <- ggplot() +
  geom_tile(data = contourdata, aes(x = area, y = SLRmmyr, fill = NetN)) +  # Smoothed background with continuous fill
  scale_fill_viridis_c(option = "D", direction = -1) +  # Apply continuous color scale for fill
  geom_line(data = scenarios, aes(x = Atotal, y = rSLR, color = scenarioname, group = scenario)) +  # Overlay the line plot
  scale_color_manual(values = plot_colors) +
  labs(title = NULL, x = "Total Area", y = "Wetland Elevation Change (mm/yr)") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 12),  # Adjust Y-axis title size
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    legend.position = "right",  # Legend on the right
    legend.box = "vertical",  # Arrange legend items vertically
    legend.text = element_text(size = 8),  # Adjust legend text size
    legend.key.size = unit(0.5, 'cm'),  # Adjust legend key size
    legend.spacing.x = unit(0.5, 'cm'),  # Adjust horizontal spacing in the legend
    legend.spacing.y = unit(0.2, 'cm')   # Adjust vertical spacing in the legend
  ) +
  guides(fill = guide_colorbar(title = "Net N Gain", barwidth = 10, barheight = 1))  # Add and customize colorbar

print(plot_elev_area_contour)


# Create a grid of values

rSLR_values <- seq(3.5, max(scenarios$rSLR), by = .1)  # SL increments of 0.1 for smoothness
Amarsh_values <- seq(min(30000), max(100000), by = 10000)  # Amarsh increments of 500 for better resolution
Amangrove_values <- seq(min(100000), max(300000), by = 10000)  # Amangrove increments of 500 for better resolution

contour_grid <- expand.grid(

  rSLR = rSLR_values,
  Amarsh = Amarsh_values,
  Amangrove = Amangrove_values
)

# Calculate SL based on rSLR at 2100
contour_grid$SL <- contour_grid$rSLR^(0.1*contour_grid$rSLR) 

# Calculate Atotal
contour_grid$Atotal <- contour_grid$Amarsh + contour_grid$Amangrove

# Calculate N gain for the contour scenario
contour_grid$Ngainmarsh <- ((contour_grid$Amarsh) * (contour_grid$SL/10) * (1.78/1000) * 10^10) / 10^12
contour_grid$Ngainmangrove <- ((contour_grid$Amangrove) * (contour_grid$SL/10) * (1.78/1000) * 10^10) / 10^12
contour_grid$Ngaintotal <- contour_grid$Ngainmarsh + contour_grid$Ngainmangrove

# Calculate N loss for the contour scenario
contour_grid$Nlossmarsh <- ifelse(   
  contour_grid$Amarsh < min(scenarios$A2020marsh),
  (contour_grid$Amarsh - min(scenarios$A2020marsh)) * 30 * (1.78 / 1000) * 10^10 / 10^12,
  0)
contour_grid$Nlossmangrove <- ifelse(
  contour_grid$Amangrove < min(scenarios$A2020mangrove),
  (contour_grid$Amangrove - min(scenarios$A2020mangrove)) * 30 * (1.78 / 1000) * 10^10 / 10^12,
  0
)
contour_grid$Nlosstotal <- contour_grid$Nlossmarsh + contour_grid$Nlossmangrove

# Calculate net N change for the contour scenario
contour_grid$netNmarsh <- contour_grid$Ngainmarsh + contour_grid$Nlossmarsh
contour_grid$netNmangrove <- contour_grid$Ngainmangrove + contour_grid$Nlossmangrove
contour_grid$netNtotal <- contour_grid$netNmarsh + contour_grid$netNmangrove

# Calculate the cumulative sum of rSLR within each scenario
contour_grid <- contour_grid %>%
  mutate(SL = cumsum(rSLR))

plot_elev_area <- ggplot() +
  geom_raster(data = contour_grid, aes(x = Atotal, y = SLR, fill = netNtotal)) +  # Smoothed background with continuous fill
  scale_fill_viridis_c(option = "D", direction = -1) +  # Apply continuous color scale for fill
  geom_line(data = scenarios, aes(x = Atotal, y = rSLR, color = scenarioname, group = scenario)) +  # Overlay the line plot
  scale_color_manual(values = plot_colors) +
  labs(title = NULL, x = "Total Area", y = "Wetland Elevation Change") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.box = "vertical",
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, 'cm'),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.2, 'cm')
  ) +
  guides(fill = guide_colorbar(title = "Net N Gain"))  # Add a colorbar legend for the fill

# Print the updated plot
print(plot_elev_area)


# Specify the folder for saving the recategorized data
Scenarios <- "Scenarios"

# Create the output folder if it doesn't exist
if (!dir.exists(Scenarios)) dir.create(Scenarios)

# Write the recategorized data frame to a new CSV file in the "Recategorized" folder
Scenarios <- file.path(Scenarios, "scenarios.csv")
write.csv(scenarios, Scenarios, row.names = FALSE)
