
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

# Define the sequence of years
years <- 2020:2100

# Create the data frame for each year
scenarios <- data.frame(
  scenario = rep(1:6, each = length(years)),
  scenarioname = rep(c('Area loss/low slr', 'Area loss/high slr', 'Area stable/low slr', 
                       'Area stable/high slr', 'Area gain/low slr', 'Area gain/high slr'), 
                     each = length(years)),
  R2020 = 3.5,
  R2100 = rep(c(4, 8, 4, 8, 4, 8), each = length(years)),
  A2020marsh = 52880,
  A2020mangrove = 147349,
  year = rep(years, times = 6)
)

################################################################
##############################################################

# Define the sequence of historical years (1970 to 2019)
historical_years <- 1970:2019

# Create the data frame for the historical scenario
historical <- data.frame(
  scenario = rep(0, length(historical_years)),  # Scenario identifier
  scenarioname = rep('Historical', length(historical_years)),  # Scenario name
  historical_year = historical_years,  # Years from 1970 to 2019
  
  # Linear increase of rSLR from 2.1 to 3.5
  rSLR = seq.int(2.1, 3.5, length.out = length(historical_years)))
#	year year-1970	Atotal	Amarsh	Amangrove
#1970	  0	      308000	  81356	  226712   based on RAMSAR report
#1999	  29	    206764	  53944	  152920  based on Murray et al. 2022
#2019	  49	    200239	  52880	  147359  based on Murray et al. 2022
  
#Using broken linear model for area
historical$Amarsh <- ifelse(historical$historical_year<1999, 
                            (historical$historical_year-1970)*(53944-81356)/29+ 81356, 
                            (historical$historical_year-1970)*(52880-53944)/20+ (55487))
                            
historical$Amangrove <- ifelse(historical$historical_year<1999, 
                               (historical$historical_year-1970)*(152920-226712)/29+ 226712, 
                               (historical$historical_year-1970)*(147359-152920)/20+ (160983))

# Calculate Atotal as Amarsh + Amangrove
historical$Atotal <- historical$Amarsh + historical$Amangrove

# Calculate cumulative sea level (SL) with an adjustment factor
historical$SL <- cumsum(historical$rSLR) - 137.5

# Calculate nitrogen gain for marshes and mangroves in Tg
historical$Ngainmarsh <- ((historical$Amarsh) * (historical$SL / 10) * (1.78 / 1000) * 10^10) / 10^12
historical$Ngainmangrove <- ((historical$Amangrove) * (historical$SL / 10) * (1.63 / 1000) * 10^10) / 10^12
historical$Ngaintotal <- historical$Ngainmarsh + historical$Ngainmangrove

# Set 2020 area averages
historical$A2020marsh <- mean(scenarios$A2020marsh)
historical$A2020mangrove <- mean(scenarios$A2020mangrove)

# Calculate N loss using Ndensity for marsh and mangrove
historical$Nlossmarsh <- ifelse(   
  historical$Amarsh > historical$A2020marsh,
  (historical$Amarsh - historical$A2020marsh) * 30 * (1.78 / 1000) * 10^10 / 10^12,
  0
)

historical$Nlossmangrove <- ifelse(
  historical$Amangrove > historical$A2020mangrove,
  (historical$Amangrove - historical$A2020mangrove) * 30 * (1.63 / 1000) * 10^10 / 10^12, 
  0
)

historical$Nlosstotal <- historical$Nlossmarsh + historical$Nlossmangrove

# Calculate net nitrogen total
historical$netNtotal <- historical$Ngaintotal + historical$Nlosstotal

# View the data frame
print(historical)

##############################################################
###########################################################

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

# wetland elevation gain, marsh maximal gain is 5 mm yr-1, mangrove max = 7.8, if rSLR > than the max, wetland elevation gain = 0
# 9-13-24 OK I set it back to just keep up with all SLR
scenarios <- scenarios %>%
  mutate(marshgain = rSLR,  #ifelse(rSLR > 5, 0, rSLR),
         mangrovegain = rSLR) #ifelse(rSLR > 7.8, 0, rSLR))

# Calculate the cumulative sum of marsh and mangrove elevation within each scenario
scenarios <- scenarios %>%
  group_by(scenario) %>%
  mutate(marshelev = cumsum(marshgain), mangroveelev = cumsum(mangrovegain)) %>%
  ungroup()

# Calculate N gain using Ndensity for marsh and mangrove = 1.78 and 1.63 mg N cm-3
#Area (km2) * elevation change in cm * N density g cm-3 * 10^10 cm2/km2
scenarios$Ngainmarsh <- ((scenarios$Amarsh) * (scenarios$SL/10) * (1.78/1000) *10^10)/10^12
scenarios$Ngainmangrove <- ((scenarios$Amangrove) * (scenarios$SL/10) * (1.63/1000) * 10^10)/10^12
scenarios$Ngaintotal <- scenarios$Ngainmarsh + scenarios$Ngainmangrove

#add up annual wetland Ngain through vertical accretion
#scenarios <- scenarios %>%  
#  group_by(scenario) %>%
#  mutate(marshNgain = cumsum(Ngainmarsh), mangroveNgain= cumsum(Ngainmangrove)) %>%
#  ungroup()

scenarios$Ngaintotal <- scenarios$Ngainmarsh + scenarios$Ngainmangrove

# Calculate N loss using Ndensity for marsh, mangrove = 1.78 and 1.63 mg N cm-3, assuming 30 cm of erosive loss from area that is lost
scenarios$Nlossmarsh <- ifelse(   
  scenarios$Amarsh < scenarios$A2020marsh,
  (scenarios$Amarsh - scenarios$A2020marsh) * 50 * (1.78 / 1000) * 10^10 / 10^12,
  0)
scenarios$Nlossmangrove <- ifelse(
  scenarios$Amangrove < scenarios$A2020mangrove,
  (scenarios$Amangrove - scenarios$A2020mangrove) * 50 * (1.63 / 1000) * 10^10 / 10^12,
  0
)
scenarios$Nlosstotal <- scenarios$Nlossmarsh + scenarios$Nlossmangrove

# Calculate net N change
scenarios$netNmarsh <- scenarios$Ngainmarsh + scenarios$Nlossmarsh
scenarios$netNmangrove <- scenarios$Ngainmangrove + scenarios$Nlossmangrove
scenarios$netNtotal <- scenarios$netNmarsh + scenarios$netNmangrove


scenarios$scenarioname <- factor(scenarios$scenarioname, levels = c('Area gain/low slr', 'Area gain/high slr','Area loss/low slr', 'Area loss/high slr', 'Area stable/low slr', 
                                                                    'Area stable/high slr' ))


# Define plot colors
plot_colors <- brewer.pal(6, "Paired")



####################################################
# Create a common legend theme
legend_theme <- theme(
  legend.position = "right",
  legend.box = "vertical",
  legend.text = element_text(size = 8),
  legend.key.size = unit(0.5, 'cm'),      # Size of legend keys
  legend.spacing.x = unit(0.5, 'cm'),    # Horizontal spacing between legend items
  legend.spacing.y = unit(0.2, 'cm')     # Vertical spacing between legend items
)



#####################################################
#####################################################

# Merge historical data into scenarios data frame for plotting
historical_plot_data <- historical %>%
  select(historical_year, Ngainmarsh, Ngainmangrove, Ngaintotal, Nlossmarsh, Nlossmangrove, Nlosstotal, netNtotal, Atotal, SL,rSLR) %>%
  mutate(
    year = historical_year,
    scenarioname = "Historical",  # Set a unique name for historical data
    scenario = 0  # Use a distinct identifier for historical data
  )


library(ggplot2)
library(dplyr)
library(patchwork)

# Define line types for scenarios
line_types <- c("solid", "dashed")  # Solid for other scenarios, Dashed for scenario = 0

# Combine historical and scenario data
combined_data <- bind_rows(scenarios, historical_plot_data)


# Define a new column in combined_data to specify the line type
combined_data <- combined_data %>%
  mutate(linetype = ifelse(scenario == 0, "solid", "dotted"))

# Plot net N over time
plot_netN <- ggplot(combined_data, aes(x = year, y = netNtotal, color = scenarioname, group = scenario, linetype = linetype)) +
  geom_line(size=.8) +
  scale_color_manual(values = c(plot_colors, "black")) +  # Add color for historical data
  #scale_linetype_manual(values = line_types) +  # Apply custom line types
  labs(title = NULL, x = "Year", y = "Net N Gain") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.ticks.y = element_line(size=.5), axis.text.y = element_text(size = 14), axis.text.x=element_blank(),axis.line = element_line(size=.5, color="gray50"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(2000, 2100)) +  # Set x-axis limits to start at 2000
  legend_theme

# Plot gross N gain over time
plot_Ngain <- ggplot(combined_data, aes(x = year, y = Ngaintotal, color = scenarioname, group = scenario, linetype = linetype)) +
  geom_line(size=.8) +
  scale_color_manual(values = c(plot_colors, "black")) +  # Add color for historical data
  #scale_linetype_manual(values = line_types) +  # Apply custom line types
  labs(title = NULL, x = "Year", y = "Gross N Gain") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.ticks.y = element_line(size=.5), axis.text.y = element_text(size = 14),axis.text.x=element_blank(),axis.line = element_line(size=.5, color="gray50"),panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(2000, 2100)) +  # Set x-axis limits to start at 2000
  scale_y_continuous(limits = c(-20, 250))
  legend_theme

  
  #jitter N Loss so that all traces can be seen 
 
  jittered_data <- combined_data %>%
    mutate(Nlosstotal = ifelse(is.na(R2100), 
                               Nlosstotal, 
                               ifelse(R2100 > 5, 
                                      Nlosstotal + 2, 
                                      Nlosstotal)))
  
# Plot N loss over time
plot_Nloss <- ggplot(jittered_data, aes(x = year, y = Nlosstotal, color = scenarioname, group = scenario, linetype = linetype)) +
  geom_line(size=.8) +
  scale_color_manual(values = c(plot_colors, "black")) +  # Add color for historical data
  #scale_linetype_manual(values = line_types) +  # Apply custom line types
  labs(title = NULL, x = "Year", y = "Gross N Loss") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.ticks = element_line(size=.5), axis.text = element_text(size = 14), axis.line = element_line(size=.5, color="gray50"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(2000, 2100)) + 
  scale_y_continuous(limits = c(-150, 50)) + 
  legend_theme

# Combine the plots into a vertical layout
N_by_year <- plot_netN / plot_Ngain / plot_Nloss

# Display the combined plot
print(N_by_year)


###########################################################
# Plot elevation by year

# Combined plot: Overlay mangrove and marsh elevation for each R2100 level
plot_combined_elev_year <- ggplot(scenarios) +
  
  # Line for mangrove elevation at each R2100 level
  geom_line(aes(x = year, y = mangroveelev + 2, color = "Mangrove", linetype = as.factor(R2100), group = interaction(R2100, "Mangrove"))) +
  
  # Line for marsh elevation at each R2100 level
  geom_line(aes(x = year, y = marshelev - 2, color = "Marsh", linetype = as.factor(R2100), group = interaction(R2100, "Marsh"))) +
  
  # Add dotted line for historical data
  geom_line(data = historical_plot_data, aes(x = historical_year, y = SL, color = "Historical", linetype = "dotted"), size = 1) +
  
  # Define colors for mangrove vs marsh and historical data
  scale_color_manual(values = c("Mangrove" = "salmon", "Marsh" = "lightseagreen", "Historical" = "black")) +
  
  # Different linetypes for R2100 levels
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "dotted")) +  # Ensure dotted is added for historical data
  
  labs(title = NULL, x = "Year", y = "Wetland Elevation Change (mm)") +
  
  # Minimal theme and remove gridlines
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right") +
  
  # Customize legends for both color (mangrove vs marsh vs historical) and linetype (R2100 levels)
  guides(color = guide_legend(title = "Elevation Type"), linetype = guide_legend(title = "Sea Levels Scenarios"))

# Print the combined plot
print(plot_combined_elev_year)

###########################################################
# Plot rSLR by year

# Combined plot: Overlay mangrove and marsh elevation for each R2100 level
plot_combined_elev_year <- ggplot(scenarios) +
  
  # Line for mangrove elevation at each R2100 level
 # geom_line(aes(x = year, y = rSLR + 2, color = "Mangrove", linetype = as.factor(R2100), group = interaction(R2100, "Mangrove"))) +
  
  # Line for marsh elevation at each R2100 level
 geom_line(aes(x = year, y = rSLR - 2, color = "Marsh", linetype = as.factor(R2100))) +
  
  # Add dotted line for historical data
 # geom_line(data = historical_plot_data, aes(x = historical_year, y = SL, color = "Historical", linetype = "dotted"), size = 1) +
  
  # Define colors for mangrove vs marsh and historical data
  scale_color_manual(values = c("Mangrove" = "salmon", "Marsh" = "lightseagreen", "Historical" = "black")) +
  
  # Different linetypes for R2100 levels
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "dotted")) +  # Ensure dotted is added for historical data
  
  labs(title = NULL, x = "Year", y = "Wetland Elevation Change (mm)") +
  
  # Minimal theme and remove gridlines
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right") +
  
  # Customize legends for both color (mangrove vs marsh vs historical) and linetype (R2100 levels)
  guides(color = guide_legend(title = "Elevation Type"), linetype = guide_legend(title = "Sea Levels Scenarios"))

# Print the combined plot
print(plot_combined_elev_year)

####################################################
#Plot area by year 
# Combined plot: Overlay mangrove and marsh area
plot_combined_elev_year <- ggplot(scenarios) +
  
  # Line for mangrove elevation at each R2100 level
  geom_line(aes(x = year, y = Atotal, color = "Future", linetype = "dashed", group = scenario)) +
  
  # Line for marsh elevation at each R2100 level
  #geom_line(aes(x = year, y = Amarsh, color = "Marsh", linetype = "solid", group = scenario)) +
  
  # Add dotted line for historical data
  geom_line(data = historical_plot_data, aes(x = historical_year, y = Atotal, color = "Historical", linetype = "solid"), size = 0.5) +
  
  # Define colors for mangrove vs marsh and historical data
  scale_color_manual(values = c("Future" = "red4",  "Historical" = "black")) +
  
  # Different linetypes for scenarios
 scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "dotted", "solid")) +  # Ensure dotted is added for historical data
  
  labs(title = NULL, x = "Year", y = "Wetland Area change (km2)") +
  
  # Minimal theme and remove gridlines
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "right") +
  
  # Customize legends for both color (mangrove vs marsh vs historical) and linetype (Area scenarios)
  guides(color = guide_legend(title = "Era"), linetype = guide_legend(title = "AreaScenario"))

# Print the combined plot
print(plot_combined_elev_year)


##################################################################

##################################################################
#####CONTOUR

# Create a grid of values for the background contour representing net N gain
# 9-5-2024

SL_values <- seq(-100, 500, by = 5)  # SL increments of 0.1 for smoothness
Amarsh_values <- seq(min(5000), max(90000), by = 1000)  # Amarsh increments of 5000 for better resolution
#Amangrove_values <- seq(min(5000), max(250000), by = 5000)  # Amangrove increments of 5000 for better resolution

contour_grid <- expand.grid(
  
  SL = SL_values,
  Amarsh = Amarsh_values
  #Amangrove = Amangrove_values
)

# Calculate Atotal
contour_grid$Amangrove = contour_grid$Amarsh*2.78647881997
contour_grid$Atotal <- contour_grid$Amarsh + contour_grid$Amangrove

# Calculate N gain for the contour scenario
contour_grid$Ngainmarsh <- ((contour_grid$Amarsh) * (contour_grid$SL/10) * (1.78/1000) * 10^10) / 10^12
contour_grid$Ngainmangrove <- ((contour_grid$Amangrove) * (contour_grid$SL/10) * (1.63/1000) * 10^10) / 10^12
contour_grid$Ngaintotal <- contour_grid$Ngainmarsh + contour_grid$Ngainmangrove

# Calculate N loss for the contour scenario
contour_grid$Nlossmarsh <- ifelse(   
  contour_grid$Amarsh < 52880,
  (contour_grid$Amarsh - 52880) * 50 * (1.78 / 1000) * 10^10 / 10^12,0)
contour_grid$Nlossmangrove <- ifelse(
  contour_grid$Amangrove < 147349,
  (contour_grid$Amangrove - 147349) * 50 * (1.63 / 1000) * 10^10 / 10^12,0)
contour_grid$Nlosstotal <- contour_grid$Nlossmarsh + contour_grid$Nlossmangrove

# Calculate net N change for the contour scenario
contour_grid$netNmarsh <- contour_grid$Ngainmarsh + contour_grid$Nlossmarsh
contour_grid$netNmangrove <- contour_grid$Ngainmangrove + contour_grid$Nlossmangrove
contour_grid$netNtotal <- contour_grid$netNmarsh + contour_grid$netNmangrove

# Specify the folder for saving the recategorized data
Scenarios <- "Scenarios"

# Create the output folder if it doesn't exist
if (!dir.exists(Scenarios)) dir.create(Scenarios)

# Write the recategorized data frame to a new CSV file in the "Recategorized" folder
Scenarios <- file.path(Scenarios, "scenarios.csv")
write.csv(scenarios, Scenarios, row.names = FALSE)

###########################################################################


library(ggplot2)
library(scales)
library(tibble)
library(colorspace)
library(dplyr)  # For filtering

# Filter combined_data for years > 1999
filtered_data <- combined_data %>% filter(year > 1983)


plot_elev_area <- ggplot() +
  
  # Smoothed background with continuous fill
  #geom_tile(data = contour_grid, aes(x = Atotal, y = SL, fill = netNtotal)) + 
  #scale_fill_continuous_divergingx(palette = 'BrBg') + 
  
  geom_tile(data = contour_grid, aes(x = Atotal, y = SL, fill = netNtotal)) + 
  scale_fill_gradientn(
    colors = c("darkorange1", "white", "navyblue"), # More colors can be added here
    values = scales::rescale(c(min(contour_grid$netNtotal), 0, max(contour_grid$netNtotal))),
    limits = c(min(contour_grid$netNtotal), max(contour_grid$netNtotal)) # Set limits if needed
  ) +
  theme_minimal()+ # Optional: change the theme as needed
  
  # scale_fill_gradient2(name=waiver(), low = "red", mid="white", high = "blue", space = "Lab", na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
  #scale_fill_gradientn(colours=terrain.colors(100))+
  
  #just show years 2000-2100
 
  # Black lines for each scenario
  geom_line(data = filtered_data, aes(x = Atotal, y = SL, linetype = scenarioname, group = scenario), 
            color = "gray25", size=0.6) +
  
  # Format x-axis with tick marks and comma-separated labels
  scale_x_continuous(name = "Total wetland area (km2)", labels = comma) +
  
  # Define different dash types for scenarios
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid", "solid", "dotted")) +
  
  # Customize labels
  labs(title = NULL, y = expression("Cumulative wetland elevation change (mm)")) +
  
  # Adjust the theme
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks = element_line(color = "gray25"),  # Set ticks color
    axis.ticks.length = unit(0.15, 'cm'),  # Set tick length
    panel.border=element_blank(),
    axis.text= element_text(size = 10),
    axis.title = element_text(size = 12),
    axis.ticks.x = element_line(size = 0.5),  # Customize X-axis ticks
    axis.ticks.y = element_line(size = 0.5),  # Customize Y-axis ticks
    legend.position = "bottom",  # Position legend to the right
    legend.box = "horizontal", 
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, 'cm'),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.spacing.y = unit(0.2, 'cm')
  ) +
  
  # Add a colorbar legend for the fill
  guides(fill = guide_colorbar(title = "Net N Gain"))

# Plot
print(plot_elev_area)

# Specify the folder for saving the recategorized data
Scenarios <- "Scenarios"

# Create the output folder if it doesn't exist
if (!dir.exists(Scenarios)) dir.create(Scenarios)

# Write the recategorized data frame to a new CSV file in the "Recategorized" folder
Scenarios <- file.path(Scenarios, "scenariosfiltered.csv")
write.csv(filtered_data, Scenarios, row.names = FALSE)

# Specify the folder for saving the recategorized data
Scenarios <- "Scenarios"

# Create the output folder if it doesn't exist
if (!dir.exists(Scenarios)) dir.create(Scenarios)
Scenarios <- file.path(Scenarios, "contourgrid.csv")
write.csv(contour_grid, Scenarios, row.names = FALSE)