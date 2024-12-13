# Run Blue N project
# This script controls all the other relevant scripts

# Get data
script_folder <- "scripts/"

# List all files in the script folder with a .R extension
script_files <- list.files(script_folder, pattern = "\\.R$", full.names = TRUE)

# Loop through each script file and source it
for (script_file in script_files) {
  source(script_file)
}

# Combine data
source("scripts/synthesize/stackfromCCN.r")
source("scripts/synthesize/stackfromMaxwell.r")
source("scripts/synthesize/combine_all.r")

# Plot data
source("outputs/plotscripts/Fig1.HistoScatterplot.r")          # Fig 1
source("outputs/plotscripts/Fig2.NAR.r")                       # Fig 2
source("scenarios/Scenarios10-10-24.r")                        # Fig 4
source("outputs/plotscripts/FigS1.Map_of_sites by habitat.r")  # Fig S1
source("outputs/plotscripts/FigS2.violinplotofCNbyBD.r")       # Fig S2

# Stats
source("scripts/stats/N, CN, Ndensity.r")  # three mixed models

