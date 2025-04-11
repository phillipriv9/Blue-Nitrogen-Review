# BlueNitrogen

The compiled Blue N database is called “AllCombined.csv” in data>combined
To generate most plots from the paper, use .r files in outputs>Plotscripts
To generate Figure 4, use “scenarios10-10-24.r” in Scenarios folder
To run statistical tests, use .r files in scripts>stats

Description of each folder

Folder "data" contains raw data, refined data and final collated data files. 

subfolders
-> raw_data- contains all raw datafiles that are not linked to a permanent url
-> coredata- contains some metadatafiles from maxwell database
-> refined- data that has been tidied from raw version
-> CCN- similar to refined, but from CCN. contains merged and refined core + metadata from some sources in the CCN
-> combined- all refined data stacked together
-> carbon- files needed to extrapolate to global NAR


Folder "outputs" contains plots, some data tables and scripts to generate them 

subfolder
->plotscripts

Folder "Scenarios" contains the code to generate projections 



Folder "scripts"
subfolder
  -> data_refinement- contains code for refining raw data
  -> stats- contains code for statistical tests
  -> synthesize- code for appending data from individual refined datafiles
