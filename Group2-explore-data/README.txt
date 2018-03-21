#03/21/2018 Nina Lany

This folder contains the scripts to prepare the L3 data and perform basic data checks prior to analysis.

1. First, make a script for each dataset in the ‘format_data’ directory that reads in raw data (typically L0, typically from Google Drive), puts it in the correct working group format, and saves it as a single csv to the directory Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by+year_and_space. If more than one group of organisms was observed in a dataset (e.g. mobile inverts, sessile inverts, coral, and fish), each group gets its own formatting script and L3 data file. The script 1_format_raw_data.R is a template you can use. 

2. Then, run the script 3_explore_comm_dat.R on each dataset in the L3 folder. This script performs QA/QC on the community data (i.e. OBSERVATION_TYPE = TAXON_COUNT). It plots the spatio-temporal sampling effort and species accumulation curve for each L3 dataset, and saves them to folders in this directory. When QA/QC on the community data is complete, add metadata to the Google Sheet L3-DATA-list in the directory Google Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by+year_and_space.


Note: the scripts 2_explore_spatial_dat and 4_explore_environmental_dat are less developed because we don’t immediately need them for the first planned analysis. They perform QA/QC on the spatial locations of plots and on the environmental covariates calculated from data collected at LTER sites.