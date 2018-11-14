# LTER Radii Workflow
#### Annie C. Smith
#### November 14, 2018
### Data

We decided to use the coordinates for each site listed on the LTER websites. For more complicated sites like the Northern Lakes LTER, we use the coordinates of each subsite (e.g., Lake Mendota, Trout Bog) so as to get the most accurate data for that specific sampling location.
+ The specific plot center point dataset is located in 'summarize-sat_data/coords/LTER_coordinates_all.csv', and can be added to as necessary. All sites should be included as of 11/13/18.

### Pre-processing

Data were processed to remove poor-quality data and summarized to the annual time scale in Google Earth Engine (see link below).

Detailed metadata for MODIS LST, MODIS NDVI, SST, and elevation data are located in the (variable)_README.pdf files. The metadata include QA decisions.

### Radii extraction

Radii extraction code -- https://code.earthengine.google.com/810b9872cb198030b2992f704c9cb982.

All of the processing is done in a single script. A basic run-through of the processing steps is included below.

1. Import site location data.
2. Import NDVI data, filter according to QA arguments. 
3. Calculate annual average values, with scalar applied, for all years available (2000-2018 considered). * Note that 2018 will be slightly skewed as this was calculated in early November. *
4. Repeat steps 2 and 3 for LST and SST. There is no QA for SST.
5. Import the single-date elevation raster.
6. Create a reducer to extract mean and standard deviation within each radius. Other reducers may be added later.
7. Extract mean/sd values for each radius and variable (lst, ndvi, etc.) using the extract_vals function. This could be done all at once (see the centroid_polys function), but is more memory efficient if split up.
8. Export data to Google Drive. This is slightly time intensive, but not too bad. All export functions could be added to the script if necessary. Looping exports doesn't work nicely in Earth Engine.

Once the script has been started, you have to go to the 'tasks' tab in the righthand EE box, and click 'run' for all of the export tasks. These will submit the job to the Google servers. If there are memory issues, increase the 'tileScale' argument in the extract_vals function. This increases the number of workers per job.

### Post-processing

Download all files from Google Drive. Clean and concatenate all result files using the 'clean_EE_radii_exports.R' script. This takes all scripts, removes any extraneous (geo) data, and pastes them together. It also gives the option of plotting the results to ensure that all points are in the correct location and have reasonable values.




