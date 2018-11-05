# GEODIVERSITY EXTRACTION BATCH SCRIPT v2
# Uses only GDAL commands for less memory usage.

# Edited 08 Jan 2018: Use $SCRATCH and $TMPDIR
# Edited 09 Jan 2018: Read directly from $SCRATCH, write to $TMPDIR
# Edited 11 Jan 2018: Get all variables from table. (rename to master_extract.r)
# Edited 09 Feb 2018: Update scratch path.
# Edited 19 Feb 2018: Add escape characters to file path.
# Edited 17 Apr 2018: Change BBS coordinates to the route midpoints, not centroids, and add smaller radii.
# Edited 20 Apr 2018: Change scratch path to the FFS17 path.
# Edited 31 Aug 2018 ACS: Changed master_extract.r to master_extract_lter.r, with changes 
#                         to make script apply to LTER sites.

# Workflow:
# 1. Use extractBox() to make square of maximum radius (300 km) around focal point
# 2. Use extractFromCircle() to make a bunch of circles of different radii, getting summary statistics each time.

slice <- as.numeric(Sys.getenv('PBS_ARRAYID'))
tmp_path <- Sys.getenv('TMPDIR')
scratch_path <- '/mnt/ffs17/groups/nasabio/VRTs_writable'

# get arguments
args <- (commandArgs(TRUE))

taxon <- as.character(args[1])
geovar <- as.character(args[2])

# Load functions to do the extraction
source('/mnt/research/nasabio/code/extractbox.r')
source('/mnt/research/nasabio/code/extractfromcircle_stack.r')

# Load variable table
vartable <- read.csv('/mnt/research/nasabio/data/geodiv_table_for_gdal_lter.csv', stringsAsFactors = FALSE)

# Get all options from variable table (by location (i.e., taxon) and variable).
k <- which(vartable$variable.id == geovar & vartable$Dataset.name == paste(geovar, taxon, sep = '_'))
n_layers <- vartable$N.layers[k]
raster_file_name <- vartable$File.name[k]
max_radius <- vartable$Max.radius[k]
categorical <- geovar %in% c('gea','gea_5k','soil')

# Load coordinates for correct taxon.	
if (taxon == 'mcmurdo') {
  n_slices <- vartable$N.slices.mcmurdo[k] # number of points for station
  coords <- read.csv('/mnt/research/nasabio/data/lter/LTER_coordinates.csv')
  coords <- coords[coords$site == 'MCM', ] 
  radii <- c(5, 10, 20, 30, 40, 50, 75, 100, 150, 200, 300, 400, 500) # km
}	
if (taxon == 'palmer') {
  n_slices <- vartable$N.slices.palmer[k] # number of points for station
  coords <- read.csv('/mnt/research/nasabio/data/lter/LTER_coordinates.csv')
  coords <- coords[coords$site == 'PAL', ]
  radii <- c(5, 10, 20, 30, 40, 50, 75, 100, 150, 200, 300, 400, 500) # km
}
if (taxon == 'usa') {
  n_slices <- vartable$N.slices.usa[k] # number of points for station - note: changed 
  # temporally to test in NTL sites
  coords <- read.csv('/mnt/research/nasabio/data/lter/LTER_coordinates.csv')
  coords <- coords[!(coords$site %in% list('MCM', 'PAL', 'ARC', 'BNZ', 'LUQ')), ]
  radii <- c(5, 10, 20, 30, 40, 50, 75, 100, 150, 200, 300, 400, 500) # km
}
if (taxon == 'ak') {
  n_slices <- vartable$N.slices.ak[k] # number of points for station
  coords <- read.csv('/mnt/research/nasabio/data/lter/LTER_coordinates.csv')
  coords <- coords[coords$site %in% list('ARC', 'BNZ') ]
  radii <- c(5, 10, 20, 30, 40, 50, 75, 100, 150, 200, 300, 400, 500) # km
}
if (taxon == 'pr') {
  n_slices <- vartable$N.slices.pr[k] # number of points for station
  coords <- read.csv('/mnt/research/nasabio/data/lter/LTER_coordinates.csv')
  coords <- coords[coords$site == 'LUQ']
  radii <- c(5, 10, 20, 30, 40, 50, 75, 100, 150, 200, 300, 400, 500) # km
}

radii <- radii[radii <= max_radius]

# Get row indexes for the slice of coordinate matrix to be extracted.
rowidx <- round(seq(0, nrow(coords), length.out = n_slices + 1)) # for LTER, set n_slices to length of coords
# slice is the PBS_JOBARRAY argument from geoextract.sh
# (1 job per site, run this script the same, so # sites of job array ids)

stats_by_point <- list()

i <- slice

focalpoint <- with(coords, cbind(lon, lat))[i, , drop = FALSE]	

extractBox(coords = focalpoint,
           raster_file = file.path(scratch_path, raster_file_name),
           radius = max_radius,
           fp = tmp_path,
           filetags = paste(geovar, i, sep = '_'))

file_i <- paste("\'", tmp_path, "\'", "/bbox_", geovar, "_", i, ".tif", sep = "")

stats_by_point[[length(stats_by_point) + 1]] <- 
  extractFromCircle(coords = focalpoint,
                    raster_file = file_i,
                    radii = radii,
                    fp = tmp_path,
                    filetag = paste(geovar, i, sep = '_'),
                    nlayers = n_layers,
                    is_categorical = categorical,
                    delete_temp = TRUE)

if (file.exists(paste(tmp_path, "/bbox_", geovar, "_", i, ".tif", sep = ""))) {
  deleteBox(file_i)
}

save(stats_by_point, file = paste0('/mnt/research/nasabio/data/lter/', taxon, '/', geovar, '_', slice, '.r'))
