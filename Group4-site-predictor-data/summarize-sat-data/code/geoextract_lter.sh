#!/bin/sh -login
#PBS -l walltime=4:00:00
#PBS -l nodes=1
#PBS -l mem=8gb
#PBS -N extr
#PBS -j oe
#PBS -m n
#PBS -t 1-11 							# one arrayjob per site (only 11 right now for NTL)

# Geodiversity extraction shell script
# Runs for any geodiversity variable and any taxon (bbs, fia)
# Must include the following arguments in the -v flag of qsub:
# taxon = one of the following: bbs, fia
# geovar = one of the following: bioclim1k, bioclim5k, biocloud1k, biocloud5k, dhi, elevation, aspect, slope, tpi, hf, gea, night, soil

module load R/3.2.0 GDAL

cd /mnt/research/nasabio/code/

# Cobble together the R command using the environmental variables supplied.
Rscript master_extract_lter.r $taxon $geovar

qstat -f $PBS_JOBID 

# submit using qsub geoextract_lter.sh -v taxon=usa,geovar=modis_lst