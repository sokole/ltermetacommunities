#!/bin/sh -login
#PBS -l walltime=4:00:00
#PBS -l nodes=1:ppn=1
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
cp /mnt/research/nasabio/code/master_extract.r $TMPDIR/rcode.r
cd $TMPDIR
now="$(date +'%d%h%Y%H%M')"

# Cobble together the R command using the environmental variables supplied.
cmd="R CMD BATCH --no-save --no-restore '--args taxon=\""$taxon"\" geovar=\""$geovar"\"' rcode.r /mnt/ffs17/groups/nasabio/routputfiles/"$taxon"_"$geovar"_"$PBS_ARRAYID"_"$now".txt"
eval $cmd
