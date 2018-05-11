# ---------------------------------------- #
# Make metadata table - 21 Mar 2018     #
# ---------------------------------------- #

# Clear environment
rm(list = ls())

# Make sure your working environment is set to the GitHub repository ltermetacommunities. 
#Check to make sure working directory is correct
if(basename(getwd())!="ltermetacommunities"){cat("Plz change your working directory. It should be 'ltermetacommunities'")}

file.list <- list.files("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/metadata_tables")
#remove pesky icon if it appears
file.list <- file.list[file.list!="Icon\r"]

metadata_table <- read.csv(paste("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/metadata_tables/", file.list[1], sep=""))

for (file in file.list[2:length(file.list)]){
	 a <- read.csv(paste("~/Google Drive File Stream/My Drive/LTER Metacommunities/LTER-DATA/L3-aggregated_by_year_and_space/metadata_tables/", file, sep=""))
	 metadata_table <- rbind(metadata_table,a)
}

write.csv(metadata_table, file = "~/Google Drive File Stream/My Drive/LTER Metacommunities/Manuscripts/MS3_metacom-stability-analysis/supplemental_methods_figs/metadata_table.csv", row.names=F)