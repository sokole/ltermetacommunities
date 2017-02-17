#######################################################
# check for packages
#######################################################
package.list <- c('vegan','reshape2','dplyr', 'tibble')
for(i in package.list){
  if(!i %in% row.names(installed.packages())) install.packages(i)
}

library(dplyr)

#######################################################
# -- download data off google drive using google-id
#######################################################
data_id_googledrive <- "0BwguSiR80XFZa0NDckg5RmoyQVk" #JRN Lizards data
download.link <- paste0("https://drive.google.com/uc?export=download&id=",data_id_googledrive)
d.in.long <- read.csv(file = download.link, header = T,
                      stringsAsFactors = FALSE)

#######################################################
# -- get community data, make wide
#######################################################
d.comm.long <- subset(d.in.long, OBSERVATION_TYPE == 'TAXON_COUNT')

d.comm.wide <- d.comm.long %>% 
  reshape2::dcast(SITE_ID + DATE ~ VARIABLE_NAME,
                  value.var = 'VALUE',
                  fun.aggregate = mean)