### checks relationship between radius and mean/sd for lter satellite data ###

# Written by AC Smith 26 Nov 2018
# Last edited by AC Smith 26 Nov 2018


# load packages -----------------------------------------------------------

library(ggplot2)

# load data ---------------------------------------------------------------

data <- read.csv('/home/annie/Documents/MSU_postdoc/lter/data/lter_radii_data.csv', stringsAsFactors = FALSE)

# change form of data for plotting ----------------------------------------

# gather into mean, sd, with var as separate column
data <- data %>%
  gather(key = 'var', value = 'value', c(8, 9, seq(11, 16))) %>%
  extract(var, c('summary_stat', 'var'), "([a-zA-z]+)\\_([a-zA-z]+)") %>%
  spread(summary_stat, value) %>%
  select(-X)

# add single site column
data$siteid <- paste(data$site, data$subsite, sep = '_')

# condense to make ribbon plot
simple_data <- data %>%
  group_by(site, subsite, siteid, lat, lon, radius, var) %>%
  summarize(max_mean = max(mean, na.rm = TRUE), min_mean = min(mean, na.rm = TRUE),
            max_sd = max(sd, na.rm = TRUE), min_sd = min(sd, na.rm = TRUE),
            mean = mean(mean, na.rm = TRUE), sd = mean(sd, na.rm = TRUE)) %>%
  as.data.frame

# explore plots -----------------------------------------------------------

# plotting function
plot_rad <- function(site, subsite, var) {
  # takes characters for site, subsite, and variable (e.g., sst, ndvi)
  # returns plot
  if (is.na(subsite)) {
    radplot <- ggplot(data[data$var == var & data$site == site & is.na(data$subsite), ], aes(x = radius, y = mean, group = year)) +
      geom_line()
  } else {
    radplot <- ggplot(data[data$var == var & data$site == site & data$subsite == subsite, ], aes(x = radius, y = mean, group = year)) +
      geom_line()
  }
  
  return(radplot)
}

# each line represents a year
plot_rad('VCR', as.character(NA), 'sst')

