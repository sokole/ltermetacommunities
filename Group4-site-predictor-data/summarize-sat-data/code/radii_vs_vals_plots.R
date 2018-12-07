### checks relationship between radius and mean/sd for lter satellite data ###

# Written by AC Smith 26 Nov 2018
# Last edited by AC Smith 26 Nov 2018


# load packages -----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)

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
plot_rad <- function(site, subsite, var, metric) {
  # takes characters for site, subsite, variable (e.g., sst, ndvi), and metric (mean, etc.)
  # returns plot
  
  if (metric == 'mean'){
    if (is.na(subsite)) {
      radplot <- ggplot(data[data$var == var & data$site == site & is.na(data$subsite), ], aes(x = radius, y = mean, group = year)) +
        geom_line() +
        xlab('Radius (km)') +
        ylab('Mean') +
        theme_bw() +
        theme(panel.grid = element_blank())
    } else {
      radplot <- ggplot(data[data$var == var & data$site == site & data$subsite == subsite, ], aes(x = radius, y = mean, group = year)) +
        geom_line() +
        xlab('Radius (km)') +
        ylab('Mean') +
        theme_bw() +
        theme(panel.grid = element_blank())
    }
  } else if (metric == 'sd') {
    if (is.na(subsite)) {
      radplot <- ggplot(data[data$var == var & data$site == site & is.na(data$subsite), ], aes(x = radius, y = sd, group = year)) +
        geom_line() +
        xlab('Radius (km)') +
        ylab('Std. Dev.') +
        theme_bw() +
        theme(panel.grid = element_blank())
    } else {
      radplot <- ggplot(data[data$var == var & data$site == site & data$subsite == subsite, ], aes(x = radius, y = sd, group = year)) +
        geom_line() +
        xlab('Radius (km)') +
        ylab('Std. Dev.') +
        theme_bw() +
        theme(panel.grid = element_blank())
    }
  }
  
  return(radplot)
}

# each line represents a year

# create and export plot for each site, subsite, variable, metric combination
for (i in unique(data$site)) {
  for (j in c('mean', 'sd')) {
    for (k in unique(data$var)) {
      site_data <- data[data$site == i & data$var == k,]
      if (length(unique(site_data$subsite)) > 1) {
        for (s in unique(site_data$subsite)) {
          meanplot <- plot_rad(i, s, k, 'mean')
          sdplot <- plot_rad(i, s, k, 'sd')
          
          ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/', 
                       i, '_', s, '_', k, '_mean_plot.png', sep = ''), meanplot, device = 'png')
          ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/', 
                       i, '_', s, '_', k, '_sd_plot.png', sep = ''), sdplot, device = 'png')
        }
      } else if (!is.na(site_data$subsite[1])) {
        meanplot <- plot_rad(i, site_data$subsite[1], k, 'mean')
        sdplot <- plot_rad(i, site_data$subsite[1], k, 'sd')
        
        ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/', 
                     i, '_', site_data$subsite[1],'_', k, '_mean_plot.png', sep = ''), 
               meanplot, device = 'png')
        ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/', 
                     i, '_', site_data$subsite[1], '_', k, '_sd_plot.png', sep = ''), 
               sdplot, device = 'png')
      } else {
        meanplot <- plot_rad(i, as.character(NA), k, 'mean')
        sdplot <- plot_rad(i, as.character(NA), k, 'sd')
        
        ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/', 
                     i, '_NA_', k, '_mean_plot.png', sep = ''), 
               meanplot, device = 'png')
        ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/', 
                     i, '_NA_', k, '_sd_plot.png', sep = ''), 
               sdplot, device = 'png')
      }
    }
  }
}


# calculate temporal variability ------------------------------------------

# we will grab the SD for the radius that best matches the actual 
# size of the site
temp_var <- data %>%
  group_by(site, subsite, radius, var) %>%
  summarize(mean_sd = sd(mean, na.rm = TRUE),
            sd_sd = sd(mean, na.rm = TRUE))

write.csv(temp_var, '/home/annie/Documents/MSU_postdoc/lter/data/temporal_sd.csv')
