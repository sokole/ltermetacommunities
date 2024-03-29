### puts data in correct format and plots relationships between radii and values ###

# Written by AC Smith 26 Nov 2018
# Last edited by AC Smith 07 Jan 2019

# load packages -----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(swatches)
library(raster)

# load data ---------------------------------------------------------------

data <- read.csv('/home/annie/Documents/MSU_postdoc/lter/data/lter_radii_data_July302019.csv', stringsAsFactors = FALSE)

# change form of data for plotting ----------------------------------------

# gather into mean, sd, with var as separate column
data <- data %>%
  gather(key = 'var', value = 'value', c(2, 6, 9, seq(11, 19))) %>%
  tidyr::extract(var, c('summary_stat', 'var'), "([a-zA-z]+)\\_([a-zA-z]+)") %>%
  spread(summary_stat, value)

# add single site column
data$siteid <- paste(data$site, data$subsite, sep = '_')

# condense to make ribbon plot
simple_data <- data %>%
  rename(lat = lat.x) %>%
  group_by(site, subsite, siteid, lat, lon, radius, var) %>%
  summarize(max_mean = max(mean, na.rm = TRUE), min_mean = min(mean, na.rm = TRUE),
            max_sd = max(sd, na.rm = TRUE), min_sd = min(sd, na.rm = TRUE),
            mean = mean(mean, na.rm = TRUE), sd = mean(sd, na.rm = TRUE)) %>%
  as.data.frame

# calculate temporal variability ------------------------------------------

# we will grab the CV (%) for the radius that best matches the actual 
# size of the site (if there are actually any differences across radii)
temp_var <- data %>%
  group_by(site, subsite, radius, var) %>%
  summarize(mean_sd = sd(mean, na.rm = TRUE),
            sd_sd = sd(sd, na.rm = TRUE)) %>%
  filter(var != 'elev')

# calculate linear slope up to 60km ---------------------------------------

# average slope of mean/sd (for all years) from 1 - 60 km radius
slope_data <- data %>%
  group_by(site, subsite, var) %>%
  filter(radius <= 60) %>%
  summarize(slope_60km_mean = if (sum(!is.na(mean)) < 2) {as.numeric(NA)} else {lm(mean ~ radius)$coefficients[[2]]},
            slope_60km_sd = if (sum(!is.na(sd)) < 2) {as.numeric(NA)} else {lm(sd ~ radius)$coefficients[[2]]})

# create main model dataframe ---------------------------------------------

# slim down the data
main_data <- data %>%
  filter(radius <= 1 & year == 2014 & var == 'elev') %>%
  rename(lat = lat.x) %>%
  dplyr::select(site, subsite, lat, lon) 

# add temporal SD info
main_data <- main_data %>%
  left_join(temp_var, by = c('site', 'subsite')) %>% 
  dplyr::select(-sd_sd) %>%
  filter(radius <= 1) %>%
  spread(var, mean_sd) %>%
  rename(lst_temporal_sd = lst,
         ndvi_temporal_sd = ndvi,
         chla_temporal_sd = chla,
         sst_temporal_sd = sst)

# add spatial SD info
main_data <- main_data %>%
  left_join(slope_data, by = c('site', 'subsite')) %>%
  dplyr::select(-slope_60km_mean) %>%
  spread(var, slope_60km_sd) %>%
  dplyr::select(-lst, -ndvi, -sst, -chla) %>%
  rename(elevation_spatial_sd_slope_60km = elev,
         bathymetry_spatial_sd_slope_60km = bath)

# can slim down further if you know what are marine vs. 
# terrestrial sites (to temperature/productivity/elevation)
# can also make a readme

write.csv(main_data, '/home/annie/Documents/MSU_postdoc/ltermetacommunities/Group4-site-predictor-data/summarize-sat-data/data/lter_centroid_satdata_July302019.csv', row.names = FALSE)

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
        ggtitle(paste(toupper(site), '\n', toupper(var), 'MEAN')) +
        theme_bw() +
        theme(panel.grid = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = c(0.5, 0.5)),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12)
        )
    } else {
      radplot <- ggplot(data[data$var == var & data$site == site & data$subsite == subsite, ], aes(x = radius, y = mean, group = year)) +
        geom_line() +
        xlab('Radius (km)') +
        ylab('Mean') +
        ggtitle(paste(toupper(site), toupper(subsite), '\n', toupper(var), 'MEAN')) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              plot.title = element_text(size = 16, face = "bold", hjust = c(0.5, 0.5)),
              axis.title.x = element_text(size = 14, face = "bold"),
              axis.title.y = element_text(size = 14, face = "bold"),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12)
        )
    }
  } else if (metric == 'sd') {
    if (is.na(subsite)) {
      radplot <- ggplot(data[data$var == var & data$site == site & is.na(data$subsite), ], aes(x = radius, y = sd, group = year)) +
        geom_line() +
        xlab('Radius (km)') +
        ylab('Std. Dev.') +
        ggtitle(paste(toupper(site), '\n', toupper(var), 'SD')) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              plot.title = element_text(size = 16, face = "bold", hjust = c(0.5, 0.5)),
              axis.title.x = element_text(size = 14, face = "bold"),
              axis.title.y = element_text(size = 14, face = "bold"),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12)
        )
    } else {
      radplot <- ggplot(data[data$var == var & data$site == site & data$subsite == subsite, ], aes(x = radius, y = sd, group = year)) +
        geom_line() +
        xlab('Radius (km)') +
        ylab('Std. Dev.') +
        ggtitle(paste(toupper(site), toupper(subsite), '\n', toupper(var), 'SD')) +
        theme_bw() +
        theme(panel.grid = element_blank(),
              plot.title = element_text(size = 16, face = "bold", hjust = c(0.5, 0.5)),
              axis.title.x = element_text(size = 14, face = "bold"),
              axis.title.y = element_text(size = 14, face = "bold"),
              axis.text.x = element_text(size = 12),
              axis.text.y = element_text(size = 12)
        )
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
          
          ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/value_by_radius/', 
                       i, '_', s, '_', k, '_mean_plot.png', sep = ''), meanplot, device = 'png')
          ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/value_by_radius/', 
                       i, '_', s, '_', k, '_sd_plot.png', sep = ''), sdplot, device = 'png')
        }
      } else if (!is.na(site_data$subsite[1])) {
        meanplot <- plot_rad(i, site_data$subsite[1], k, 'mean')
        sdplot <- plot_rad(i, site_data$subsite[1], k, 'sd')
        
        ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/value_by_radius/', 
                     i, '_', site_data$subsite[1],'_', k, '_mean_plot.png', sep = ''), 
               meanplot, device = 'png')
        ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/value_by_radius/', 
                     i, '_', site_data$subsite[1], '_', k, '_sd_plot.png', sep = ''), 
               sdplot, device = 'png')
      } else {
        meanplot <- plot_rad(i, as.character(NA), k, 'mean')
        sdplot <- plot_rad(i, as.character(NA), k, 'sd')
        
        ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/value_by_radius/', 
                     i, '_NA_', k, '_mean_plot.png', sep = ''), 
               meanplot, device = 'png')
        ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/value_by_radius/', 
                     i, '_NA_', k, '_sd_plot.png', sep = ''), 
               sdplot, device = 'png')
      }
    }
  }
}

# plot radii vs. temporal CV ----------------------------------------------

# define color palette(Pantone 2018, 'Attitude')
palette1 <- read_ase('/home/annie/Documents/MSU_postdoc/lter/data/PANTONE Color Of The Year Palettes/PantoneCOY18-Attitude.ase',
                     use_names = FALSE)

plot_cv <- function(site, subsite) {
  # takes characters for site, subsite
  # returns plot

  if (is.na(subsite)) {
    radplot <- ggplot(temp_var[temp_var$site == site & is.na(temp_var$subsite), ], aes(x = radius, y = mean_cv, group = var)) +
      geom_line(aes(colour = var), size = 1.5) +
      xlab('Radius (km)') +
      ylab('CV') +
      ggtitle(paste(toupper(site), '\n', 'CV of MEAN')) +
      scale_colour_manual(values = palette1[1:3], 
                        breaks = c("lst", "sst", "ndvi"),
                        labels = c("LST", "SST", "NDVI")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            plot.title = element_text(size = 16, face = "bold", hjust = c(0.5, 0.5)),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size = 14)
      ) 
  } else {
    radplot <- ggplot(temp_var[temp_var$site == site & temp_var$subsite == subsite, ], aes(x = radius, y = mean_cv, group = var)) +
      geom_line(aes(colour = var), size = 1.5) +
      xlab('Radius (km)') +
      ylab('CV') +
      ggtitle(paste(toupper(site), toupper(subsite), '\n', 'CV of MEAN')) +
      scale_colour_manual(values = palette1[1:3], 
                          breaks = c("lst", "sst", "ndvi", 'chla'),
                          labels = c("LST", "SST", "NDVI", "CHL-A")) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            plot.title = element_text(size = 16, face = "bold", hjust = c(0.5, 0.5)),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.title = element_blank(),
            legend.position = 'bottom',
            legend.text = element_text(size = 14)
      ) 
  }
}

# create and export plot for each site, subsite, variable, metric combination
for (i in unique(data$site)) {
  site_data <- data[data$site == i & data$var == j,]
  if (length(unique(site_data$subsite)) > 1) {
    for (s in unique(site_data$subsite)) {
      meanplot <- plot_cv(i, s)
      
      ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/temporal_cv_by_radius/', 
                   i, '_', s, '_tempCV_plot.png', sep = ''), meanplot, device = 'png')
    }
  } else if (!is.na(site_data$subsite[1])) {
    meanplot <- plot_cv(i, site_data$subsite[1])
    
    ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/temporal_cv_by_radius/', 
                 i, '_', site_data$subsite[1], '_tempCV_plot.png', sep = ''), 
           meanplot, device = 'png')
  } else {
    meanplot <- plot_cv(i, as.character(NA))
    
    ggsave(paste('/home/annie/Documents/MSU_postdoc/lter/figures/temporal_cv_by_radius/', 
                 i, '_NA_tempCV_plot.png', sep = ''), 
           meanplot, device = 'png')
  }
}
