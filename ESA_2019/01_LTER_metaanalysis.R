

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'PerformanceAnalytics', 'ggthemes', 'vegan', 
                  'gridExtra', 'grid', 'viridis', 'lme4', 'lmerTest')) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package)
    library(package, character.only = T)
  }
}

theme_set(theme_minimal())

# 1. IMPORT DATA SETS ------------------------------------------------------------
df <- read_csv("ESA_2019/output/metacom_data_for_models.csv")
colnames(df)
df <- df %>%
  select(-c(X1, dataset_id, l3_filename, start_year, end_year, n_years_observed, 
            study_duration, messages, dataset_google_id)) 

# Subset data sets by standardization type
q.0 <- filter(df, standardization_method == "q_order_0") 
agg <- filter(df, variability_type == "agg") 
h   <- filter(df, standardization_method == "h") 
hT  <- filter(df, standardization_method == "hT") 

# Spread to wide format
q0.wide <- spread(q.0, metric, metric_value)
h.wide <- spread(h, metric, metric_value)
hT.wide <- spread(hT, metric, metric_value)
agg.wide <- spread(agg, metric, metric_value)


# Visiualize correlations between continuous variables
q.wide <- q0.wide %>%
  na.omit()
names(q.wide)
q.wide %>% select_if(is.numeric) %>% 
  chart.Correlation(., histogram = TRUE, pch = "+")

var.h <- h.wide %>%
  na.omit()
var.h %>% select_if(is.numeric) %>% 
  chart.Correlation(., histogram = TRUE, pch = "+")

var.hT <- hT.wide %>%
  na.omit()
var.hT %>% select_if(is.numeric) %>% 
  chart.Correlation(., histogram = TRUE, pch = "+")

agg.wide <- agg.wide %>%
 select(-standardization_method) %>% 
  na.omit()
names(agg.wide)
agg.wide %>% select_if(is.numeric) %>% 
  chart.Correlation(., histogram = TRUE, pch = "+")


# visualize differences for metacommunity factors, by biome.
(p <- ggplot(var.hT, aes(x = body.size, y = phi_var, 
                         colour = body.size, group = body.size)) + 
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    facet_grid(. ~ biome) +
    theme_bw())

ggplot(hT, aes(x = ndvi_temporal_sd, y = metric_value, 
               color = biome, shape = body.size)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ metric, scales = "free_y") +
  ggsave("ESA_2019/figs/ndvi_var.png")

ggplot(hT, aes(x = temp_temporal_sd, y = metric_value, 
               color = biome, shape = body.size)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ metric, scales = "free_y") +
  ggsave("ESA_2019/figs/temp_var.png")

ggplot(hT, aes(x = lat, y = metric_value, 
               color = biome, shape = body.size)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ metric, scales = "free_y") +
  ggsave("ESA_2019/figs/lat_var.png")






library(maps)
library(ggrepel)
scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
  return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}    

us_states <- map_data("state")

hT %>% filter(metric == "phi_var") %>% 
  ggplot(aes(x = lon, y = lat, 
               color = metric_value)) + 
  geom_polygon(data = us_states, mapping = aes(x = long, y = lat, group = group),
               fill = "white", color = "black") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_point(alpha = 0.5, size = 4) +
  geom_label_repel(aes(label = site)) +
  scale_color_viridis_c() +
  scale_x_longitude(xmin = -150, step = 30) +
  scale_y_latitude(step = 30) +
  labs(x = "", y = "", color = "Spatial Synchrony (phi)")




# create models. 
q.mod <- lm(gamma_div_cv ~ (ndvi_temporal_sd + temp_temporal_sd +
                              env_heterogeneity) * biome * organism_group * dispersal.habit, data = q.wide)
q.mod.sel <- step(q.mod)
summary(q.mod.sel)

var.h.mod <- lm(gamma_var ~ (ndvi_temporal_sd + temp_temporal_sd +
                               env_heterogeneity) * biome * organism_group * dispersal.habit, data = var.h)
var.h.mod.sel <- step(var.h.mod)
summary(var.h.mod.sel)

agg.mod <- lm(gamma_div_cv ~ (ndvi_temporal_sd + temp_temporal_sd +
                                env_heterogeneity) * biome * organism_group * dispersal.habit, data = agg.wide)
q.mod.sel <- step(q.mod)
summary(q.mod.sel)

