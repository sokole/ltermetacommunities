

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

