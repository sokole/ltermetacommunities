options(stringsAsFactors = FALSE)

library(tidyverse)

# read in data
d_wide <- read.csv('dat-RESULTS2018-05-09.csv')

wide_col_names <- names(d_wide)
response_var_names <- wide_col_names[grepl('alpha|gamma|beta|_bd',wide_col_names)]

d_long <- d_wide %>% tidyr::gather_(
  gather_cols = response_var_names,
  key_col = 'diversity_metric_name',
  value_col = 'diversity_metric_value'
)

# div metric names
# "CV_alpha_0"             "CV_beta_0"              "CV_gamma_0"             "CV_alpha_2"             "CV_beta_2"             
# "CV_gamma_2"             "alpha_temporal_bd_rate" "gamma_temporal_bd_rate" "phi_bd"  

d_plot <- d_long %>% filter(diversity_metric_name %in% c('alpha_temporal_bd_rate','gamma_temporal_bd_rate','phi_bd'))
d_plot$biome2 <- gsub('freshwater','terrestrial', d_plot$biome)
d_plot$biome2 <- gsub('terrestrial','terr. and freshwater',d_plot$biome2)

d_plot$trophic.group2 <- ifelse(d_plot$trophic.group == 'primary producers', 'primary producers', 'consumers')

d_plot$dispersal.type2 <- d_plot$dispersal.type
d_plot$dispersal.type2[grepl('passive',d_plot$dispersal.type2)] <- 'passive'
d_plot$dispersal.type2[grepl('active',d_plot$dispersal.type2)] <- 'active'


#########################
# marine vs. terrestrial
#########################
graphics.off()
windows(5,7)
ggplot(d_plot,
       aes(temp_SD, diversity_metric_value,
           shape = biome2,
           lty = biome2)) +
  geom_point(aes(color = site)) +
  stat_smooth(method = 'lm') +
  facet_grid(diversity_metric_name ~ .,
             scales = 'free_y') +
  theme_bw()

savePlot(
  paste0('Fig_compositional_change_marine_v_terrestrial-',Sys.Date(),'.pdf'), 
  type = 'pdf')

#########################
# trophic groups
#########################

graphics.off()
windows(5,7)
ggplot(d_plot,
       aes(temp_SD, diversity_metric_value,
           shape = trophic.group2,
           lty = trophic.group2)) +
  geom_point(aes(color = site)) +
  stat_smooth(method = 'lm') +
  facet_grid(diversity_metric_name ~ .,
             scales = 'free_y') +
  theme_bw()


savePlot(paste0('Fig_compositional_change_by_trophic_group-',Sys.Date(),'.pdf'), 
         type = 'pdf')

#########################
# dispersal groups
#########################
graphics.off()
windows(5,7)
graphics.off()
windows(5,7)
ggplot(d_plot,
       aes(temp_SD, diversity_metric_value,
           shape = dispersal.type2,
           lty = dispersal.type2)) +
  geom_point(aes(color = site)) +
  stat_smooth(method = 'lm') +
  facet_grid(diversity_metric_name ~ .,
             scales = 'free_y') +
  theme_bw()


savePlot(paste0('Fig_compositional_change_by_dispersal_type-',Sys.Date(),'.pdf'), 
         type = 'pdf')
  
