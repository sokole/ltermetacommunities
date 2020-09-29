alpha <- read.csv("Manuscripts/MS3/data/L4_alpha_variability_analysis_results_2019-07-31.csv")

alpha %>% 
  ggplot(aes(x = mean_alpha, y = comp_alpha_var_rate, color = dataset_file_name)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  geom_smooth(method = 'lm', color = 'black', aes(color = NULL)) +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = "none") 

alpha %>% 
  ggplot(aes(x = mean_alpha, y = agg_alpha_var_rate, color = dataset_file_name)) +
  geom_point() +
  geom_smooth(method = 'lm', se = F) +
  geom_smooth(method = 'lm', color = 'black', aes(color = NULL)) +
  scale_x_log10() +
  scale_y_log10() +
  theme(legend.position = "none")
