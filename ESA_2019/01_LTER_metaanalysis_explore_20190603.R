

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'PerformanceAnalytics', 'ggthemes', 'vegan', 
                  'gridExtra', 'grid', 'viridis', 'lme4', 'lmerTest', 'car',
                  'ggpubr', 'ggrepel', 'cowplot', 'RLRsim')) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package)
    library(package, character.only = T)
  }
}

theme_set(theme_base() + 
            theme(plot.background = element_blank()))

# 1. IMPORT DATA SETS ------------------------------------------------------------
df <- read_csv("ESA_2019/output/metacom_data_for_models.csv")
length(unique(df$l3_filename)) #33 

colnames(df)
df <- df %>%
  select(-c(X1, dataset_id, l3_filename, start_year, end_year, n_years_observed, 
            study_duration, messages, dataset_google_id)) 

#combine micro and meso (only two micro, not enough to parse out), then change
# terminology to large vs small body organisms. 
# NOTE: For now considering molluscs active dispersers since they are "mobile"
df <- df %>%
  mutate(body.size = replace(body.size, body.size == "micro", "meso")) %>%
  mutate(body.size = replace(body.size, body.size == "macro", "large")) %>%
  mutate(body.size = replace(body.size, body.size == "meso", "small")) %>%
  mutate(dispersal.habit = replace(dispersal.habit, 
                                   dispersal.habit == "need to look at taxa", "active"))
  

# pull out diversity values
div <- filter(df, variability_type == "divpart_time_series")

# Subset data by standardization type
q.0 <- filter(df, standardization_method == "q_order_0")
h   <- filter(df, standardization_method == "h") 
hT  <- filter(df, standardization_method == "hT") 

#subset data for wang & Loreau aggregate metric
agg <- filter(df, variability_type == "agg") 


# Spread to wide format
q0.wide <- spread(q.0, metric, metric_value)
h.wide <- spread(h, metric, metric_value)
hT.wide <- spread(hT, metric, metric_value)
agg.wide <- spread(agg, metric, metric_value)
div.wide <- spread(div, metric, metric_value)

length(q0.wide$site)
length(h.wide$site)
length(hT.wide$site)
length(agg.wide$site)
length(div.wide$site)

# Visiualize correlations between continuous variables
var.h <- h.wide %>%
  na.omit()
var.h %>% select_if(is.numeric) %>% 
  chart.Correlation(., histogram = TRUE, pch = "+")


# initial models -----------------------------------------------------------------
#For now use hellenger transformed metrics of community turnover 
# (based on relative abundances) until we know which data sets have measures of
# abundance 

# Relative importance of local and spatial variability for regional variability
lm_gamma_alpha <- lm(log(gamma_var) ~ log(alpha_var), 
                     data = h.wide)
par(mfrow = c(2,2))
plot(lm_gamma_alpha) # assumptions met
summary(lm_gamma_alpha)

(alpha_vs_gamma_var <- ggplot(aes(x = (alpha_var_rate), y = (gamma_var_rate), label = site),
                              data = h.wide) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    geom_label_repel(size = 2) +
    scale_y_log10() +
    scale_x_log10() +
    xlab(expression(paste(alpha, " variability"))) +
    ylab(expression(paste(gamma," variability"))) +
    ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

lm_gamma_phi <- lm(log(gamma_var_rate) ~ log(phi_var), data = h.wide)
par(mfrow = c(2,2))
plot(lm_gamma_phi) 
summary(lm_gamma_phi)

(phi_vs_gamma_var <- ggplot(aes(x = phi_var, y = gamma_var_rate, label = site),
                              data = h.wide) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    geom_label_repel(size = 2) +
    scale_y_log10() +
    scale_x_log10() +
    xlab(expression(paste(phi, " variability"))) +
    ylab(expression(paste(gamma," variability"))) +
    #annotate("text", x = 0.2, y = 0.6, label = "italic(P) < 0.001", parse = TRUE) +
    ggsave("ESA_2019/figs/variability_phi-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

# alpha and phi are independent
(alpha_vs_phi_var <- ggplot(aes(x = alpha_var_rate, y = phi_var, label = site),
                              data = h.wide) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    geom_label_repel(size = 2) +
    scale_y_log10() +
    scale_x_log10() +
    xlab(expression(paste(alpha, " variability"))) +
    ylab(expression(paste(phi," variability"))) +
    ggsave("ESA_2019/figs/variability_alpha-phi.png", width = 6, height = 4, units = "in", dpi = 600)
)
lm_alpha_phi <- lm(log(phi_var) ~ log(alpha_var), data = h.wide)
par(mfrow = c(2,2))
plot(lm_alpha_phi) # assumptions met
summary(lm_alpha_phi)

# DIversity stability 
#------------------------------------------------------------------------------
div_stab_comp <- cbind.data.frame(h.wide, div.wide[,c("alpha_div_mean", "beta_div_mean", "gamma_div_mean")]) %>% 
  filter(gamma_div_mean > 0)
(div_stab_gamma_h <- div_stab_comp %>% 
   ggplot(aes(x = gamma_div_mean, y = gamma_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", gamma, "-diversity")),
         y = expression(paste("Comp. ", gamma, "-variability")))  +
    scale_x_log10() 
    #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

(div_stab_alpha_h <- div_stab_comp %>% 
    ggplot(aes(x = alpha_div_mean, y = alpha_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", gamma, "-diversity")),
         y = expression(paste("Comp. ", alpha, "-variability"))) +
    scale_x_log10() 
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

(div_stab_beta_h <- div_stab_comp %>% 
    ggplot(aes(x = beta_div_mean, y = gamma_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Comp. ", gamma, "-variability"))) 
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_agg <- cbind.data.frame(agg.wide, div.wide[,c("alpha_div_mean", "beta_div_mean", "gamma_div_mean")]) %>% 
  filter(gamma_div_mean > 0)
(div_stab_gamma_agg <- div_stab_agg %>% 
    ggplot(aes(x = gamma_div_mean, y = gamma_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", gamma, "-diversity")),
         y = expression(paste("Agg. ", gamma, "-variability"))) +
    scale_x_log10()
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

(div_stab_alpha_agg <- div_stab_agg %>% 
    ggplot(aes(x = alpha_div_mean, y = alpha_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Agg. ", alpha, "-variability")))  +
    scale_x_log10()
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

(div_stab_beta_agg <- div_stab_agg %>% 
    ggplot(aes(x = beta_div_mean, y = gamma_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Agg. ", gamma, "-variability"))) 
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

plot_grid(div_stab_gamma_agg, div_stab_gamma_h, 
          div_stab_beta_agg, div_stab_beta_h,
          div_stab_alpha_agg, div_stab_alpha_h, 
          align = "hv", ncol = 2) +
  ggsave("ESA_2019/figs/diversity_variability.png", width = 2*4, height = 3*3, units = "in", dpi = 600)


(div_stab_phi_h <- div_stab_comp %>% 
    ggplot(aes(x = beta_div_mean, y = phi_var, label = site)) +
  stat_smooth(method = "lm", se = T, size = 1, color = "black") +
  geom_point(size = 2, alpha = 0.7) +
    scale_y_continuous(limits = c(0,1)) +
  #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Comp. ", phi, "-variability"))) 
#ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

(div_stab_phi_agg <- div_stab_agg %>% 
    ggplot(aes(x = beta_div_mean, y = phi_var, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7) +
    scale_y_continuous(limits = c(0,1))+
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Agg. ", phi, "-variability"))) 
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

plot_grid(div_stab_phi_agg, div_stab_phi_h, 
          align = "hv", ncol = 2) +
  ggsave("ESA_2019/figs/beta_phi.png", width = 2*4, height = 3, units = "in", dpi = 600) + 
  ggsave("ESA_2019/figs/beta_phi.pdf", width = 2*4, height = 3, units = "in")

agg.mod <-  lm((gamma_var_rate) ~ log10(gamma_div_mean), data = div_stab_agg)
comp.mod <- lm((gamma_var_rate) ~ log10(gamma_div_mean), data = div_stab_comp)
summary(agg.mod)
summary(comp.mod)
phi.agg.mod <-  lm(phi_var ~  beta_div_mean, data = div_stab_agg)
phi.comp.mod <- lm(phi_var ~  beta_div_mean, data = div_stab_comp)
summary(phi.agg.mod)
summary(phi.comp.mod)


## Possible environmental drivers
bind_rows(div_stab_agg, div_stab_comp) %>% 
  mutate(trophic.group = stringr::str_to_sentence(trophic.group)) %>% 
  gather(temp_temporal_sd, ndvi_temporal_sd, env_heterogeneity, key = env_var, value = variability) %>% 
  mutate(env_var = ifelse(env_var == "temp_temporal_sd", "Temperature Variability (sd)", 
                          ifelse(env_var == "ndvi_temporal_sd", "NDVI Variability (sd)", "Spatial Heterogeneity (Geodiversity)"))) %>% 
  mutate(variability_type = ifelse(variability_type == "agg", "Aggregate", "Compositional")) %>% 
  ggplot(aes(x = variability, y = gamma_var_rate, label = site, color = trophic.group)) + 
  geom_point() + 
  geom_smooth(method = 'lm', size = 1, se = F) +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top", plot.background = element_blank()) +
  facet_grid(variability_type ~ env_var, scales = "free_x", switch = "both") +
  labs(x = "", y = "Metacommunity Variability") +
  scale_color_pander(name = "Trophic Group") +
  ggsave("ESA_2019/figs/environmental_drivers.png", width = 10, height = 7.5, units = "in", dpi = 600)

summary(lm(log(gamma_var_rate) ~ trophic.group * (env_heterogeneity + ndvi_temporal_sd + temp_temporal_sd), data = cbind.data.frame(div_stab_agg, div_stab_comp)))

## compositional versus aggregate variability

comp_agg_stab <- agg.wide[,c("alpha_var_rate", "phi_var", "gamma_var_rate")] 
names(comp_agg_stab) <- paste0("agg_", names(comp_agg_stab))
comp_agg_stab <- cbind.data.frame(h.wide, comp_agg_stab) 
comp_agg_stab

comp_agg_stab %>% 
  ggplot(aes(x = alpha_var_rate, xend = gamma_var_rate, 
             y = agg_alpha_var_rate, yend = agg_gamma_var_rate, 
             label = paste(site, organism))) + 
  geom_point(alpha = 0.8, color = "blue", size = 2) +
  geom_point(alpha = 0.8, size = 2,  mapping = aes(x = gamma_var_rate, y = agg_gamma_var_rate)) +
  geom_segment(alpha = 0.8, arrow = arrow(length = unit(.2, "cm"))) +
  #geom_text_repel(size = 2.5) +
  coord_fixed() +
  labs(x = "Compositional variability",
       y = "Aggregate variability") +
  ggsave("ESA_2019/figs/comp_agg_compare.png", width = 6, height = 4, dpi = 600)
# 
# (alpha_comp_agg_plot <- comp_agg_stab %>% 
#   ggplot(aes(x = alpha_var_rate, y = agg_alpha_var_rate, label = paste(site, organism))) + 
#   geom_point(size = 1) + 
#   geom_text_repel(size = 2.5) +
#   scale_x_continuous(limits = c(0,.1)) + 
#   scale_y_continuous(limits = c(0,.1)) +
#   labs(x = expression(paste("Compositional ", alpha, "-variability")),
#        y = expression(paste("Aggregate ", alpha, "-variability"))))
# (gamma_comp_agg_plot <- comp_agg_stab %>% 
#   ggplot(aes(x = gamma_var_rate, y = agg_gamma_var_rate, label = paste(site, organism))) + 
#   geom_point(size = 1) +
#   geom_text_repel(size = 2.5) +
#   scale_x_continuous(limits = c(0,.1)) + 
#   scale_y_continuous(limits = c(0,.1)) +
#   labs(x = expression(paste("Compositional ", gamma, "-variability")),
#     y = expression(paste("Aggregate ", gamma, "-variability"))))
# 
# plot_grid(alpha_comp_agg_plot, gamma_comp_agg_plot, 
#           align = "hv", ncol = 2) +
#   ggsave("ESA_2019/figs/comp_agg_compare.png", width = 10, height = 5, units = "in", dpi = 600)

# magnitude of change
comp_agg_stab %>% 
  mutate(name = paste(site, organism),
         agg_diff = agg_alpha_var_rate - agg_gamma_var_rate,
         comp_diff = alpha_var_rate - gamma_var_rate,
         total_diff = sqrt(agg_diff^2 + comp_diff^2)) %>% 
  arrange(total_diff) %>% filter(!is.na(total_diff)) %>% 
  ggplot(aes(x = total_diff, xend = 0, y = reorder(name, total_diff), yend = reorder(name, total_diff))) +
  geom_point() +
  geom_segment() +
  theme_minimal() +
  #geom_vline(xintercept = 0, color = "gray50") 
  labs(y = "", x = "Total ")

# whether stability is aggregate vs comp biased
comp_agg_stab %>% 
  mutate(ratio = log10(agg_phi_var/phi_var), 
         name = paste(site, organism),
         agg_diff = agg_alpha_var_rate - agg_gamma_var_rate,
         comp_diff = alpha_var_rate - gamma_var_rate,
         total_diff = sqrt(1/agg_phi_var + 1/phi_var)) %>% 
  arrange(ratio) %>% filter(!is.na(ratio)) %>% 
  ggplot(aes(x = ratio, xend = 0, 
             y = reorder(name, ratio), yend = reorder(name, ratio))) +
  geom_point(mapping = aes(size = total_diff), show.legend = F) +
  geom_segment() +
  theme_minimal() +
  geom_vline(xintercept = 0, color = "gray50") + 
  labs(y = "", x = expression(paste("Log"["10"], "(", phi["agg."], "/", phi["comp."], ")"))) +
  ggsave("ESA_2019/figs/phi_ratio.png", width = 7, height = 7*3/4, units = "in", dpi = 600)


# explaining comp vs. var stability
comp_agg_stab_derived <- comp_agg_stab %>% 
  mutate(ratio = log10(agg_phi_var/phi_var), 
         name = paste(site, organism),
         agg_diff = agg_alpha_var_rate - agg_gamma_var_rate,
         comp_diff = alpha_var_rate - gamma_var_rate,
         total_diff = sqrt(agg_diff^2 + comp_diff^2)) %>% 
  bind_cols(div.wide[,21:26])
comp_agg_stab_derived %>% 
  ggplot(aes(x = env_heterogeneity, y = total_diff)) +
  geom_point() +
  geom_smooth(method = 'lm')
comp_agg_stab_derived %>% 
  ggplot(aes(x = beta_div_mean, y = log(total_diff))) +
  geom_point()

#--------------
# How do variability component differ with metacommunity features
# visualize differences for metacommunity factors, by biome.

# Currently only 2 data sets each for freshwater and marine small body organisms. 
# collaps marine and freshwater into single "aquatic" biome to increase sample
# size for analysis

h.wide$biome2 <- "terrestrial"
h.wide[h.wide$biome == "freshwater" | h.wide$biome == "marine", ]$biome2 <- "aquatic"


(phi_size_biome <- ggplot(h.wide, aes(x = body.size, y = phi_var, 
                                       colour = biome2, fill = biome2)) + 
   geom_point() +
   geom_smooth(method = "lm") + 
   #geom_jitter(shape = 16, position = position_jitter(0.2)) +
   #theme_bw()+
    labs(y = expression(paste("Compositional ", phi, "-variability"))) +
   theme(legend.position = "none") +
    scale_x_log10() + 
    ggsave("ESA_2019/figs/body.size_by_biome_phi.png"))

(gam_size_biome <- ggplot(h.wide, aes(x = body.size, y = gamma_var, 
                                       colour = biome2, fill = biome2)) + 
    geom_point() +
    geom_smooth(method = "lm") +
    #theme_bw()+
    theme(legend.position = "none") +
    scale_x_log10() + 
    labs(y = expression(paste("Compositional ", gamma, "-variability")))+
  ggsave("ESA_2019/figs/body.size_by_biome_gamma.png"))

(alpha_size_biome <- ggplot(h.wide, aes(x = body.size, y = alpha_var, 
                                         colour = biome2, fill = biome2)) + 
    geom_point() +
    geom_smooth(method = "lm" ) +
    #theme_bw() +
    theme(legend.position = "bottom") +
    labs(x = "Body Size (m)",
         y = expression(paste("Compositional ", alpha, "-variability"))) +
    scale_x_log10() +
    ggsave("ESA_2019/figs/body.size_by_biome_alpha.png"))

png(file = "ESA_2019/figs/bodysize_biome_h.png", width = 4, height = 9, units = 'in', res = 1000)
ggarrange(gam_size_biome + xlab(""), phi_size_biome + xlab(""), alpha_size_biome, 
          ncol = 1, common.legend = F)
dev.off()

# Test above patterns with mixed model. Random effect for organism type nested within sites
lmer.model1 <- lmer(phi_var ~ log(n.plots) + biome2 * body.size + (1 | site/organism_group), 
                    data = h.wide)
summary(lmer.model1)
anova(lmer.model1)

#checking assumptions
plot(lmer.model1)
qqnorm(resid(lmer.model1)) 





div_stab_comp$biome2 <- "terrestrial"
div_stab_comp[div_stab_comp$biome == "freshwater" | div_stab_comp$biome == "marine", ]$biome2 <- "aquatic"

lmer.model2 <- lmer(log(gamma_var) ~ log(gamma_div_mean) + biome2*body.size + 
                      env_heterogeneity + temp_temporal_sd + (1 | organism_group), 
                    data = div_stab_comp)
summary(lmer.model2)
anova(lmer.model2)
confint(lmer.model2, method = "boot")

#checking assumptions
par(mfrow = c(1,1))
plot(lmer.model2)
qqnorm(resid(lmer.model2)) 


lmer.model3 <- lmer(alpha_var ~ log10(alpha_div_mean) + biome2 * body.size + (1 | organism_group), 
                    data = div_stab_comp)
summary(lmer.model3)
anova(lmer.model3)
confint(lmer.model3, method = "boot")

#checking assumptions
par(mfrow = c(1,1))
plot(lmer.model3)
qqnorm(resid(lmer.model3)) 

agg.wide$biome2 <- "terrestrial"
agg.wide[agg.wide$biome == "freshwater" | agg.wide$biome == "marine", ]$biome2 <- "aquatic"

lmer.model4 <- lmer(log(gamma_var) ~ log(n.taxa) + biome2 * body.size + (1 | site/organism_group), 
                    data = agg.wide)
summary(lmer.model4)
anova(lmer.model4)

#checking assumptions
par(mfrow = c(1,1))
plot(lmer.model4)
qqnorm(resid(lmer.model4)) 


lmer.model5 <- lmer(log(alpha_var) ~ log(n.taxa) + biome2 * body.size + (1 | site/organism_group), 
                    data = agg.wide)
summary(lmer.model5)
anova(lmer.model5)

#checking assumptions
par(mfrow = c(1,1))
plot(lmer.model5)
qqnorm(resid(lmer.model5)) 

# metacommunity variability has important covariates to account for in models
summary(lmer(log(gamma_var) ~ log(n.taxa) + (1 | site/organism_group), data = h.wide))
summary(lmer(log(alpha_var) ~ log(n.taxa) + (1 | site/organism_group), data = h.wide))
summary(lmer(phi_var ~ log(n.plots) + (1 | site/organism_group), data = h.wide))


# Effects of temporal variability in NDVI
# dispersal habit by ndvi variability interaction
lmer.model2 <- lmer(phi_var ~ log(n.plots) + dispersal.habit * log(ndvi_temporal_sd) + 
                      (1 | site/organism_group), 
                    data = h.wide)
summary(lmer.model2)
anova(lmer.model2)
#checking assumptions
par(mfrow = c(1,1))
plot(lmer.model2)
qqnorm(resid(lmer.model2))

ggplot(h.wide, aes(x = log(ndvi_temporal_sd), y = phi_var, 
      colour = dispersal.habit, group = dispersal.habit)) + 
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = F) +
    facet_grid(. ~ dispersal.habit) + 
    theme_bw() +
    theme(legend.position = "none") +
    ggsave("ESA_2019/figs/phi_disp_ndvi.png", width = 6, height = 4, dpi = 600, units = "in")

lmer.model2 <- lmer(phi_var ~ log(n.plots) + body.size * log(ndvi_temporal_sd) + 
                      (1 | site/organism_group), 
                    data = h.wide)
summary(lmer.model2)
anova(lmer.model2)
#checking assumptions
par(mfrow = c(1,1))
plot(lmer.model2)
qqnorm(resid(lmer.model2))

ggplot(h.wide, aes(x = log(ndvi_temporal_sd), y = phi_var, 
                   colour = body.size, group = body.size)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ body.size) + 
  theme_bw() +
  theme(legend.position = "none") +
  ggsave("ESA_2019/figs/phi_body_ndvi.png", width = 6, height = 4, dpi = 600, units = "in")


lmer.model2 <- lmer(phi_var ~ log(n.plots) + body.size * log(temp_temporal_sd) + 
                      (1 | site/organism_group), 
                    data = h.wide)
summary(lmer.model2)
anova(lmer.model2)
#checking assumptions
par(mfrow = c(1,1))
plot(lmer.model2)
qqnorm(resid(lmer.model2))

ggplot(h.wide, aes(x = log(temp_temporal_sd), y = phi_var, 
                   colour = body.size, group = body.size)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ body.size) + 
  theme_bw() +
  theme(legend.position = "none") +
  ggsave("ESA_2019/figs/phi_body_temp.png", width = 6, height = 4, dpi = 600, units = "in")

# neither gamma nor alpha var are related to interactions between dispersal habit
# or body size and ndvi, temp, or env heterogeneity.




lmer.modelx <- lmer(phi_var ~ log(n.plots) + body.size * env_heterogeneity + 
                      (1 | site/organism_group), 
                    data = h.wide)
anova(lmer.modelx)

ggplot(h.wide, aes(x = env_heterogeneity, y = phi_var, 
                   colour = body.size, group = body.size)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = F) +
  facet_grid(. ~ body.size) + 
  theme_bw() +
  theme(legend.position = "none") +
  ggsave("ESA_2019/figs/phi_body_envhet.png", width = 6, height = 4, dpi = 600, units = "in")

par(mfrow = c(1,1))
plot(lmer.modelx)
qqnorm(resid(lmer.modelx))















#------------------------------------------------------------------------------
#what causes variation in local, spatial, and regional var?

#Models to rest results from ESA abstract
# Result 1: 
ln.gamma <- lm(log(gamma_var) ~ log10(n.taxa) + temp_temporal_sd + ndvi_temporal_sd + env_heterogeneity,
               data = h.wide)
par(mfrow = c(2,2))
plot(ln.gamma)
summary(ln.gamma)
avPlots(ln.gamma)



# ln.gamma <- lm(log(gamma_var) ~ temp_temporal_sd,
#                data = hT.wide[hT.wide$body.size == "macro" & hT.wide$biome == "terrestrial",])
# par(mfrow = c(2,2))
# plot(ln.gamma)
# summary(ln.gamma)
# avPlots(ln.gamma)
# 




# latitudinal gradient in synchrony?
ln.phi <- aov(phi_var ~ log(n.plots) + biome * dispersal.habit,
              data = hT.wide)
par(mfrow = c(2,2))
plot(ln.phi)
summary(ln.phi)
avPlots(ln.phi)


ln.phi <- lm(phi_var ~ log(n.plots) + abs(lat) * dispersal.habit + biome2,
              data = h.wide)
par(mfrow = c(2,2))
plot(ln.phi)
summary(ln.phi)
avPlots(ln.phi)

h.wide %>% 
  ggplot(aes(x = abs(lat), y = phi_var, color = biome2)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# visualize differences for metacommunity factors, by biome.
(phi_size_biome <- ggplot(h.wide, aes(x = body.size, y = phi_var, 
                         colour = body.size, group = body.size)) + 
    geom_boxplot() +
    facet_grid(. ~ biome) +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw()+
    theme(legend.position = "none"))

(gam_size_biome <- ggplot(h.wide, aes(x = body.size, y = log(gamma_var), 
                          colour = body.size, group = body.size)) + 
    geom_boxplot() +
    facet_grid(. ~ biome) +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw()+
    theme(legend.position = "none"))

(alpha_size_biome <- ggplot(h.wide, aes(x = body.size, y = log(alpha_var), 
                          colour = body.size, group = body.size)) + 
    geom_boxplot() +
    facet_grid(. ~ biome) +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw() +
    theme(legend.position = "none"))

jpeg(file = "ESA_2019/figs/bodysize_biome.jpeg", width = 6.5, height = 5, units = 'in', res = 1000)
ggarrange(gam_size_biome, alpha_size_biome, phi_size_biome, 
                 ncol = 2, nrow = 2, common.legend = F)
dev.off()

# visualize differences for metacommunity factors, by biome.
(phi_biome <- ggplot(h.wide, aes(x = biome2, y = phi_var, 
                          colour = biome2, group = biome2)) + 
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(gam_biome <- ggplot(h.wide, aes(x = biome2, y = log(gamma_var), 
                          colour = biome2, group = biome2)) + 
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(alpha_biome <- ggplot(h.wide, aes(x = biome2, y = log(alpha_var), 
                          colour = biome2, group = biome2)) + 
    geom_boxplot() +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

jpeg(file = "biome.jpeg", width = 6.5, height = 2.5, units = 'in', res = 1000)
ggarrange(gam_biome, phi_biome, alpha_biome, 
          ncol = 3, nrow = 1, common.legend = TRUE, legend = "bottom")
dev.off()

#dispersal habit
(phi_disp_biome <- ggplot(h.wide[h.wide$dispersal.habit != "need to look at taxa",], 
             aes(x = dispersal.habit, y = phi_var, 
                 colour = dispersal.habit, 
                 group = dispersal.habit)) + 
    geom_boxplot() +
    facet_grid(. ~ biome2) +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(gam_disp_biome <- ggplot(h.wide[h.wide$dispersal.habit != "need to look at taxa",], 
             aes(x = dispersal.habit, y = log(gamma_var), 
                          colour = dispersal.habit, 
                          group = dispersal.habit)) + 
    geom_boxplot() +
    facet_grid(. ~ biome2) +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

(alpha_disp_biome <- ggplot(h.wide[h.wide$dispersal.habit != "need to look at taxa",], 
             aes(x = dispersal.habit, y = log(alpha_var), 
                 colour = dispersal.habit, 
                 group = dispersal.habit)) + 
    geom_boxplot() +
    facet_grid(. ~ biome2) +
    geom_jitter(shape = 16, position = position_jitter(0.2)) +
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()))

jpeg(file = "ESA_2019/figs/dispersal_biome.jpeg", width = 6.5, height = 5, units = 'in', res = 1000)
ggarrange(gam_disp_biome, alpha_disp_biome, phi_disp_biome, 
          ncol = 2, nrow = 2, common.legend = TRUE, legend = "bottom")
dev.off()

# ##
# 
# hT.wide$biome2 <- "terrestrial"
# hT.wide[hT.wide$biome == "freshwater" | hT.wide$biome == "marine", ]$biome2 <- "aquatic"
# 
# ggplot(hT.wide, aes(x = temp_temporal_sd, y = log(gamma_var))) + 
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "lm", se = T) +
#   facet_grid(. ~ biome2) +
#   ggsave("gam_temp_var.jpeg")
# 
# lm.gamma <- aov(log(gamma_var) ~ n.years + log(n.taxa) + temp_temporal_sd * biome2,
#                data = hT.wide)
# par(mfrow = c(2,2))
# plot(lm.gamma)
# summary(lm.gamma)
# 
# lm.gamma2 <- aov(log(gamma_var) ~ n.years + log(n.taxa) + temp_temporal_sd * body.size,
#                data = hT.wide)
# par(mfrow = c(2,2))
# plot(lm.gamma2)
# summary(lm.gamma2)
# 
# lm.gamma2 <- aov(log(gamma_var) ~ n.years + log(n.taxa) + env_heterogeneity * body.size,
#                  data = hT.wide)
# par(mfrow = c(2,2))
# plot(lm.gamma2)
# summary(lm.gamma2)
# avPlots(lm.gamma2)
# 
# lm.gamma2 <- aov(log(gamma_var) ~ n.years + log(n.taxa) + ndvi_temporal_sd * body.size,
#                  data = hT.wide)
# par(mfrow = c(2,2))
# plot(lm.gamma2)
# summary(lm.gamma2)
# 
# 
# 
# 
# lm.phi <- aov(phi_var ~ log(n.plots) + biome2 * body.size,
#                 data = hT.wide)
# par(mfrow = c(2,2))
# plot(lm.phi)
# summary(lm.phi)
# 
# lmer.model1 <- lmer(phi_var ~ log(n.plots) + biome2 * body.size + (1 | site), 
#                     data = hT.wide)
# summary(lmer.model1)
# anova(lmer.model1)
# 
# lmer.model2 <- lmer(phi_var ~ log(n.plots) + abs(lat) * dispersal.habit + (1 | site), 
#                     data = hT.wide[hT.wide$dispersal.habit != "need to look at taxa",])
# summary(lmer.model2)
# anova(lmer.model2)
# 
# lmer.model3 <- lmer(log(gamma_var) ~ n.years + log(n.taxa) + temp_temporal_sd * biome2 + (1 | site), 
#                     data = hT.wide)
# summary(lmer.model3)
# anova(lmer.model3)
# 
# 
# 
# (phi_size_biome <- ggplot(hT.wide, aes(x = body.size, y = phi_var, 
#                                        colour = body.size, group = body.size)) + 
#     geom_boxplot() +
#     facet_grid(. ~ biome2) +
#     geom_jitter(shape = 16, position = position_jitter(0.2)) +
#     theme_bw()+
#     theme(legend.position = "none") +
#     ggsave("phi_size_biome.jpeg"))
# 
# (phi_dis.lat <- ggplot(h.wide,
#                           aes(x = log(abs(lat)), y = phi_var, 
#                           colour = dispersal.habit, group = dispersal.habit)) + 
#     geom_point(alpha = 0.5) +
#     geom_smooth(method = "lm", se = T) +
#     facet_grid(. ~ dispersal.habit) + 
#     theme_bw() +
#     theme(legend.position = "none") +
#     ggsave("phi_disp_lat.jpeg"))
#     
#     
#     
# ggplot(hT, aes(x = ndvi_temporal_sd, y = metric_value, 
#                color = biome, shape = body.size)) + 
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "lm", se = F) +
#   facet_wrap(~ metric, scales = "free_y") +
#   ggsave("ESA_2019/figs/ndvi_var.png")
# 
# ggplot(hT, aes(x = temp_temporal_sd, y = metric_value, 
#                color = biome, shape = body.size)) + 
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE) +
#   facet_wrap(~ metric, scales = "free_y") +
#   ggsave("ESA_2019/figs/temp_var.png")
# 
# ggplot(hT, aes(x = lat, y = metric_value, 
#                color = biome, shape = body.size)) + 
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "lm", se = FALSE) +
#   facet_wrap(~ metric, scales = "free_y") +
#   ggsave("ESA_2019/figs/lat_var.png")
# 
# 
# library(car)
# # latitudinal gradient in synchrony?
# ln.phi <- aov(phi_var ~ log(n.plots) + lat * dispersal.habit,
#              data = hT.wide)
# par(mfrow = c(2,2))
# plot(ln.phi)
# summary(ln.phi)
# avPlots(ln.phi)
# 
# ggplot(hT.wide, aes(x = abs(lat), y = log(alpha_var))) + 
#   geom_point(alpha = 0.5) +
#   geom_smooth(method = "lm", se = T) +
#   facet_grid(. ~ dispersal.habit) +
#   ggsave("gam_temp_var.jpeg")
# 
# 
# 
# 
# ln.gamma <- lm(log(gamma_var) ~ temp_temporal_sd,
#              data = hT.wide[hT.wide$body.size == "macro" & hT.wide$biome == "terrestrial",])
# par(mfrow = c(2,2))
# plot(ln.gamma)
# summary(ln.gamma)
# avPlots(ln.gamma)
# 
# 
# 


