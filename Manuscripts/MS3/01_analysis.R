

rm(list = ls())

# Check for and install required packages
for (package in c('tidyverse', 'PerformanceAnalytics', 'ggthemes', 'vegan', 
                  'gridExtra', 'grid', 'viridis', 'lme4', 'lmerTest', 'car',
                  'ggpubr', 'ggrepel', 'cowplot', 'RLRsim', 'broom', 'patchwork')) {
  if (!require(package, character.only = T, quietly = T)) {
    install.packages(package)
    library(package, character.only = T)
  }
}

theme_set(theme_base() + 
            theme(plot.background = element_blank(),
                  legend.title = element_text(size = 10),
                  legend.text = element_text(size = 8)))

pal <- colorspace::darken(RColorBrewer::brewer.pal(n = 10, name = "Set3"), amount = .2)

# 1. IMPORT DATA SETS ------------------------------------------------------------
df <- read_csv("Manuscripts/MS3/output/metacom_data_for_models.csv")
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

df$organism_group <- as.factor(df$organism_group)
  

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
    ggsave("Manuscripts/MS3/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
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
    ggsave("Manuscripts/MS3/figs/variability_phi-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
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
    ggsave("Manuscripts/MS3/figs/variability_alpha-phi.png", width = 6, height = 4, units = "in", dpi = 600)
)
lm_alpha_phi <- lm(log(phi_var) ~ log(alpha_var), data = h.wide)
par(mfrow = c(2,2))
plot(lm_alpha_phi) # assumptions met
summary(lm_alpha_phi)

# DIversity stability 
#------------------------------------------------------------------------------
div_stab_comp <- cbind.data.frame(h.wide, div.wide[,c("alpha_div_mean", "beta_div_mean", "gamma_div_mean")]) %>% 
  filter(gamma_div_mean > 0)

div_stab_gamma_mod <- (lm((gamma_var_rate) ~ log10(gamma_div_mean), data = div_stab_comp))
modsum <- summary(div_stab_gamma_mod)
div_stab_gamma_mod <- glance(div_stab_gamma_mod)
(p_val <- as.character(round(div_stab_gamma_mod$p.value,2)))
(r2 <- as.character(round(div_stab_gamma_mod$r.squared,2)))
summary(lm((gamma_var_rate) ~ log10(gamma_div_mean), data = div_stab_comp))

(div_stab_gamma_h <- div_stab_comp %>% 
   ggplot(aes(x = gamma_div_mean, y = gamma_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, linetype = "dashed", color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", gamma, "-diversity")),
         y = expression(paste("Comp. ", gamma, "-variability")),
         color = "Organism group")  +
    scale_x_log10() +
    scale_color_manual(values = pal, drop = FALSE) +
    annotate("text", x = 10, y = 0.045, label = bquote(atop(paste(r^2, "= 0.13"),
                                                                 "p = 0.07")))
    #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_alpha_mod <- (lm((alpha_var_rate) ~ log10(alpha_div_mean), data = div_stab_comp))
modsum2 <- summary(div_stab_alpha_mod)
div_stab_alpha_mod <- glance(div_stab_alpha_mod)
div_stab_alpha_mod$p.value
div_stab_alpha_mod$r.squared
(div_stab_alpha_h <- div_stab_comp %>% 
    ggplot(aes(x = alpha_div_mean, y = alpha_var_rate, label = site)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Comp. ", alpha, "-variability")),
         color = "Organism group") +
    scale_color_manual(values = pal, drop = FALSE) +
    scale_x_log10() +
    annotate("text", x = 100, y = 0.1, label = bquote("p = 0.40"))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_alpha_gamma_mod <- (lm((gamma_var_rate) ~ log10(alpha_div_mean), data = div_stab_comp))
modsum3 <- summary(div_stab_alpha_gamma_mod)
div_stab_alpha_gamma_mod <- glance(div_stab_alpha_gamma_mod)
div_stab_alpha_gamma_mod$p.value
div_stab_alpha_gamma_mod$r.squared
(div_stab_alpha_gamma_h <- div_stab_comp %>% 
    ggplot(aes(x = alpha_div_mean, y = gamma_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Comp. ", gamma, "-variability")),
         color = "Organism group") +
    scale_color_manual(values = pal, drop = FALSE) +
    scale_x_log10()  +
    annotate("text", x = 3, y = 0.045, label = bquote(atop(paste(r^2, "= 0.15"),
                                                            "p = 0.05")))  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_beta_mod <- (lm((gamma_var_rate) ~ log10(beta_div_mean), data = div_stab_comp))
modsum4 <- summary(div_stab_beta_mod)
div_stab_beta_mod <- glance(div_stab_beta_mod)
div_stab_beta_mod$p.value
div_stab_beta_mod$r.squared
(div_stab_beta_h <- div_stab_comp %>% 
    ggplot(aes(x = beta_div_mean, y = gamma_var_rate, label = site)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Comp. ", gamma, "-variability")),
         color = "Organism group") +
    annotate("text", x = 4.75, y = 0.045, label = bquote("p = 0.41"))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_agg <- cbind.data.frame(agg.wide, div.wide[,c("alpha_div_mean", "beta_div_mean", "gamma_div_mean")]) %>% 
  filter(gamma_div_mean > 0)

div_stab_gamma_agg_mod <- (lm((gamma_var_rate) ~ log10(gamma_div_mean), data = div_stab_agg))
modsum5 <- summary(div_stab_gamma_agg_mod)
div_stab_gamma_agg_mod <- glance(div_stab_gamma_agg_mod)
div_stab_gamma_agg_mod$p.value
div_stab_gamma_agg_mod$r.squared
(div_stab_gamma_agg <- div_stab_agg %>% 
    ggplot(aes(x = gamma_div_mean, y = gamma_var_rate, label = site)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", gamma, "-diversity")),
         y = expression(paste("Agg. ", gamma, "-variability")),
         color = "Organism group") +
    scale_x_log10() +
    annotate("text", x = 120, y = 0.04, label = bquote("p = 0.40"))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_alpha_agg_mod <- (lm((alpha_var_rate) ~ log10(alpha_div_mean), data = div_stab_agg))
modsum6 <- summary(div_stab_alpha_agg_mod)
div_stab_alpha_agg_mod <- glance(div_stab_alpha_agg_mod)
div_stab_alpha_agg_mod$p.value
div_stab_alpha_agg_mod$r.squared
(div_stab_alpha_agg <- div_stab_agg %>% 
    ggplot(aes(x = alpha_div_mean, y = alpha_var_rate, label = site)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Agg. ", alpha, "-variability")),
         color = "Organism group")  +
    scale_x_log10() +
    annotate("text", x = 100, y = 0.1, label = bquote("p = 0.10"))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_alpha_gamma_agg_mod <- (lm((gamma_var_rate) ~ log10(alpha_div_mean), data = div_stab_agg))
modsum7 <- summary(div_stab_alpha_gamma_agg_mod)
div_stab_alpha_gamma_agg_mod$p.value
div_stab_alpha_gamma_agg_mod$r.squared
(div_stab_alpha_gamma_agg <- div_stab_agg %>% 
    ggplot(aes(x = alpha_div_mean, y = gamma_var_rate, label = site)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Agg. ", gamma, "-variability")),
         color = "Organism group")  +
    scale_x_log10() +
    annotate("text", x = 90, y = 0.04, label = bquote("p = 0.88"))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_beta_agg_mod <- (lm((gamma_var_rate) ~ log10(beta_div_mean), data = div_stab_agg))
modsum8 <- summary(div_stab_beta_agg_mod)
div_stab_beta_agg_mod$p.value
div_stab_beta_agg_mod$r.squared
(div_stab_beta_agg <- div_stab_agg %>% 
    ggplot(aes(x = beta_div_mean, y = gamma_var_rate, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Agg. ", gamma, "-variability")),
         color = "Organism group")  + 
    annotate("text", x = 4.75, y = 0.0375, label = bquote(atop(paste(r^2, "= 0.20"),
                                                            "p = 0.02")))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

# combine figs
div_stab_gamma_agg + div_stab_gamma_h +
  div_stab_beta_agg + div_stab_beta_h +
  div_stab_alpha_gamma_agg + div_stab_alpha_gamma_h +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 3) +
  ggsave("Manuscripts/MS3/figs/diversity_variability.png", width = 2.5*4, height = 3*3, units = "in", dpi = 1000)

# plot_grid(div_stab_gamma_agg, div_stab_gamma_h, 
#           div_stab_beta_agg, div_stab_beta_h,
#           div_stab_alpha_gamma_agg, div_stab_alpha_gamma_h, 
#           align = "hv", ncol = 2, labels = "AUTO") +
#   ggsave("Manuscripts/MS3/figs/diversity_variability.png", width = 2*4, height = 3*3, units = "in", dpi = 600)


div_stab_phi_mod <- broom::glance(lm(phi_var ~ (beta_div_mean), data = div_stab_comp))
div_stab_phi_mod$p.value
div_stab_phi_mod$r.squared
(div_stab_phi_h <- div_stab_comp %>% 
    ggplot(aes(x = beta_div_mean, y = phi_var, label = site)) +
  stat_smooth(method = "lm", se = T, size = 1, color = "black") +
  geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_color_manual(values = pal, drop = FALSE) +
  #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Comp. Synchrony (", BD[phi], ")")),
         color = "Organism group")  +
    annotate("text", x = 4.75, y = 0.9, label = bquote(atop(paste(r^2, "= 0.31"),
                                                               "p = 0.006")))
#ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_phi_agg_mod <- broom::glance(lm(phi_var ~ (beta_div_mean), data = div_stab_agg))
div_stab_phi_agg_mod$p.value
div_stab_phi_agg_mod$r.squared
(div_stab_phi_agg <- div_stab_agg %>% 
    ggplot(aes(x = beta_div_mean, y = phi_var, label = site)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_y_continuous(limits = c(0,1))+
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Agg. Synchrony (",phi,")")),
         color = "Organism group") +
    annotate("text", x = 4.75, y = 0.9, label = bquote(atop(paste(r^2, "= 0.21"),
                                                             "p = 0.02")))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)


# combine figs
(div_stab_phi_agg + labs(x = "")) + div_stab_phi_h + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 2) +
  ggsave("Manuscripts/MS3/figs/beta_phi.png", width = 1.5*4, height = 2*3, units = "in", dpi = 1000)

# plot_grid(div_stab_phi_agg, div_stab_phi_h, 
#           align = "hv", ncol = 2, labels = "AUTO") +
#   ggsave("Manuscripts/MS3/figs/beta_phi.png", width = 2*4, height = 3, units = "in", dpi = 600) + 
#   ggsave("Manuscripts/MS3/figs/beta_phi.pdf", width = 2*4, height = 3, units = "in")

agg.mod <-  lm((gamma_var_rate) ~ log10(gamma_div_mean), data = div_stab_agg)
comp.mod <- lm((gamma_var_rate) ~ log10(gamma_div_mean), data = div_stab_comp)
summary(agg.mod)
summary(comp.mod)
phi.agg.mod <-  lm(phi_var ~  beta_div_mean, data = div_stab_agg)
phi.comp.mod <- lm(phi_var ~  beta_div_mean, data = div_stab_comp)
summary(phi.agg.mod)
summary(phi.comp.mod)

div_stab_gamma_agg + div_stab_gamma_h +
  div_stab_phi_agg + div_stab_phi_h +
  div_stab_alpha_agg + div_stab_alpha_h +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 3) +
  ggsave("Manuscripts/MS3/figs/diversity_variability_multiscale.png", width = 2.5*4, height = 3*3, units = "in", dpi = 1000)

# ## Possible environmental drivers
# bind_rows(div_stab_agg, div_stab_comp) %>% 
#   mutate(trophic.group = stringr::str_to_sentence(trophic.group)) %>% 
#   gather(temp_temporal_sd, ndvi_temporal_sd, env_heterogeneity, key = env_var, value = variability) %>% 
#   mutate(env_var = ifelse(env_var == "temp_temporal_sd", "Temperature Variability (sd)", 
#                           ifelse(env_var == "ndvi_temporal_sd", "NDVI Variability (sd)", "Spatial Heterogeneity (Geodiversity)"))) %>% 
#   mutate(variability_type = ifelse(variability_type == "agg", "Aggregate", "Compositional")) %>% 
#   ggplot(aes(x = variability, y = gamma_var_rate, label = site, color = trophic.group, fill = trophic.group)) + 
#   geom_point() + 
#   geom_smooth(method = 'lm', size = 1, se = T) +
#   theme(strip.background = element_blank(),
#         strip.placement = "outside",
#         legend.position = "top", plot.background = element_blank()) +
#   facet_grid(variability_type ~ env_var, scales = "free_x", switch = "both") +
#   labs(x = "", y = "Metacommunity Variability") +
#   scale_color_pander(name = "Trophic Group") +
#   scale_fill_pander(name = "Trophic Group") +
#   ggsave("Manuscripts/MS3/figs/environmental_drivers.png", width = 10, height = 7.5, units = "in", dpi = 600)
# 
# summary(lm(log(gamma_var_rate) ~ trophic.group * (env_heterogeneity + ndvi_temporal_sd + temp_temporal_sd), data = cbind.data.frame(div_stab_agg, div_stab_comp)))

## compositional versus aggregate variability

comp_agg_stab <- agg.wide[,c("alpha_var_rate", "phi_var", "gamma_var_rate")] 
names(comp_agg_stab) <- paste0("agg_", names(comp_agg_stab))
comp_agg_stab <- cbind.data.frame(h.wide, comp_agg_stab) 
comp_agg_stab

#comp_agg_stab <- comp_agg_stab %>% 
#  mutate(organism_group = ifelse(organism_group == "sessile invertebrates", "invertebrates", organism_group))

comp_agg_fig <- na.omit(comp_agg_stab) %>% 
  ggplot(aes(label = site, 
             color = organism_group, group = paste(site, organism))) + 
  geom_point(mapping = aes(x = alpha_var_rate, y = agg_alpha_var_rate), alpha = 0.75, size = 3, shape = 22) +
  geom_point(mapping = aes(x = gamma_var_rate, y = agg_gamma_var_rate), alpha = 0.75, size = 3, shape = 19) +
  geom_segment(aes(x = alpha_var_rate, xend = gamma_var_rate, 
                  y = agg_alpha_var_rate, yend = agg_gamma_var_rate),
               alpha = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
  geom_text_repel(mapping = aes(x = gamma_var_rate, y = agg_gamma_var_rate), show.legend = F, size = 2) +
  scale_x_log10() + 
  scale_y_log10() +
  #geom_text_repel(size = 2.5) +
  scale_color_manual(values = pal, drop = FALSE) +
  theme(legend.position = "none") +
  coord_fixed() +
  labs(x = "Compositional variability",
       y = "Aggregate variability",
       color = "Organism group") +
  ggsave("Manuscripts/MS3/figs/comp_agg_compare.png", width = 4, height = 4, dpi = 600)
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

# # magnitude of change
# comp_agg_stab %>% 
#   mutate(name = paste(site, organism),
#          agg_diff = agg_alpha_var_rate - agg_gamma_var_rate,
#          comp_diff = alpha_var_rate - gamma_var_rate,
#          total_diff = sqrt(agg_diff^2 + comp_diff^2)) %>% 
#   arrange(total_diff) %>% filter(!is.na(total_diff)) %>% 
#   ggplot(aes(x = total_diff, xend = 0, y = reorder(name, total_diff), yend = reorder(name, total_diff))) +
#   geom_point() +
#   geom_segment() +
#   theme_minimal() +
#   #geom_vline(xintercept = 0, color = "gray50") 
#   labs(y = "", x = "Total ")

# # whether stability is aggregate vs comp biased
# comp_agg_stab %>% 
#   mutate(ratio = log10(agg_phi_var/phi_var), 
#          name = paste(site, organism),
#          agg_diff = agg_alpha_var_rate - agg_gamma_var_rate,
#          comp_diff = alpha_var_rate - gamma_var_rate,
#          total_diff = sqrt(1/agg_phi_var + 1/phi_var)) %>% 
#   arrange(ratio) %>% filter(!is.na(ratio)) %>% 
#   ggplot(aes(x = ratio, xend = 0, 
#              y = reorder(name, ratio), yend = reorder(name, ratio))) +
#   geom_point(mapping = aes(size = total_diff), show.legend = F) +
#   geom_segment() +
#   theme_minimal() +
#   geom_vline(xintercept = 0, color = "gray50") + 
#   labs(y = "", x = expression(paste("Log"["10"], "(", phi["agg."], "/", phi["comp."], ")"))) +
#   ggsave("Manuscripts/MS3/figs/phi_ratio.png", width = 7, height = 7*3/4, units = "in", dpi = 600)

tot_stab_fig <- comp_agg_stab %>% 
  mutate(ratio = log10(agg_phi_var/phi_var), 
         name = site,
         agg_diff = log10(agg_alpha_var_rate) - log10(agg_gamma_var_rate),
         comp_diff = log10(alpha_var_rate) - log10(gamma_var_rate),
         total_diff = sqrt(agg_diff^2 + comp_diff^2)) %>% 
  arrange(ratio) %>% filter(!is.na(ratio)) %>% 
  
  ggplot(aes(x = ratio, xend = 0, 
             y = total_diff,  label = site, 
             color = organism_group, group = paste(site, organism))) +
  geom_vline(xintercept = 0, alpha = 0.2, linetype = "dashed") +
  geom_point(size = 3, alpha = 0.75) +
  scale_color_manual(values = pal, drop = FALSE) +
  geom_text_repel(show.legend = F, size = 2) +
  labs(y = bquote(atop("Total spatial stabilization",
        scriptstyle(paste("(",
              sqrt(paste("(CV"[alpha]^2 - "CV"[gamma]^2, ")"^2 + 
                      "(BD"[alpha]^"h" - "BD"[gamma]^"h",")"^2)))))),
      x = expression(paste("Log"["10"], "(", phi["agg."], "/", phi["comp."], ")")),
      color = "Organism group") +
  ggsave("Manuscripts/MS3/figs/total_spatial_stabilization.png", width = 7, height = 7, units = "in", dpi = 600)


phi_compare <- comp_agg_stab %>% 
  ggplot(aes(y = agg_phi_var,
             x = phi_var,  label = site, 
             color = organism_group, group = paste(site, organism))) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.25, linetype = "dashed") +
  geom_point(size = 3, alpha = 0.75) +
  scale_color_manual(values = pal, drop = FALSE) +
  geom_text_repel(show.legend = F, size = 2) +
  labs(color = "Organism group",
       y = expression(paste("Agg. Spatial Synchrony (",phi,")")),
       x = expression(paste("Comp. Spatial Synchrony (",BD[phi],")"))) +
  coord_fixed() + 
  ggsave("Manuscripts/MS3/figs/phi_comparison.png", width = 6, height = 3/4*6, dpi = 600)

comp_agg_fig + phi_compare + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 2) +
  ggsave("Manuscripts/MS3/figs/agg_comp_panel.png", width = 7, height = 10, dpi = 1000)

