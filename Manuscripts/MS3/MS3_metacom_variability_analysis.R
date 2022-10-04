library(here)
library(tidyverse)
library(ggthemes)
library(patchwork)
library(broom)
library(ggrepel)
library(lme4)
library(sjPlot)
library(MuMIn)

theme_set(theme_base() + 
            theme(plot.background = element_blank(),
                  legend.title = element_text(size = 10),
                  legend.text = element_text(size = 8)))

pal <- colorspace::darken(RColorBrewer::brewer.pal(n = 10, name = "Set3"), amount = .2)

# 1. IMPORT DATA SETS ------------------------------------------------------------
metacom_var <- read_csv(here("Manuscripts/MS3/data/L4_metacommunity_variability_analysis_results_2022-02-10.csv"))
local_var <- read_csv(here("Manuscripts/MS3/data/L4_local_variability_analysis_results_2022-02-10.csv"))
env_var <- read_csv(here("Manuscripts/MS3/data/lter_centroid_satdata.csv"))
data_list <- read_csv(here("Manuscripts/MS3/data/L3_DATA_list.csv"))

metacom_var <- data_list %>% 
  rename(dataset_file_name = l3_filename) %>% 
  left_join(metacom_var, by = "dataset_file_name")

organism_group_key <- (metacom_var %>% 
    select(dataset_id, organism_group)) %>% distinct()

local_var <- data_list %>% 
  rename(dataset_file_name = l3_filename) %>% 
  left_join(local_var, by = "dataset_file_name")

unique(local_var$dataset_id) #33
unique(metacom_var$dataset_id) #33

# set up data frames

metacom_divstab_comp_dat <- metacom_var %>% select(dataset_id, `LTER site`, organism_group, variability_type, standardization_method, metric, metric_value) %>% 
  filter(standardization_method == "h") %>% 
  pivot_wider(names_from = "metric", values_from = "metric_value") %>% 
  left_join(metacom_var %>% 
              filter(variability_type == "divpart_time_series") %>% 
              select(dataset_id, metric, metric_value) %>% 
              pivot_wider(names_from = "metric", values_from = "metric_value"), 
            by = "dataset_id")

metacom_divstab_agg_dat <- metacom_var %>% select(dataset_id, `LTER site`, organism_group, variability_type, metric, metric_value) %>% 
  filter(variability_type == "agg") %>% 
  pivot_wider(names_from = "metric", values_from = "metric_value") %>% 
  left_join(metacom_var %>% 
              filter(variability_type == "divpart_time_series") %>% 
              select(dataset_id, metric, metric_value) %>% 
              pivot_wider(names_from = "metric", values_from = "metric_value"), 
            by = "dataset_id")

sum(metacom_divstab_agg_dat$dataset_id %in% metacom_divstab_comp_dat$dataset_id)

local_var <- local_var %>% filter(dataset_id %in% metacom_divstab_agg_dat$dataset_id)

local_divstab_regs <-  local_var %>% 
  select(dataset_id, `LTER site`, SITE_ID, organism_group, metric, metric_value) %>% 
  pivot_wider(names_from = "metric", values_from = "metric_value")


# 2. LOCAL DIVERSITY STABILITY -----------------------------------------------
# This shows patterns within metacommunities about local community richness and variability
# Then compares across all sites, to show that sometimes these local relationships are stronger than others

local_div_stab_comp_alpha_mod <- lm(BD ~ site_mean_alpha_div, data = local_divstab_regs)
local_div_stab_comp_alpha_fit <- glance(local_div_stab_comp_alpha_mod)
(p_val <- as.character(round(local_div_stab_comp_alpha_fit$p.value,2))) # 0.16
(r2 <- as.character(round(local_div_stab_comp_alpha_fit$r.squared,2))) # 0.


local_div_stab_agg_alpha_mod <- lm(CV ~ site_mean_alpha_div, data = local_divstab_regs)
local_div_stab_agg_alpha_fit <- glance(local_div_stab_agg_alpha_mod)
(p_val <- as.character(round(local_div_stab_agg_alpha_fit$p.value,2))) # 0.
(r2 <- as.character(round(local_div_stab_agg_alpha_fit$r.squared,2))) # 0.09


### mixed effects models
local_dataset_for_mods <- local_var %>% select(dataset_id, `LTER site`, SITE_ID, organism_group, metric, metric_value) %>% 
  pivot_wider(names_from = "metric", values_from = "metric_value") 

local_comp_mod_lmm <- glmer(BD ~ site_mean_alpha_div + (site_mean_alpha_div|dataset_id), data = local_dataset_for_mods, family = "gaussian")
summary(local_comp_mod_lmm)
plot(local_comp_mod_lmm)

local_agg_mod_lmm <- glmer(CV ~ site_mean_alpha_div + (site_mean_alpha_div|dataset_id), data = local_dataset_for_mods, family = "gaussian")
summary(local_agg_mod_lmm)
plot(local_agg_mod_lmm)


summary(local_agg_mod_lmm)
coef(local_agg_mod_lmm)

summary(local_comp_mod_lmm)
coef(local_comp_mod_lmm)


r2m_comp <- r.squaredGLMM(local_comp_mod_lmm)[1]
r2c_comp <- r.squaredGLMM(local_comp_mod_lmm)[2]

r2m_agg <- r.squaredGLMM(local_agg_mod_lmm)[1]
r2c_agg <- r.squaredGLMM(local_agg_mod_lmm)[2]

### make figs

local_divstab_comp_fig <- local_var %>% select(dataset_id, `LTER site`, SITE_ID, organism_group, metric, metric_value) %>% 
  pivot_wider(names_from = "metric", values_from = "metric_value") %>% 
                          
  ggplot(aes(x = site_mean_alpha_div, y = BD)) + 
  geom_point(mapping = aes(group = dataset_id, color = organism_group), alpha = 0.3) + 
  geom_smooth(mapping = aes(group= dataset_id, color = organism_group), method = "lm",size=0.5, se = F, show.legend = FALSE) + 
  #geom_smooth(method = "lm", se = F, color = "black", size = 2, linetype = "dashed") +
  labs(x = expression(paste("Mean ", alpha, "-diversity")), y = expression(paste("Comp. ", alpha, "-variability (", BD^h[alpha],")")), color = "Organism group") + 
  scale_color_manual(values = pal) +
  annotate("text", x = 40, y = 0.8, label = bquote(atop(paste(R[m]^2, "= 0.0009", ),
                                                        paste(R[c]^2, "= 0.620"))))
local_divstab_comp_fig


local_divstab_agg_fig <- local_var %>% select(dataset_id, `LTER site`, SITE_ID, organism_group, metric, metric_value) %>% 
  pivot_wider(names_from = "metric", values_from = "metric_value") %>% 
  
  ggplot(aes(x = site_mean_alpha_div, y = CV)) + 
  geom_point(mapping = aes(group = dataset_id, color = organism_group), alpha = 0.3) + 
  geom_smooth(mapping = aes(group = dataset_id, color = organism_group), method = "lm", size =0.5, se = F, show.legend = FALSE) + 
  geom_smooth(method = "lm", se = F, color = "black", size = 1.5) +
  labs(x = expression(paste("Mean ", alpha, "-diversity")), y = expression(paste("Agg. ", alpha, "-variability (CV)")), color = "Organism group") + 
  scale_color_manual(values = pal) +
  annotate("text", x = 40, y = 1.5, label = bquote(atop(paste(R[m]^2, "= 0.0932", ),
                                                        paste(R[c]^2, "= 0.8479"))))
local_divstab_agg_fig


local_divstab_fig <- local_divstab_agg_fig + local_divstab_comp_fig + 
  plot_layout(ncol = 1, guides = "collect") + plot_annotation(tag_levels = "A")
ggsave(filename = here("Manuscripts/MS3/figs/local_divstab_fig.png"), plot = local_divstab_fig, dpi = 600, width = 6, height = 6*3/4*2, bg = "white")



# 3. REGIONAL DIVERSITY STABILITY ------------------------------------------------
# What happens when we look at the regional scale?
# Do we see richness-variability relationships with composition and aggregate at across metacommunities?

div_stab_comp_gamma_mod <- (lm(gamma_var_rate ~ gamma_div_mean, data = metacom_divstab_comp_dat))
div_stab_comp_beta_mod <- (lm(phi_var ~ beta_div_mean, data = metacom_divstab_comp_dat))
div_stab_comp_alpha_mod <- (lm(alpha_var_rate ~ alpha_div_mean, data = metacom_divstab_comp_dat))
div_stab_comp_alpha_gamma_mod <- (lm(gamma_var_rate ~ alpha_div_mean, data = metacom_divstab_comp_dat))
div_stab_comp_beta_gamma_mod <- (lm(gamma_var_rate ~ beta_div_mean, data = metacom_divstab_comp_dat))




div_stab_agg_gamma_mod <- (lm(gamma_var_rate ~ gamma_div_mean, data = metacom_divstab_agg_dat))
div_stab_agg_beta_mod <- (lm(phi_var ~ beta_div_mean, data = metacom_divstab_agg_dat))
div_stab_agg_alpha_mod <- (lm(alpha_var_rate ~ alpha_div_mean, data = metacom_divstab_agg_dat))
div_stab_agg_alpha_gamma_mod <- (lm(gamma_var_rate ~ alpha_div_mean, data = metacom_divstab_agg_dat))
div_stab_agg_beta_gamma_mod <- (lm(gamma_var_rate ~ beta_div_mean, data = metacom_divstab_agg_dat))


# regional compositional variability

div_stab_comp_gamma_fit <- glance(div_stab_comp_gamma_mod)
(p_val <- as.character(round(div_stab_comp_gamma_fit$p.value,2))) # 0.05
(r2 <- as.character(round(div_stab_comp_gamma_fit$r.squared,2))) # 0.17

(div_stab_gamma_h <- metacom_divstab_comp_dat %>% 
    ggplot(aes(x = gamma_div_mean, y = gamma_var_rate, label = `LTER site`)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", gamma, "-diversity")),
         y = expression(paste("Comp. ", gamma, "-variability (", BD^h [gamma], ")")),
         color = "Organism group")  +
    #scale_x_log10() +
    scale_color_manual(values = pal, drop = FALSE) +
    annotate("text", x = 10, y = 0.045, label = bquote(atop(paste(R^2, "= 0.17", ),
                                                            "p = 0.05")))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)


div_stab_comp_alpha_fit <- glance(div_stab_comp_alpha_mod)
(p_val <- as.character(round(div_stab_comp_alpha_fit$p.value,2))) # 0.86
(r2 <- as.character(round(div_stab_comp_alpha_fit$r.squared,2))) # 0.
(div_stab_alpha_h <- metacom_divstab_comp_dat %>% 
    ggplot(aes(x = alpha_div_mean, y = alpha_var_rate, label = `LTER site`)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Comp. ", alpha, "-variability")),
         color = "Organism group") +
    scale_color_manual(values = pal, drop = FALSE) +
    #scale_x_log10() +
    annotate("text", x = 30, y = 0.09, label = "p = 0.863")
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)


div_stab_alpha_gamma_fit <- glance(div_stab_comp_alpha_gamma_mod)
div_stab_alpha_gamma_fit$p.value # 0.023
div_stab_alpha_gamma_fit$r.squared # 0.232
(div_stab_alpha_gamma_h <- metacom_divstab_comp_dat %>% 
    ggplot(aes(x = alpha_div_mean, y = gamma_var_rate, label = `LTER site`)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Comp. ", gamma, "-variability")),
         color = "Organism group") +
    scale_color_manual(values = pal, drop = FALSE) +
    #scale_x_log10()  +
    annotate("text", x = 3, y = 0.045, label = bquote(atop(paste(R^2, "= 0.23"),
                                                           "p = 0.02")))  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_beta_gamma_fit <- glance(div_stab_comp_beta_gamma_mod)
div_stab_beta_gamma_fit$p.value # 0.2
div_stab_beta_gamma_fit$r.squared
(div_stab_beta_h <- metacom_divstab_comp_dat %>% 
    ggplot(aes(x = beta_div_mean, y = gamma_var_rate, label = `LTER site`)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Comp. ", gamma, "-variability")),
         color = "Organism group") +
    annotate("text", x = 4.75, y = 0.045, label = bquote("p = 0.2"))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

# regional aggregate variability
div_stab_gamma_agg_fit <- glance(div_stab_agg_gamma_mod)
div_stab_gamma_agg_fit$p.value # 0.202
div_stab_gamma_agg_fit$r.squared # 0.08
(div_stab_gamma_agg <- metacom_divstab_agg_dat %>% 
    ggplot(aes(x = gamma_div_mean, y = gamma_var_rate, label = `LTER site`)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", gamma, "-diversity")),
         y = expression(paste("Agg. ", gamma, "-variability (", CV^2, ")")),
         color = "Organism group") +
    annotate("text", x = 65, y = 0.025, label = bquote("p = 0.20"))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_alpha_agg_fit <- glance(div_stab_agg_alpha_mod)
div_stab_alpha_agg_fit$p.value # 0.17
div_stab_alpha_agg_fit$r.squared # 0.0927
(div_stab_alpha_agg <- metacom_divstab_agg_dat %>% 
    ggplot(aes(x = alpha_div_mean, y = alpha_var_rate, label = `LTER site`)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Agg. ", alpha, "-variability")),
         color = "Organism group")  +
    # scale_x_log10() +
    annotate("text", x = 30, y = .09, label = bquote("p = 0.17"))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_alpha_gamma_agg_fit <- glance(div_stab_agg_alpha_gamma_mod)
div_stab_alpha_gamma_agg_fit$p.value # 0.927
div_stab_alpha_gamma_agg_fit$r.squared # 0
(div_stab_alpha_gamma_agg <- metacom_divstab_agg_dat %>% 
    ggplot(aes(x = alpha_div_mean, y = gamma_var_rate, label = `LTER site`)) +
    #stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", alpha, "-diversity")),
         y = expression(paste("Agg. ", gamma, "-variability")),
         color = "Organism group")  +
    #scale_x_log10() +
    annotate("text", x = 30, y = 0.025, label = "p = 0.927")
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

div_stab_beta_gamma_agg_fit <- glance(div_stab_agg_beta_gamma_mod)
div_stab_beta_gamma_agg_fit$p.value # 0.0095
div_stab_beta_gamma_agg_fit$r.squared # 0.29
(div_stab_beta_agg <- metacom_divstab_agg_dat %>% 
    ggplot(aes(x = beta_div_mean, y = gamma_var_rate, label = `LTER site`)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Agg. ", gamma, "-variability")),
         color = "Organism group")  + 
    annotate("text", x = 4.75, y = 0.02, label = bquote(atop(paste(R^2, "= 0.29"),
                                                               "p = 0.01")))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

# combine figs
div_stab_multipanel_fig <- div_stab_gamma_agg + div_stab_gamma_h +
  div_stab_beta_agg + div_stab_beta_h +
  div_stab_alpha_gamma_agg + div_stab_alpha_gamma_h +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 3)
ggsave(filename = "Manuscripts/MS3/figs/diversity_variability.png", plot = div_stab_multipanel_fig, width = 2.5*4, height = 3*3, units = "in", dpi = 1000, bg = "white")


summary(div_stab_agg_gamma_mod)
summary(div_stab_comp_gamma_mod)

# 4. COMPARE BETA DIVERSITY WITH PHI, SYNCHRONY
summary(div_stab_comp_beta_mod)
div_stab_phi_fit <- glance(div_stab_comp_beta_mod)
div_stab_phi_fit$p.value # 2 e-4
div_stab_phi_fit$r.squared # 0.49
(div_stab_phi_h <- metacom_divstab_comp_dat %>% 
    ggplot(aes(x = beta_div_mean, y = phi_var, label = `LTER site`)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Comp. Synchrony (", BD^h[phi], ")")),
         color = "Organism group")  +
    annotate("text", x = 4.75, y = 0.9, label = bquote(atop(paste(R^2, "= 0.49"),
                                                            "p = 0.0003")))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)

summary(div_stab_agg_beta_mod)
div_stab_phi_agg_fit <- glance(div_stab_agg_beta_mod)
div_stab_phi_agg_fit$p.value # 0.006
div_stab_phi_agg_fit$r.squared # 0.32
(div_stab_phi_agg <- metacom_divstab_agg_dat %>% 
    ggplot(aes(x = beta_div_mean, y = phi_var, label = `LTER site`)) +
    stat_smooth(method = "lm", se = T, size = 1, color = "black") +
    geom_point(size = 2, alpha = 0.7, mapping = aes(color = organism_group)) +
    scale_y_continuous(limits = c(0,1))+
    scale_color_manual(values = pal, drop = FALSE) +
    #geom_label_repel(size = 2) +
    labs(x = expression(paste("Mean ", beta, "-diversity")),
         y = expression(paste("Agg. Synchrony (",phi,")")),
         color = "Organism group") +
    annotate("text", x = 4.75, y = 0.9, label = bquote(atop(paste(R^2, "= 0.32"),
                                                            "p = 0.006")))
  #ggsave("ESA_2019/figs/variability_alpha-gamma.png", width = 6, height = 4, units = "in", dpi = 600)
)


# combine figs
div_stab_phi_beta_fig <- (div_stab_phi_agg) + div_stab_phi_h + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 2)
ggsave(filename = "Manuscripts/MS3/figs/beta_phi.png", plot = div_stab_phi_beta_fig, width = 1.5*4, height = 2*3, units = "in", dpi = 1000, bg = "white")

# Compare alpha-alpha, beta-beta-, gamma-gamma
div_stab_multiscale_partition_fig <- 
  (local_divstab_agg_fig + theme(legend.position = "none")) + (local_divstab_comp_fig + theme(legend.position = "none")) +
  div_stab_phi_agg + div_stab_phi_h +
  div_stab_gamma_agg + div_stab_gamma_h +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 3)
ggsave(filename = "Manuscripts/MS3/figs/diversity_variability_multiscale.png",plot = div_stab_multiscale_partition_fig, width = 2.7*4, height = 3.2*3, units = "in", dpi = 1000, bg = "white")



# 4. COMPOSITIONAL VS AGGREGATE VARIABILITY -----------------------------------
# 

comp_agg_stab <- left_join(metacom_divstab_agg_dat, metacom_divstab_comp_dat, by = c("dataset_id", "LTER site", "organism_group"), suffix = c("_agg", "_comp"))


comp_agg_fig <- na.omit(comp_agg_stab) %>% 
  ggplot(aes(label = `LTER site`, 
             color = organism_group, group = paste(`LTER site`, organism_group))) + 
  #geom_point(mapping = aes(x = alpha_var_rate_comp, y = alpha_var_rate_agg), alpha = 0.5, size = 3, shape = 22) +
  #geom_point(mapping = aes(x = gamma_var_rate_comp, y = gamma_var_rate_agg), alpha = 0.5, size = 3, shape = 19) +
  geom_segment(aes(x = alpha_var_rate_comp, xend = gamma_var_rate_comp, 
                   y = alpha_var_rate_agg, yend = gamma_var_rate_agg),
               alpha = 0.7, arrow = arrow(length = unit(.2, "cm"), type = "closed", angle = 15)) +
  #geom_text_repel(mapping = aes(x = gamma_var_rate_comp, y = gamma_var_rate_agg), show.legend = F, size = 2, min.segment.length = 0.15, force_pull = 1.2, max.time = 1) +
  scale_x_log10(limits = c(0.001, NA), breaks = c(0.001, .01, .1)) + 
  scale_y_log10() +
  #geom_text_repel(size = 2.5) +
  scale_color_manual(values = pal, drop = FALSE) +
  theme(legend.position = "none") +
  coord_fixed() +
  labs(x = "Compositional variability",
       y = "Aggregate variability",
       color = "Organism group")
ggsave(filename = "Manuscripts/MS3/figs/comp_agg_compare.png",plot = comp_agg_fig, bg = "white", width = 6, height = 6, dpi = 600)
 



s_rho <- cor(comp_agg_stab$phi_var_comp, comp_agg_stab$phi_var_agg, use = "pairwise.complete", method = "spearman")

phi_compare <- na.omit(comp_agg_stab) %>% 
  ggplot(aes(y = phi_var_agg,
             x = phi_var_comp,  label = `LTER site`, 
             color = organism_group, group = paste(`LTER site`, `organism_group`))) +
  geom_abline(slope = 1, intercept = 0, alpha = 0.25, linetype = "dashed") +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_manual(values = pal, drop = FALSE) +
  geom_text_repel(show.legend = F, size = 3) +
  labs(color = "Organism group",
       y = expression(paste("Agg. Spatial Synchrony (",phi,")")),
       x = expression(paste("Comp. Spatial Synchrony (",BD[phi],")"))) +
  coord_fixed() +
  scale_x_continuous(breaks = c(0.2, 0.4, 0.6, 0.8)) + 
  theme(legend.position = "right") +
  annotate("text", x = .75, y = 0.1, size = 5, label = expression(paste(rho, "= 0.65")))
ggsave("Manuscripts/MS3/figs/phi_comparison.png",plot = phi_compare, bg = "white", width = 6, height = 3/4*6, dpi = 600)

phi_compare_fig <- comp_agg_fig + phi_compare + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", nrow = 1)
ggsave("Manuscripts/MS3/figs/agg_comp_panel.png", plot = phi_compare_fig, bg = "white", width = 8, height = 6, dpi = 600)

