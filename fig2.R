library(tidyverse)
library(patchwork)
#library(ggrepel)
library(ggalt)
library(mapppdr)
library(hrbrthemes)

# define regional colors to match Fig 2
ea_border <- rgb(153, 20, 36, maxColorValue = 255)
ross_border <- rgb(18, 145, 209, maxColorValue = 255)
ap_border <- rgb(27, 39, 110, maxColorValue = 255)
pgeo_dot <- "#5DB86A"
l_size <- 2

# load data
data_fig3_1 <- readRDS("google_drive/data_fig3_1.rds")
data_fig3_2 <- readRDS("google_drive/data_fig3_2.rds")
work <- data.frame(
  rmse1 = data_fig3_1$rmse1,
  rmse2 = data_fig3_1$rmse2,
  site_id = data_fig3_1$site_names, 
  trad_R2 = data_fig3_1$trad_R2,
  R2_fore = round(data_fig3_2$cor_tt_fore^2, 2),
  R2_hind = round(data_fig3_2$cor_tt_hind^2, 2),
  rmse_fore = data_fig3_2$rmse_fore,
  rmse_hind = data_fig3_2$rmse_hind,
  rmse_fore_null = data_fig3_2$rmse_fore_null,
  rmse_hind_null = data_fig3_2$rmse_hind_null)
work <- work %>%
  left_join(sites, by = "site_id") %>%
  mutate(
    type = case_when(
      ccamlr_id == "48.1" ~ 3,
      ccamlr_id == "88.1" ~ 2,
      ccamlr_id == "58.4.2" ~ 1,
      ccamlr_id == "58.4.1" ~ 4)) %>%
  mutate(
    fill_val = case_when(
      ccamlr_id == "48.1" ~ ap_border,
      ccamlr_id == "88.1" ~ ross_border,
      ccamlr_id == "58.4.2" ~ ea_border,
      ccamlr_id == "58.4.1" ~ pgeo_dot)) %>%
  arrange(desc(type), trad_R2) %>%
  mutate(name = factor(site_id, levels = site_id)) %>%
  mutate(key = seq(1:24)) %>%
  mutate(fore_improv = ifelse(rmse_fore > rmse_fore_null, 0, 1)) %>%
  mutate(hind_improv = ifelse(rmse_hind > rmse_hind_null, 0, 1))

# R2 for all data
g1 <- ggplot() +
  geom_segment(data = work, aes(x = -.03, y = name, xend = trad_R2, yend = name), size = 0.15, color = "gray60") +
  geom_point(data = work, aes(y = name, x = trad_R2), col = "black", size = 3, pch = 21, fill = "orange") +
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 16.5, ymax = 24.5), alpha = .75, fill = ea_border) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 5.5, ymax = 16.5), fill = ross_border, alpha = .75) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 1.5, ymax = 5.5), fill = ap_border, alpha = .75) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = .5, ymax = 1.5), fill = pgeo_dot) +  
  geom_label(data = work, aes(x = -.08, y = key, label = name), fill = NA, color = "black", size = l_size, label.size = 0) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.13, .9)) +
  theme_tq(base_family = "Helvetica") +
  theme(
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 9),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank(),
    panel.border = element_blank()) +
  labs(y = "All Data") 

# R2 for forecast
g2 <- ggplot() +
  geom_segment(data = work, aes(x = -.03, y = name, xend = R2_fore, yend = name), size = 0.15, color = "gray60") +
  geom_point(data = work, aes(y = name, x = R2_fore), col = "black", size = 3, pch = 21, fill = "orange") +
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 16.5, ymax = 24.5), alpha = .75, fill = ea_border) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 5.5, ymax = 16.5), fill = ross_border, alpha = .75) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 1.5, ymax = 5.5), fill = ap_border, alpha = .75) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = .5, ymax = 1.5), fill = pgeo_dot) +  
  geom_label(data = work, aes(x = -.08, y = key, label = name), fill = NA, color = "black", size = l_size, label.size = 0) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.13, .9)) +
  theme_tq(base_family = "Helvetica") +
  theme(
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 9),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank(),
    panel.border = element_blank()) +
  labs(y = "Forecast")

# R2 for hindcast
g3 <- ggplot() +
  geom_segment(data = work, aes(x = -.03, y = name, xend = R2_hind, yend = name), size = 0.15, color = "gray60") +
  geom_point(data = work, aes(y = name, x = R2_hind), col = "black", size = 3, pch = 21, fill = "orange") +
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 16.5, ymax = 24.5), alpha = .75, fill = ea_border) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 5.5, ymax = 16.5), fill = ross_border, alpha = .75) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = 1.5, ymax = 5.5), fill = ap_border, alpha = .75) +  
  geom_rect(aes(xmin = -.13, xmax = -.03, ymin = .5, ymax = 1.5), fill = pgeo_dot) +  
  geom_label(data = work, aes(x = -.08, y = key, label = name), fill = NA, color = "black", size = l_size, label.size = 0) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.13, .9)) +
  theme_tq(base_family = "Helvetica") +
  theme(
    axis.text.y = element_blank(),
    text = element_text(size = 9),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank(),
    panel.border = element_blank()) +
  labs(x = bquote('R'^2), y = "Hindcast")

# RMSE for all data
g4 <- ggplot() +
  geom_segment(data = work, aes(x = -.02, y = name, xend = rmse1, yend = name), size = 0.15, color = "gray60") +
  geom_segment(data = work, aes(x = rmse1, y = name, xend = rmse2, yend = name), size = 2, color = "gray40") +
  geom_point(data = work, aes(y = name, x = rmse1), fill = "orange", size = 3, pch = 21, col = "black") +
  geom_point(data = work, aes(y = name, x = rmse2), fill = "white", size = 1.5, col = "black", pch = 21) +
  scale_alpha_manual(name = NULL,
                     values = c(1, 1),
                     breaks = c("Observed", "Fitted"),
                     guide = guide_legend(override.aes = list(linetype = c(0, 1),
                                                              shape = c(16, NA),
                                                              color = "black") ) ) +
geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 16.5, ymax = 24.5), fill = ea_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 5.5, ymax = 16.5), fill = ross_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 1.5, ymax = 5.5), fill = ap_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = .5, ymax = 1.5), fill = pgeo_dot) +  
  geom_label(data = work, aes(x = -.0475, y = key, label = name), fill = NA, color = "black", size = l_size, label.size = 0) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.075, .5), breaks = c(0, .1, .2, .3, .4), labels = c("0.0", "0.1", "0.2", "0.3", "0.6")) +
  theme_tq(base_family = "Helvetica") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 9),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank(),
    panel.border = element_blank())

# RMSE for forecast
litc <- work %>%
  dplyr::filter(site_id == "LITC") %>%
  mutate(rmse_fore = rmse_fore - .2) %>%
  mutate(rmse_fore_null = rmse_fore_null - .2)

g5 <- ggplot() +
  geom_segment(data = work, aes(x = -.02, y = name, xend = rmse_fore, yend = name), size = 0.15, color = "gray60") +
  geom_segment(data = work, aes(x = rmse_fore, y = name, xend = rmse_fore_null, yend = name, color = as.factor(fore_improv)), size = 2) +
  geom_segment(data = litc, aes(x = rmse_fore, y = name, xend = rmse_fore_null, yend = name, color = as.factor(fore_improv)), size = 2) +
  scale_color_manual(values = c("#e3e2e1", "gray40")) + 
  geom_point(data = work, aes(y = name, x = rmse_fore), col = "black", size = 3, pch = 21, fill = "orange") +
  geom_point(data = work, aes(y = name, x = rmse_fore_null), col = "black", size = 1.5, pch = 21, fill = "white") +
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 16.5, ymax = 24.5), fill = ea_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 5.5, ymax = 16.5), fill = ross_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 1.5, ymax = 5.5), fill = ap_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = .5, ymax = 1.5), fill = pgeo_dot) +  
  geom_label(data = work, aes(x = -.0475, y = key, label = name), fill = NA, color = "black", size = l_size, label.size = 0) +
  geom_segment(data = litc, aes(x = -.02, y = name, xend = .38, yend = name), size = 0.15, color = "gray60") +
  geom_segment(data = litc, aes(x = .385, y = name, xend = rmse_fore_null, yend = name), size = 0.15, color = "gray60") +
  geom_point(data = litc, aes(y = name, x = .38), col = "gray60", size = 3, pch = 47) +
  geom_point(data = litc, aes(y = name, x = .385), col = "gray60", size = 3, pch = 47) +
  geom_point(data = litc, aes(y = name, x = rmse_fore), col = "black", size = 3, pch = 21, fill = "orange") +
  geom_point(data = litc, aes(y = name, x = rmse_fore_null), col = "black", size = 1.5, pch = 21, fill = "white") +
  scale_x_continuous(expand = c(0, 0), limits = c(-.075, .5), breaks = c(0, .1, .2, .3, .4), labels = c("0.0", "0.1", "0.2", "0.3", "0.6")) +
  theme_tq(base_family = "Helvetica") +
  theme(
    legend.position="none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    text = element_text(size = 9),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank(),
    panel.border = element_blank())  
  
# RMSE for hindcast
g6 <- ggplot() +
  geom_segment(data = work, aes(x = -.02, y = name, xend = rmse_hind, yend = name), size = 0.15, color = "gray60") +
  geom_segment(data = work, aes(x = rmse_hind, y = name, xend = rmse_hind_null, yend = name, color = as.factor(hind_improv)), size = 2) +
  scale_color_manual(values = c("#e3e2e1", "gray40")) + 
  geom_point(data = work, aes(y = name, x = rmse_hind), col = "black", size = 3, pch = 21, fill = "orange") +
  geom_point(data = work, aes(y = name, x = rmse_hind_null), col = "black", size = 1.5, pch = 21, fill = "white") +
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 16.5, ymax = 24.5), fill = ea_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 5.5, ymax = 16.5), fill = ross_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = 1.5, ymax = 5.5), fill = ap_border, alpha = .75) +  
  geom_rect(aes(xmin = -.075, xmax = -.02, ymin = .5, ymax = 1.5), fill = pgeo_dot) +  
  geom_label(data = work, aes(x = -.0475, y = key, label = name), fill = NA, color = "black", size = l_size, label.size = 0) +
  scale_x_continuous(expand = c(0, 0), limits = c(-.075, .5), breaks = c(0, .1, .2, .3, .4), labels = c("0.0", "0.1", "0.2", "0.3", "0.6")) +
  theme_tq(base_family = "Helvetica") +
  theme(
    legend.position="none",
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 9),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank(),
    panel.border = element_blank()) +
  xlab("RMSE")
  
(g1 + g4) / (g2 + g5) / (g3 + g6) + plot_annotation(tag_levels = 'A')
ggsave("fig3_backup.pdf", width = 18, height = 24, units = "cm")

