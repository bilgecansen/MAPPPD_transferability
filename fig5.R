library(tidyverse)
library(patchwork)
library(ggrepel)
library(ggalt)

ross_border <- rgb(18, 145, 209, maxColorValue = 255)

dat_fig5 <- readRDS("google_drive/data_fig5.rds")
ross_rmse <- dat_fig5$ross_rmse
ip <- dat_fig5$ip
sp_slope_mean <- dat_fig5$sp_slope_mean
sp_slope_min <- dat_fig5$sp_slope_min
sp_slope_max <- dat_fig5$sp_slope_max
g_dat <- dat_fig5$g_dat
ross_sites <- names(g_dat)
ross_sites <- ross_sites[order(ross_sites)]
work <- data.frame(
  ip = ip,
  sp_slope_mean = sp_slope_mean,
  sp_slope_min = sp_slope_min,
  sp_slope_max = sp_slope_max,
  ross_sites = ross_sites)

plot_site <- function(site) {
  y_pred1 <- t(g_dat[[site]][[1]][,1:4000])
  x1 <- g_dat[[site]][[1]][,4001]
  min_seq <- seq(0.025, 0.25, by = 0.01)
  mpa_min2 <- apply(y_pred1, 2, quantile, min_seq) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = everything())
  mpa_min2$x <- rep(x1, times = length(min_seq))
  mpa_min2$qq <- rep(min_seq, each = length(x1))
  max_seq <- seq(0.975, 0.75, by = -0.01)
  mpa_max2 <- apply(y_pred1, 2, quantile, max_seq) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = everything())
  mpa_rib <- cbind(mpa_min2, mpa_max2$value)
  colnames(mpa_rib)[c(2,5)] <- c("min", "max")
  return(mpa_rib)
}

t_size <- 6
p_size <- 2

mpa_rib <- plot_site("CMID")
site2 <- "CMID"
g2 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site2)$dist, y = filter(ross_rmse, sites == site2)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site2)$dist, y = filter(ross_rmse, sites == site2)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  labs(y = "", x = "", title = site2) +
  scale_y_continuous(limits = c(.5, 1.75), breaks = c(.5, 1, 1.5)) +
  scale_x_continuous(limits = c(125, 475), breaks = c(150, 250, 350, 450)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

mpa_rib <- plot_site("CHAL")
site3 <- "CHAL"
g3 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site3)$dist, y = filter(ross_rmse, sites == site3)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site3)$dist, y = filter(ross_rmse, sites == site3)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  labs(y = "", x = "", title = site3) +
  scale_x_continuous(limits = c(100, 625), breaks = c(150, 300, 450, 600)) +
  scale_y_continuous(limits = c(.5, 1.75), breaks = c(.5, 1, 1.5)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

mpa_rib <- plot_site("BEAU")
site4 <- "BEAU"
g4 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site4)$dist, y = filter(ross_rmse, sites == site4)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site4)$dist, y = filter(ross_rmse, sites == site4)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  labs(y = "", x = "", title = site4) +
  scale_x_continuous(limits = c(0,570), breaks = c(0, 190, 380, 570)) +
  scale_y_continuous(limits = c(.5, 1.75), breaks = c(.5, 1, 1.5)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

mpa_rib <- plot_site("INEX")
site5 <- "INEX"
g5 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site5)$dist, y = filter(ross_rmse, sites == site5)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site5)$dist, y = filter(ross_rmse, sites == site5)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  #labs(y = "RMSE Ratio", x = "Distance (km)", title = site5) +
  labs(y = "", x = "", title = site5) +
  scale_x_continuous(limits = c(190, 360), breaks = c(200, 250, 300, 350)) +
  scale_y_continuous(limits = c(.5, 1.75), breaks = c(.5, 1, 1.5)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

layout <- "
AB
CD
"
g2 + g3 + g4 + g5 + plot_layout(design = layout) + plot_annotation(tag_levels = 'A')

ggsave("fig7.pdf", width = 18, height = 14, units = "cm")
