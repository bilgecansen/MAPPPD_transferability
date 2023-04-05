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

mpa_rib <- plot_site("CRZE")
site6 <- "CRZE"
g6 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site6)$dist, y = filter(ross_rmse, sites == site6)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site6)$dist, y = filter(ross_rmse, sites == site6)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  labs(y = "", x = "", title = site6) +
  scale_x_continuous(limits = c(0, 600), breaks = c(0, 200, 400, 600)) +
  scale_y_continuous(limits = c(.75, 2), breaks = c(1, 1.5, 2)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

mpa_rib <- plot_site("CRZW")
site7 <- "CRZW"
g7 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site7)$dist, y = filter(ross_rmse, sites == site7)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site7)$dist, y = filter(ross_rmse, sites == site7)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  labs(y = "", x = "", title = site7) +
  scale_x_continuous(limits = c(0, 600), breaks = c(0, 200, 400, 600)) +
  scale_y_continuous(limits = c(.75, 2), breaks = c(1, 1.5, 2)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

mpa_rib <- plot_site("ROYD")
site8 <- "ROYD"
g8 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site8)$dist, y = filter(ross_rmse, sites == site8)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site8)$dist, y = filter(ross_rmse, sites == site8)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  labs(y = "", x = "", title = site8) +
  scale_x_continuous(limits = c(0, 600), breaks = c(0, 200, 400, 600)) +
  scale_y_continuous(limits = c(.75, 2), breaks = c(1, 1.5, 2)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

mpa_rib <- plot_site("BRDN")
site9 <- "BRDN"
g9 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site9)$dist, y = filter(ross_rmse, sites == site9)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site9)$dist, y = filter(ross_rmse, sites == site9)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  #labs(y = "RMSE Ratio", x = "Distance (km)", title = site5) +
  labs(y = "", x = "", title = site9) +
  scale_x_continuous(limits = c(0, 600), breaks = c(0, 200, 400, 600)) +
  scale_y_continuous(limits = c(.5, 3.5), breaks = c(1, 2, 3)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

mpa_rib <- plot_site("BRDM")
site10 <- "BRDM"
g10 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site10)$dist, y = filter(ross_rmse, sites == site10)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site10)$dist, y = filter(ross_rmse, sites == site10)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  #labs(y = "RMSE Ratio", x = "Distance (km)", title = site5) +
  labs(y = "", x = "", title = site10) +
  scale_x_continuous(limits = c(0, 600), breaks = c(0, 200, 400, 600)) +
  scale_y_continuous(limits = c(.75, 2), breaks = c(1, 1.5, 2)) +
  scale_alpha_continuous(range = 0.25, 0.75) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = t_size),
    axis.title = element_text(size = t_size),
    text = element_text(size = t_size),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())  

mpa_rib <- plot_site("BRDS")
site11 <- "BRDS"
g11 <- ggplot() +
  geom_ribbon(data = mpa_rib, mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "gray60") +
  geom_hline(yintercept = 1, color = "red", alpha = .75, size = .5) +
  geom_point(aes(x = filter(ross_rmse, sites == site11)$dist, y = filter(ross_rmse, sites == site11)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "white", alpha = 1) +
  geom_point(aes(x = filter(ross_rmse, sites == site11)$dist, y = filter(ross_rmse, sites == site11)$rmse), size = p_size, pch = 21, 
             color = "black", fill= "black", alpha = .5) +
  theme(legend.position = "none") +
  #labs(y = "RMSE Ratio", x = "Distance (km)", title = site5) +
  labs(y = "", x = "", title = site11) +
  scale_x_continuous(limits = c(0, 600), breaks = c(0, 200, 400, 600)) +
  scale_y_continuous(limits = c(.75, 2), breaks = c(1, 1.5, 2)) +
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
EF
"

g6 + g7 + g8 + g9 + g10 + g11 + plot_layout(design = layout) + plot_annotation(tag_levels = 'A')

ggsave("fig6.pdf", width = 18, height = 21, units = "cm")
