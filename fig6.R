library(tidyverse)
library(patchwork)
library(ggrepel)
library(ggalt)

ross_border <- rgb(18, 145, 209, maxColorValue = 255)

dat_fig5 <- readRDS("../google_drive/data_fig5.rds")
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

i <- which(work$ross_sites %in% c("CMID", "CHAL", "BEAU", "INEX"))
work$ross_island <- 1
work$ross_island[i] <- 0

set.seed(4)
g1 <- ggplot(work, aes(x = ip, y = sp_slope_mean, label = ross_sites)) +
  geom_text_repel(size = 2, color= "black", segment.color = "gray60", point.size = 3) +
  geom_linerange(aes(x = ip, ymin = sp_slope_min, ymax = sp_slope_max), color = "gray30") +
  geom_point(aes(shape = as.factor(ross_island)), size = 2.5, col = "black", fill = "white") + 
  geom_point(aes(shape = as.factor(ross_island)), size = 2.5, col = "black", fill = ross_border, alpha = .75) + 
  scale_shape_manual(values = c(22, 21)) + 
  scale_x_continuous(limits = c(.4, .75), breaks = c(.4, .5, .6, .7)) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 7),
    text = element_text(size = 7),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()) +
  labs(x = "Weighted Permutation Entropy (WPE)", y = "Distance Decay (Slope)")

ggsave("fig8temp.pdf", width = 12, height = 8, units = "cm")
# add a text box of a certain font and size and border color
# pull apart pdf and select certain element and copy it
# check and change font size
