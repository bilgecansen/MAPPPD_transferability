
library(tidyverse)
library(patchwork)
library(ggrepel)
library(MCMCvis)

# Install with devtools::install_github("hrbrmstr/ggalt")
library(ggalt)

theme_set(theme_bw())


# Figure 1 ----------------------------------------------------------------

data_fig1 <- readRDS("data_fig1.rds")

N <- data_fig1$N
N2 <- data_fig1$N2
N3 <- data_fig1$N3
N4 <- data_fig1$N4

g1 <- ggplot() +
  geom_line(aes(x = 11:50, y = N), size = 1.5, col = "grey") +
  geom_point(aes(x = 11:50, y = N), size = 3, col = "blue2") +
  labs(y = "N", x = "Time Steps")

g2 <- ggplot() +
  geom_line(aes(x = 11:50, y = N2), size = 1.5, col = "grey") +
  geom_point(aes(x = 11:50, y = N2), size = 3, col = "blue2") +
  labs(y = "N", x = "Time Steps")

g3 <- ggplot() +
  geom_line(aes(x = 11:50, y = N3), size = 1.5, col = "grey") +
  geom_point(aes(x = 11:50, y = N3), size = 3, col = "blue2") +
  labs(y = "N", x = "Time Steps")

g4 <- ggplot() +
  geom_line(aes(x = 11:50, y = N4), size = 1.5, col = "grey") +
  geom_point(aes(x = 11:50, y = N4), size = 3, col = "blue2") +
  labs(y = "N", x = "Time Steps")

(g1 + g2) / (g3 + g4)


# Figure 3 ----------------------------------------------------------------

data_fig3_1 <- readRDS("data_fig3_1.rds")
data_fig3_2 <- readRDS("data_fig3_2.rds")

site_names <- data_fig3_1$site_names
site_names2 <- data_fig3_1$site_names2

rmse1 <- data_fig3_1$rmse1
rmse2 <- data_fig3_1$rmse2
trad_R2 <- data_fig3_1$trad_R2

cor_tt_fore <- data_fig3_2$cor_tt_fore
cor_tt_hind <- data_fig3_2$cor_tt_hind
R2_fore <- round(cor_tt_fore^2, 2)
R2_hind <- round(cor_tt_hind^2, 2)
rmse_fore <- data_fig3_2$rmse_fore
rmse_hind <- data_fig3_2$rmse_hind
rmse_fore_null <- data_fig3_2$rmse_fore_null
rmse_hind_null <- data_fig3_2$rmse_hind_null

g1 <- ggplot() +
  geom_point(mapping = aes(x = site_names, y = trad_R2), col = "darkred", size = 8) +
  geom_segment(aes(y = 0, 
                   x = site_names, 
                   yend = trad_R2, 
                   xend = site_names), 
               color = "darkred") +
  labs(y = "R2") +
  scale_y_continuous(limits = c(0,0.9)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90)) +
  geom_text(aes(x = site_names, y = trad_R2, label = trad_R2), color = "white", size = 2.5)

g2 <- ggplot(mapping = aes(x = rmse1, xend = rmse2, y = site_names2)) + 
  geom_dumbbell(size = 2, color = "#e3e2e1",
                colour_x = "navy", colour_xend = "darkgreen",
                dot_guide = TRUE, dot_guide_size = 0.25) + 
  geom_point(aes(x = rmse1, y = site_names2, colour = "Colony-covariate Model"), size = 3) + 
  geom_point(aes(x = rmse2, y = site_names2, colour = "Null Model"), size = 3) +
  labs(x = "RMSE", y = NULL) +
  scale_color_manual(name = NULL, 
                     values = c("Colony-covariate Model" = "lightblue", 
                                "Null Model" = "darkgreen")) +
  scale_x_continuous(limits = c(0,0.7), breaks = seq(0,5,0.1)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 12))

g3 <- ggplot() +
  geom_point(mapping = aes(x = site_names, y = R2_fore), col = "darkred", size = 8) +
  geom_segment(aes(y = 0, 
                   x = site_names, 
                   yend = R2_fore, 
                   xend = site_names), 
               color = "darkred") +
  labs(y = "Forecast R2") +
  scale_y_continuous(limits = c(0,0.9)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90)) +
  geom_text(aes(x = site_names, y = R2_fore, label = R2_fore), color = "white", size = 2.5)

g4 <- ggplot(mapping = aes(x = rmse_fore, xend = rmse_fore_null, y = site_names2)) + 
  geom_dumbbell(size = 2, color = "#e3e2e1",
                colour_x = "navy", colour_xend = "darkgreen",
                dot_guide = TRUE, dot_guide_size = 0.25) +
  geom_point(aes(x = rmse_fore, y = site_names2, colour = "Forecast with Colony-covariate Model"), size = 3) + 
  geom_point(aes(x = rmse_fore_null, y = site_names2, colour = "Forecast with Null Model"), size = 3) +
  scale_color_manual(name = NULL, 
                     values = c("Forecast with Colony-covariate Model" = "navy", 
                                "Forecast with Null Model" = "darkgreen")) +
  scale_x_continuous(limits = c(0,0.7), breaks = seq(0,5,0.1)) +
  labs(x = "RMSE", y = NULL) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 12))

g5 <- ggplot() +
  geom_point(mapping = aes(x = site_names, y = R2_hind), col = "darkred", size = 8) +
  geom_segment(aes(y = 0, 
                   x = site_names, 
                   yend = R2_hind, 
                   xend = site_names), 
               color = "darkred") +
  labs(y = "Hindcast R2") +
  scale_y_continuous(limits = c(0,0.9)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90)) +
  geom_text(aes(x = site_names, y = R2_hind, label = R2_hind), color = "white", size = 2.5)

g6 <- ggplot(mapping = aes(x = rmse_hind, xend = rmse_hind_null, y = site_names2)) + 
  geom_dumbbell(size = 2, color = "#e3e2e1",
                colour_x = "magenta4", colour_xend = "darkgreen",
                dot_guide = TRUE, dot_guide_size = 0.25) +
  geom_point(aes(x = rmse_hind, y = site_names2, colour = "Hindcast with Colony-covariate Model"), size = 3) + 
  geom_point(aes(x = rmse_hind_null, y = site_names2, colour = "Hindcast with Null Model"), size = 3) +
  scale_color_manual(name = NULL, 
                     values = c("Hindcast with Colony-covariate Model" = "magenta4", 
                                "Hindcast with Null Model" = "darkgreen")) +
  scale_x_continuous(limits = c(0,0.7), breaks = seq(0,5,0.1)) +
  labs(x = "RMSE", y = NULL) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 12))

(g1 + g2) / (g3 + g4) / (g5 + g6)
ggsave("fig3.eps", width = 12, height = 16, units = "in")


# Figure 4 ----------------------------------------------------------------

data_fig4 <- readRDS("data_fig4.rds")

m2 <- data_fig4$m2
idx_N <- data_fig4$idx_N
site_names <- data_fig4$site_names

g1 <- ggplot(mapping = aes(x = m2[1:14,1], y = m2[1:14,2])) +
  geom_point(size = 8, alpha = 0.75, col = "blue2") +
  geom_label_repel(aes(label = site_names[idx_N]), size = 3) + 
  labs(y = "Forecast RMSE", x = "Weighted Permutation Entropy") +
  #scale_color_manual(name = "CCAMLR Regions", 
                     #values = c("48.1" = "blue2", 
                                #"88.1" = "magenta3",
                                #"58.4.1" = "darkgreen",
                                #"58.4.2" = "darkred")) +
  scale_y_continuous(limits = c(0.05, 0.35), breaks = seq(0, 0.3, 0.05)) +
  scale_x_continuous(limits = c(0.40, 0.76), breaks = seq(0, 0.8, 0.1)) +
  theme(legend.position = "bottom",
        title = element_text(size = 18),
        legend.text = element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 16))

g2 <-  ggplot(mapping = aes(x = m2[15:28,1], y = m2[15:28,2])) +
  geom_point(size = 8, alpha = 0.75, col = "blue2") +
  geom_label_repel(aes(label = site_names[idx_N]), size = 3) +
  labs(y = "Hindcast RMSE", x = "Weighted Permutation Entropy") +
  #scale_color_manual(name = "CCAMLR Regions", 
                     #values = c("48.1" = "blue2", 
                                #"88.1" = "magenta3",
                                #"58.4.1" = "darkgreen",
                                #"58.4.2" = "darkred")) +
  scale_y_continuous(limits = c(0.05, 0.35), breaks = seq(0, 0.3, 0.05)) +
  scale_x_continuous(limits = c(0.40, 0.76), breaks = seq(0, 0.8, 0.1)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14),
        title = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_blank(), #element_text(size = 12),
        axis.text.x = element_text(size = 16))

(g1 + g2)  #+
#plot_annotation(tag_levels = 'A') & 
#theme(plot.tag = element_text(size = 20, hjust = 0, vjust = 0))
ggsave("fig4.jpeg", width = 12, height = 12, units = "in")


# Figure 5 ----------------------------------------------------------------

dat_fig5 <- readRDS("data_fig5.rds")

ross_rmse <- dat_fig5$ross_rmse
ip <- dat_fig5$ip
sp_slope_mean <- dat_fig5$sp_slope_mean
sp_slope_min <- dat_fig5$sp_slope_min
sp_slope_max <- dat_fig5$sp_slope_max
g_dat <- dat_fig5$g_dat

ross_sites <- names(g_dat)
ross_sites <- ross_sites[order(ross_sites)]

g_main <- ggplot(mapping = aes(x = ip, y = sp_slope_mean)) +
  geom_segment(mapping = aes(x = ip, xend = ip, y = sp_slope_min, yend = sp_slope_max)) +
  geom_point(size = 5) +
  geom_label_repel(aes(label = ross_sites), size = 3) + 
  labs(x = "WPE", y = "Slope (Spatial RMSE ratio vs distance)",
       title = bquote("Sites in CCAMLR 88.1," ~ rho ~ "= 0.62 (0.06, 0.91)")) +
  theme(axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        plot.title = element_text(size = 20))

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
  
  idx_line <- sample(1:nrow(y_pred1), 2000)
  ind_lines <- y_pred1[idx_line,] %>%
    as.data.frame() %>%
    pivot_longer(cols = everything()) %>%
    mutate(x = rep(x1, times = 2000)) %>%
    mutate(line = rep(1:2000, each = length(x1))) %>%
    select(-name)
  
  ggplot() +
    geom_ribbon(data = mpa_rib, 
                mapping = aes(x = x, ymin = min, ymax = max, group = qq, alpha = qq), fill = "blue") +
    #geom_line(data = ind_lines, mapping = aes(x = x, y = value, group = line), col = "firebrick", alpha = 0.03) +
    geom_point(aes(x = filter(ross_rmse, sites == site)$dist,
                   y = filter(ross_rmse, sites == site)$rmse), 
               col = "darkred", size = 5) +
    theme(legend.position = "none") +
    labs(y = "RMSE Ratio", x = "Distance", title = site) +
    #scale_x_continuous(breaks = seq(1960, 2055, 5)) +
    scale_y_continuous(limits = c(0.5, 3.5)) +
    scale_alpha_continuous(range = 0.07, 0.5)
}


for (i in 1:length(ross_sites)) {
  print(plot_site(ross_sites[i]))
}


# Figure 6 ----------------------------------------------------------------

data_fig6 <- readRDS("data_fig6.rds")

data_pop <- data_fig6$data_pop
data_dem <- data_fig6$data_dem
res_tt_fore <- data_fig6$res_tt_fore
res_tt_hind <- data_fig6$res_tt_hind

plot_r <- function(x, test, title) {
  
  min_season <- filter(data_pop$abundance_initial,  site_id == x)$season
  max_season <- max(filter(data_pop$abundance_nests,  site_id == x)$season) - 1
  all_season <- min_season:max_season
  #idx_season <- all_season - min_season + 1
 
  r <- data_dem[x][[1]]
  
  mu_pred_fore <- MCMCsummary(res_tt_fore[x][[1]], params = "mu_pred")[,1]
  mu_pred_hind <- MCMCsummary(res_tt_hind[x][[1]], params = "mu_pred")[,1]
  
  theme_set(theme_bw())
  g_main <- ggplot() +
    #geom_line(data = dat, mapping = aes(x = season, y = r), size = 2, col = "grey", alpha = 0.5) +
    #geom_point(data = dat, mapping = aes(x = season, y = r), col = "red", size = 5) +
    labs(y = "Population Growth", title = title) + 
    theme(axis.title.x = element_blank(),
           legend.position = "none",
           axis.title.y = element_text(size = 10),
           axis.text.y = element_text(size = 6),
           axis.text.x = element_text(size = 6),
           panel.grid.minor = element_blank(), 
           title = element_text(size = 10),
           panel.border = element_blank()) +
    scale_x_continuous(breaks = seq(1980, 2020, 5)) +
    scale_y_continuous(limits = c(-0.9, 0.9))
  
  if (test == "hind") {
    g <- g_main +
      geom_line(aes(x = min_season:max_season, y = r), size = 1, 
                col = "grey", alpha = 0.5) +
      geom_line(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), size = 1, 
                col = "black", alpha = 0.5) +
      geom_point(aes(x = (min_season + length(mu_pred_hind)):max_season, 
                     y = r[(length(mu_pred_hind)+1):length(all_season)]), 
                 fill = "darkgreen", size = 2, col = "black", alpha = 0.8, pch = 21) +
      geom_point(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), 
                     y = r[1:length(mu_pred_hind)]), 
                 fill = "orange", size = 2, col = "black", alpha = 0.8, pch = 21) +
      geom_point(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), 
                 fill = "lightblue", size = 2, col = "black", alpha = 0.8, pch = 21)
  }
  
  if (test == "fore") {
    g <- g_main + 
      geom_line(aes(x = min_season:max_season, y = r), size = 1, 
                col = "grey", alpha = 0.5) +
      geom_line(aes(x = (max_season - length(mu_pred_fore) + 1):max_season, y = mu_pred_fore), size = 1, 
                col = "black", alpha = 0.5) +
      geom_point(aes(x = min_season:(max_season - length(mu_pred_fore)), 
                     y = r[1:(length(all_season) - length(mu_pred_fore))]), 
                 fill = "darkgreen", size = 2, col = "black", alpha = 0.8, pch = 21) +
      geom_point(aes(x = (max_season - length(mu_pred_fore) + 1):max_season, 
                     y = r[(length(all_season) - length(mu_pred_fore) + 1):length(all_season)]), 
                 fill = "orange", size = 2, col = "black", alpha = 0.8, pch = 21) +
      geom_point(aes(x = (max_season - length(mu_pred_fore) + 1):max_season, y = mu_pred_fore), 
                 fill = "lightblue", size = 2, col = "black", alpha = 0.8, pch = 21)
      
  }
  
  g
    
}

plot_r("BRDN", test = "fore", title = "Cape Bird North")
plot_r("PGEO", test = "fore", title = "Point Geologié")
plot_r("CRZE", test = "fore", title = "Cape Crozier East")
plot_r("CRZE", test = "hind", title = "Cape Crozier East")
