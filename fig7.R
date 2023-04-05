library(tidyverse)
library(patchwork)
library(ggrepel)
library(ggalt)
library(MCMCvis)
library(RColorBrewer)

t_size <- 2

data_fig6 <- readRDS("../google_drive/data_fig6.rds")

data_pop <- data_fig6$data_pop
data_dem <- data_fig6$data_dem
res_tt_fore <- data_fig6$res_tt_fore
res_tt_hind <- data_fig6$res_tt_hind

plot_r <- function(x, test) {
  
  min_season <- filter(data_pop$abundance_initial,  site_id == x)$season
  max_season <- max(filter(data_pop$abundance_nests,  site_id == x)$season) - 1
  all_season <- min_season:max_season
  #idx_season <- all_season - min_season + 1
  
  r <- data_dem[x][[1]]
  
  mu_pred_fore <- MCMCsummary(res_tt_fore[x][[1]], params = "mu_pred")[,1]
  mu_pred_hind <- MCMCsummary(res_tt_hind[x][[1]], params = "mu_pred")[,1]
  
  #theme_set(theme_bw())
  g_main <- ggplot() +
    labs(y= " ") + 
    theme_minimal(base_family = "Helvetica") +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 9),
      text = element_text(size = 9),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()) +  
    scale_x_continuous(limits = c(1982, 2017), breaks = seq(1985, 2015, 10)) +
    scale_y_continuous(limits = c(-0.9, 0.9), breaks = c(-.5, 0, .5))
  
  if (test == "hind") {
    g <- g_main +
      geom_line(aes(x = min_season:max_season, y = r), size = .75, col = "gray70") +
      geom_line(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), size = .75, col = "gray40") +
      geom_point(aes(x = (min_season + length(mu_pred_hind)):max_season, 
                     y = r[(length(mu_pred_hind)+1):length(all_season)]), 
                 fill = "#F04393", size = t_size, pch = 21, col = "black") +
      geom_point(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), 
                     y = r[1:length(mu_pred_hind)]), 
                 fill = "#240E8B", size = t_size, pch = 21, col = "black") +
      geom_point(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), 
                 fill = "#F9C449", size = t_size, pch = 21, col = "black")
  }
  
  if (test == "fore") {
    g <- g_main + 
      geom_line(aes(x = min_season:max_season, y = r), size = .75, col = "gray70") +
      geom_line(aes(x = (max_season - length(mu_pred_fore) + 1):max_season, y = mu_pred_fore), size = .75, col = "gray40") +
      geom_point(aes(x = min_season:(max_season - length(mu_pred_fore)), 
                     y = r[1:(length(all_season) - length(mu_pred_fore))]), 
                 fill = "#F04393", size = t_size, pch = 21, col = "black") +
      geom_point(aes(x = (max_season - length(mu_pred_fore) + 1):max_season, 
                     y = r[(length(all_season) - length(mu_pred_fore) + 1):length(all_season)]), 
                 fill = "#240E8B", size = t_size, pch = 21, col = "black") +
      geom_point(aes(x = (max_season - length(mu_pred_fore) + 1):max_season, y = mu_pred_fore), 
                 fill = "#F9C449", size = t_size, pch = 21, col = "black")
      

  }
  
  g
  
}

#p1 <- plot_r("BRDN", test = "fore") + labs(title = "BRDN Forecast") 
#p2 <- plot_r("PGEO", test = "fore") + labs(title = "PGEO Forecast")
p1 <- plot_r("CRZE", test = "fore") + labs(title = "CRZE Forecast")
p2 <- plot_r("CRZE", test = "hind") + labs(title = "CRZE Hindcast")
p1 <- p1 + theme(axis.title.x = element_blank())
p2 <- p2 + theme(axis.title.x = element_blank())
#p3 <- p3 + theme(axis.title.x = element_blank())
p2 <- p2 + labs(x = "Year")
(p1 + p2) / (p3 + p4) + plot_annotation(tag_levels = 'A')

p1 / p2  + labs(x = "Year") + plot_annotation(tag_levels = 'A')
ggsave("fig5new.pdf", width = 15, height = 15, units = "cm")

