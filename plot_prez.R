
library(tidyverse)
library(MCMCvis)
library(foreach)

data_pop <- readRDS("data_pop_adpe.rds")
data_dem <- readRDS("data_dem_adpe.rds")

plot_r <- function(x) {
  
  min_season <- filter(data_pop$abundance_initial,  site_id == x)$season
  max_season <- max(filter(data_pop$abundance_nests,  site_id == x)$season)
  all_season <- min_season:max_season
  dat_season <- c(filter(data_pop$abundance_initial, site_id == x)$season, 
                  filter(data_pop$abundance_nests, site_id == x)$season)
  
  idx_season <- as.numeric(all_season %in% dat_season)
  idx_season2 <- idx_season[1:(length(idx_season)-1)] + idx_season[2:length(idx_season)]
  
  dat <- data.frame(r = data_dem$r[x][[1]],
                    season = min_season:(max_season-1),
                    source = as.factor(idx_season2))
  
  mu_pred_fore <- MCMCsummary(res_tt$fore[x][[1]], params = "mu_pred")[,1]
  mu_pred_hind <- MCMCsummary(res_tt$hind[x][[1]], params = "mu_pred")[,1]

  theme_set(theme_bw())
  ggplot() +
    geom_line(data = dat, mapping = aes(x = season, y = r), size = 2, col = "grey", alpha = 0.5) +
    geom_point(data = dat, mapping = aes(x = season, y = r), col = "red", size = 5) +
    labs(y = "Population Growth", title = x) + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16)) +
    scale_x_continuous(breaks = seq(1980, 2020, 5)) +
    scale_y_continuous(limits = c(-0.9, 0.9)) +
    geom_point(aes(x = (max_season - length(mu_pred_fore)):(max_season-1), y = mu_pred_fore), 
               col = "darkblue", size = 5) +
    geom_line(aes(x = (max_season - length(mu_pred_fore)):(max_season-1), y = mu_pred_fore), size = 2, 
              col = "black", alpha = 0.5) +
    geom_point(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), 
               col = "darkblue", size = 5) +
    geom_line(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), size = 2, 
              col = "black", alpha = 0.5)
}

plot_r("BRDN")
plot_r("PGEO")
plot_r("CRZE")

ggsave("efi_plot1.jpeg", width = 10, height = 8, units = "in")

min_season <- filter(data_pop$abundance_initial,  site_id == "PGEO")$season
max_season <- max(filter(data_pop$abundance_nests,  site_id == "PGEO")$season)
all_season <- min_season:max_season
dat_season <- c(filter(data_pop$abundance_initial, site_id == "PGEO")$season, 
                filter(data_pop$abundance_nests, site_id == "PGEO")$season)

idx_season <- as.numeric(all_season %in% dat_season)
idx_season2 <- idx_season[1:(length(idx_season)-1)] + idx_season[2:length(idx_season)]

dat <- data.frame(r = data_dem$r["PGEO"][[1]],
                  season = min_season:(max_season-1),
                  source = as.factor(idx_season2))

mu_pred <- MCMCsummary(res_tt$fore$PGEO, params = "mu_pred")[,1]

theme_set(theme_bw())
ggplot() +
  geom_line(data = dat, mapping = aes(x = season, y = r), size = 2, col = "grey", alpha = 0.5) +
  geom_point(data = dat, mapping = aes(x = season, y = r, col = source), size = 5) +
  labs(y = "Population Growth", title = "PGEO") + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16)) +
  scale_color_manual(name = NULL, 
                     values = c("2" = "red", 
                                "1" = "pink",
                                "0" = "grey")) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(limits = c(-0.65, 0.51)) +
  geom_point(aes(x = 2007:2016, y = mu_pred), col = "darkblue", size = 5) +
  geom_line(aes(x = 2007:2016, y = mu_pred), size = 2, col = "black", alpha = 0.5)

ggsave("efi_plot2.jpeg", width = 10, height = 8, units = "in")

min_season <- filter(data_pop$abundance_initial,  site_id == "CRZE")$season
max_season <- max(filter(data_pop$abundance_nests,  site_id == "CRZE")$season)
all_season <- min_season:max_season
dat_season <- c(filter(data_pop$abundance_initial, site_id == "CRZE")$season, 
                filter(data_pop$abundance_nests, site_id == "CRZE")$season)

idx_season <- as.numeric(all_season %in% dat_season)
idx_season2 <- idx_season[1:(length(idx_season)-1)] + idx_season[2:length(idx_season)]

dat <- data.frame(r = data_dem$r["CRZE"][[1]],
                  season = min_season:(max_season-1),
                  source = as.factor(idx_season2))

mu_pred_fore <- MCMCsummary(res_tt$fore$CRZE, params = "mu_pred")[,1]
mu_pred_hind <- MCMCsummary(res_tt$hind$CRZE, params = "mu_pred")[,1]

theme_set(theme_bw())
ggplot() +
  geom_line(data = dat, mapping = aes(x = season, y = r), size = 2, col = "grey", alpha = 0.5) +
  geom_point(data = dat, mapping = aes(x = season, y = r, col = source), size = 5) +
  labs(y = "Population Growth", title = "CRZE") + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16)) +
  scale_color_manual(name = NULL, 
                     values = c("2" = "red", 
                                "1" = "pink",
                                "0" = "grey")) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  #scale_y_continuous(limits = c(-0.65, 0.51)) +
  geom_point(aes(x = (max_season - length(mu_pred_fore)):2017, y = mu_pred_fore), 
             col = "darkblue", size = 5) +
  geom_line(aes(x = (max_season - length(mu_pred_fore)):2017, y = mu_pred_fore), size = 2, 
            col = "black", alpha = 0.5) +
  geom_point(aes(x = 1985:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), 
             col = "darkblue", size = 5) +
  geom_line(aes(x = 1985:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), size = 2, 
            col = "black", alpha = 0.5)


min_season <- filter(data_pop$abundance_initial,  site_id == "BRDS")$season
max_season <- max(filter(data_pop$abundance_nests,  site_id == "BRDS")$season)
all_season <- min_season:max_season
dat_season <- c(filter(data_pop$abundance_initial, site_id == "BRDS")$season, 
                filter(data_pop$abundance_nests, site_id == "BRDS")$season)

idx_season <- as.numeric(all_season %in% dat_season)
idx_season2 <- idx_season[1:(length(idx_season)-1)] + idx_season[2:length(idx_season)]

dat <- data.frame(r = data_dem$r["BRDS"][[1]],
                  season = min_season:(max_season-1),
                  source = as.factor(idx_season2))

mu_pred_fore <- MCMCsummary(res_tt$fore$BRDS, params = "mu_pred")[,1]
mu_pred_hind <- MCMCsummary(res_tt$hind$BRDS, params = "mu_pred")[,1]


theme_set(theme_bw())
ggplot() +
  geom_line(data = dat, mapping = aes(x = season, y = r), size = 2, col = "grey", alpha = 0.5) +
  geom_point(data = dat, mapping = aes(x = season, y = r, col = source), size = 5) +
  labs(y = "Population Growth", title = "BRDS") + 
  theme(axis.title.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16)) +
  scale_color_manual(name = NULL, 
                     values = c("2" = "red", 
                                "1" = "pink",
                                "0" = "grey")) +
  scale_x_continuous(breaks = seq(1980, 2020, 5)) +
  #scale_y_continuous(limits = c(-0.65, 0.51)) +
  geom_point(aes(x = 2008:2018, y = mu_pred_fore), col = "darkblue", size = 5) +
  geom_line(aes(x = 2008:2018, y = mu_pred_fore), size = 2, col = "black", alpha = 0.5) +
  geom_point(aes(x = 1983:1992, y = mu_pred_hind), col = "darkblue", size = 5) +
  geom_line(aes(x = 1983:1992, y = mu_pred_hind), size = 2, col = "black", alpha = 0.5)

world <- map_data("world")

z <- readRDS("data_fig6.rds")

adelie_sites <- read.csv("adeliesites_acbrs.csv") #%>%
  #filter(site_id %in% data_pop$site_list$site_id)

adelie_sites2 <- adelie_sites %>%
  filter(site_id %in% sites[idx_N])
adelie_sites2$int_pr <- z$m2[1:14,1]
adelie_sites2$rmse <- z$m2[1:14,2]

ggplot() + 
  geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_path(data = world, mapping = aes(x = long, y = lat, group = group)) + 
  scale_y_continuous(breaks = (-2:2) * 30) + 
  scale_x_continuous(breaks = (-4:4) * 45) + 
  coord_map("ortho", orientation = c(-90, 0, 0), ylim = c(-85, -60), xlim = c(-200, -210))  + 
  geom_point(data = adelie_sites2, 
             mapping = aes(x = longitude, y = latitude, col = int_pr), 
             size = 5) +#, #col = "darkblue") + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.title = element_blank()) +
  scale_colour_gradientn(colours = hcl.colors(100, palette = "GnBu", rev = T, alpha = 0.8)[30:100])

ggsave("prez_map1.jpeg", width = 6, height = 12, units = "in")

ggplot() + 
  geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_path(data = world, mapping = aes(x = long, y = lat, group = group)) + 
  scale_y_continuous(breaks = (-2:2) * 30) + 
  scale_x_continuous(breaks = (-4:4) * 45) + 
  coord_map("ortho", orientation = c(-90, 0, 0), ylim = c(-85, -60), xlim = c(-200, -210))  + 
  geom_point(data = adelie_sites2, 
             mapping = aes(x = longitude, y = latitude, col = rmse), 
             size = 5) +#, #col = "darkblue") + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.title = element_blank()) +
  scale_colour_gradientn(colours = hcl.colors(100, palette = "GnBu", rev = T, alpha = 0.8)[30:100])

ggsave("prez_map2.jpeg", width = 6, height = 12, units = "in")

ggplot() + 
  geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_path(data = world, mapping = aes(x = long, y = lat, group = group)) + 
  scale_y_continuous(breaks = (-2:2) * 30) + 
  scale_x_continuous(breaks = (-4:4) * 45) + 
  coord_map("ortho", orientation = c(-90, 0, 0), ylim = c(-90, -60))  + 
  geom_point(data = filter(adelie_sites, site_id %in% c("BRDN", "PGEO")), 
             mapping = aes(x = longitude, y = latitude), 
             size = 10, col = "darkblue") + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.title = element_blank())

ggsave("prez_map2.jpeg", width = 12, height = 12, units = "in")

ggplot() + 
  geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), color = "black", fill = "white") + 
  geom_path(data = world, mapping = aes(x = long, y = lat, group = group)) + 
  scale_y_continuous(breaks = (-2:2) * 30) + 
  scale_x_continuous(breaks = (-4:4) * 45) + 
  coord_map("ortho", orientation = c(-90, 0, 0), ylim = c(-90, -60))  + 
  geom_point(data = filter(adelie_sites, site_id %in% c("CROZ")), 
             mapping = aes(x = longitude, y = latitude), 
             size = 10, col = "darkblue") + 
  theme(axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank(),
        legend.title = element_blank())

ggsave("prez_map3.jpeg", width = 12, height = 12, units = "in")


min_season <- filter(data_pop$abundance_initial,  site_id == "CRZE")$season
max_season <- max(filter(data_pop$abundance_nests,  site_id == "CRZE")$season) - 1
all_season <- min_season:max_season

r <- data_dem["CRZE"][[1]]
mu_pred_hind <- MCMCsummary(res_tt_hind["CRZE"][[1]], params = "mu_pred")[,1]

ggplot() + 
  geom_line(aes(x = min_season:max_season, y = r), size = 2, 
            col = "grey", alpha = 0.5) +
  geom_line(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), size = 2, 
            col = "black", alpha = 0.5) +
  geom_point(aes(x = (min_season + length(mu_pred_hind):max_season), 
                 y = r[(length(mu_pred_hind)+1):length(all_season)]), 
             col = "darkgreen", size = 5) #+
  geom_point(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), 
                 y = r[1:length(mu_pred_hind)]), 
             col = "orange", size = 5) +
  geom_point(aes(x = min_season:(min_season + length(mu_pred_hind) - 1), y = mu_pred_hind), 
             col = "darkblue", size = 5)

N_all2 <- foreach(i = 1:n_sites) %do% {
  MCMCsummary(res_null, params = params_N[[i]], ISB = F, exact = T)[,c(1,3,5)]
}


params_r <- 
  foreach(i = 1:n_sites) %:% 
  foreach(h = 1:(n_seasons-1), .combine = "c") %do%
  paste("r", "[", i, ",", h, "]", sep = "")

r_all2 <- foreach(i = 1:n_sites) %do% {
  MCMCsummary(res_null, params = params_N[[i]], ISB = F, exact = T)[,c(1,3,5)]
}

names(season_relative) <- data_pop$site_list$site_id
sites <- data_pop$site_list$site_id
n_sites <- length(sites)  

season_relative <- foreach(i = 1:n_sites) %do% {
  min_year <- data_pop$abundance_initial$season_relative[i]
  max_year <- max(filter(data_pop$abundance_nests, site_id == sites[i])$season_relative)
  
  c(min_year, max_year)
}

plot_traj_N <- function(x, ymin, ymax, title, ci = F) {
    min_season <- filter(data_pop$abundance_initial,  site_id == x)$season
    max_season <- max(filter(data_pop$abundance_nests,  site_id == x)$season)
    all_season <- min_season:max_season
    dat_season <- c(filter(data_pop$abundance_initial, site_id == x)$season, 
                    filter(data_pop$abundance_nests, site_id == x)$season)
    
    idx_season <- season_relative[[which(sites == x)]]
    idx_season <- idx_season[1]:idx_season[2]
    
    dat <- data.frame(N_mean = N_all2[[which(sites == x)]][idx_season, 1],
                      N_low = N_all2[[which(sites == x)]][idx_season, 2],
                      N_high =  N_all2[[which(sites == x)]][idx_season, 3],
                      season = min_season:max_season)
    #source = as.factor(idx_season2))
    
    theme_set(theme_bw())
    g <- ggplot() +
      #geom_segment(mapping = aes(y = dat$N_low, yend = dat$N_high, x = dat$season, xend = dat$season),
                   #col = "darkblue") +
      geom_line(mapping = aes(x = dat$season, y = dat$N_mean), size = 1, col = "grey", alpha = 0.5) +
      geom_point(mapping = aes(x = dat$season, y = dat$N_mean), pch = 21, col = "black", size = 2, alpha = 0.75, 
                 fill = "darkblue") +
      #geom_point(mapping = aes(x = counts$season, y = counts$count), size = 3, col = "red") +
      labs(y = "Abundance", title = title) + 
      theme(axis.title.x = element_blank(),
            legend.position = "none",
            axis.title.y = element_text(size = 10),
            axis.text.y = element_text(size = 6),
            axis.text.x = element_text(size = 6),
            panel.grid.minor = element_blank(), 
            title = element_text(size = 10),
            panel.border = element_blank()) +
      scale_x_continuous(breaks = seq(1980, 2020, 5)) +
      scale_y_continuous(limits = c(ymin, ymax))
    
    if (ci == T) g <- g + geom_segment(mapping = aes(y = dat$N_low, yend = dat$N_high, x = dat$season, xend = dat$season),
                                  col = "darkblue")
    
    g
    
}

plot_traj_N("BRDN", ymin = 9.5, ymax = 11.01, title = "Cape Bird North", ci = T)
ggsave("prez_traj_N1.jpeg", width = 12, height = 8, units = "cm")

plot_traj_N("PGEO", ymin = 9.5, ymax = 11.01,  title = "Point Geologié", ci = T)
ggsave("prez_traj_N2.jpeg", width = 12, height = 8, units = "cm")

plot_traj_N("BRDN", ymin = 9.5, ymax = 11.01, title = "Cape Bird North", ci = F)
ggsave("prez_traj_N3.jpeg", width = 12, height = 8, units = "cm")

plot_traj_N("PGEO", ymin = 9.5, ymax = 11.01,  title = "Point Geologié", ci = F)
ggsave("prez_traj_N4.jpeg", width = 12, height = 8, units = "cm")

plot_traj_r <- function(x, title) {
  min_season <- filter(data_pop$abundance_initial,  site_id == x)$season
  max_season <- max(filter(data_pop$abundance_nests,  site_id == x)$season)
  all_season <- min_season:max_season
  dat_season <- c(filter(data_pop$abundance_initial, site_id == x)$season, 
                  filter(data_pop$abundance_nests, site_id == x)$season)
  
  idx_season <- as.numeric(all_season %in% dat_season)
  idx_season2 <- idx_season[1:(length(idx_season)-1)] + idx_season[2:length(idx_season)]
  
  dat <- data.frame(r = data_dem$r[x][[1]],
                    season = min_season:(max_season-1),
                    source = as.factor(idx_season2))
  
  theme_set(theme_bw())
  ggplot(dat, aes(x = season, y = r)) +
    geom_line(size = 1, col = "grey", alpha = 0.5) +
    geom_point(col = "black", fill = "darkred", pch = 21, size = 2, alpha = 0.8) +
    labs(y = "Population Growth", title = title) + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          axis.title.y = element_text(size = 10),
          axis.text.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          panel.grid.minor = element_blank(), 
          title = element_text(size = 10),
          panel.border = element_blank()) +
    scale_x_continuous(breaks = seq(1980, 2020, 5))
}

plot_traj_r("BRDN", title = "Cape Bird North")
ggsave("prez_traj_r1.jpeg", width = 12, height = 8, units = "cm")

plot_traj_r("PGEO", title = "Point Geologié")
ggsave("prez_traj_r2.jpeg", width = 12, height = 8, units = "cm")

var_plots <- readRDS("plots_var.rds")
var_plots$PGEO$g +
  labs(title = "PGEO") +
  theme(axis.title.y = element_text(size = 24),
        axis.text.y = element_text(size = 20),
        title = element_text(size = 24))

ggsave("prez_vars_PGEO.jpeg", width = 12, height = 8, units = "in")

