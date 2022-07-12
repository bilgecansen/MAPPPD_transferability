
library(foreach)
library(tidyverse)
library(rstan)
library(MCMCvis)

data_pop <- readRDS("data_pop_adpe.rds")
data_stan_null <- readRDS("data_stan_null_adpe.rds")


# Calculate long-term ice area average ------------------------------------

sites <- data_pop$site_list$site_id

ice <- readRDS("data_forced_finn_500km_adpe.rds") %>%
  filter(season > 1978) %>%
  filter(site_id %in% sites) %>%
  select(site_id, season, aice) %>%
  group_by(site_id) %>%
  summarise(aice_avg = mean(aice))

aice_std <- (ice$aice_avg - mean(ice$aice_avg))/sd(ice$aice_avg)
data_stan_null$ice <- cbind(aice_std, aice_std^2)

# Run Stan model ----------------------------------------------------------

options(mc.cores = parallel::detectCores())

res_null <- stan(file = 'null_pop_v7.stan', 
                 data = data_stan_null,
                 iter = 4000,
                 control = list(adapt_delta = 0.99, 
                                max_treedepth = 15))

saveRDS(res_null, "results_null_pop_adpe.rds")


# Extract demographic parameters ------------------------------------------

n_sites <- data_stan_null$n_sites
n_seasons <- data_stan_null$n_seasons
sites <- data_pop$site_list$site_id

params_r <- 
  foreach(i = 1:n_sites) %:% 
    foreach(h = 1:(n_seasons-1), .combine = "c") %do%
      paste("r", "[", i, ",", h, "]", sep = "")

params_N <- 
  foreach(i = 1:n_sites) %:% 
    foreach(h = 1:n_seasons, .combine = "c") %do%
      paste("lz", "[", i, ",", h, "]", sep = "")

r_all <- foreach(i = 1:n_sites) %do% {
  MCMCsummary(res_null, params = params_r[[i]], ISB = F, exact = T)[,1]
}

N_all <- foreach(i = 1:n_sites) %do% {
  exp(MCMCsummary(res_null, params = params_N[[i]], ISB = F, exact = T)[,1])
}

season_relative <- foreach(i = 1:n_sites) %do% {
  min_year <- data_pop$abundance_initial$season_relative[i]
  max_year <- max(filter(data_pop$abundance_nests, site_id == sites[i])$season_relative)
  
  c(min_year, max_year)
}

names(season_relative) <- data_pop$site_list$site_id

seasons <-  foreach(i = 1:n_sites) %do% {
  min_year <- data_pop$abundance_initial$season[i]
  max_year <- max(filter(data_pop$abundance_nests, site_id == sites[i])$season)
  
  c(min_year, max_year)
}

names(seasons) <- data_pop$site_list$site_id

# subset model results with years with data
r_all <- foreach(i = 1:n_sites) %do% {
  r_all[[i]][season_relative[[i]][1]:(season_relative[[i]][2]-1)]
}

names(r_all) <- data_pop$site_list$site_id

N_all <- foreach(i = 1:n_sites) %do% {
  N_all[[i]][season_relative[[i]][1]:(season_relative[[i]][2])]
}

names(N_all) <- data_pop$site_list$site_id

data_dem <- list(r = r_all,
                 N = N_all,
                 seasons = seasons,
                 season_relative = season_relative)

saveRDS(data_dem, "data_dem_adpe.rds")


# Population trajectory plots ---------------------------------------------

# Growth trajectories
plot_traj_r <- function(x) {
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
    geom_line(size = 2, col = "grey", alpha = 0.5) +
    geom_point(aes(col = source), size = 5) +
    labs(y = "Population Growth", title = x) + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16)) +
    scale_color_manual(name = NULL, 
                       values = c("2" = "red", 
                                  "1" = "pink",
                                  "0" = "grey")) +
    scale_x_continuous(breaks = seq(1980, 2020, 5))
}

if (!any(list.files() == "trajectories")) dir.create("trajectories")
for (i in 1:length(sites)) {
  print(plot_traj_r(sites[i]))
  filename <- paste("trajectories/plot_", sites[i], "_traj_r.jpeg", sep = "")
  ggsave(filename, width = 10, height = 8, units = "in")
}

# save time series
data_ts <- foreach (i = 1:length(sites)) %do% {
  
  min_season <- filter(data_pop$abundance_initial,  site_id == sites[i])$season
  max_season <- max(filter(data_pop$abundance_nests,  site_id == sites[i])$season)
  all_season <- min_season:max_season
  dat_season <- c(filter(data_pop$abundance_initial, site_id == sites[i])$season, 
                  filter(data_pop$abundance_nests, site_id == sites[i])$season)
  
  idx_season <- as.numeric(all_season %in% dat_season)
  idx_season2 <- idx_season[1:(length(idx_season)-1)] + idx_season[2:length(idx_season)]
  
  dat <- data.frame(r = data_dem$r[sites[i]][[1]],
                    season = min_season:(max_season-1),
                    source = as.factor(idx_season2))
  
  return(dat)
}

names(data_ts) <- sites

saveRDS(data_ts, "data_ts.rds")

# Abundance trajectories
N_all2 <- foreach(i = 1:n_sites) %do% {
  MCMCsummary(res_null, params = params_N[[i]], ISB = F, exact = T)[,c(1,3,5)] %>%
    exp()
}

plot_traj_N <- function(x) {
  min_season <- filter(data_pop$abundance_initial,  site_id == x)$season
  max_season <- max(filter(data_pop$abundance_nests,  site_id == x)$season)
  all_season <- min_season:max_season
  dat_season <- c(filter(data_pop$abundance_initial, site_id == x)$season, 
                  filter(data_pop$abundance_nests, site_id == x)$season)
  
  idx_season <- season_relative[[which(sites == x)]]
  idx_season <- idx_season[1]:idx_season[2]
  
  counts <- rbind(filter(data_pop$abundance_initial, site_id == x), 
                  filter(data_pop$abundance_nests, site_id == x))
  
  dat <- data.frame(N_mean = N_all2[[which(sites == x)]][idx_season, 1],
                    N_low = N_all2[[which(sites == x)]][idx_season, 2],
                    N_high =  N_all2[[which(sites == x)]][idx_season, 3],
                    season = min_season:max_season)
                    #source = as.factor(idx_season2))
  
  theme_set(theme_bw())
  ggplot() +
    geom_segment(mapping = aes(y = dat$N_low, yend = dat$N_high, x = dat$season, xend = dat$season),
                 col = "darkblue") +
    geom_line(mapping = aes(x = dat$season, y = dat$N_mean), size = 2, col = "grey", alpha = 0.5) +
    geom_point(mapping = aes(x = dat$season, y = dat$N_mean), size = 5, alpha = 0.75, col = "darkblue") +
    geom_point(mapping = aes(x = counts$season, y = counts$count), size = 3, col = "red") +
    labs(y = "Abundance", title = x) + 
    theme(axis.title.x = element_blank(),
          legend.position = "none",
          axis.title.y = element_text(size = 20),
          axis.text.y = element_text(size = 16),
          axis.text.x = element_text(size = 16)) +
    scale_x_continuous(breaks = seq(1980, 2020, 5))
}

for (i in 1:length(sites)) {
  print(plot_traj_N(sites[i]))
  filename <- paste("trajectories/plot_", sites[i], "_traj_N.jpeg", sep = "")
  ggsave(filename, width = 10, height = 8, units = "in")
}
