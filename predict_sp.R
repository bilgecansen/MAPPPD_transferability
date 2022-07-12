
library(foreach)
library(tidyverse)
library(rstan)
library(MCMCvis)
library(scales)
library(mapppdr)
library(geosphere)

# Install with devtools::install_github("thomasp85/patchwork")
library(patchwork)

# Install with devtools::install_github("hrbrmstr/ggalt")
library(ggalt)

res_lm <- readRDS("results_lm.rds")
var_plots <- readRDS("plots_var.rds")
data_pop <- readRDS("data_pop_adpe.rds")
data_pop_all <- readRDS("data_pop_all_adpe.rds")
data_dem <- readRDS("data_dem_adpe.rds")
sites <- data_pop$site_list$site_id

# env data
#env_1500 <- readRDS("data_forced_std_finn_1500km_adpe.rds") %>%
  #filter(site_id %in% data_pop$site_list$site_id) %>%
  #filter(season %in% data_pop$seasons) %>%
  #rename_with(function(x) paste(x, "_1500", sep = ""), .cols = !(contains("site_id") | contains("season")))

env <- readRDS("data_forced_std_finn_500km_adpe.rds") %>%
  filter(site_id %in% data_pop$site_list$site_id) %>%
  filter(season %in% data_pop$seasons) %>%
  rename_with(function(x) paste(x, "_500", sep = ""), .cols = !(contains("site_id") | contains("season")))

#env <- bind_cols(env_1500, env_500[,c(-1,-2)])

# Data for Stan
data_stan <- foreach (i = 1:length(sites)) %do% {
  
  env_site <- filter(env, site_id == sites[i]) %>% 
    filter(season >= data_dem$seasons[[i]][1] & season < data_dem$seasons[[i]][2]) %>%
    select(-site_id, -season)
  
  dat <- list(y = data_dem$r[[i]], 
              X = env_site,
              N = nrow(env_site),
              M = ncol(env_site))
  
  return(dat)
  
}

names(data_stan) <- data_pop$site_list$site_id

data_stan_lm <- foreach (i = 1:length(data_stan)) %do% {
  
  dat <- list(y = data_stan[[i]]$y, 
              X = data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]],
              X_pred = data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]],
              N = nrow(data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]]),
              L = nrow(data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]]),
              M = 3)

  return(dat)
  
}

names(data_stan_lm) <- data_pop$site_list$site_id

# Spatial predictability --------------------------------------------------

sites_ross_model <- c("ROYD", "CHAL", "CRZE", "CRZW", "BEAU", "BRDS", "CMID", "BRDN", "INEX",  "BRDM", "FRAE")

predict_r <- function(res, env, pred_site) {
  
  model_site <- names(res)
  model <- res[[1]]
  
  beta <- MCMCchains(model, params = "beta")
  alpha <- MCMCchains(model, params = "alpha")
  
  years <- data_dem$seasons[pred_site][[1]][1]:(data_dem$seasons[pred_site][[1]][2]-1)
  
  env <- filter(env, site_id == pred_site) %>% 
    filter(season %in% years) %>%
    select(ends_with(var_plots[model_site][[1]]$vars[1:3])) %>%
    as.matrix()
  
  r <- foreach(i = 1:nrow(env), .combine = "cbind") %do% {
    alpha + beta %*% env[i,]
  }
  colnames(r) <- 1:nrow(env)
  
  r_mean <- apply(r, 2, mean)
  r_obs <- data_stan_lm[pred_site][[1]]$y
  rmse_null <- sqrt(sum((mean(r_mean) - r_obs)^2)/length(r_obs))
  rmse <- sqrt(sum((r_mean - r_obs)^2)/length(r_obs))/rmse_null
  cor_r <- round(cor(r_mean, r_obs),2)
  
  g <- ggplot() + 
    geom_line(mapping = aes(x = years, y = r_mean), col = "darkblue") +
    geom_point(mapping = aes(x = years, y = r_mean), col = "darkblue", size = 3) +
    geom_line(aes(x = years , y = r_obs), col = "darkred") +
    geom_point(aes(x = years , y = r_obs), col = "darkred", size = 3) +
    labs(title = paste(paste(model_site, "predicts", pred_site, sep = " "), paste("(", cor_r, ")"), sep = " "),
         x = "Years", y = "Growth Rate")
  
  results <- list(r_mean = r_mean,
                  r_obs = r_obs,
                  years = years,
                  rmse = rmse,
                  cor_r = cor_r,
                  g = g)
  
  return(results)
  
}

sp_pred_ross <- 
  foreach(i = 1:length(sites_ross_model)) %:%
  foreach(h = 1:length(sites_ross_model)) %do% {
    predict_r(res = res_lm[sites_ross_model[i]], env, sites_ross_model[h])
  }

names(sp_pred_ross) <- sites_ross_model

sp_rmse_ross <- map(sp_pred_ross, function(x) map_dbl(x, function(y) y$rmse))
sp_rmse_ross <- map2(sp_rmse_ross, 1:length(sp_rmse_ross), function(x,y) x[-y])

sp_cor_ross <- map(sp_pred_ross, function(x) map_dbl(x, function(y) y$cor_r))
sp_cor_ross <- map2(sp_cor_ross, 1:length(sp_cor_ross), function(x,y) x[-y])

# Site distance calculations
all_sites <- mapppdr::sites

site_dist_ross <- 
  foreach(i = 1:length(sites_ross_model)) %:%
  foreach(h = 1:length(sites_ross_model), .combine = "c") %do% {
    
    site1 <- filter(all_sites, site_id == sites_ross_model[i]) %>%
      select(longitude, latitude)
    
    site2 <- filter(all_sites, site_id == sites_ross_model[h]) %>%
      select(longitude, latitude)
    
    distCosine(site1, site2)/1000
  }

site_dist_ross <- map(site_dist_ross, function(x) x[which(x>0)])

rmse_dat_ross <- data.frame(rmse = unlist(sp_rmse_ross),
                           dist = unlist(site_dist_ross),
                           sites = rep(names(sp_rmse_ross), 
                                       each = length(sp_rmse_ross[[1]])))

cor_dat_ross <- data.frame(cor = unlist(sp_cor_ross),
                           dist = unlist(site_dist_ross),
                           sites = rep(names(sp_cor_ross), 
                                       each = length(sp_cor_ross[[1]])))


# WPE vs spatial forecast horizon -----------------------------------------

options(mc.cores = parallel::detectCores())

sp_slope <- foreach(i = 1:length(sites_ross_model)) %do% {
  dat <- filter(rmse_dat_ross, sites == sites_ross_model[i])
  
  data_lm <- list(y = dat$rmse,
                  X = (dat$dist - mean(dat$dist))/sd(dat$dist),
                  X_pred = dat$dist,
                  N = nrow(dat),
                  L = nrow(dat))
  
  res <- stan(file = 'lm.stan', 
              data = data_lm,
              iter = 2000)
  
  sl <- MCMCsummary(res, params = "beta")
  return(sl)
}

names(sp_slope) <- sites_ross_model
sp_slope <- sp_slope[order(names(sp_slope))]
sp_slope <- sp_slope[-9]

sp_slope_mean <- foreach(i = 1:length(sp_slope), .combine = "c") %do% sp_slope[[i]][1,1]
sp_slope_min <- foreach(i = 1:length(sp_slope), .combine = "c") %do% sp_slope[[i]][1,3]
sp_slope_max <- foreach(i = 1:length(sp_slope), .combine = "c") %do% sp_slope[[i]][1,5]

dat_ip <- readRDS("data_fig4.rds")
ip <- dat_ip$m2[which(rownames(dat_ip$m2)[1:14] %in% sites_ross_model),1]

stan_cor_test <- function(x) {
  
  res <- stan(file = 'correlation.stan', 
              data = list(x = x, N = nrow(x)),
              iter = 2000)
  
  MCMCsummary(res, params = "rho")
  
}

stan_cor_test(cbind(ip, sp_slope_mean))

theme_set(theme_bw())
g_main <- ggplot(mapping = aes(x = ip, y = sp_slope_mean)) +
  geom_point(size = 5) +
  labs(x = "WPE", y = "Slope (RMSE ratio vs distance)",
       title = bquote("Sites in CCAMLR 88.1," ~ rho ~ "= 0.49 (-0.14, 0.87)")) +
  theme(axis.title.x = element_text(size = 22),
        axis.title.y = element_text(size = 22),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        plot.title = element_text(size = 20))

get_lm_lines <- function(x, y, mean_x, sd_x) {
  
  X_pred <- seq(min(x), max(x), length.out = 1000)
  
  res <- stan(file = 'lm.stan', 
              data = list(y = y, X = x, X_pred = X_pred, L = length(X_pred), N = length(x)),
              iter = 2000)
  
  y_pred <- t(MCMCchains(res, params = "mu_pred"))
  y_pred2 <- cbind(y_pred, x_pred = (X_pred*sd_x) + mean_x)
  colnames(y_pred2)[1:4000] <- 1:4000
  y_pred3 <- pivot_longer(as.data.frame(y_pred2), -contains("x_pred"), values_to = "ts", names_to = "iteration") %>%
    arrange("iteration", "x_pred")
  
  return(list(y_pred2, y_pred3))
}

g_dat <- foreach(i = 1:length(sites_ross_model)) %do% {
  
  dat <- filter(rmse_dat_ross, sites == sites_ross_model[i])
  
  get_lm_lines(x = (dat$dist - mean(dat$dist))/sd(dat$dist),
                    mean_x = mean(dat$dist),
                    sd_x <- sd(dat$dist),
                    y = dat$rmse)
}
g_dat <- g_dat[-11]
names(g_dat) <- sites_ross_model[-11]

dat_fig5 <- list(ross_rmse = rmse_dat_ross,
                 ross_cor = cor_dat_ross,
                 ip = ip,
                 sp_slope_mean = sp_slope_mean,
                 sp_slope_min = sp_slope_min,
                 sp_slope_max = sp_slope_max,
                 g_dat = g_dat)

saveRDS(dat_fig5, "data_fig5.rds")

# Example plot
ggplot() + 
  geom_line(data = g_dat[["CRZE"]], mapping = aes(x = x_pred, y = ts, group = iteration),
            col = "grey", alpha = 0.1)  +
  geom_point(aes(x = filter(rmse_dat_ross, sites == "CRZE")$dist,
                 y = filter(rmse_dat_ross, sites == "CRZE")$rmse), 
             col = "darkred", size = 5) +
  labs(y = NULL, x = NULL, title = "CRZE")


