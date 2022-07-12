
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

#env_1500_raw <- readRDS("data_forced_finn_1500km_adpe.rds") %>%
  #filter(site_id %in% data_pop$site_list$site_id) %>%
  #filter(season %in% data_pop$seasons) %>%
  #rename_with(function(x) paste(x, "_1500", sep = ""), .cols = !(contains("site_id") | contains("season")))

env_raw <- readRDS("data_forced_finn_500km_adpe.rds") %>%
  filter(site_id %in% data_pop$site_list$site_id) %>%
  filter(season %in% data_pop$seasons) %>%
  rename_with(function(x) paste(x, "_500", sep = ""), .cols = !(contains("site_id") | contains("season")))

#env_raw <- bind_cols(env_1500_raw, env_500_raw[,c(-1,-2)])

# Data for Stan
data_stan <- foreach (i = 1:length(sites)) %do% {
  
  env_site <- filter(env, site_id == sites[i]) %>% 
    filter(season >= data_dem$seasons[[i]][1] & season < data_dem$seasons[[i]][2]) %>%
    select(-site_id, -season)
  
  env_site_raw <- filter(env_raw, site_id == sites[i]) %>% 
    filter(season >= data_dem$seasons[[i]][1] & season < data_dem$seasons[[i]][2]) %>%
    select(-site_id, -season)
  
  dat <- list(y = data_dem$r[[i]], 
              X = env_site,
              X_raw = env_site_raw,
              N = nrow(env_site),
              M = ncol(env_site))
  
  return(dat)
  
}

names(data_stan) <- data_pop$site_list$site_id


# Variable Selection with Finnish Horseshoe -------------------------------

options(mc.cores = parallel::detectCores())

res_finn <- foreach(i = 1:length(sites)) %do% {
  
  data_stan[[i]]$scale <- 0.1
  
  stan(file = 'lm_finn.stan', 
       data = data_stan[[i]],
       iter = 3000,
       control = list(adapt_delta = 0.9999, 
                      max_treedepth = 20))
  
}

# rerun models with convergence issues
for (i in 1:length(data_stan)) {
  print(i)
  print(check_hmc_diagnostics(res_finn[[i]]))
}

data_stan[[3]]$scale <- 0.05
res_finn[[3]] <- stan(file = 'lm_finn.stan', 
                       data = data_stan[[22]],
                       iter = 3000,
                       control = list(adapt_delta = 0.9999, 
                                      max_treedepth = 20))

data_stan[[23]]$scale <- 0.05
res_finn[[23]] <- stan(file = 'lm_finn.stan', 
                       data = data_stan[[22]],
                       iter = 3000,
                       control = list(adapt_delta = 0.9999, 
                                      max_treedepth = 20))

names(res_finn) <- data_pop$site_list$site_id

saveRDS(res_finn, "results_finn.rds")

# Plot slopes
plot_beta <- function(res_finn, dat, site_name) {
  beta <-  abs(MCMCsummary(res_finn, params = "beta")[,1])
  idx_beta <- order(beta, decreasing = T)
  var_names <- colnames(dat)
  
  theme_set(theme_bw())
  g1 <- ggplot() +
    geom_point(mapping = aes(x = factor(var_names[idx_beta], levels = var_names[idx_beta]), 
                             y = beta[idx_beta]), col = "darkblue", size = 2) +
    geom_segment(aes(y = 0, 
                     x = var_names[idx_beta], 
                     yend = beta[idx_beta], 
                     xend = var_names[idx_beta]), 
                 color = "darkblue", size = 1) +
    labs(y = "Slope Estimate (absolute)", title = site_name) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          axis.line.y = element_line(colour = "black"),
          axis.ticks.x = element_blank())
  
  res <- list(g = g1,
              vars = var_names[idx_beta[1:10]])
  
  return(res)
}

var_plots <- foreach(i = 1:length(res_finn)) %do% plot_beta(res_finn[[i]], env[,-c(1:2)], names(res_finn)[i])
names(var_plots) <- data_pop$site_list$site_id

saveRDS(var_plots, "plots_var.rds")


# Linear regression for calculating R^2 -----------------------------------

data_stan_lm <- foreach (i = 1:length(data_stan)) %do% {
    
  dat <- list(y = data_stan[[i]]$y, 
              X = data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]],
              X_raw = data_stan[[i]]$X_raw[,var_plots[[i]]$vars[1:3]],
              X_pred = data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]],
              N = nrow(data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]]),
              L = nrow(data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]]),
              M = 3)
    
  return(dat)
  
}

names(data_stan_lm) <- data_pop$site_list$site_id

# Check correlations among selected variables
corr <- foreach(i = 1:length(data_stan_lm)) %do% {
  cor(data_stan_lm[[i]]$X)
}

names(corr) <- data_pop$site_list$site_id

# Fit model
res_lm <- foreach(i = 1:length(sites)) %do% {
  
  stan(file = 'lm_multi.stan', 
       data = data_stan_lm[[i]],
       iter = 2000)

}

names(res_lm) <- data_pop$site_list$site_id
saveRDS(res_lm, "results_lm.rds")

# Plot R2
trad_R2 <- foreach(i = 1:length(res_lm), .combine = "c") %do% {
  
  mu_pred <- MCMCsummary(res_lm[[i]], params = "mu_pred")[,1]
  r_obs <- data_stan_lm[[i]]$y
  
  R2 <- cor(mu_pred, r_obs)^2
  
  return(round(R2, 2))
  
}

rmse1 <- foreach(i = 1:length(res_lm), .combine = "c") %do% {
  
  mu_pred <- MCMCsummary(res_lm[[i]], params = "mu_pred")[,1]
  r_obs <- data_stan_lm[[i]]$y
  
  sqrt(sum((mu_pred - r_obs)^2)/length(r_obs))
  #mae2 <- sum(abs(mean(r_obs) - r_obs))/length(r_obs)
  
  #return(round(mae1/mae2, 2))
}

rmse2 <- foreach(i = 1:length(res_lm), .combine = "c") %do% {
  
  r_obs <- data_stan_lm[[i]]$y
  sqrt(sum((mean(r_obs) - r_obs)^2)/length(r_obs))
  
  #return(round(mae1/mae2, 2))
}

site_names <- factor(data_pop$site_list$site_id, 
                      levels = c("RUMP", "MAME", "TORI", "HUKU", "ONGU", "BENT", "MIZU", "YTRE", 
                                 "CRZE", "CRZW", "ROYD", "FRAE", "CMID", "BRDN", "BEAU", "BRDS",   
                                 "BRDM", "INEX", "CHAL", "LITC", "ARDL","PETE", "LLAN", "PGEO"))
site_names2 <- factor(data_pop$site_list$site_id, 
                      levels = c("RUMP", "MAME", "TORI", "HUKU", "ONGU", "BENT", "MIZU", "YTRE", 
                                 "CRZE", "CRZW", "ROYD", "FRAE", "CMID", "BRDN", "BEAU", "BRDS",   
                                 "BRDM", "INEX", "CHAL", "LITC", "ARDL","PETE", "LLAN", "PGEO")[24:1])

fig3_data <- list(trad_R2 = trad_R2,
                  rmse1 = rmse1,
                  rmse2 = rmse2,
                  site_names = site_names,
                  site_names2 = site_names2)

saveRDS(fig3_data, "data_fig3_1.rds")


# Univariate Plots --------------------------------------------------------

var_names <- read.csv("variable_names.csv")[,2:3]

plot_uni <- function(data_stan_lm, site_no, var_no) {
  
  site_name <- names(data_stan_lm)[site_no]
  
  xname <- names(data_stan_lm[[site_no]]$X)[var_no]
  
  var_name <- str_split(xname, "_")[[1]][1]
  
  if (var_name != "photoC") {
    
    z <- str_split(xname, "_")[[1]][-1]
    idx <- which(var_names[,1] == var_name)
  
  } else {
    
    z <- str_split(xname, "_")[[1]][-(1:4)]
    idx <- which(str_detect(var_names[,1], var_name))
    
  }
  
  if (length(z)>1) {
    labx <- paste(var_names[idx,2], "(", paste(z[1], z[2], sep = ", "), ")", sep = "")
  } else {
    labx <- paste(var_names[idx,2], "(", z, ")", sep = "")
  }
  
  g <- ggplot(mapping = aes(x = data_stan_lm[[site_no]]$X[,var_no], y = data_stan_lm[[site_no]]$y)) +
    geom_point() + 
    geom_smooth(method = "lm", color = "darkblue") +
    labs(y = "Annual Growth", x = labx, title = site_name)
  
  return(g)
  
}

uni_plots <- 
  foreach(i = 1:21) %:%
  foreach(h = 1:3) %do% {
    plot_uni(data_stan_lm, site_no = i, var_no = h)
  }

for (i in 1:21) {
  for (h in 1:3) {
    plot_name <- paste("figures/", paste("plot", sites[i], h, ".jpeg", sep = ""), sep ="")
    print(uni_plots[[i]][[h]])
    ggsave(plot_name, width = 10, height = 8, units = "in")
  }
}
