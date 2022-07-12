
library(foreach)
library(tidyverse)
library(rstan)
library(MCMCvis)
library(scales)
library(mapppdr)
library(ggrepel)

# Install with devtools::install_github("thomasp85/patchwork")
library(patchwork)

# Install with devtools::install_github("hrbrmstr/ggalt")
library(ggalt)

source("fit_pe.R")

data_pop <- readRDS("data_pop_adpe.rds")
data_pop_all <- readRDS("data_pop_all_adpe.rds")
data_dem <- readRDS("data_dem_adpe.rds")
sites <- data_pop$site_list$site_id

var_plots <- readRDS("plots_var.rds")

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

data_stan_lm <- foreach (i = 1:length(data_stan)) %do% {
  
  dat <- list(y = data_stan[[i]]$y, 
              X = data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]],
              X_pred = data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]],
              X_raw = data_stan[[i]]$X_raw[,var_plots[[i]]$vars[1:3]],
              N = nrow(data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]]),
              L = nrow(data_stan[[i]]$X[,var_plots[[i]]$vars[1:3]]),
              M = 3)
    
  return(dat)
  
}

names(data_stan_lm) <- data_pop$site_list$site_id


# Linear regression with training-test split ------------------------------

options(mc.cores = parallel::detectCores())

data_stan_tt_fore <- foreach (i = 1:length(data_stan_lm)) %do% {
  
  total_ss <- data_stan_lm[[i]]$N
  tr_ss <- round(data_stan_lm[[i]]$N*0.7)
  
  dat <- list(y = data_stan_lm[[i]]$y[1:tr_ss], 
              X = data_stan_lm[[i]]$X[1:tr_ss,],
              X_raw = data_stan_lm[[i]]$X_raw[1:tr_ss,],
              X_pred = data_stan_lm[[i]]$X[(tr_ss+1):total_ss,],
              X_pred_raw = data_stan_lm[[i]]$X_raw[(tr_ss+1):total_ss,],
              N = tr_ss,
              L = total_ss - tr_ss,
              M = 3)
  
  return(dat)
  
}

data_stan_tt_hind <- foreach (i = 1:length(data_stan_lm)) %do% {
  
  total_ss <- data_stan_lm[[i]]$N
  tt_ss <- round(data_stan_lm[[i]]$N*0.3)
  
  dat <- list(y = data_stan_lm[[i]]$y[(tt_ss+1):total_ss], 
              X = data_stan_lm[[i]]$X[(tt_ss+1):total_ss,],
              X_raw = data_stan_lm[[i]]$X_raw[(tt_ss+1):total_ss,],
              X_pred = data_stan_lm[[i]]$X[1:tt_ss,],
              X_pred_raw = data_stan_lm[[i]]$X_raw[1:tt_ss,],
              N = total_ss - tt_ss,
              L = tt_ss,
              M = 3)
  
  return(dat)
  
}

names(data_stan_tt_fore) <- data_pop$site_list$site_id
names(data_stan_tt_hind) <- data_pop$site_list$site_id

res_tt_fore <- foreach(i = 1:length(sites)) %do% {
  
  stan(file = 'lm_multi.stan', 
       data = data_stan_tt_fore[[i]],
       iter = 2000)
  
}

names(res_tt_fore) <- data_pop$site_list$site_id

res_tt_hind <- foreach(i = 1:length(sites)) %do% {
  
  stan(file = 'lm_multi.stan', 
       data = data_stan_tt_hind[[i]],
       iter = 2000)
  
}

names(res_tt_hind) <- data_pop$site_list$site_id

res_tt <- list(fore = res_tt_fore, hind = res_tt_hind)
saveRDS(res_tt, "results_lm_tt.rds")

# Load RF results
rf_pred <- readRDS("results_rf_pred.rds")

# Plots
site_names <- factor(data_pop$site_list$site_id, 
                     levels = c("RUMP", "MAME", "TORI", "HUKU", "ONGU", "BENT", "MIZU", "YTRE", 
                                "CRZE", "CRZW", "ROYD", "FRAE", "CMID", "BRDN", "BEAU", "BRDS",   
                                "BRDM", "INEX", "CHAL", "LITC", "ARDL","PETE", "LLAN", "PGEO")[24:1])

r_pred_fore <- foreach(i = 1:length(res_tt_fore)) %do% MCMCchains(res_tt_fore[[i]], params = "mu_pred")
r_pred_hind <- foreach(i = 1:length(res_tt_hind)) %do% MCMCchains(res_tt_hind[[i]], params = "mu_pred")

r_mean_fore <- map(r_pred_fore, function(x) apply(x, 2, mean))
r_mean_hind <- map(r_pred_hind, function(x) apply(x, 2, mean))
names(r_mean_fore) <- data_pop$site_list$site_id
names(r_mean_hind) <- data_pop$site_list$site_id

r_obs_fore <- foreach(i = 1:length(data_stan)) %do% 
  data_stan[[i]]$y[(data_stan_tt_fore[[i]]$N+1):data_stan[[i]]$N]
names(r_obs_fore) <- data_pop$site_list$site_id

r_obs_hind <- foreach(i = 1:length(data_stan)) %do% 
  data_stan[[i]]$y[1:data_stan_tt_hind[[i]]$L]
names(r_obs_hind) <- data_pop$site_list$site_id

rmse_fore <- foreach(i = 1:length(r_mean_fore), .combine = "c") %do% {
  round(sqrt(sum((r_mean_fore[[i]] - r_obs_fore[[i]])^2)/length(r_obs_fore[[i]])), 2)
}
rmse_fore_null <- foreach(i = 1:length(r_obs_hind), .combine = "c") %do% {
  round(sqrt(sum((mean(data_stan_tt_fore[[i]]$y) - r_obs_fore[[i]])^2)/length(r_obs_fore[[i]])), 2)
}
names(rmse_fore) <- data_pop$site_list$site_id
names(rmse_fore_null) <- data_pop$site_list$site_id

rmse_hind <- foreach(i = 1:length(r_mean_hind), .combine = "c") %do% {
  round(sqrt(sum((r_mean_hind[[i]] - r_obs_hind[[i]])^2)/length(r_obs_hind[[i]])), 2)
}
rmse_hind_null <- foreach(i = 1:length(r_obs_hind), .combine = "c") %do% {
  round(sqrt(sum((mean(data_stan_tt_hind[[i]]$y) - r_obs_hind[[i]])^2)/length(r_obs_hind[[i]])), 2)
}
names(rmse_hind) <- data_pop$site_list$site_id
names(rmse_hind_null) <- data_pop$site_list$site_id

cor_tt_fore <- foreach(i = 1:length(r_mean_fore), .combine = "c") %do% 
  round(cor(r_mean_fore[[i]], r_obs_fore[[i]]), 2)
names(cor_tt_fore) <- data_pop$site_list$site_id

cor_tt_hind <- foreach(i = 1:length(r_mean_hind), .combine = "c") %do% 
  round(cor(r_mean_hind[[i]], r_obs_hind[[i]]), 2)
names(cor_tt_hind) <- data_pop$site_list$site_id

fig3_data <- list(cor_tt_fore = cor_tt_fore,
                  cor_tt_hind = cor_tt_hind,
                  rmse_fore = rmse_fore,
                  rmse_hind = rmse_hind,
                  rmse_fore_null = rmse_fore_null,
                  rmse_hind_null = rmse_hind_null,
                  site_names = site_names)

saveRDS(fig3_data, "data_fig3_2.rds")

# RF vs RHP
dat_rf_rhp <- data.frame(cor_rf = c(rf_pred$cor_fore, rf_pred$cor_hind),
                         cor_rhp = c(cor_tt_fore[-20], cor_tt_hind[-20]),
                         rmse_rf = c(rf_pred$rmse_fore, rf_pred$rmse_hind),
                         rmse_rhp = c(rmse_fore[-20], rmse_hind[-20]))

dat_rf_rhp$cor_group <- as.factor(dat_rf_rhp$cor_rhp > dat_rf_rhp$cor_rf)
dat_rf_rhp$rmse_group <- as.factor(dat_rf_rhp$rmse_rhp > dat_rf_rhp$rmse_rf)

saveRDS(dat_rf_rhp, "data_sup_rf.rds")

# Intrinsic predictability of full data
idx_N <- which(map_dbl(data_dem$N, length) > 29)

int_pr <- map_dbl(data_dem$N[idx_N], function(x) 
  PE(x = log(x), weighted = T,  word_length = 4, tau = 1, tie_method = "average"))

m1 <- cbind(c(int_pr, int_pr), 
            c(cor_tt_fore[idx_N], cor_tt_hind[idx_N]))

m2 <- cbind(c(int_pr, int_pr),
            c(rmse_fore[idx_N], rmse_hind[idx_N]),
            c(rmse_fore[idx_N]/rmse_fore_null[idx_N], rmse_hind[idx_N]/rmse_hind_null[idx_N])) %>%
  as.data.frame()

m2$CCAMLR <- c("48.1", "88.1", "88.1", "88.1", "88.1", "88.1", "88.1", "88.1", "88.1", "88.1", 
               "48.1", "48.1", "58.4.2", "88.1", 
               "48.1", "88.1", "88.1", "88.1", "88.1", "88.1", "88.1", "88.1", "88.1", "88.1", 
               "48.1", "48.1", "58.4.2", "88.1")

fig4_data <- list(m2 = m2,
                  idx_N = idx_N,
                  site_names = site_names)

saveRDS(fig4_data, "data_fig4.rds")


# Simulate simple time series ---------------------------------------------

# constant growth
N <- 100
ts <- 49
r <- 1.05

for (i in 1:ts) {
  N[i+1] <- N[i]*r
}

PE(x = N, weighted = T,  word_length = 3, tau = 1, tie_method = "average")

# Limit cycle
log_map <- function(r, sd) {
  
  N <- 0.5
  e <- rnorm(49, 0, sd)
  
  for (i in 1:ts) {
    N[i+1] <- N[i]*r*(1-N[i]) + e[i]
  }
  
  return(N[-(1:10)])
}

N2 <- log_map(r = 3.4, sd = 0)
PE(x = N2, weighted = T,  word_length = 3, tau = 1, tie_method = "average")

# Chaos
N3 <- log_map(r = 3.9, sd = 0)
PE(x = N3, weighted = T,  word_length = 3, tau = 1, tie_method = "average")

# Stochastic
N4 <- log_map(r = 0, sd = 0.1)
PE(x = N4, weighted = T,  word_length = 3, tau = 1, tie_method = "average")

data_fig1 <- list(N = N,
                  N2 = N2,
                  N3 = N3,
                  N4 = N4)

saveRDS(data_fig1, "data_fig1.rds")


# data for growth trajectories --------------------------------------------

data_pop <- readRDS("data_pop_adpe.rds")
data_dem <- readRDS("data_dem_adpe.rds")

data_fig6 <- list(data_pop = data_pop,
                  data_dem = data_dem$r[c("CRZE", "BRDN", "PGEO")],
                  res_tt_fore = res_tt$fore[c("CRZE", "BRDN", "PGEO")],
                  res_tt_hind = res_tt$hind[c("CRZE", "BRDN", "PGEO")])

saveRDS(data_fig6, "data_fig6.rds")


# Results package ---------------------------------------------------------

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "aice"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "aicen"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "ardg"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "divu"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "shear"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "uatm"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "vatm"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "rain"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "zooC"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "HMXL"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "photo"))) %>%
  as.numeric() %>%
  sum()

map_lgl(var_plots, function(x) any(str_detect(x$vars[1:3], "TEMP"))) %>%
  as.numeric() %>%
  sum()

data_fig3_1 <- readRDS("data_fig3_1.rds")
R2 <- data_fig3_1$trad_R2
mean(R2)
min(R2)
max(R2)

mean(cor_tt_fore^2)
min(cor_tt_fore^2)
max(cor_tt_fore^2)

mean(cor_tt_hind^2)
min(cor_tt_hind^2)
max(cor_tt_hind^2)

which((rmse_fore/rmse_fore_null) < 1) %>%
  length()
which((rmse_hind/rmse_hind_null) < 1) %>%
  length()

(1-(rmse_fore/rmse_fore_null))[which((rmse_fore/rmse_fore_null) < 1)] %>%
  mean()
(1-(rmse_fore/rmse_fore_null))[which((rmse_fore/rmse_fore_null) < 1)] %>%
  sd()

(1-(rmse_hind/rmse_hind_null))[which((rmse_hind/rmse_hind_null) < 1)] %>%
  mean()
(1-(rmse_hind/rmse_hind_null))[which((rmse_hind/rmse_hind_null) < 1)] %>%
  sd()

(1-(rmse_fore/rmse_fore_null))[which((rmse_fore/rmse_fore_null) >= 1)] %>%
  mean()
(1-(rmse_fore/rmse_fore_null))[which((rmse_fore/rmse_fore_null) >= 1)] %>%
  sd()

(1-(rmse_hind/rmse_hind_null))[which((rmse_hind/rmse_hind_null) >= 1)] %>%
  mean()
(1-(rmse_hind/rmse_hind_null))[which((rmse_hind/rmse_hind_null) >= 1)] %>%
  sd()

stan_cor_test <- function(x) {
  
  res <- stan(file = 'correlation.stan', 
              data = list(x = x, N = nrow(x)),
              iter = 2000)
  
  MCMCsummary(res, params = "rho")
  
}

stan_cor_test(m2[1:14,1:2])
stan_cor_test(m2[15:28,1:2])

stan_cor_test(m2[1:14,c(1,3)])
stan_cor_test(m2[15:28,c(1,3)])


# Density Dependence ------------------------------------------------------

data_stan_ttdd_fore <- foreach (i = 1:length(data_stan_lm)) %do% {
  
  total_ss <- data_stan_lm[[i]]$N
  tr_ss <- round(data_stan_lm[[i]]$N*0.7)
  
  lz <- log(data_dem$N[[i]])
  lz <- lz[-length(lz)]
  
  X_dd <- cbind(data_stan_lm[[i]]$X, lz)
  
  dat <- list(y = data_stan_lm[[i]]$y[1:tr_ss], 
              X = X_dd[1:tr_ss,],
              X_pred = X_dd[(tr_ss+1):total_ss,],
              N = tr_ss,
              L = total_ss - tr_ss,
              M = 4)
  
  return(dat)
  
}
names(data_stan_ttdd_fore) <- data_pop$site_list$site_id

data_stan_ttdd_hind <- foreach (i = 1:length(data_stan_lm)) %do% {
  
  total_ss <- data_stan_lm[[i]]$N
  tt_ss <- round(data_stan_lm[[i]]$N*0.3)
  
  lz <- log(data_dem$N[[i]])
  lz <- lz[-length(lz)]
  
  X_dd <- cbind(data_stan_lm[[i]]$X, lz)
  
  dat <- list(y = data_stan_lm[[i]]$y[(tt_ss+1):total_ss], 
              X = X_dd[(tt_ss+1):total_ss,],
              X_pred = X_dd[1:tt_ss,],
              N = total_ss - tt_ss,
              L = tt_ss,
              M = 4)
  
  return(dat)
  
}
names(data_stan_ttdd_hind) <- data_pop$site_list$site_id

res_ttdd_fore <- foreach(i = 1:length(data_stan_lm)) %do% {
  
  stan(file = 'lm_multi_dd.stan', 
       data = data_stan_ttdd_fore[[i]],
       iter = 2000)
  
}

res_ttdd_hind <- foreach(i = 1:length(data_stan_lm)) %do% {
  
  stan(file = 'lm_multi_dd.stan', 
       data = data_stan_ttdd_hind[[i]],
       iter = 2000)
  
}

res_tt_dd <- list(fore = res_ttdd_fore, hind = res_ttdd_hind)
saveRDS(res_tt_dd, "results_lm_tt_dd.rds")

r_pred_fore_dd <- foreach(i = 1:length(res_ttdd_fore)) %do% MCMCchains(res_ttdd_fore[[i]], params = "mu_pred")
r_pred_hind_dd<- foreach(i = 1:length(res_ttdd_hind)) %do% MCMCchains(res_ttdd_hind[[i]], params = "mu_pred")

r_mean_fore_dd <- map(r_pred_fore_dd, function(x) apply(x, 2, mean))
names(r_mean_fore_dd) <- data_pop$site_list$site_id

r_mean_hind_dd <- map(r_pred_hind_dd, function(x) apply(x, 2, mean))
names(r_mean_fore_dd) <- data_pop$site_list$site_id

rmse_fore_dd <- foreach(i = 1:length(data_stan_lm), .combine = "c") %do% {
  round(sqrt(sum((r_mean_fore_dd[[i]] - r_obs_fore[[i]])^2)/length(r_obs_fore[[i]])), 2)
}
names(rmse_fore_dd) <- data_pop$site_list$site_id

rmse_hind_dd <- foreach(i = 1:length(data_stan_lm), .combine = "c") %do% {
  round(sqrt(sum((r_mean_hind_dd[[i]] - r_obs_hind[[i]]))^2/length(r_obs_hind[[i]])), 2)
}
names(rmse_hind_dd) <- data_pop$site_list$site_id

cor_tt_fore_dd <- foreach(i = 1:length(data_stan_lm), .combine = "c") %do% 
  round(cor(r_mean_fore_dd[[i]], r_obs_fore[[i]]), 2)
names(cor_tt_fore) <- data_pop$site_list$site_id

cor_tt_hind_dd <- foreach(i = 1:length(data_stan_lm), .combine = "c") %do% 
  round(cor(r_mean_hind_dd[[i]], r_obs_hind[[i]]), 2)
names(cor_tt_hind) <- data_pop$site_list$site_id

data_fig_sup_dd <- list(cor_tt_fore_dd = cor_tt_fore_dd,
                  cor_tt_hind_dd = cor_tt_hind_dd,
                  rmse_fore_dd = rmse_fore_dd,
                  rmse_hind_dd = rmse_hind_dd,
                  site_names = site_names)

saveRDS(data_fig_sup_dd, "data_fig_sup_dd.rds")
