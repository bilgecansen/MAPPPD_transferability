# Fit random forests models

library(foreach)
library(tidyverse)
library(MCMCvis)
library(scales)
library(party)
library(permimp)
library(caret)
library(pdp)
library(randomForest)
library(ggrepel)

# Install with devtools::install_github("thomasp85/patchwork")
library(patchwork)

data_pop <- readRDS("data_pop_adpe.rds")
data_pop_all <- readRDS("data_pop_all_adpe.rds")
data_dem <- readRDS("data_dem_adpe.rds")

sites <- data_pop$site_list$site_id
sites_ap <- filter(data_pop$site_list, ccamlr_id == 48.1)$site_id
sites_ross <- filter(data_pop$site_list, ccamlr_id == 88.1)$site_id
sites_ea <- filter(data_pop$site_list, ccamlr_id == "58.4.2")$site_id

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

# Data for RF
prep_rf_dat <- function(site) {
  
  idx <- which(sites == site)
  
  dat <- filter(env, site_id == site) %>% 
    filter(season >= data_dem$seasons[[idx]][1] & season < data_dem$seasons[[idx]][2]) %>%
    select(-site_id, -season)
  
  dat$r <- data_dem$r[[idx]]
  
  return(dat)
}

data_rf_ap <- foreach (i = 1:length(sites_ap), .combine = "rbind") %do% 
  prep_rf_dat(sites_ap[i])

data_rf_ross <- foreach (i = 1:length(sites_ross), .combine = "rbind") %do% 
  prep_rf_dat(sites_ross[i])

data_rf_ea <- foreach (i = 1:length(sites_ea), .combine = "rbind") %do% 
  prep_rf_dat(sites_ea[i])

# Random Forests ----------------------------------------------------------

nvar <- ncol(data_rf_ap)-1
mtry <- round(nvar*seq(0.1,0.9, 0.1))
ntree <- 2000

# all data
rf_all_ap <- foreach(i = 1:length(mtry)) %do% 
  cforest(r ~ ., data = data_rf_ap, controls = cforest_unbiased(ntree = ntree, mtry = mtry[i]))

rf_all_ross <- foreach(i = 1:length(mtry)) %do% 
  cforest(r ~ ., data = data_rf_ross, controls = cforest_unbiased(ntree = ntree, mtry = mtry[i]))

rf_all_ea <- foreach(i = 1:length(mtry)) %do% 
  cforest(r ~ ., data = data_rf_ea, controls = cforest_unbiased(ntree = ntree, mtry = mtry[i]))

rmse_ap <- foreach(i = 1:length(mtry), .combine = "c") %do% 
  cforestStats(rf_all_ap[[i]])[1]

rmse_ross <- foreach(i = 1:length(mtry), .combine = "c") %do% 
  cforestStats(rf_all_ross[[i]])[1]

rmse_ea <- foreach(i = 1:length(mtry), .combine = "c") %do% 
  cforestStats(rf_all_ea[[i]])[1]

rf_sel_ap <- cforest(r ~ ., data = data_rf_ap, 
                     controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ap)]))
cforestStats(rf_sel_ap)

rf_sel_ross <- cforest(r ~ ., data = data_rf_ross, 
                     controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ross)]))
cforestStats(rf_sel_ross)

rf_sel_ea <- cforest(r ~ ., data = data_rf_ea, 
                     controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ea)]))
cforestStats(rf_sel_ea)

rf_sel <- list(ap = rf_sel_ap,
               ross = rf_sel_ross,
               ea = rf_sel_ea)

saveRDS(rf_sel, "results_rf.rds")

# Variable importance
varimp_rf_ap <- permimp(rf_sel$ap, conditional = T, progressBar = T)
varimp_rf_ap <- varimp_rf_ap$values[order(varimp_rf_ap$values, decreasing = T)]

varimp_rf_ross <- permimp(rf_sel$ross, conditional = T, progressBar = T)
varimp_rf_ross <- varimp_rf_ross$values[order(varimp_rf_ross$values, decreasing = T)]

varimp_rf_ea <- permimp(rf_sel$ea, conditional = T, progressBar = T)
varimp_rf_ea <- varimp_rf_ea$values[order(varimp_rf_ea$values, decreasing = T)]

varimp_rf <- list(ap = varimp_rf_ap,
                  ross = varimp_rf_ross,
                  ea = varimp_rf_ea)

saveRDS(varimp_rf, "varimp_rf.rds")

theme_set(theme_bw())
ggplot(mapping = aes(x = factor(names(varimp_rf_ap)[1:10], 
                                levels = names(varimp_rf_ap)[1:10]),
                     y = varimp_rf_ap[1:10])) +
  geom_col(fill = "dark blue") +
  labs(y = "Variable Importance", title = "Antarctic Peninsula") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8),
        axis.text.y = element_blank())

ggplot(mapping = aes(x = factor(names(varimp_rf_ea)[1:10], 
                                levels = names(varimp_rf_ea)[1:10]),
                     y = varimp_rf_ea[1:10])) +
  geom_col(fill = "dark blue") +
  labs(y = "Variable Importance", title = "Eastern Antarctica") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8),
        axis.text.y = element_blank())

ggplot(mapping = aes(x = factor(names(varimp_rf_ross)[1:10], 
                                levels = names(varimp_rf_ross)[1:10]),
                     y = varimp_rf_ea[1:10])) +
  geom_col(fill = "dark blue") +
  labs(y = "Variable Importance", title = "Ross Sea") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.8),
        axis.text.y = element_blank())


# Training-test splits with random forests --------------------------------

# Forecasts
data_rf_tr_fore_ap <- foreach (i = 1:length(sites_ap), .combine = "rbind") %do% {
  
  dat <- prep_rf_dat(sites_ap[i])
  
  tr_ss <- round(nrow(dat)*0.7)
  
  dat <- dat[1:tr_ss,]
  
  return(dat)
  
}

data_rf_tr_fore_ross <- foreach (i = 1:length(sites_ross), .combine = "rbind") %do% {
  
  dat <- prep_rf_dat(sites_ross[i])
  
  tr_ss <- round(nrow(dat)*0.7)
  
  dat <- dat[1:tr_ss,]
  
  return(dat)
  
}

data_rf_tr_fore_ea <- foreach (i = 1:length(sites_ea), .combine = "rbind") %do% {
  
  dat <- prep_rf_dat(sites_ea[i])
  
  tr_ss <- round(nrow(dat)*0.7)
  
  dat <- dat[1:tr_ss,]
  
  return(dat)
  
}

data_rf_ts_fore_ap <- foreach (i = 1:length(sites_ap)) %do% {
  
  dat <- prep_rf_dat(sites_ap[i])
  
  total_ss <- nrow(dat)
  tr_ss <- round(nrow(dat)*0.7)
  
  dat <- dat[(tr_ss+1):total_ss,]
  
  return(dat)
  
}

data_rf_ts_fore_ross <- foreach (i = 1:length(sites_ross)) %do% {
  
  dat <- prep_rf_dat(sites_ross[i])
  
  total_ss <- nrow(dat)
  tr_ss <- round(nrow(dat)*0.7)
  
  dat <- dat[(tr_ss+1):total_ss,]
  
  return(dat)
  
}

data_rf_ts_fore_ea <- foreach (i = 1:length(sites_ea)) %do% {
  
  dat <- prep_rf_dat(sites_ea[i])
  
  total_ss <- nrow(dat)
  tr_ss <- round(nrow(dat)*0.7)
  
  dat <- dat[(tr_ss+1):total_ss,]
  
  return(dat)
  
}

rf_fore_ap <- cforest(r ~ ., data = data_rf_tr_fore_ap, 
                     controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ap)]))

rf_fore_ross <- cforest(r ~ ., data = data_rf_tr_fore_ross, 
                       controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ross)]))

rf_fore_ea <- cforest(r ~ ., data = data_rf_tr_fore_ea, 
                     controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ea)]))

cor_r_ap_fore <- c()
rmse_ap_fore <- c()
for (i in 1:length(sites_ap)) {
  
  r_pred <- predict(rf_fore_ap, newdata = data_rf_ts_fore_ap[[i]][,-121])
  r <-  data_rf_ts_fore_ap[[i]]$r

  cor_r_ap_fore[i] <- cor(r_pred, r)
  rmse_ap_fore[i] <- sqrt(sum((r_pred - r)^2)/length(r))
  
}

cor_r_ross_fore <- c()
rmse_ross_fore <- c()
for (i in 1:length(sites_ross)) {
  
  r_pred <- predict(rf_fore_ross, newdata = data_rf_ts_fore_ross[[i]][,-121])
  r <-  data_rf_ts_fore_ross[[i]]$r
  
  cor_r_ross_fore[i] <- cor(r_pred, r)
  rmse_ross_fore[i] <- sqrt(sum((r_pred - r)^2)/length(r))
  
}

cor_r_ea_fore <- c()
rmse_ea_fore <- c()
for (i in 1:length(sites_ea)) {
  
  r_pred <- predict(rf_fore_ea, newdata = data_rf_ts_fore_ea[[i]][,-121])
  r <-  data_rf_ts_fore_ea[[i]]$r
  
  cor_r_ea_fore[i] <- cor(r_pred, r)
  rmse_ea_fore[i] <- sqrt(sum((r_pred - r)^2)/length(r))
  
}

cor_r_fore <- c(cor_r_ap_fore, cor_r_ea_fore, cor_r_ross_fore)
names(cor_r_fore) <- c(sites_ap, sites_ea, sites_ross)
cor_r_fore <- cor_r_fore[order(names(cor_r_fore))]

rmse_fore <- c(rmse_ap_fore, rmse_ea_fore, rmse_ross_fore)
names(rmse_fore) <- c(sites_ap, sites_ea, sites_ross)
rmse_fore <- rmse_fore[order(names(rmse_fore))]

# Hindcasts
data_rf_tr_hind_ap <- foreach (i = 1:length(sites_ap), .combine = "rbind") %do% {
  
  dat <- prep_rf_dat(sites_ap[i])
  
  total_ss <- nrow(dat)
  tt_ss <- round(nrow(dat)*0.3)
  
  dat <- dat[(tt_ss+1):total_ss,]
  
  return(dat)
  
}

data_rf_tr_hind_ross <- foreach (i = 1:length(sites_ross), .combine = "rbind") %do% {
  
  dat <- prep_rf_dat(sites_ross[i])
  
  total_ss <- nrow(dat)
  tt_ss <- round(nrow(dat)*0.3)
  
  dat <- dat[(tt_ss+1):total_ss,]
  
  return(dat)
  
}

data_rf_tr_hind_ea <- foreach (i = 1:length(sites_ea), .combine = "rbind") %do% {
  
  dat <- prep_rf_dat(sites_ea[i])
  
  total_ss <- nrow(dat)
  tt_ss <- round(nrow(dat)*0.3)
  
  dat <- dat[(tt_ss+1):total_ss,]
  
  return(dat)
  
}

data_rf_ts_hind_ap <- foreach (i = 1:length(sites_ap)) %do% {
  
  dat <- prep_rf_dat(sites_ap[i])
  
  tt_ss <- round(nrow(dat)*0.3)
  
  dat <- dat[1:tt_ss,]
  
  return(dat)
  
}

data_rf_ts_hind_ross <- foreach (i = 1:length(sites_ross)) %do% {
  
  dat <- prep_rf_dat(sites_ross[i])
  
  tt_ss <- round(nrow(dat)*0.3)
  
  dat <- dat[1:tt_ss,]
  
  return(dat)
  
}

data_rf_ts_hind_ea <- foreach (i = 1:length(sites_ea)) %do% {
  
  dat <- prep_rf_dat(sites_ea[i])
  
  tt_ss <- round(nrow(dat)*0.3)
  
  dat <- dat[1:tt_ss,]
  
  return(dat)
  
}

rf_hind_ap <- cforest(r ~ ., data = data_rf_tr_hind_ap, 
                      controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ap)]))

rf_hind_ross <- cforest(r ~ ., data = data_rf_tr_hind_ross, 
                        controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ross)]))

rf_hind_ea <- cforest(r ~ ., data = data_rf_tr_hind_ea, 
                      controls = cforest_unbiased(ntree = 8000, mtry = mtry[which.min(rmse_ea)]))

cor_r_ap_hind <- c()
rmse_ap_hind <- c()
for (i in 1:length(sites_ap)) {
  
  r_pred <- predict(rf_hind_ap, newdata = data_rf_ts_hind_ap[[i]][,-121])
  r <-  data_rf_ts_hind_ap[[i]]$r
  
  cor_r_ap_hind[i] <- cor(r_pred, r)
  rmse_ap_hind[i] <- sqrt(sum((r_pred - r)^2)/length(r))
  
}

cor_r_ross_hind <- c()
rmse_ross_hind <- c()
for (i in 1:length(sites_ross)) {
  
  r_pred <- predict(rf_hind_ross, newdata = data_rf_ts_hind_ross[[i]][,-121])
  r <-  data_rf_ts_hind_ross[[i]]$r
  
  cor_r_ross_hind[i] <- cor(r_pred, r)
  rmse_ross_hind[i] <- sqrt(sum((r_pred - r)^2)/length(r))
  
}

cor_r_ea_hind <- c()
rmse_ea_hind <- c()
for (i in 1:length(sites_ea)) {
  
  r_pred <- predict(rf_hind_ea, newdata = data_rf_ts_hind_ea[[i]][,-121])
  r <-  data_rf_ts_hind_ea[[i]]$r
  
  cor_r_ea_hind[i] <- cor(r_pred, r)
  rmse_ea_hind[i] <- sqrt(sum((r_pred - r)^2)/length(r))
  
}

cor_r_hind <- c(cor_r_ap_hind, cor_r_ea_hind, cor_r_ross_hind)
names(cor_r_hind) <- c(sites_ap, sites_ea, sites_ross)
cor_r_hind <- cor_r_hind[order(names(cor_r_hind))]

rmse_hind <- c(rmse_ap_hind, rmse_ea_hind, rmse_ross_hind)
names(rmse_hind) <- c(sites_ap, sites_ea, sites_ross)
rmse_hind <- rmse_hind[order(names(rmse_hind))]

rf_pred <- list(cor_fore = cor_r_fore,
                cor_hind = cor_r_hind,
                rmse_fore = rmse_fore,
                rmse_hind = rmse_hind)

saveRDS(rf_pred, "results_rf_pred.rds")

