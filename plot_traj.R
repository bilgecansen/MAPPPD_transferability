
library(tidyverse)

data_pop <- readRDS("data_pop_adpe.rds")
data_dem <- readRDS("data_dem_adpe.rds")

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