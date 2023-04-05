library(mapppdr)
library(tidyverse)
library(sf)
library(ggplot2)
library(ggspatial)
library(ggnewscale)

r <- readRDS("google_drive/data_ts (2).rds")
wpe <- data.frame(wpe = readRDS("google_drive/wpe.rds"))
wpe$site_id <- rownames(wpe)
r <- bind_rows(r, .id = "site_id") %>%
  left_join(sites, by = "site_id") %>%
  left_join(wpe, by = "site_id")

site_list <- r %>%
  dplyr::select(site_id, site_name, region, ccamlr_id, wpe) %>%
  distinct()

min_wpe <- base::min(site_list$wpe, na.rm = TRUE)
max_wpe <- base::max(site_list$wpe, na.rm = TRUE)

ea_border <- rgb(153, 20, 36, maxColorValue = 255)
ross_border <- rgb(18, 145, 209, maxColorValue = 255)
ap_border <- rgb(27, 39, 110, maxColorValue = 255)
pgeo_dot <- "#5DB86A"

no_wpe1 <- rgb(173, 156, 203, maxColorValue = 255)
no_wpe2 <- rgb(139, 11, 176, maxColorValue = 255)
no_wpe3 <- rgb(56, 14, 97, maxColorValue = 255)
no_wpe4 <- rgb(251, 201, 199, maxColorValue = 255)

b_size <- 1
d_size <- 3
  
####################################################################################################################################
# graticule generator

grat <- function(
  lats = c(-89, -85, -80, -75, -70, -65, -60), 
  lon_delta = 5, 
  smooth_val = .1) {
  i <- 1
  lon_lines <- list()  
  for (lon in seq(0, 359, lon_delta)) {
    min_lat <- base::min(lats)
    max_lat <- base::max(lats)
    lon_lines[[i]] <- st_as_sf(st_sfc(st_linestring(matrix(c(lon, lon, min_lat, max_lat), ncol = 2)), crs = 4326)) %>%
      mutate(label = lon)
    i <- i + 1
  }
  i <- 1
  lat_lines <- list()  
  lon_pts <- seq(0, 360, smooth_val)
  for (lat in lats) {
    m <- matrix(c(lon_pts, rep(lat, length(lon_pts))), ncol = 2)
    lat_lines[[i]] <- st_as_sf(st_sfc(st_linestring(m), crs = 4326))  %>%
      mutate(label = lat)
    i <- i + 1
  }
  x <- do.call(what = sf:::rbind.sf, lon_lines)
  y <- do.call(what = sf:::rbind.sf, lat_lines)
  out <- rbind(x, y)
  return(out)
}
out <- grat()

####################################################################################################################################
# Ross Sea

ant_coast <- sf::st_read("~/Downloads/add_coastline_medium_res_polygon_v7_5/add_coastline_medium_res_polygon_v7_5.shp") %>%
  sf::st_simplify(preserveTopology = TRUE, dTolerance = 1000) %>%
  mutate(surface = ifelse(surface == "land", "land", "ice"))
focal_sites <- site_list %>%
  dplyr::filter(ccamlr_id %in% c("88.1")) %>%
  dplyr::select(site_id, wpe) %>%
  distinct() %>%
  left_join(sites_sf, by = "site_id") %>%
  st_as_sf()
sea_block <- focal_sites %>%
  sf::st_transform(3031) %>%
  sf::st_buffer(30000) %>%
  sf::st_bbox() %>%
  st_as_sfc()

land_sf <- sf::st_intersection(ant_coast, sea_block)
out <- grat(lats = seq(-90, -60, 1), lon_delta = 5, smooth_val = .1) 
grat_overlay <- sf::st_intersection(out, sea_block %>% st_transform(4326)) %>%
  dplyr::filter(label != -72)
start <- c(170, -74)
cell <- start + c(0, 0, 0, -.53425, 1.125, -.53425, 1.125, 0, 0, 0)
cesm2 <- st_sfc(st_polygon(list(matrix(cell, ncol = 2, byrow = TRUE))), crs = 4326)

ggplot() +
  geom_sf(data = sea_block, fill = "#A1D6E2", alpha = .4, color = "yellow") + 
  geom_sf(data = land_sf, aes(fill = surface), color = "black", size = 1.25/.pt) + 
  scale_fill_manual(values = c("white", "#EBEBEB")) +
  new_scale_fill() +
  geom_sf(data = grat_overlay, color = "black", size = .075/.pt) + 
  geom_sf(data = focal_sites %>% dplyr::filter(!(site_id %in% c("CRZE", "CRZW", "BRDM", "BRDN", "BRDS", "ROYD"))), aes(fill = wpe), color = "black", size = d_size, pch = 21, alpha = 1) +
  scale_fill_distiller(palette = "YlGn", na.value = no_wpe3, limits = c(min_wpe, max_wpe)) +
  geom_sf(data = cesm2, color = "black", fill = NA, size = 2/.pt) + 
  geom_sf(data = sea_block, color = ross_border, size = b_size, fill = NA) + 
  coord_sf(datum = 3031) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#  annotation_scale(location = "bl", pad_y = unit(1, "cm"), pad_x = unit(3.6, "cm")) 
  annotation_scale(location = "bl", text_cex = .5, height = unit(0.1, "cm"), pad_y = unit(.6, "cm"), pad_x = unit(1.95, "cm")) 
#ggsave(paste0("maps/ross_", b_size, "_", d_size, ".pdf"))
ggsave(paste0("maps/ross_actual_", b_size, "_", d_size, ".pdf"), width = 4.0687 , height = 9.1205, units = "cm")
ross <- sea_block

################################################################################
# Ross Island

ant_coast <- sf::st_read("~/Downloads/add_coastline_medium_res_polygon_v7_5/add_coastline_medium_res_polygon_v7_5.shp") %>%
  mutate(surface = ifelse(surface == "land", "land", "ice"))
focal_sites <- r %>%
  dplyr::filter(site_id %in% c("CRZE", "CRZW", "BRDM", "BRDN", "BRDS", "ROYD")) %>%
  dplyr::select(site_id, wpe) %>%
  distinct() %>%
  left_join(sites_sf, by = "site_id") %>%
  st_as_sf()
sea_block <- focal_sites %>%
  sf::st_transform(3031) %>%
  sf::st_buffer(8500) %>%
  sf::st_bbox() %>%
  st_as_sfc() 
land_sf <- sf::st_intersection(ant_coast, sea_block)

ggplot() +
  geom_sf(data = sea_block, fill = "#A1D6E2", alpha = .4) + 
  geom_sf(data = land_sf, aes(fill = surface), color = "black", size = 1.25/.pt) + 
  scale_fill_manual(values = c("white", "#EBEBEB")) +
  new_scale_fill() +
  geom_sf(data = focal_sites, aes(fill = wpe), color = "black", size = 2.5, pch = 21, alpha = 1) +
  scale_fill_distiller(palette = "YlGn", na.value = no_wpe3, limits = c(min_wpe, max_wpe)) +
  geom_sf(data = sea_block, color = ross_border, size = b_size, fill = NA) + 
  coord_sf(datum = 3031) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  #annotation_scale(location = "bl", pad_y = unit(.8, "cm"), pad_x = unit(1.1, "cm")) 
  annotation_scale(location = "bl", pad_y = unit(.3, "cm"), pad_x = unit(.4, "cm"), height = unit(.15, "cm")) 
ggsave(paste0("maps/ross_island_actual_", b_size, "_", d_size,  ".pdf"), width = 5.7385 , height = 3.6219, units = "cm")

################################################################################
# EA

ant_coast <- sf::st_read("~/Downloads/add_coastline_medium_res_polygon_v7_5/add_coastline_medium_res_polygon_v7_5.shp")
focal_sites <- r %>%
  dplyr::filter(ccamlr_id %in% c("58.4.2")) %>%
  dplyr::select(site_id, wpe) %>%
  distinct() %>%
  left_join(sites_sf, by = "site_id") %>%
  st_as_sf()
sea_block <- focal_sites %>%
  sf::st_transform(3031) %>%
  sf::st_buffer(8000) %>%
  sf::st_bbox() %>%
  st_as_sfc() 
land_sf <- sf::st_intersection(ant_coast, sea_block)
out <- grat(lats = seq(-90, -60, .5), lon_delta = .5, smooth_val = .1) 
grat_overlay <- sf::st_intersection(out, sea_block %>% st_transform(4326))

ggplot() +
  geom_sf(data = sea_block, fill = "#A1D6E2", alpha = .4) + 
  geom_sf(data = land_sf, aes(fill = surface), color = "black", size = 1.25/.pt) + 
  scale_fill_manual(values = c("white", "#EBEBEB")) +
  new_scale_fill() +
  geom_sf(data = grat_overlay, color = "black", size = .075/.pt) + 
  geom_sf(data = focal_sites, color = "black", fill = no_wpe3, size = d_size, pch = 21, alpha = 1) +
  geom_sf(data = sea_block, color = ea_border, size = b_size, fill = NA) + 
  coord_sf(datum = 3031) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#  annotation_scale(location = "br", pad_y = unit(1.1, "cm"), pad_x = unit(.9, "cm")) 
annotation_scale(location = "br", pad_y = unit(.6, "cm"), pad_x = unit(.5, "cm"), height = unit(.1, "cm")) 
ggsave(paste0("maps/ea_actual_", b_size, "_", d_size,  ".pdf"), width = 5.1842, height = 7.6753, unit= "cm")
ea <- sea_block

# ggplot() +
#   geom_sf(data = sea_block, fill = "#A1D6E2", alpha = .4) + 
#   geom_sf(data = land_sf, aes(fill = surface), color = "black", size = 1.25/.pt) + 
#   geom_sf_label(data = grat_overlay, aes(label = label), nudge_y = 1000) +
#   geom_sf(data = grat_overlay, color = "black", size = .075/.pt) + 
#   geom_sf_label(data = focal_sites, aes(label = site_id), nudge_y = 500) +
#   geom_sf(data = focal_sites, color = "red", size = 2, alpha = .75) +
#   coord_sf(datum = 3031) +
#   scale_fill_manual(values = c("white", "#EBEBEB")) +
#   theme_minimal() +
#   theme(
#     axis.title.x = element_blank(), 
#     axis.text.x = element_blank(), 
#     axis.ticks = element_blank(), 
#     axis.title.y = element_blank(), 
#     axis.text.y = element_blank(),
#     panel.border = element_blank(), 
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()) +
#   theme(legend.position = "none") +
#   theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#   annotation_scale() 
# ggsave("eap_labeled.pdf")

################################################################################
# AP

ant_coast <- sf::st_read("~/Downloads/add_coastline_medium_res_polygon_v7_5/add_coastline_medium_res_polygon_v7_5.shp") %>%
  sf::st_simplify(preserveTopology = TRUE, dTolerance = 300) %>%
  mutate(surface = ifelse(surface == "land", "land", "ice"))
focal_sites <- r %>%
  dplyr::filter(ccamlr_id %in% c("48.1", "48.2")) %>%
  dplyr::select(site_id, wpe) %>%
  distinct() %>%
  left_join(sites_sf, by = "site_id") %>%
  st_as_sf()
sea_block <- focal_sites %>%
  sf::st_transform(3031) %>%
  sf::st_buffer(20000) %>%
  sf::st_bbox() %>%
  st_as_sfc() 
land_sf <- sf::st_intersection(ant_coast, sea_block)
out <- grat(lats = seq(-90, -60, 1), lon_delta = 5, smooth_val = .1) 
grat_overlay <- sf::st_intersection(out, sea_block %>% st_transform(4326))

ggplot() +
  geom_sf(data = sea_block, fill = "#A1D6E2", alpha = .4) + 
  geom_sf(data = land_sf, aes(fill = surface), color = "black", size = 1.25/.pt) + 
  scale_fill_manual(values = c("#EBEBEB")) +
  new_scale_fill() +
  geom_sf(data = grat_overlay, color = "black", size = .075/.pt) + 
  geom_sf(data = focal_sites %>% dplyr::filter(!(site_id %in% c("CRZE", "CRZW", "BRDM", "BRDN", "BRDS", "ROYD"))), aes(fill = wpe), color = "black", size = d_size, pch = 21, alpha = 1) +
  scale_fill_distiller(palette = "YlGn", na.value = no_wpe3, limits = c(min_wpe, max_wpe)) +
  geom_sf(data = sea_block, color = ap_border, size = b_size, fill = NA) + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#  annotation_scale(location = "bl", pad_y = unit(1, "cm"), pad_x = unit(.65, "cm")) 
    annotation_scale(location = "bl", pad_y = unit(.7, "cm"), pad_x = unit(.45, "cm"), height = unit(.1, "cm")) 
  ggsave(paste0("maps/ap_actual", b_size, "_", d_size, ".pdf"), width = 4.5829, height = 9.5398, unit = "cm")
ap <- sea_block

# ggplot() +
#   geom_sf(data = sea_block, fill = "#A1D6E2", alpha = .4) + 
#   geom_sf(data = land_sf, aes(fill = surface), color = "black", size = 1.25/.pt) + 
#   geom_sf_label(data = grat_overlay, aes(label = label), nudge_y = 1000) +
#   geom_sf(data = grat_overlay, color = "black", size = .075/.pt) + 
#   geom_sf_label(data = focal_sites, aes(label = site_id), nudge_y = 5000) +
#   geom_sf(data = focal_sites, color = "red", size = 2, alpha = .75) +
#   coord_sf(datum = 3031) +
#   scale_fill_manual(values = c("white", "#EBEBEB")) +
#   theme_minimal() +
#   theme(
#     axis.title.x = element_blank(), 
#     axis.text.x = element_blank(), 
#     axis.ticks = element_blank(), 
#     axis.title.y = element_blank(), 
#     axis.text.y = element_blank(),
#     panel.border = element_blank(), 
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()) +
#   theme(legend.position = "none") +
#   theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
#   annotation_scale() 
# ggsave("ap_labeled.pdf")
################################################################################
# main map

ant_coast <- sf::st_read("~/Downloads/add_coastline_medium_res_polygon_v7_5/add_coastline_medium_res_polygon_v7_5.shp") %>%
  sf::st_simplify(preserveTopology = TRUE, dTolerance = 10000) %>%
  dplyr::filter(surface == "land")
grat_overlay <- grat(lats = c(-89, -80, -70, -60), lon_delta = 30, smooth_val = .1) 

ggplot() +
  geom_sf(data = grat_overlay %>% dplyr::filter(label == -60) %>% st_cast("POLYGON") %>% st_transform(3031), fill = "#A1D6E2", alpha = .4) + 
  geom_sf(data = ant_coast, fill = "#EBEBEB", color = "black", size = 1.25/.pt) + 
  geom_sf(data = grat_overlay, color = "black", size = .5/.pt) + 
  geom_sf(data = ross %>% st_cast("LINESTRING"), color = ross_border, size = 1) + 
  geom_sf(data = ea %>% st_cast("LINESTRING"), color = ea_border, size = 1) + 
  geom_sf(data = ap %>% st_cast("LINESTRING"), fill = "yellow", color = ap_border, size = 1) + 
  geom_sf(data = sites_sf %>% dplyr::filter(site_id == "PGEO"), color = pgeo_dot, size = 2) + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    panel.border = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin = grid::unit(c(0,0,0,0), "mm")) +
  annotation_scale(location = "bl", pad_y = unit(.75, "cm"), pad_x = unit(3, "cm"), height = unit(.1, "cm")) 
ggsave("maps/antarctica_actual.pdf", width = 9.2787, height = 8.5843, units = "cm")

focal_sites <- site_list %>%
  dplyr::select(site_id, wpe) %>%
  distinct() %>%
  left_join(sites_sf, by = "site_id") %>%
  st_as_sf()

ggplot() +
  geom_sf(data = focal_sites, aes(fill = wpe), color = "black", size = 5, pch = 21, alpha = 1) +
  scale_fill_distiller(palette = "YlGn", na.value = no_wpe3, limits = c(min_wpe, max_wpe)) +
  theme(legend.position = "bottom")
ggsave("maps/just_the_legend_please_and_thank_you.pdf")

################################################################################
# 
# 
# 
# grat_ant <- sf::st_graticule(
#   ant_coast,
#   lat = c(-89, -80, -70), 
#   lon = seq(0, 360, 30))
# 
# ggplot() +
#   geom_sf(data = ant_coast, fill = "#f0f4eb", color = "black", size = 1.25/.pt) + 
#   geom_sf(data = grat_overlay, color = "gray70", size = 2/.pt) + 
#   #geom_sf(data = ross_sea %>% st_cast("LINESTRING"), color = "orange", size = 1) + 
#   #geom_sf(data = eap_sea %>% st_cast("LINESTRING"), color = "purple", size = 1) + 
#   #geom_sf(data = ap_sea %>% st_cast("LINESTRING"), color = "green", size = 1) + 
#   #geom_sf(data = sites_sf %>% dplyr::filter(site_id == "PGEO"), color = "yellow", size = 2) + 
#   coord_sf(datum = NULL) +
#   scale_fill_manual(values = c("white", "gray90")) +
#   theme_minimal() +
#   theme(
#     axis.title.x = element_blank(), 
#     axis.text.x = element_blank(), 
#     axis.ticks = element_blank(), 
#     axis.title.y = element_blank(), 
#     axis.text.y = element_blank(),
#     panel.border = element_blank(), 
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()) +
#   theme(legend.position = "none") +
#   theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) 
# 
# ggsave("~/Desktop/antarctica.pdf", width = 3, height = 3)
# 
# ################################################################################
# # main map
# 
# library(tidyverse)
# library(ggrepel)
# library(ggtext)
# library(systemfonts)
# 
# 
# theme_set(theme_minimal())
# theme_set(theme_minimal(base_family = "Helvetica", base_size = 16))
# theme_update(
#   axis.text.x = element_markdown(margin = margin(rep(0, 4))),
#   axis.text.x.top = element_markdown(margin = margin(rep(0, 4))),
#   axis.text.y = element_text(family = "Helvetica", face = "bold", 
#                              size = 14, vjust = 0),
#   axis.ticks = element_blank(),
#   #axis.ticks.length.x = unit(.4, "lines"),
#   axis.title = element_blank(),
#   legend.position = "none",
#   panel.grid = element_blank(),
#   plot.margin = margin(20, 40, 20, 40),
# #  plot.background = element_rect(fill = "#f0f4eb", color = "#f0f4eb"),
# #  panel.background = element_rect(fill = "#f0f4eb", color = "#f0f4eb"),
#   plot.background = element_rect(fill = "white", color = "white"),
#   panel.background = element_rect(fill = "white", color = "white"),
# plot.title = element_text(family = "Lora", color = "#365e25", 
#                             size = 28, face = "bold",
#                             margin = margin(t = 15)),
#   plot.subtitle = element_markdown(color = "#365e25", size = 14,
#                                    lineheight = 1.35,
#                                    margin = margin(t = 15, b = 30)),
#   plot.title.position = "plot",
#   plot.caption = element_text(color = "#56963c", size = 10,
#                               margin = margin(t = 25))
# )
# sss
# x <- r %>% dplyr::filter(ccamlr_id %in%  c("58.4.1", "58.4.2", "48.1"))
# x <- r %>% dplyr::filter(site_id == "CMID")
# library(ggh4x)
# ggplot(data = x, aes(x = season, y = r)) +
#   ylim(-1, 1) +
#   geom_segment(
#     data = tibble(y = seq(-.75, .75, by = .75), x1 = 1980, x2 = 2020),
#     aes(x = x1, xend = x2, y = y, yend = y),
#     inherit.aes = FALSE,
#     color = "grey91",
#     size = .5
#   ) +  
#   geom_segment(
#     data = tibble(y = 0, x1 = 1980, x2 = 2020),
#     aes(x = x1, xend = x2, y = y, yend = y),
#     inherit.aes = FALSE,
#     color = "grey71",
#     size = .5) +
#   geom_line(color = "#228b22", size = .5) +
#   geom_point(shape = 16, color = "orange", size = 1.5) + 
#   geom_point(data = x, shape = 16, color = "#228b22", size = 1.5) + 
#   theme(axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))
# #  facet_grid(rows = vars(site_id)) +
# #  force_panelsizes(rows = 1, cols = 4, TRUE) 
# h <- 1.6
# ggsave("~/Desktop/brdm.pdf", width = 1.25 * h, height = h)
# 
# 2.5/2
# 
#   
#   facet_grid(rows = vars(site_id)) +
#   force_panelsizes(rows = c(1,5), cols = c(1,5), TRUE)
# 
# ggsave("~/Desktop/88.1.pdf", width = 4, height = 8)
# 
# 
# library(grid)
# gt = ggplot_gtable(ggplot_build(g))
# grid.draw(gt)
# #gt$widths[5] = .5*gt$widths[5]
# gt$heights[7] = 2*gt$heights[7]
# for(i in seq(7,27, 2)) {gt$heights[i] = 2*gt$heights[i]}
# grid.draw(gt)
# 
# 
# 
# png("~/Desktop/881.png")
# grid.draw(gt)
# dev.off()
# 
# ggsave("~/Desktop/88.1.pdf", width = 4, height = 8)
# 
# 
# scale_y_continuous(
#   expand = c(0, 0),
#   breaks = seq(-1, 1, by = .25)) +     
#   
# x <- r %>% dplyr::filter(ccamlr_id == "88.1")
# 
# ggplot(r %>% dplyr::filter(ccamlr_id == "88.1"), aes(x = season, y = site_id)) +
#   geom_segment(
#     data = tibble(y = seq(-1, 1, by = .25), x1 = 2000, x2 = 2020),
#     aes(x = x1, xend = x2, y = y, yend = y),
#     inherit.aes = FALSE,
#     color = "grey91",
#     size = .6) +
#   geom_vline(
#     xintercept = seq(1980, 2020, by = 5),
#     color = "grey91", 
#     size = .6
#   ) +  
#   
#   
#   
#   ## colored lines
#   geom_line(aes(x = season, y = r), size = .9) 
# 
# 
# ggplot(df_mac_indexed_2008 %>% filter(group != "other"), 
#        aes(year, price_rel, group = iso_a3)) + 
#   ## geometric annotations
#   geom_vline(
#     xintercept = seq(2000, 2020, by = 5),
#     color = "grey91", 
#     size = .6
#   ) +
#   geom_segment(
#     data = tibble(y = seq(-4, 3, by = 1), x1 = 2000, x2 = 2020),
#     aes(x = x1, xend = x2, y = y, yend = y),
#     inherit.aes = FALSE,
#     color = "grey91",
#     size = .6
#   ) +
#   geom_segment(
#     data = tibble(y = 0, x1 = 2000, x2 = 2020),
#     aes(x = x1, xend = x2, y = y, yend = y),
#     inherit.aes = FALSE,
#     color = "grey60",
#     size = .8
#   ) +
#   geom_vline(
#     aes(xintercept = ref_year), 
#     color = "grey40",
#     linetype = "dotted",
#     size = .8
#   ) +
#   ## grey lines
#   geom_line(
#     data = df_mac_indexed_2008 %>% filter(group == "other"),
#     color = "grey75",
#     size = .6,
#     alpha = .5
#   ) +
#   ## colored lines
#   geom_line(
#     aes(color = group),
#     size = .9
#   ) +
#   ## text annotations
#   annotate(
#     "text", x = 2008.15, y = -3.35, 
#     label = "2008",
#     family = "Avenir Next Condensed",
#     size = 8,
#     color = "grey40",
#     hjust = 0
#   ) +
#   geom_text_repel(
#     aes(color = group,
#         label = name_lab),
#     family = "Avenir Next Condensed",
#     fontface = "bold",
#     size = 8,
#     direction = "y",
#     xlim = c(2020.8, NA),
#     hjust = 0,
#     segment.size = .7,
#     segment.alpha = .5,
#     segment.linetype = "dotted",
#     box.padding = .4,
#     segment.curvature = -0.1,
#     segment.ncp = 3,
#     segment.angle = 20
#   ) +
#   ## coordinate system + scales
#   coord_cartesian(
#     clip = "off",
#     ylim = c(-4, 3)
#   ) +
#   scale_x_continuous(
#     expand = c(0, 0),
#     limits = c(2000, 2023.5), 
#     breaks = seq(2000, 2020, by = 5)
#   ) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     breaks = seq(-4, 3, by = 1),
#     labels = glue::glue("{format(seq(-4, 3, by = 1), nsmall = 2)}$")
#   ) +
#   scale_color_manual(
#     values = c(rcartocolor::carto_pal(n = n, name = "Bold")[1:n-1], "grey50")
#   ) +
#   labs(
#     title = "Compared to the financial crisis in 2008, how much more or less do you have to pay for a Big Mac today?",
#     subtitle = "The <i>index chart</i> visualizes the price changes (in USD) of a Big Mac based on a 2008 as index year. The <b>Big Mac Index</b> is published by The Economist as an informal way to provide a test of the<br>extent to which market exchange rates result in goods costing the same in different countries. It <i>seeks to make exchange-rate theory a bit more digestible</i> and takes its name from the Big Mac,<br>a hamburger sold at McDonald's restaurants.",
#     caption = "Visualization by Cédric Scherer  •  Data by The Economist  •  The index chart shows the 27 countries that provide Big mac prices for all years from 2000 to 2020. In case a country was reported twice per year, the mean value was visualized."
#   ) +
#   theme(plot.subtitle = element_markdown(size = 20))
# 
# 
# 
# 
# 
# ggplot(r %>% filter(ccamlr_id == "88.1"), 
#        aes(season, r, group = site_id)) + 
#   ## geometric annotations
#   geom_vline(
#     xintercept = seq(1980, 2020, by = 5),
#     color = "grey91", 
#     size = .6
#   ) +
#   geom_segment(
#     data = tibble(y = seq(-1, 1, by = .25), x1 = 1980, x2 = 2020),
#     aes(x = x1, xend = x2, y = y, yend = y),
#     inherit.aes = FALSE,
#     color = "grey91",
#     size = .6
#   ) +
#   geom_segment(
#     data = tibble(y = 0, x1 = 1980, x2 = 2020),
#     aes(x = x1, xend = x2, y = y, yend = y),
#     inherit.aes = FALSE,
#     color = "grey60",
#     size = .8
#   ) +
#   ## colored lines
#   geom_line(
#     aes(color = site_id),
#     size = .9
#   ) +
#   ## text annotations
#   annotate(
#     "text", x = 2008.15, y = -3.35, 
#     label = "2008",
#     family = "Avenir Next Condensed",
#     size = 8,
#     color = "grey40",
#     hjust = 0
#   )  +
#   ## coordinate system + scales
#   coord_cartesian(
#     clip = "off",
#     ylim = c(-1, 1)
#   ) +
#   scale_x_continuous(
#     expand = c(0, 0),
#     limits = c(2000, 2023.5), 
#     breaks = seq(2000, 2020, by = 5)
#   ) +
#   scale_y_continuous(
#     expand = c(0, 0),
#     breaks = seq(-1, 1, by = .25),
#     labels = seq(-1, 1, by = .25)
#   ) +
#   scale_color_manual(
#     values = c(rcartocolor::carto_pal(n = n, name = "Bold")[1:11], "grey50")
#   ) +
#   labs(
#     title = "Compared to the financial crisis in 2008, how much more or less do you have to pay for a Big Mac today?",
#     subtitle = "The <i>index chart</i> visualizes the price changes (in USD) of a Big Mac based on a 2008 as index year. The <b>Big Mac Index</b> is published by The Economist as an informal way to provide a test of the<br>extent to which market exchange rates result in goods costing the same in different countries. It <i>seeks to make exchange-rate theory a bit more digestible</i> and takes its name from the Big Mac,<br>a hamburger sold at McDonald's restaurants.",
#     caption = "Visualization by Cédric Scherer  •  Data by The Economist  •  The index chart shows the 27 countries that provide Big mac prices for all years from 2000 to 2020. In case a country was reported twice per year, the mean value was visualized."
#   ) +
#   theme(plot.subtitle = element_markdown(size = 20))

