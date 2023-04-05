library(tidyverse)
library(patchwork)
library(ggrepel)
library(ggalt)
library(mapppdr)
library(hrbrthemes)

# define regional colors to match Fig 2
ea_border <- rgb(153, 20, 36, maxColorValue = 255)
ross_border <- rgb(18, 145, 209, maxColorValue = 255)
ap_border <- rgb(27, 39, 110, maxColorValue = 255)
pgeo_dot <- "#5DB86A"
l_size <- 2

data_fig4 <- readRDS("google_drive/data_fig4.rds")

m2 <- data_fig4$m2
idx_N <- data_fig4$idx_N
site_names <- data_fig4$site_names
work <- m2[1:14,] %>%
  mutate(site_id = rownames(.)) %>%
  mutate(
    fill_val = case_when(
      CCAMLR == "48.1" ~ ap_border,
      CCAMLR == "88.1" ~ ross_border,
      CCAMLR == "58.4.2" ~ ea_border,
      CCAMLR == "58.4.1" ~ pgeo_dot))

set.seed(42)
g1 <- ggplot(work, aes(x = V1, y = V2, label = site_id)) +
  geom_text_repel(size = 2.6, box.padding = .7, max.overlaps = 9, segment.color = "gray60") +
  geom_point(aes(fill = fill_val), pch = 21, size = 4, alpha = 0.75, col = "black") + 
  scale_fill_manual(values = c(ross_border, ea_border, pgeo_dot), labels = c("Ross Sea", "Antarctic Peninsula", " Pointe Géologie")) +
  scale_y_continuous(limits = c(.05, .35), breaks = c(.1, .2, .3)) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = c(.31, .745),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "gray90"),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    text = element_text(size = 8),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()) +
  labs(x = "Weighted Permutation Entropy (WPE)", y = "Forecast RMSE")
g1

work <- m2[15:28,] %>%
  mutate(site_id = substr(rownames(.), 1, 4)) %>%
  mutate(
    fill_val = case_when(
      CCAMLR == "48.1" ~ ap_border,
      CCAMLR == "88.1" ~ ross_border,
      CCAMLR == "58.4.2" ~ ea_border,
      CCAMLR == "58.4.1" ~ pgeo_dot))

set.seed(42)
g2 <- ggplot(work, aes(x = V1, y = V2, label = site_id)) +
  geom_text_repel(size = 2.5, box.padding = .7, segment.color = "gray60") +
  geom_point(aes(fill = fill_val), pch = 21, size = 4, alpha = 0.75, col = "black") + 
  scale_fill_manual(values = c(ross_border, ea_border, pgeo_dot)) +
  scale_y_continuous(limits = c(.05, .35), breaks = c(.1, .2, .3)) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 8),
    text = element_text(size = 8),
    panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank()) +
  labs(x = "Weighted Permutation Entropy (WPE)", y = "Hindcast RMSE")
g2

(g1 + g2) + plot_annotation(tag_levels = 'A')
ggsave("fig4.pdf", width = 18, height = 9, units = "cm")


