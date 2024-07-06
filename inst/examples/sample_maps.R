# === === === === === === === === === === === === === === === === === === === ===

# create sample maps

# === === === === === === === === === === === === === === === === === === === ===

# dependencies ####

## packages
devtools::load_all()
library(dplyr)
library(ggplot2)
library(tigris)

## define theme
map_theme <- function(base_size = 28, base_family = "sans", legend_size = 1.5){

  (ggthemes::theme_foundation(base_size = base_size, base_family = base_family) +
     theme(
       line = element_line(colour = "black"),
       rect = element_rect(fill = NA, linetype = 1, colour = '#898989'),
       text = element_text(colour = '#3C3C3C'),
       axis.line = element_blank(),
       axis.text = element_blank(),
       axis.ticks = element_blank(),
       axis.title = element_blank(),
       panel.background = element_rect(fill = '#FFFFFF', color = NA),
       panel.border = element_blank(),
       panel.grid = element_blank(),
       panel.spacing = unit(0, "lines"),
       plot.background = element_rect(fill = '#FFFFFF', color = NA),
       legend.justification = c(0, 0),
       legend.background = element_rect(fill = '#FFFFFF'),
       legend.key = element_rect(fill = '#FFFFFF'),
       legend.key.size = unit(legend_size, units="cm"),
       legend.position = "right",
       legend.direction = "vertical",
       legend.box = "vertical",
       plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
       plot.caption = element_text(hjust = "0")))

}

# === === === === === === === === === === === === === === === === === === === ===

# load counties ####

cook <- counties(state = 17, cb = TRUE) %>%
  filter(GEOID == "17031") %>%
  select(GEOID, NAMELSAD)

stl <- counties(state = 29, cb = TRUE) %>%
  filter(GEOID %in% c("29189", "29510")) %>%
  select(GEOID, NAMELSAD) %>%
  mutate(NAMELSAD = ifelse(GEOID == "29510", "St. Louis City", NAMELSAD))

# === === === === === === === === === === === === === === === === === === === ===

# load ZIP codes ####

zcta5_chi <- zi_get_geometry(year = 2020, starts_with = "60", cb = TRUE)

zcta5_607 <- zcta5_chi %>%
  mutate(zcta3 = substr(GEOID, 1, 3)) %>%
  filter(zcta3 == "607")

zcta5_chi_master <- sf::st_intersection(zcta5_chi, cook)
zcta5_chi <- filter(zcta5_chi, GEOID %in% zcta5_chi_master$GEOID)
rm(zcta5_chi_master)

zcta5_stl <- zi_get_geometry(year = 2020, starts_with = "63", cb = TRUE)

zcta5_631 <- zcta5_stl %>%
  mutate(zcta3 = substr(GEOID, 1, 3)) %>%
  filter(zcta3 == "631")

zcta3_631 <- zcta5_631 %>%
  group_by(zcta3) %>%
  summarise()

zcta5_stl_master <- sf::st_intersection(zcta5_stl, stl)
zcta5_stl <- filter(zcta5_stl, GEOID %in% zcta5_stl_master$GEOID)
rm(zcta5_stl_master)

# === === === === === === === === === === === === === === === === === === === ===

# load ZIP codes ####

p <- ggplot() +
  geom_sf(data = zcta5_chi, fill = NA, color = "#cccccc", lwd = .4) +
  geom_sf(data = zcta5_607, lwd = .8) +
  geom_sf(data = cook, fill = NA, lwd = 1.2) +
  ggsflabel::geom_sf_label_repel(data = zcta5_607, mapping = aes(label = GEOID),
                                 nudge_x = c(-.24, -.25, .18, .22),
                                 nudge_y = c(-.025, -.05, -.03, .03),
                                 size = 6) +
  map_theme()


ggsave(filename = "inst/extdata/cook_county.png", plot = p,
       width = 1024 * 0.352778,
       height = 768 * 0.352778,
       units = "mm", dpi = 300)

p <- ggplot() +
  geom_sf(data = zcta5_stl, fill = NA, color = "#cccccc", lwd = .4) +
  geom_sf(data = zcta5_631, color = "#cccccc", lwd = .4) +
  geom_sf(data = stl, fill = NA, lwd = 1.2) +
  geom_sf(data = zcta3_631, fill = NA, lwd = 1.2, color = "red") +
  map_theme()

ggsave(filename = "inst/extdata/stl_county.png", plot = p,
       width = 1024 * 0.352778,
       height = 768 * 0.352778,
       units = "mm", dpi = 300)
s
