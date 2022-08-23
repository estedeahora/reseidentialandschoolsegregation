# Packages

library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(ggpubr)

# Urban habitat Map (left)

p1 <- CARTO$BP |>
  mutate(TIPO_RED = case_when(TIPO_ASENT %in% c("Barrio Urbanizado",
                                                "Barrios municipales",
                                                "Conjunto Habitacional") ~ "Social housing",

                              TIPO_ASENT %in% c("Villas", "Asentamientos precarios",
                                                "Nùcleos Habitacionales Transitorios") ~ "Urban informal\nneighborhoods",
                              T ~ "Others"
                              )) |>
  ggplot() +
  geom_sf(data = CARTO$BARRIO, fill = NA) +
  geom_sf(aes(fill = TIPO_RED), color = NA) +
  scale_fill_brewer("Urban habitat", palette = "Set2") +
  annotation_scale(location = "br") +
  annotation_north_arrow(width = unit(1, "cm"), aes(location = "tr")) +
  guides(fill = guide_legend(title.position="top"))+
  theme_void() +
  theme(legend.position = "bottom")

# M Index's Map (right)

p2 <- CARTO$CABA |>
  filter(!is.na(ls)) |>
  select(ID, M = ls, p) |>
  ggplot() +
  geom_sf(data = CARTO$COMUNA) +
  geom_sf(aes(fill = M), color = NA, alpha = 0.6) +
  scale_fill_gradient2("Mutual Information Index (M)",
                       high = "darkred",
                       mid = "darkolivegreen2",
                       low = "deepskyblue",
                       midpoint = SEGRE$tab_indic$Residential[3]) +
  theme_void() +
  guides(fill = guide_colourbar(title.position="top"))+
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.key.width = unit(1.9,"line"),
        legend.direction = "horizontal")

# Save "Residential-Segreg.png"

ggsave(filename =  here::here("analysis/figures/Residential-Segreg.png"),
       plot = ggarrange(p1, p2), width = 25, height = 20, units = "cm")

rm(p1, p2)
