# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)

library(ggplot2)
library(ggsci)
library(ggpubr)
library(grid)

library(flextable)

library(sf)

library(lpSolve)

# library(magick)

# Toy data --------------------------------------------------------------------

# Residential area db
pol_base1 <- list(rbind(c(0,0), c(0, -100), c(100, -100), c(200, 0), c(0, 0) ) )
pol_base2 <- list(rbind(c(0,0), c(0, 100), c(100, 100), c(200, 0), c(0, 0) ) )

resid <- st_sfc(st_polygon(list(pol_base1[[1]] * (-1))),
                st_polygon(pol_base2),
                st_polygon(list(pol_base2[[1]] * (-1))),
                st_polygon(pol_base1)) |>
  st_as_sf() |>
  mutate(resid = paste0("r", 1:4),
         POPULATION = c(100, 200, 120, 180),
         FEATURE_A_prop = c(0.6, 0.9, 0.4, 0.1),
         FEATURE_A = POPULATION * FEATURE_A_prop,
         FEATURE_B = POPULATION * (1-FEATURE_A_prop)
         )

rm(pol_base1, pol_base2)

# School db
school <- st_sfc(st_point(c(60, 80)),
                 st_point(c(-40, 30)),
                 st_point(c(-30, -40)))|>
  st_as_sf() |>
  mutate(school = paste0("s", 1:3),
         SIZE = c(330, 200, 100))

# sfc_LINESTRING(net)

LINE_Between <- full_join(data.frame(geo1 = st_centroid(resid)$x,
                                     resid = paste0("r", 1:4),
                                     id = NA),
                          data.frame(geo1 = school$x,
                                     school = paste0("s", 1:3),
                                     id = NA),
                          by = "id") |>
  st_as_sf() |>
  select(-id)

LINE_Between <- st_sfc(
  mapply(function(a, b){
    st_cast(st_union(a, b),"LINESTRING")
  },
  LINE_Between$geometry.x, LINE_Between$geometry.y,
  SIMPLIFY = FALSE)) |>
  cbind(st_drop_geometry(LINE_Between[c("resid", "school" )])) |>
  st_as_sf()

# LP Model ------------------------------------------------------------

# Cost Matrix
M_COST <- st_distance(st_centroid(resid), school )
rownames(M_COST) <- resid$resid
colnames(M_COST) <- school$school

# Signs restriction
if(sum(resid$POPULATION) <= sum(school$SIZE)){
  row.signs <- rep("=", nrow(M_COST))
  col.signs <- rep("<=", ncol(M_COST))
}else{
  row.signs <- rep("<=", nrow(M_COST))
  col.signs <- rep("=", ncol(M_COST))
}

# values restrictions
row.rhs <- resid$POPULATION
col.rhs <- school$SIZE

# Resolution lp model
ALLOCATION_MODEL <- lp.transport(M_COST, "min", presolve = 1,
                                 row.signs, row.rhs, col.signs, col.rhs)

rownames(ALLOCATION_MODEL$solution) <- rownames(M_COST)
colnames(ALLOCATION_MODEL$solution) <- colnames(M_COST)

rm(row.rhs, col.rhs, M_COST, row.signs, col.signs)

# Student and Feature distribution -------------------------

M_ALLOCATION <- ALLOCATION_MODEL$solution |>
  as_tibble(rownames = "resid") |>
  pivot_longer(cols = s1:s3, names_to = "school") |>
  left_join(resid |>
              st_drop_geometry() |>
              select(resid, FEATURE_A_prop),
            by = "resid") |>
  mutate(FEATURE_A = value * FEATURE_A_prop,
         FEATURE_B = value * (1- FEATURE_A_prop))

LINE_Between <- LINE_Between |>
  left_join(M_ALLOCATION, by = c("resid", "school")) |>
  filter(value > 0) |>
  mutate(label = paste0(value, "\n(", FEATURE_A, "/", FEATURE_B, ")"))

# Feature by allocation

rad_aux <- resid |>
  st_drop_geometry() |>
  mutate(FEATURE_A = FEATURE_A_prop,
         FEATURE_B = 1-FEATURE_A_prop) |>
  select(FEATURE_A, FEATURE_B) |>
  as.matrix()

base <- ALLOCATION_MODEL$solution

if(!all(rownames(base) == resid$resid) |
   !all(colnames(base) == school$school) ){
  stop("ID Radio en matriz de asignación no coinciden con base original")
}

for(i in colnames(rad_aux)){
  school[[i]] <- apply(base * rad_aux[ , i], 2, sum)
}

school <- school |>
  mutate(ALLOCATED = FEATURE_A + FEATURE_B,
         FREE_ALLOCATED = SIZE - ALLOCATED)

rm(rad_aux, i, base)

# Figure: Model-Allocation --------------------------------------------

p1 <- ggplot() +
  geom_sf(data = resid, aes(fill = FEATURE_A), alpha = 0.5) +
  geom_sf(data = st_centroid(resid), shape = 3) +
  geom_sf(data = LINE_Between, aes(color = value),
          linetype = 2, color ="darkblue") +
  geom_sf_text(data = st_centroid(LINE_Between),
               aes(label = label), size = 2,
               nudge_x = c(0, 20, -15, 0, 20, 15),
               nudge_y = c(20, 5, 10, -15, 0, 10) ) +
  geom_sf_text(data = st_centroid(resid), aes(label = resid),
               nudge_x = c(-50, 50, -50, 50 )) +
  geom_sf(data = school, color = "tomato4", size = 3) +
  geom_sf_text(data = school, aes(label = school),
               nudge_x = c(0, 15, 15),
               nudge_y = 15, color = "tomato4") +
  scale_fill_material("light-green") +
  guides(color = "none",
         fill = "none") +
  theme_void()

t1 <- resid |>
  st_drop_geometry() |>
  select(-FEATURE_A_prop) |>
  flextable() |>
  set_header_labels(resid = "Residential Area",
                    POPULATION = "Students",
                    FEATURE_A = "Feature A",
                    FEATURE_B = "Feature B") |>
  color(j = 3:4, color = "grey60", part = "all") |>
  autofit() |>
  as_raster()

t2 <- school |>
  st_drop_geometry() |>
  select(-ALLOCATED) |>
  flextable() |>
  set_header_labels(school = "School",
                    SIZE = "Capacity",
                    FEATURE_A = "Feature A",
                    FEATURE_B = "Feature B",
                    FREE_ALLOCATED = "Unused") |>
  color(j = 3:5, color = "grey60", part = "all") |>
  autofit() |>
  as_raster()

p2 <- ggarrange(grid::rasterGrob(t1), NA, grid::rasterGrob(t2),
                ncol = 3, widths = c(16, 2, 20), align = "v")

p_allocation <- ggarrange(p1, p2, nrow = 2, widths = c(4, 1.4))

ggsave(filename = here::here("analysis/figures/Model-Allocation.png"),
       plot = p_allocation,
       dpi = 320, width = 12, height = 9, units = "cm")

rm(p1, p2, t1, t2, school, resid, LINE_Between,
   M_ALLOCATION, ALLOCATION_MODEL, p_allocation)
