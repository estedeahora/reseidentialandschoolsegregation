# Packages ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)

library(sf)
library(sfnetworks)
library(tidygraph)

library(units)
library(lpSolve)

# Data --------------------------------------------------------------------

load(here::here("analysis/data/multi-domain-segregation.RData") )
rm(AUX, MOD_lp)

radios <- CARTO$CABA[1:180, ] |>
  select(ID, population_12_17 = P_SEC, PRI, SEC, SUP) |>
  mutate(ID = paste0("C", str_pad(1:n(), 3, pad = "0")))

schools <- CARTO$SEC[1:40, ] |>
  select(ID_s, PRI, SEC, SUP) |>
  mutate(capacity = PRI + SEC + SUP) |>
  mutate(ID_s = paste0("S", str_pad(1:n(), 2, pad = "0")))

street <- roxel

bb <- street|> summarise() |> st_convex_hull()

sum(schools$capacity)-sum(radios$population_12_17)

# 'radios.geojson' contain the 'radios' geometry (census units) with 5 variables:
#   * ID: Census unit ID
#   * population_12_17: population between 12 and 17 years of age.
#   * PRI/SEC/SUP:  total number of household by educational level.


# 'school.geojson' contain the 'school' geometry with 4 variables:
#   * ID: School ID
#   * capacity: number of students enrolled in the previous year.
#   * PRI/SEC/SUP: total number of students by educational level.

# 'street.geojson' contain the street network

# Step 1 -------------------

demand <- radios$population_12_17

radios <- radios |>
  mutate(N = PRI + SEC + SUP,
         across(.cols = c(PRI, SEC, SUP),
                .fns = ~.x/N) ) |>
  st_centroid()

  # # Ajuste para prueba
  set.seed(123)
  radios <- st_sf(st_drop_geometry(radios),
                  geom = st_sfc(st_sample(bb, nrow(radios))))

# Step 2 ----------------

supply <- schools$capacity

  # # Ajuste para prueba
  set.seed(123)
  schools <- st_sf(st_drop_geometry(schools),
                   geom = st_sfc( st_sample(bb, nrow(schools))) )

# Step 3 ----------------

street <- as_sfnetwork(street,
                       directed = F,
                       length_as_weight = T) |>
  activate("edges") |>
  # Clean network
  convert(to_spatial_subdivision) |>
  convert(to_spatial_smooth) |>
  activate("edges") |>
  mutate(weight = edge_length()) |>
  arrange(weight) |>
  convert(to_spatial_simple) |>
  # Filtrar nodos aislados
  activate("nodes") |>
  filter(!node_is_isolated()) |>
  # Retener primer componente de la red
  filter(group_components() == 1)

# Blend network (adding near nodes to schools and radios)
street <- street |>
  st_network_blend(radios) |>
  st_network_blend(schools) |>
  activate("edges") |>
  mutate(weight = edge_length())

# Straight line distance from radio to node
DIS <- st_distance(radios, activate(street, "nodes")) |> drop_units()
radios$from <- as.character(max.col(-DIS))
radios$rad_min <- apply(DIS, 1, min)

from <- unique(radios$from)

# Straight line distance from school to node
DIS <- st_distance(schools, activate(street, "nodes")) |> drop_units()
schools$to <- as.character(max.col(-DIS))
schools$esc_min <- apply(DIS, 1, min)

to <- unique(schools$to)

# Network matrix distance
DIS <- st_network_cost(street, from = from, to = to) |> drop_units()
rownames(DIS) <- from
colnames(DIS) <- to

DIS <- DIS %>%
  as_tibble(rownames = "from") %>%
  pivot_longer(cols = -from, names_to = "to", values_to = "DIS_n")

DIS <- radios |>
  select(ID, from, rad_min) |>
  st_drop_geometry() |>
  left_join(DIS, by = "from")

DIS <- schools |>
  select(ID_s, to, esc_min) |>
  st_drop_geometry() |>
  left_join(DIS, by = "to")

# Cost matrix
C <- DIS %>%
  mutate(across(.cols = c("DIS_n", "rad_min", "esc_min"),
                .fns = ~round(.x) ),
         # Total distance
         DIS_t = DIS_n + rad_min + esc_min,
         # Rescale (better performance)
         DIS_t = DIS_t /10000) |>
  select(ID, ID_s, DIS_t) |>
  pivot_wider(id_cols = ID, names_from = ID_s, values_from = DIS_t) %>%
  data.frame(check.names = F, row.names = 1) |>
  as.matrix()

rm(DIS,from, to)

# step 4 ------------------------------------------------------------------

res <- lp.transport(C, direction = "min",
                    col.signs = rep("<=", length(supply)),
                    row.signs = rep("=", length(demand)),
                    col.rhs = supply,
                    row.rhs = demand)


res$objval <- res$objval * 10
res$objval

A <- res$solution

colnames(A) <- colnames(C)
rownames(A) <- rownames(C)

# Step 5 ------------------------------------------------------------------

model <- A |>
  as_tibble(rownames = "ID") |>
  pivot_longer(cols = -ID, names_to = "ID_s") |>
  filter(value > 0) |>
  left_join(radios |>
              st_drop_geometry() |>
              select(ID, PRI:SUP),
            by = "ID") |>
  mutate(across(.cols = PRI:SUP,
                .fns = ~.x*value) ) |>
  group_by(ID_s) |>
  summarise(across(.cols = PRI:SUP,
                   .fns = ~sum(.x),
                   .names = "{col}_model") )

schools <- left_join(schools, model, by ="ID_s")

# plot -------------------------------------------------------------------------
library(ggplot2)
ggplot() +
  # geom_sf(data = bb, color = "grey") +
  geom_sf(data = st_as_sf(street), color = "darkgreen", alpha = 0.3) +
  geom_sf(data = st_as_sf(activate(street, "nodes")), color = "darkgreen", alpha = 0.3) +
  geom_sf(data = schools, color = "tomato4", alpha = 0.3)  +
  geom_sf(data = radios, color = "aquamarine3", alpha = 0.3)  +
  # geom_sf(data =  schools |>
  #           mutate(model_cap = round(PRI_model + SEC_model + SUP_model)) |>
  #           filter(model_cap < capacity),
  #         color = "tomato4", alpha = 1, shape = 4, size = 5)  +
  # geom_sf(data = filter(schools, ID_s == "S02"),
  #         color = "tomato4", alpha = 1, shape = 4, size = 5)  +
  # geom_sf(data = filter(radios, ID == "C054"),
  #         color = "aquamarine3", alpha = 1, shape = 4, size = 5)  +
  theme_void()
