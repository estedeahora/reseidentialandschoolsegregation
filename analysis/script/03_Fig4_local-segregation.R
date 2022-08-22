# Packages ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(gridExtra)

library(flextable)

library(ggplot2)
library(ggpubr)

library(sf)

library(gstat)
library(segregation)
library(OasisR)

# Data ----------------------------------------------------------------

# Local M Real
set.seed(321)
loc_REAL <- CARTO$SEC |>
  st_drop_geometry() |>
  filter(!is.na(PRI)) |>
  select(ID_s, SECTOR, PRI:SUP) |>
  pivot_longer(cols = PRI:SUP, names_to = "EDU",
               values_to = "n") |>
  mutual_local(group = "EDU", unit = "ID_s", weight = "n",
               se = T, n_bootstrap = AUX$bootstrap,
               wide = T ) |>
  mutate(ls = ifelse(ls < 0, 0, ls)) |>
  select(ID_s, ls_real = ls)

# Local Structural Difference
loc_MODEL <- SEGRE$tab_desc |>
  filter(!is.na(ID_s) & stat == "ls_diff_mean") |>
  mutate(sig = !map_lgl(CI, \(x) x[1] < 0 & x[2] > 0) ) |>
  select(ID_s, diff = est, sig)

# Common database
base <- CARTO$SEC |>
  mutate(n =  PRI + SEC + SUP) |>
  select(ID_s, n, SECTOR) |>
  left_join(loc_REAL, by = "ID_s") |>
  right_join(loc_MODEL, by = "ID_s") |>
  mutate(ls_est = ls_real + diff,
         ls_est = ifelse(ls_est < 0, 0, ls_est) )

rm(loc_REAL, loc_MODEL)

# Figures -----------------------------------------------------------------

mM <- base |>
  st_drop_geometry() |>
  filter(!is.na(SECTOR)) |>
  group_by(SECTOR) |>
  summarise(R2 = cor(ls_real,
                     ls_est,
                     use = "complete.obs" )^2 * 100 ) |>
  mutate(R2_lab = paste0("R[", SECTOR,"]^{2}", " == ", sprintf("%.2f", signif(R2, 3))),)

p1 <- base |>
  filter(sig) |>
  ggplot(mapping = aes(x = ls_real, y = ls_est, shape = SECTOR) ) +
  geom_point(mapping = aes(size = n, color = diff),
             alpha = 0.8) +
  geom_point(data = base |> filter(!sig),
             size = 0.7, alpha = 0.3 , color = "darkgreen") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "tomato3") +
  # geom_text(x = c(0.4, 0.4), y = c(0.9, 0.8), data= mM, color = "grey40",
  #           label = paste0("R2 ", mM$SECTOR, ": ", round(mM$R2, 1), "%")) +
  geom_text(x = c(0.3, 0.3), y = c(0.9, 0.8), data= mM,
            color = "grey40", aes(label = R2_lab), parse = TRUE
            ) +
  geom_hline(yintercept = 0, alpha = 0.7) +
  geom_vline(xintercept = 0, alpha = 0.8) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = expression(paste("Real Local School Segregation (",
                            L[s]^{Real}, ")", sep = "")),
       y = expression(paste("Modelled Local School Segregation (",
                            L[s]^{Model}, ")", sep = "")),
       shape = "") +
  scale_color_gradient2() +
  theme_minimal() +
  theme(legend.position = "none")

# Geo localización de diferencias -------------------------------------

p2 <- base |>
  filter(sig) |>
  ggplot(aes(shape = SECTOR)) +
  geom_sf(aes(color = diff, size = n)) +
  geom_sf(data = base[!base$sig, ],
          size = 0.7, color = "darkgreen", alpha = 0.4) +
  geom_sf(data = CARTO$COMUNA, fill = NA, shape =  NA) +
  geom_point(data = data.frame(x = 94000, y = 93200), alpha = 0.5,
             aes(x = x, y = y, fill = "Non significative \ndifference",
                 shape = NA), colour = "darkgreen") +
  scale_color_gradient2(expression(paste("Segregation Difference (",
                                         Delta, L[s], " = ",  L[s]^{Model},
                                         "-", L[s]^{Real}, ")", sep = "") ),
                        guide = guide_colorbar(title.position = "top",
                                               order = 3,
                                               barwidth = 8,
                                               direction = "horizontal"
                        )) +
  scale_shape("Sector",
              guide = guide_legend(title.position = "top",
                                   order = 1,
                                   direction = "vertical" )) +
  guides(size = "none",
         fill = guide_legend(title = " ", order = 2,
                             title.position = "top") ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.spacing.x = unit(0.5, "cm"))

p <- ggarrange(p1, p2, common.legend = T,
               legend = "bottom", legend.grob = get_legend(p2) )

ggsave(filename = here::here("analysis/figures/Local_Segregation.png"),
       plot = p, height = 15, width = 30, units = "cm")

rm(mM, p1, p2, p, base)
