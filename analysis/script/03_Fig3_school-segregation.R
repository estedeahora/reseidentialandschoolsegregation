# Packages

library(dplyr)
library(ggplot2)
library(ggpubr)

library(sf)

library(gstat)
library(variosig)

# Boxplot school M (left)

p1 <- CARTO$SEC |>
  filter(!is.na(M)) |>
  ggplot(aes(x= SECTOR, y = M)) +
  geom_boxplot(color = "grey60", outlier.alpha = 0.5,
               notch = T, width = 0.2) +
  geom_violin(alpha = 0.3, color =NA, aes(fill = SECTOR)) +
  geom_hline(yintercept = SEGRE$tab_indic$School[3],
             color = "tomato3", linetype = 3 ) +
  geom_text(x = 1.5, y = SEGRE$tab_indic$School[3] + 0.03,
            label = "Global M Index", color = "tomato4") +
  guides(fill = "none") +
  labs(x = "",
       y = "Local Mutual Information Index") +
  ggsci::scale_fill_d3() +
  theme_minimal()

# Semivariogram (right)

vg <- variogram(M ~ 1, data = CARTO$SEC |> filter(!is.na(M)))
CI_vg <- envelope(vario = vg, formula = M ~ 1,
                  conf.level = 0.99, nsim = 2000,
                  data = CARTO$SEC |> filter(!is.na(M)) |>  as_Spatial() )

p2 <-  vg |>
  cbind(low = CI_vg$lower, up = CI_vg$upper) |>
  ggplot(aes(x = dist)) +
  geom_ribbon(aes(ymin = low, ymax = up),
              color = "lightsteelblue3", fill = "grey60",
              alpha = 0.1 ) +
  geom_point(aes(y = gamma), color = "darkolivegreen") +
  geom_col(aes(y = np/200000), alpha = 0.3, fill = "dodgerblue3" ) +
  scale_y_continuous(expression(paste("Semivariance ", gamma, "(h)", sep = "")),
                     limits = c(0, 0.046),
                     sec.axis = sec_axis(~. *200000, name = "Counts (n)")) +
  scale_x_continuous("Distance between schools in meters (h)") +
  theme_minimal()

# Save "School-Segregation.png"

ggsave(here::here('analysis/figures/School-Segregation.png'),
       plot = ggarrange(p1, p2),
       width = 24, height = 12, units = "cm")

rm(p1, p2, vg, CI_vg)
