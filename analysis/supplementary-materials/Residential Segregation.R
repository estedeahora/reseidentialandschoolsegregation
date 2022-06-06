library(tidyverse)
library(sf)
library(ggspatial)
library(spdep)
library(cowplot)

db <- CARTO$CABA |> 
  filter(!is.na(ls)) |> 
  select(ID, M = ls, p)

# Neighbours lists
w <- poly2nb(db, row.names = CARTO$CABA$ID )
lw <-  nb2listw(w, style='W')

# Moran global
m_res <- moran.test(db$M, lw) |> 
  pluck("estimate")

m_plot <- moran.plot(db$M, lw, plot = F,
                     zero.policy = TRUE, 
                     labels = db$ID) |> 
  mutate(ID = labels)

# Moran Local
m_local <- localmoran(db$M, lw) |> 
  as_tibble() |> 
  rename(p = `Pr(z != E(Ii))`)

# Moran Regression
m_lm <- lm(wx ~ x, data = m_plot)

db <- cbind(db, m_plot, residuals = m_lm$residuals) |> 
  mutate(ESP = residuals < quantile(residuals, probs = 0.05) | 
           residuals > quantile(residuals, probs = 0.95))

# Figures -----------------------------------------------------------------

# M Index's Map
p1 <- db |> 
  ggplot() + 
  geom_sf(data = CARTO$COMUNA) + 
  geom_sf(aes(fill = M), color = NA, alpha = 0.6) + 
  scale_fill_gradient2(high = "darkred",
                       mid = "darkolivegreen2",
                       low = "deepskyblue", 
                       midpoint = SEGRE$tab_indic$Residential[3]) +
  theme_void() +
  theme(legend.position = c(0.2, 0), legend.direction = "horizontal")

# # Residuals Map 
# p2 <- db |> 
#   filter(ESP) |> 
#   ggplot() + 
#   geom_sf(aes(fill = residuals), color = NA) + 
#   geom_sf(data = CARTO$COMUNA, fill = NA) + 
#   scale_fill_gradient2("Moran's\nResiduals", 
#                        labels = c("Low\nlag", "0", "High\nlag"),
#                        breaks = c(min(db$residuals) + 0.05, 
#                                   0, 
#                                   max(db$residuals) - 0.05)) +
#   theme_void() +
#   theme(legend.position = c(0.65, 0), legend.direction = "horizontal")
# 
# # Moran Plot

p3 <- db |> #cbind(, residuals = m_lm$residuals) |> 
  ggplot(aes(x = x, y = wx)) + 
  geom_point(aes(color = residuals), alpha = 0.8) + 
  geom_point(data = db[!db$ESP,], 
             color = "grey60", alpha = 0.2, size = 0.1) + 
  geom_smooth(formula = y ~ x, method="lm", 
              se = F, color = "darkgreen", alpha = 0.3) + 
  geom_hline(yintercept = mean(db$wx), lty = 2) + 
  geom_vline(xintercept = mean(db$x), lty = 2) + 
  scale_color_gradient2() +
  labs(x = "Mutual Information Index (M)",
       y = "Lag M") +
  guides(color = "none")+
  theme_minimal()

# Urban habitat Map

p4 <- CARTO$BP |> 
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
  annotation_scale() +
  annotation_north_arrow(width = unit(1, "cm"), aes(location = "tl")) +
  theme_void() + 
  theme(legend.position = "bottom")

# a <- ggdraw() +
#   draw_plot(p1, x = 0, y = .1, width = .5, height = .8) +
#   draw_plot(p2, x = .5, y = .1, width = .5, height = .8) +
#   draw_plot(p3, x = .3, y = 0, width = .3, height = .3)

b <- ggarrange(p1, p4)
  
# ggsave(plot = a, "_draft/Draft_Residential-Segreg.png",
#          width = 25, height = 20, units = "cm")

ggsave(plot = b, "figure/Residential-Segreg.png",
       width = 25, height = 20, units = "cm")

rm(db, w, lw, m_res, m_plot, m_local, m_lm, p1, p2, p3, p4, a)
