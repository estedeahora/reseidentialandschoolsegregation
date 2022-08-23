# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

library(units)
library(flextable)

library(ggplot2)
library(png)
library(grid)
library(ggpubr)
library(latex2exp)

library(sf)
library(ggspatial)
library(spatstat)
library(segregation)
library(OasisR)

set_flextable_defaults(big.mark = " ",
                       font.size = 9,
                       padding.bottom = 6,
                       padding.top = 6)

# Common Data  -----------------------------------------------

ESCALA <- list(
  RADIO  = CARTO$CABA |> select(VIV, PRI, SEC, SUP),
  FRACC  = CARTO$CABA |>
    mutate(ID_FRAC = str_sub(ID, end = -3)) |>
    group_by(ID_FRAC) |>
    summarise(across(.cols = c(VIV, PRI, SEC, SUP),
                     ~sum(.x, na.rm = T))),
  BARRIO = CARTO$CABA |>
    group_by(BARRIO) |>
    summarise(across(.cols = c(VIV, PRI, SEC, SUP),
                     ~sum(.x, na.rm = T))),
  COMUNA = CARTO$CABA |>
    group_by(COMUNA) |>
    summarise(across(.cols = c(VIV, PRI, SEC, SUP),
                     ~sum(.x, na.rm = T)))
)

lev <- c("Radio", "Fracción", "Barrio", "Comuna")

# Appendix A --------------------------------------------------------------

  # ╠ Figure 1.1 Residential Features ----------------------------------------------------------------

  # Census unit scales (top-left)

  leg <- data.frame(Scale = factor(lev, levels = lev),
                   x = c(1:4, 1:4), y = c(1:4, 1:4) ) |>
    ggplot(mapping = aes(x = x, y = y, color = Scale) ) +
    geom_line() +
    scale_color_manual(values = c("grey70", "tomato3", "darkblue", "darkgreen")) +
    theme_minimal()

  leg <- ggpubr::get_legend(leg) |> ggpubr::as_ggplot()

  p1 <- ggplot() +
    geom_sf(data = ESCALA$RADIO,  fill = NA, color = "grey70" ) +
    geom_sf(data = ESCALA$FRACC,  fill = NA, color =  "tomato3") +
    geom_sf(data = ESCALA$BARRIO, fill = NA, color = "darkblue", size = 1 ) +
    geom_sf(data = ESCALA$COMUNA, fill = NA, color = "darkgreen", size = 1 ) +
    annotation_custom(ggplotGrob( leg),
                      xmin = 106000, ymin = 92000,
                      xmax = 110000, ymax = 94500) +
    annotation_scale(location = "tr") +
    theme_void()

  # Table. Census unit scales (bottom-left)

  p2 <- cbind(SCALE = lev,
              AREA = map_dbl(ESCALA, \(x) st_area(x) |>
                  set_units("km2") |>
                  mean() |>
                  round(digits = 2)),
        VIV = map_dbl(ESCALA, \(x) x$VIV |>
                  mean() |>
                  round(digits = 0)) ) |>
    as_tibble() |>
    flextable() |>
    set_header_labels(SCALE = "Census unit",
                      AREA = "Area (Km\U00B2)",
                      VIV = "Households") |>
    fontsize(size = 14, part = "all") |>
    autofit() |>
    flextable::as_raster()

  # Urban Context Tipology (right)

  p3 <- readPNG("analysis/figures/TH.png")

  p <- ggarrange(ggarrange(p1, rasterGrob(p2),
                      nrow = 2, heights = c(4, 1)),
            rasterGrob(p3),
            ncol = 2, widths = c(7, 10))

  # Save figure

  ggsave(filename = here::here("analysis/figures/Residential-Features.png"),
         plot = p, width = 30, height = 15, unit = "cm")

  rm(p1, p2, p3, p, leg)

  # ╠ Figure 1.2 Kernek Density ---------------------------------------------------

  # Ventana (w)
  w <- CARTO$CABA %>%
    summarise() %>%
    st_transform(crs = CARTO$CABA_PROY) |>
    as.owin()

  ESC <- CARTO$SEC |>
    mutate(x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) %>%
    st_drop_geometry() |>
    mutate(n = PRI+ SEC + SUP) |>
    select(n, SECTOR, x, y) |>
    split(~SECTOR)

  p <- ggarrange(plotlist = map(ESC, \(x) plot_kernel(x, w = w) ),
                 common.legend = T, legend = "bottom",
                 labels = paste(names(ESC), "Schools"),
                 hjust = -1, vjust = 1, font.label = list(size = 16))

  ggsave(filename = here::here("analysis/figures/Kernel-Distribution.png"),
         plot = p, width = 30, height = 15, unit = "cm")

  rm(w, ESC, p)

# Appendix B --------------------------------------------------------------


  # nam <- c("Colonial Historic City",
  #          "Central Business District",
  #          "High Class Residential",
  #          "Middle Class Residential",
  #          "Low Class Residential",
  #          "Social housing",
  #          "Urban Informal Neighborhoods",
  #          "Total mean")
  #
  # TH <- c("#D1E107", "#0D9743",
  #         "#0AE1A7", "#E8EC6E", "#0072B2",
  #         "#A194FC", "#CA280E", "#F07561", "#FCCB94")
  #
  #   ggplot(CARTO$CABA2) +
  #     geom_sf(aes(fill = TIPO_HABIT), alpha = 0.7, color = NA) +
  #     geom_sf(data = CARTO$COMUNA, fill = NA) +
  #     scale_fill_discrete("Entorno Urbano", type =  TH) +
  #     labs(caption = "Fuente: Elaboración propia en base a Marcos, Mera & Di Virgilio (2015)") +
  #     ggspatial::annotation_scale() +
  #     ggspatial::annotation_north_arrow(width = unit(1, "cm"), location='tr',
  #                                       which_north = "grid") +
  #     theme_void()

# Appendix C --------------------------------------------------------------


# Appendix D --------------------------------------------------------------


res <- map(ESCALA, \(x) x |>
             st_drop_geometry() |>
             select(PRI, SEC, SUP) |>
             SEG_summary(se = F)) |>
    set_names(nm = lev)

p <- Reduce(\(...) merge(..., by = "Index"), res) |>
  set_names(nm = c("Index", lev)) |>
  pivot_longer(Radio:Comuna) |>
  mutate(name = factor(name, levels = lev) ) |>
  ggplot(aes(x = name,
             y = value,
             group = Index,
             color = Index) ) +
  geom_line(alpha = 0.7) +
  geom_point() +
  scale_color_discrete(labels = TeX) +
  guides(color = guide_legend(ncol = 2, byrow = F,
                              title = "Segregation Index",
                              title.position = "top")) +
  labs(x = "Census Scale", y = "Segregation Value") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.text.align = 0)

ggsave(filename = here::here("analysis/figures/Scale-Effect.png"),
       plot = p, width = 18, height = 16, units = "cm", dpi = 300)

rm(res, lev, p, ESCALA)

