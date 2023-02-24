
library(flextable)

set_flextable_defaults(big.mark = " ",
                       font.size = 9,
                       padding.bottom = 6,
                       padding.top = 6)

# CORREL-GAP --------------------------------------------------------------

SEGRE$tab_indic[c(1:3, 11), ] |>
  mutate(gap = School - Residential,
         gapPOR = paste0(round(gap / Residential * 100, 1), "%"),
         gap = paste0(gapPOR, " (", round(gap, 2), ")") ) %>%
  select(-c(gapPOR, Model)) %>%
  flextable() |>
  set_caption(caption = "Residential and School Segregation in Buenos Aires City",
              autonum = run_autonum(seq_id = "tab", bkm = "CORREL-GAP",
                                    bkm_all = T))  |>
  align(align ='center', part = 'all') %>%
  align(j = 1, align ='left', part = 'all') %>%
  colformat_double(digits = 2) %>%
  compose_eq() |>
  vline(j = c("Index", "School"), border = fp_border()) |>
  set_header_labels(Index = "Segregation Index") |>
  footnote(j = 1, i = 3,
           value = as_paragraph(
             paste0("Values are bias-corrected. ", AUX$bootstrap,
                    " Bootstrap iterations")),
           ref_symbols = c("a")) |>
  compose(part = "header", j = "gap",
          value = as_paragraph("Gap% (", "\U0394", ")")) |>
  autofit()

# CORREL-SEGTEO -----------------------------------------------------------

SEGRE$tab_indic[c(1:3, 11), ] |>
  select(Index, Model, School) |>
  mutate(School_p = paste0(format(Model/School * 100, digits= 3), "%") ) %>%
  flextable() |>
  set_caption(caption = "School segregation based on the model and real situation",
              autonum = run_autonum(seq_id = "tab", bkm = "CORREL-SEGTEO",
                                    bkm_all = T)) |>
  # caption = "Segregación escolar según datos reales y estimados"
  align(align ='center', part = 'all') %>%
  align(j = 1, align ='left', part = 'all') %>%
  compose_eq() |>
  set_header_labels(Index = "Segregation Index",
                    School = "Real",  School_p = "% Explained") |>
  add_header_row(values = c("Segregation Index", "Segregation", "% Explained"),
                 colwidths = c(1, 2, 1), ) |>
  merge_v(j = c("Index", "School_p"), part = "header") |>
  colformat_double(digits = 2) |>
  footnote(j = 1, i = 3,
           value = as_paragraph(
             paste0("Values are bias-corrected. ", AUX$bootstrap,
                    " Bootstrap iterations")),
           ref_symbols = c("a")) |>
  autofit()

# Table 2 Structural analysis -----------------------------------------------

name_lab <- c("Real Segregation", "Model Segregation",
              "Difference (Model - Real)",
              "Addition", "Removals",
              "Educative Groups Margins",
              "Units Margins",
              "Structural Segregation")

SEGRE$tab_desc |>
  filter(is.na(ID_s)) |>
  mutate(name = name_lab,
         por = round(est/est[3]*100, 1),
         por = paste0(round(por, 1), "%"),
         por = case_when(stat %in% c("M1", "M2") ~ "",
                         T ~ por),
         CI = CI |>
           map(round, digits = 4) |>
           map(format, scientific = F) |>
           map(str_c, collapse = "/") |>
           unlist(),
         est = paste0(format(round(est, 4),
                             scientific = F),
                      " (", CI, ")")) |>
  select(name, est, por) |>
  flextable() |>
  set_caption(caption = "Decomposition of difference in M index",
              autonum = run_autonum(seq_id = "tab", bkm = "STRUCTURAL-ANALYSIS",
                                    bkm_all = T)) |>
  align(align ='center', part = 'all') %>%
  align(j = 1, align ='left', part = 'all') %>%
  align(j = 2, align ='right', part = 'all') %>%
  hline(i = 2:3, border = fp_border(width = 2)) |>
  set_header_labels(name = "", est = "Value (CI)",  por = "%") |>
  footnote(part = "header", j = 1:2,
           value = as_paragraph(
             c("Estimator method: Shapley.",
               paste0("Values are bias-corrected. ",
                      AUX$bootstrap,
                      " Bootstrap iterations.",
                      " Confidence interval (CI) = 99%)"))),
           ref_symbols = c("", "a")) |>
  autofit()

rm(name_lab)
