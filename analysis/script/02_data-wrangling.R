# Packages ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(sf)

library(OasisR)
library(segregation)

# Make SEGRE data bases --------------------------------------------

SEGRE <- list()

  # ╠ Residential db -----------------------------------------------

  SEGRE$db_RESI <- CARTO$CABA |>
    st_drop_geometry() |>
    select(ID, PRI:SUP)

  # ╠ School db (real situation) -----------------------------------

  SEGRE$db_EDU <- CARTO$SEC |>
    st_drop_geometry() |>
    select(ID_s, PRI:SUP)

  # ╚ School db (model situation) ----------------------------------

  # Radio composition
  rad_aux <- SEGRE$db_RESI |>
    select(-ID) |>
    as.matrix() |>
    prop.table(margin =  1)

  # Theoretical (modelled) School Composition
  base <- MOD_lp$res$solution
  SEGRE$db_MODEL <- data.frame(ID_s = colnames(base))

  if(!all(rownames(base) == SEGRE$db_RESI$ID)){
    stop("ID Radio en matriz de asignación no coinciden con base original")
  }

  for(i in colnames(rad_aux)){
    SEGRE$db_MODEL[[i]] <- apply(base * rad_aux[ , i], 2, sum)
  }

  rm(rad_aux, i, base)

# Calculate Residential Segregation --------------------------------

SEG_res <- SEGRE$db_RESI |>
  select(-ID) |>
  SEG_summary(bootstrap = AUX$bootstrap) |>
  rename(Residential = Value)

# Calculate School Segregation (real situation) --------------------

SEG_edu <- SEGRE$db_EDU |>
  select(-ID_s) |>
  SEG_summary(bootstrap = AUX$bootstrap) |>
  rename(School = Value)

# Calculate School Segregation (model situation) -------------------

# School segregation
SEG_model <- SEGRE$db_MODEL |>
  select(PRI:SUP) |>
  SEG_summary(bootstrap = AUX$bootstrap) |>
  rename(Model = Value)

# Summary Segregation Table ----------------------------------------

SEGRE$tab_indic <- SEG_res |>
  left_join(SEG_edu, by = "Index") |>
  left_join(SEG_model, by = "Index")

rm(SEG_res, SEG_edu, SEG_model)

# Decompose the difference between REAL and MODEL ------------------

db_teo <- SEGRE$db_MODEL  |>
  pivot_longer(cols = PRI:SUP,
               values_to = "n", names_to = "EDU")

db_real <- SEGRE$db_EDU  |>
  select(ID_s, PRI:SUP) |>
  pivot_longer(cols = PRI:SUP,
               values_to = "n", names_to = "EDU")

set.seed(321)
SEGRE$tab_desc <- mutual_difference(db_real, db_teo, group = "EDU", unit = "ID_s",
                                    CI = 0.99, se = T, n_bootstrap = AUX$bootstrap,
                                    weight = "n", method = "shapley_detailed")

rm(db_teo, db_real)
