# Packages ----------------------------------------------------------------

# library(tidyverse)
# library(sf)

# library(ggpubr)
# library(cowplot)

library(knitr)

# library(officer)
# library(officedown)
library(flextable)

# library(spdep)
# library(variosig)
# library(gstat)

# library(OasisR)
# library(segregation)

# General options ---------------------------------------------------------

options(OutDec= ".")

options(knitr.kable.NA = '')

set_flextable_defaults(big.mark = " ",
                       font.size = 9,
                       padding.bottom = 6,
                       padding.top = 6)

#  Load data --------------------------------------------------------------

load(here::here("analysis/data/multi-domain-segregation.RData") )
AUX$bootstrap <- 2500#2

# Functions ---------------------------------------------------------------

source(here::here("analysis/script/01_fx.R"),
       encoding = "UTF-8")

# Segregation computation --------------------------------------------------------

source(here::here("analysis/script/02_data-wrangling.R"),
       encoding = "UTF-8")

# save(SEGRE, file = here::here("analysis/data/segregation-data.RData"))
# load(here::here("analysis/data/segregation-data.RData"))

# Results -----------------------------------------------------------------

# Figure 1: Allocation Model
source(here::here("analysis/script/03_Fig1_allocation-model.R"),
         encoding = "UTF-8")

# Figure 2: Residential segregation
source(here::here("analysis/script/03_Fig2_residential-segregation.R"),
         encoding = "UTF-8")

# Figure 3: School segregation
source(here::here("analysis/script/03_Fig3_school-segregation.R"),
       encoding = "UTF-8")

# Figure 4: School segregation
source(here::here("analysis/script/03_Fig3_school-segregation.R"),
       encoding = "UTF-8")
