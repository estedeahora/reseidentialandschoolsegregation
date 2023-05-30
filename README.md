
# residentialschoolsegregation

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/estedeahora/residentialschoolsegregation/main?urlpath=rstudio)

### How to cite

This repository contains the data and code for our paper:

> Serrati, P. S. (2023). *School and Residential Segregation in the
> Reproduction of Urban Segregation. A Case Study in Buenos Aires*.
> *Urban Studies*, *0*(0) <https://doi.org/10.1177/00420980231178401>

A pre-print version is online here:
<https://osf.io/preprints/socarxiv/ayx3q/>.

Please cite this compendium as:

> Serrati, P. S. (2023). *Compendium of R code and data for School and
> Residential Segregation in the Reproduction of Urban Segregation. A
> Case Study in Buenos Aires*. Accessed 30 may. 2023.

## Contents

The **analysis** directory contains:

- [:file_folder: data](/analysis/data): Data used in the analysis.
- [:file_folder: figures](/analysis/figures): Plots and other
  illustrations.
- [:file_folder: scripts](/analysis/scripts): Includes R code to
  reproduce the analysis, tables, figures and supplementary materials.
- [:file_folder:
  supplementary-materials](/analysis/supplementary-materials):
  Supplementary materials including notes and other documents prepared
  and collected during the analysis.

The Appendix is available at:
<https://pabloserrati.netlify.app/en/supplementary/2023_school-residential-segregation/>

## How to run in your browser or download and run locally

This research compendium has been developed using the statistical
programming language R. To work with the compendium, you will need
installed on your computer the [R
software](https://cloud.r-project.org/) itself and optionally [RStudio
Desktop](https://rstudio.com/products/rstudio/download/).

You can download the compendium as a zip from from this URL:
[master.zip](/archive/master.zip). After unzipping: - open the `.Rproj`
file in RStudio - run `devtools::install()` to ensure you have the
packages this analysis depends on (also listed in the
[DESCRIPTION](/DESCRIPTION) file). - finally, open
`analysis/paper/paper.Rmd` and knit to produce the `paper.docx`, or run
`rmarkdown::render("analysis/paper/paper.Rmd")` in the R console

### Licenses

<!-- **Text and figures :**  [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/) -->

**Code :**
[CC-BY-NC-4.0](http://creativecommons.org/licenses/by-nc/4.0/)

**Data :**
[CC-BY-NC-4.0](http://creativecommons.org/licenses/by-nc/4.0/)

### Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
