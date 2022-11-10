#' dafnee: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Nicolas Casajus \email{nicolas.casajus@fondationbiodiversite.fr}
#' 
#' @date 2022/11/10



## Install Dependencies (listed in DESCRIPTION) ----

if (!("remotes" %in% installed.packages())) {
  install.packages("remotes")
}

remotes::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

pkgload::load_all(here::here())


## Run Project ----

source(here::here("analyses", "import-data.R"))
