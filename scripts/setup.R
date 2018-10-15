# setup.R
# load libraries and source functions used in the project.

options(scipen = 999999999)

missing_packages <- setdiff(
  c("dplyr", "purrr", "tidyr", "magrittr", "glue", "here", "devtools", "rlang", "readxl", "openxlsx", "readr", "stringr", "knitr", "kableExtra"), installed.packages())

if (length(missing_packages) != 0) {
  warning("The following packages used in this project are missing: ", paste0(missing_packages, collapse = ", "))
}

# load data manip libraries
library("dplyr")
library("purrr")
library("tidyr")
library("magrittr")
#library("rlang")
library("stringr")
library("readr")
library("readxl")

#reporting
library("kableExtra")

# utils
library("here")
library("glue")
if (!require(openxlsx)) {
  warning("Package openxlsx not available, cannot write output to xlsx.")
}


# As an alternative to loading package functions with load-all, the following files can be sourced:

source(here("R", "calc-funs-ong.R"))
source(here("R", "format-funs.R"))
source(here("R", "helper-funs.R"))
source(here("R", "npv-math-funs.R"))

source(here("R", "table-funs-ong.R"))
source(here("R", "xlsx-output.R"))

source(here("R/wells-flag-funs.R"))


