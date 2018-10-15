## File: wells-base-year-2014.R
## Date: September 2018
##
## Analysis of 2014 oil and gas wells and completions for RIA and TSD analysis
## Proposed Reconsideration of the Oil and Natural Gas Sector Emissions
## Standards for New, Reconstructed, and Modified Sources.
##
## Pre-requisites to running this script:
##  - Copy of pre-processed DrillingInfo data located in "../data-raw/"
##  - Working directory set to the `wells` directory (e.g., by opening well.Rproj)
##  - DrillingInfo data loaded into .rda file by running "scripts/di-read.R"
##  - Installed packges listed in "scripts/setup.R"
##
## This script outputs summary well and completion counts that are further used in the RIA calculations also located in this repository. If you do not have the DrillingInfo data, the summary outputs have been saved in the repository so you can still run the RIA calculations code.



####### LOAD required packages and data -------

source("scripts/setup.R")

# source("scripts/wells-di-read.R")

if (file.exists("data/di_raw.rda")) {
  # load pre-processed data produced by
  load("data/di_raw.rda")
} else {
  stop("Pre-processed DrillingInfo data not found at 'data/di_raw.rda.' Please run 'scripts/di-read.R' prior to this script.")
}



####### Subset 2014 data and reshape to tall format -------
# Code here would need to be changed for different or more years of analysis

di_14 <- di_raw %>%
  select(ENTITY_ID, FIPS_CODE, HF, STATE, COUNTY, BOE_PRAC_IP,
         ACTIVE_FLAG, ACTIVE_PROD_FLAG, SHALE_FLAG, COMPLETION_YEAR,
         GOR14, GOR14_QUAL, SUMOFLIQ14, SUMOFGAS14, SUMOFWTR14) %>%

  rename(GOR = GOR14,
         GOR_QUAL = GOR14_QUAL,
         SUMOFLIQ = SUMOFLIQ14,
         SUMOFGAS = SUMOFGAS14,
         SUMOFWTR = SUMOFWTR14) %>%
  mutate(YEAR = 2014)


###### Designate production and completion status --------

di_baseyear_1 <- di_14 %>%
  filter(YEAR == 2014) %>%
  mutate(is_producing = well_is_producing(.),
         has_comp =     well_has_comp(.),
         has_hf_comp =  well_has_hf_comp(.)) %>%

  mutate(
    LOW_PRAC_IP = if_else(BOE_PRAC_IP < 15, TRUE, FALSE) ) %>%

  # GOR breakdown
  mutate(WELLTYPE_GOR = case_when(
    (GOR > 100) | (GOR_QUAL == "Gas only") ~ "gas (GOR > 100,000)",
    (GOR < .3) & (GOR_QUAL %in% c("Liq only", "Liq+Gas")) ~ "black oils (GOR < 300)",
    (GOR >= .3) & (GOR_QUAL %in% c("Liq only", "Liq+Gas")) ~ "oil w/ assoc gas",
    TRUE ~ "unknown"
  )) %>%

  mutate(WELLTYPE6 = case_when(
    WELLTYPE_GOR %in% c("gas (GOR > 100,000)",
                     "oil w/ assoc gas",
                     "black oils (GOR < 300)") &
      LOW_PRAC_IP == FALSE ~ WELLTYPE_GOR,

    LOW_PRAC_IP == TRUE &
      WELLTYPE_GOR == "gas (GOR > 100,000)" ~ "low production -- gas",
    LOW_PRAC_IP == TRUE &
      WELLTYPE_GOR == "oil w/ assoc gas" ~ "low production -- oil w/assoc gas",
    LOW_PRAC_IP == TRUE &
      WELLTYPE_GOR == "black oils (GOR < 300)" ~ "low production -- oil only",
    TRUE ~ "unknown")) %>%

  mutate(LOCATION = case_when(
    STATE == "AK" & COUNTY == "North Slope Borough" ~ "AK-NS",
    STATE == "AK" ~ "AK-other",
    STATE %in% c("CA", "CO", "LA", "ND", "NM", "OH", "OK", "PA", "TX", "UT", "WY") ~ STATE,

    TRUE  ~ "other_states"
  ))

di_baseyear_comps <- di_baseyear_1 %>%
  filter(is_producing, has_comp)

#### INPUTS for FUGITIVES COUNTS -------------

comps_tot <- di_baseyear_comps %>% count()
# 33,678 completions at producing wells

comps_welltype6 <- di_baseyear_comps %>% count(WELLTYPE6) %>%
  mutate(percent = n / sum(n))

comps_loc <- di_baseyear_comps %>% count(LOCATION) %>%
  mutate(percent = n / sum(n))

# breakout by WELLTYPE6 **AND** LOCATION
comps_welltype6_loc <- di_baseyear_comps %>% count(WELLTYPE6, LOCATION) %>%
  mutate(percent = n / sum(n))

# make crosstab for better human-readable version
cross_welltype6_loc <- comps_welltype6_loc %>%
  select(-percent) %>%
  spread(WELLTYPE6, n)

###### Output completion counts to csv and xlsx ----------

# The same detailed results are output to both csv and xlsx. The csv results are text-based and tracked in git, and so changes in this file can be used to informally track when the analysis results have changed. The xlsx output have multiple tables and are therefore more human-readable.

write_csv(comps_welltype6_loc, "data/comps-base-year.csv")

# Output to xlsx uses the package openxlsx to output results to an xlsx file. This package requires a zip utility which is often provided by Rtools.

if (require(openxlsx)) {
  wb <- createWorkbook()

  write_sheet <- function(x, sheetName) {
    addWorksheet(wb, sheetName)
    writeData(wb, sheetName, x, 1, 1)
  }

  write_sheet(comps_tot, "Total Comps 2014")
  write_sheet(comps_welltype6, "MODEL PLANT")
  write_sheet(comps_loc, "LOCATION")
  write_sheet(comps_welltype6_loc, "MP by LOCATION")
  write_sheet(cross_welltype6_loc, "crosstab_MP by LOC")

  saveWorkbook(wb, file = "output/xlsx/comps-base-year.xlsx", overwrite = TRUE)
} else {
  warning("Package openxlsx not available. Results not output to xlsx file.")
}

