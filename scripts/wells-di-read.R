# di-read.R

# This script reads in processed DrillingInfo data and stores a subset of it in .rda format for more convenient usage in R.

# commenting/uncommenting in the below colspec allows ignoring/including variables in the data file.
source('scripts/setup.R')

di_colspec <- cols(

  .default = col_skip(), # to skip unspecified columns for now

  ENTITY_ID = col_integer(),

  COUNTY = col_character(),
  FIPS_CODE = col_character(),
  STATE = col_character(),

  COMPLETION_YEAR = col_integer(),
  HF = col_character(),

  LIQ_PRAC_IP_DAILY = col_double(),
  GAS_PRAC_IP_DAILY = col_double(),
  BOE_PRAC_IP = col_double(),

  SUMOFLIQ14 = col_double(),
  SUMOFGAS14 = col_double(),
  SUMOFWTR14 = col_double(),
  GOR14 = col_double(),
  GOR14_QUAL = col_character(),

  ACTIVE_FLAG = col_character(),
  ACTIVE_PROD_FLAG = col_character(),
  SHALE_FLAG = col_character()
)

di_raw_tx <- read_csv("data-raw/DI Export - 1of2 (TX)_2018-02-06.csv", col_types = di_colspec)
di_raw_non_tx <- read_csv("data-raw/DI Export - 2of2 (nonTX)_2018-02-06.csv", col_types = di_colspec)



di_raw <- bind_rows(di_raw_tx, di_raw_non_tx)

rm(di_raw_non_tx, di_raw_tx)

save(di_raw, file = "data/di_raw.rda")
# saving the binary file will allow for faster data re-loads by other files.
