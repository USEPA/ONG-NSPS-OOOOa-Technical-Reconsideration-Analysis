#policy_2018techreconsideration.R

# this script defines functions that take activity data scenarios (in the form of a data frame)
# and return a vector of control fates. Usually these functions will be called with something like:
# `ad_proj %>% mutate(fate = policyfun(.))`

# the input data will have columns:
# mp
# year <- series, will be maintained
# fac_count <- will be summed, should not be added over differences in mp

# attrs - attributes of the facility
# location - jurisdiction, could be grouped
# vintage - when was the facility constructed or modified

# mp, year, attrs, location, and vintage are used to determine mp-fate outcomes

# each of these functions needs to handle all mp types, attrs/location/vintage, and just once. 



#' Determine more stringent of two vectors of control fates
#' 
#' If one is bau, the other is always returned. If fates not applicable to same mp, then returns NA_character_ in that position. This function is intended to be used in the policy functions to include state action.
more_stringent <- function(x, y) {
  
  if(length(x) != length(y) & length(x) != 1 & length(y) != 1) stop("Length of fate vectors x, y must be the same.")
  if((! rlang::type_of(x) %in% c("character", "string")) | 
     (! rlang::type_of(y) %in% c("character", "string"))) stop("Type of fate   vectors x, y must be character")
  
  case_when(
    x == y ~ x,
    
    #anything dominates bau
    x == "bau" ~ y,
    y == "bau" ~ x,
    
    #fugitives
    x == "ogi_biennial" & y %in% c("ogi_annual", "ogi_stepped", "ogi_semiannual", "ogi_quarterly") ~ y,
    x == "ogi_annual" & y %in% c("ogi_stepped", "ogi_semiannual", "ogi_quarterly") ~ y,
    x == "ogi_stepped" & y %in% c("ogi_semiannual", "ogi_quarterly") ~ y,
    x == "ogi_semiannual" & y %in% c("ogi_quarterly") ~ y,
    y == "ogi_biennial" & x %in% c("ogi_annual", "ogi_stepped", "ogi_semiannual", "ogi_quarterly") ~ x,
    y == "ogi_annual" & x %in% c("ogi_stepped", "ogi_semiannual", "ogi_quarterly") ~ x,
    y == "ogi_stepped" & x %in% c("ogi_semiannual", "ogi_quarterly") ~ x,
    y == "ogi_semiannual" & x %in% c("ogi_quarterly") ~ x,
    
    #certs
    x == "no_cert" & y %in% c("inhouse_cert", "pe_cert") ~ y,
    x == "inhouse_cert" & y == "pe_cert" ~ "pe_cert",
    y == "no_cert" & x %in% c("inhouse_cert", "pe_cert") ~ x,
    y == "inhouse_cert" & x == "pe_cert" ~ "pe_cert",
    
    #controllers
    x == "lowbleed" & y == "highbleed" ~ "lowbleed",
    y == "lowbleed" & x == "highbleed" ~ "lowbleed",
    
    #recs
    x == "rec + flare" & y %in% c("rec", "flare", "vent") ~ "rec + flare",
    x == "rec" & y %in% c("flare", "vent") ~ "rec",
    x == "flare" & y == "vent" ~ "flare",
    y == "rec + flare" & x %in% c("rec", "flare", "vent") ~ "rec + flare",
    y == "rec" & x %in% c("flare", "vent") ~ "rec",
    y == "flare" & x == "vent" ~ "flare",
    
    #centri comp
    x == "wetseal_route_control" & y == "wetseal" ~ "wetseal_route_control",
    y == "wetseal_route_control" & x == "wetseal" ~ "wetseal_route_control",
    
    TRUE ~ NA_character_
  )
}

state_policy_2018 <- function(ad_proj) {
  
  mp_fates_proj <- ad_proj %>%
    mutate(fate = case_when(
      
      # Compliance with state programs for wellsite fugitives
      mp %in% c("gaswellsite", 
                "oilassocgaswellsite", 
                "oilwellsite",  
                "lp_gaswellsite",
                "lp_oilassocgaswellsite",
                "lp_oilwellsite") &
        (location %in% c("CA", "PA")) ~ "ogi_quarterly",
      
      mp %in% c("gaswellsite", 
                "oilassocgaswellsite", 
                "oilwellsite",  
                "lp_gaswellsite",
                "lp_oilassocgaswellsite",
                "lp_oilwellsite") & 
        (location %in% c("CO", "OH", "UT")) ~ "ogi_semiannual",
      
      TRUE ~ "bau"))
  
  pull(mp_fates_proj, fate)
}

nsps2_effective_year <- 2015 # vintage of facilites affected by 2016 NSPS
recon_effective_year <- 2019

#' Calculate mp-fate-proj from ad-proj for 2016 pre-NSPS baseline
#' 
pre_2016_baseline <- function(ad_proj) {
  
  state_fate <- state_policy_2018(ad_proj)
  
  scn_fate <- ad_proj %>%
    mutate(fate = case_when(
      
      mp == "gaswellsite"         ~ "bau",
      mp == "oilassocgaswellsite" ~ "bau",
      mp == "oilwellsite"         ~ "bau",
      
      mp == "lp_gaswellsite"         ~ "bau",
      mp == "lp_oilassocgaswellsite" ~ "bau",
      mp == "lp_oilwellsite"         ~ "bau",
      
      mp == "gbstation"           ~ "bau",
      mp == "transstation"        ~ "bau",
      mp == "storstation"         ~ "bau",
      
      mp == "recip_trans"         ~ "bau",
      mp == "recip_stor"          ~ "bau",
      mp == "centri_trans"        ~ "wetseal",
      mp == "centri_stor"         ~ "wetseal",
      
      mp == "contbleed_contr"     ~ "highbleed",
      
      mp == "cert" ~ "no_cert",
      
      TRUE ~ NA_character_
    ))
  
  result <- more_stringent(state_fate, scn_fate$fate)
}

#' Calculate mp-fate-proj from ad-proj for 2018 baseline (no change) scenario
#' 
policy_2018_baseline <- function(ad_proj) {

  # Not yet incorporated:
  # "diapump"     = "vent",
  # "chempump"    = "vent", # not affected
  
  state_fate <- state_policy_2018(ad_proj)
  
  scn_fate <- ad_proj %>%
    mutate(fate = case_when(
      
      vintage < 2015 ~ pre_2016_baseline(ad_proj),
      
      # Amendment
      location == "AK-NS" & 
        mp %in% c("gaswellsite", 
                  "oilassocgaswellsite", 
                  "oilwellsite",
                  "lp_gaswellsite",
                  "lp_oilassocgaswellsite",
                  "lp_oilwellsite") ~ "ogi_annual",
      
      # 2016 Final NSPS provisions -- relevant to technical reconsideration
      mp %in% c("gaswellsite",
                "oilassocgaswellsite",
                "oilwellsite", 
                "lp_gaswellsite",
                "lp_oilassocgaswellsite",
                "lp_oilwellsite") ~ "ogi_semiannual",
      
      mp %in% c("gbstation",
                "transstation",
                "storstation") ~ "ogi_quarterly",
      
      # Other 2016 Final NSPS provisions
      mp %in% c("recip_trans",
                "recip_stor") ~ "seals_3yr",
      
      mp %in% c("centri_trans",
                "centri_stor") ~ "wetseal_route_control",
      
      mp == "contbleed_contr" ~ "lowbleed",
      
      mp == "cert" ~ "pe_cert",
      
      TRUE ~ NA_character_))
  
  result <- more_stringent(state_fate, scn_fate$fate)
}

#' Calculate mp-fate-proj from ad-proj for option 1
#' 
policy_2018_opt1 <- function(ad_proj) {
  
  state_fate <- state_policy_2018(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    mutate(fate = case_when(
      
      vintage < 2015 ~ pre_2016_baseline(ad_proj),
      year < recon_effective_year ~ policy_2018_baseline(ad_proj),
      
      location == "AK-NS" & 
        mp %in% c("gaswellsite", 
                  "oilwellsite",
                  "oilassocgaswellsite", 
                  
                  "lp_gaswellsite",
                  "lp_oilassocgaswellsite",
                  "lp_oilwellsite") ~ "ogi_annual", #wellsites match baseline
      
      location == "AK-NS" & 
        mp %in% c("gbstation", 
                  "transstation", 
                  "storstation") ~ "ogi_quarterly", #no AD yet, matches baseline
      
      mp == "cert" ~ "inhouse_cert",
      
      TRUE ~ policy_2018_baseline(ad_proj) # keep at baseline
    ))
  
  result <- more_stringent(state_fate, scn_fate$fate)
}

#' Calculate mp-fate-proj from ad-proj for option 2
#' 
policy_2018_opt2 <- function(ad_proj) {
  #pol_scn <- "2018 Reconsideration Option 2"
  
  state_fate <- state_policy_2018(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    # reconsidered wellsite fugitive monitoring
    mutate(fate = case_when(
      
      vintage < 2015 ~ pre_2016_baseline(ad_proj),
      
      year < recon_effective_year ~ policy_2018_baseline(ad_proj), 
      
      location == "AK-NS" & 
        mp %in% c("gaswellsite", 
                  "oilwellsite",
                  "oilassocgaswellsite", 
                  
                  "lp_gaswellsite",
                  "lp_oilassocgaswellsite",
                  "lp_oilwellsite", #wellsites match baseline at annual
                  
                  "gbstation", 
                  "transstation", 
                  "storstation") ~ "ogi_annual", #no AD yet
      
      mp %in% c("lp_gaswellsite",
                "lp_oilassocgaswellsite",
                "lp_oilwellsite") ~ "ogi_annual",
      
      mp %in% c("gaswellsite", "oilassocgaswellsite", "oilwellsite") ~ "ogi_stepped",
      
      mp == "cert" ~ "inhouse_cert",
      
      TRUE ~ policy_2018_baseline(ad_proj) # keep at baseline
    ))
  
  result <- more_stringent(state_fate, scn_fate$fate)
}

#' Calculate mp-fate-proj from ad-proj for Option 3 scenario
#' 
policy_2018_opt3 <- function(ad_proj) {
  #pol_scn <- "2018 Reconsideration Option 3"
  
  state_fate <- state_policy_2018(ad_proj)
  
  scn_fate <- ad_proj %>%
    
    # reconsidered wellsite fugitive monitoring
    mutate(fate = case_when(
      
      vintage < 2015 ~ pre_2016_baseline(ad_proj),
      year < recon_effective_year ~ policy_2018_baseline(ad_proj),
      
      location == "AK-NS" & 
        mp %in% c("gaswellsite", 
                  "oilwellsite",
                  "oilassocgaswellsite", 
                  "lp_gaswellsite",
                  "lp_oilassocgaswellsite",
                  "lp_oilwellsite") ~ "ogi_annual", 
      
      location == "AK-NS" & 
        mp %in% c("gbstation", 
                  "transstation",
                  "storstation") ~ "ogi_annual",
      
      mp %in% c("lp_gaswellsite", 
                "lp_oilassocgaswellsite", 
                "lp_oilwellsite") ~ "ogi_biennial",
      
      mp %in% c("gaswellsite", 
                "oilassocgaswellsite", 
                "oilwellsite")  ~ "ogi_annual",
      
      mp %in% c("gbstation", "transstation", "storstation") ~ "ogi_semiannual",
      
      mp == "cert" ~ "inhouse_cert",
      
      TRUE ~ policy_2018_baseline(ad_proj) # keep at baseline
    ))
  
  result <- more_stringent(state_fate, scn_fate$fate)
}


####Proposed option with alternative monitoring freq at compressor stations

 policy_2018_ann_comps <- function(ad_proj) {
   #pol_scn <- "2018 Reconsideration Annual Compressors"

   state_fate <- state_policy_2018(ad_proj)

   scn_fate <- ad_proj %>%

     # reconsidered wellsite fugitive monitoring
     mutate(fate = case_when(

       vintage < 2015 ~ pre_2016_baseline(ad_proj),

       year < recon_effective_year ~ policy_2018_baseline(ad_proj),

       location == "AK-NS" &
         mp %in% c("gaswellsite",
                   "oilwellsite",
                   "oilassocgaswellsite",

                   "lp_gaswellsite",
                   "lp_oilassocgaswellsite",
                   "lp_oilwellsite", #wellsites match baseline at annual

                   "gbstation",
                   "transstation",
                   "storstation") ~ "ogi_annual", #no AD yet

       mp %in% c("lp_gaswellsite",
                 "lp_oilassocgaswellsite",
                 "lp_oilwellsite") ~ "ogi_biennial",

       mp %in% c("gaswellsite", "oilassocgaswellsite", "oilwellsite") ~ "ogi_annual",

       mp %in% c("gbstation", "transstation", "storstation") ~ "ogi_annual",

       mp == "cert" ~ "inhouse_cert",

       TRUE ~ policy_2018_baseline(ad_proj) # keep at baseline
     ))

   result <- more_stringent(state_fate, scn_fate$fate)
 }

 policy_2018_quarterly_comps <- function(ad_proj) {
   #pol_scn <- "2018 Reconsideration Quarterly Compressors"

   state_fate <- state_policy_2018(ad_proj)

   scn_fate <- ad_proj %>%

     # reconsidered wellsite fugitive monitoring
     mutate(fate = case_when(

       vintage < 2015 ~ pre_2016_baseline(ad_proj),

       year < recon_effective_year ~ policy_2018_baseline(ad_proj),

       location == "AK-NS" &
         mp %in% c("gaswellsite",
                   "oilwellsite",
                   "oilassocgaswellsite",
                   "lp_gaswellsite",
                   "lp_oilassocgaswellsite",
                   "lp_oilwellsite", #wellsites match baseline at annual

                   "gbstation",
                   "transstation",
                   "storstation") ~ "ogi_annual", #no AD yet

       mp %in% c("lp_gaswellsite",
                 "lp_oilassocgaswellsite",
                 "lp_oilwellsite") ~ "ogi_biennial",

       mp %in% c("gaswellsite", "oilassocgaswellsite", "oilwellsite") ~ "ogi_annual",

       mp %in% c("gbstation", "transstation", "storstation") ~ "ogi_quarterly",

       mp == "cert" ~ "inhouse_cert",

       TRUE ~ policy_2018_baseline(ad_proj) # keep at baseline
     ))

   result <- more_stringent(state_fate, scn_fate$fate)
 }


