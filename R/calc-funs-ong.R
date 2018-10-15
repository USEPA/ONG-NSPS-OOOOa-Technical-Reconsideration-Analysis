# calc-funs-ong.R
# ONG RIA calculation functions

# Scenario-specific calculations are performed by `add_scenario_calcs_v`
# Comparisons between scenarios are performed by `compare_two_scenarios`

# The concept of 'affected facilities' really only makes sense in a comparison between two scenarios.
# Total emissions and total costs can be defined within a single scenario.
# However, in many cases costs in the BAU scenario are defined as zero, so there is an implicit comparison. This is changable, however, depending on how the costs are defined, and so this approach is attempting to make it possible to define as much as possible in a single scenario manner.

# NG price
# From AEO2018, no CPP case, wellhead price per Mcf, translated from 2017$ -> 2016$

ng_price <- c(
  "2014" = 2.34 * 5.71/3.71,
  "2015" = 2.34 * 4.26/3.71, # 2014/2015 not present in AEO18, scaling based on historical citygate price ratios.
  "2016" = 2.34,
  "2017" = 2.77,
  "2018" = 2.78,
  "2019" = 3.09,
  "2020" = 3.36,
  "2021" = 3.33,
  "2022" = 3.36,
  "2023" = 3.48,
  "2024" = 3.58,
  "2025" = 3.70,
  "2026" = 3.75,
  "2027" = 3.79,
  "2028" = 3.81,
  "2029" = 3.88,
  "2030" = 3.88
)

#' Takes a model-plant/fate projection and performs calculations to add emissions and cost projections
#' The mpf projection is specific to a single scenario
#' The calculations require information on the characteristics of mp-fates 
#' 
#' @param mpf_proj a model plant-fate projection
add_scenario_calcs_v <- function(mpf_proj, mpf_chars) {
  
  # variable lists
  em_vars   <- syms(list("Methane", "VOC", "HAP"))
  gas_vars  <- syms(list("flare_wholegas", "gas_capture"))
  cost_vars <- syms(list("capital_cost", "annual_cost"))
  
  # extra conversions
  
  GWP_CH4 <- 25
  metric_tons_per_short_ton <- 0.90718474
  
  # add additional fields based on mp-fate key
  proj_extra <- left_join(mpf_proj, mpf_chars, by = c("mp", "fate")) %>%
    mutate(control_lifetime = if_else(is.na(control_lifetime), 0, control_lifetime))
  
  # calculate emissions totals, gas flaring and capture
  proj_extra_2 <- proj_extra %>%
    mutate_at(.vars = vars(!!! em_vars, !!! gas_vars, !!! cost_vars), 
              .funs = funs(. * fac_count)) %>%
    
    # calculate CH4_CO2e
    mutate(CH4_CO2e = Methane * GWP_CH4 * metric_tons_per_short_ton) %>%
    
    # calculate annualized costs w/o revenue
    mutate(gas_revenue = unname(gas_capture * ng_price[as.character(year)]))
  
  proj_extra_3 <- proj_extra_2 %>%
    
    # calculate annualized costs w/o revenue at 3% and 7% 
    mutate(ann_3 = annualized_cost(control_lifetime, .03, capital_cost, annual_cost)) %>%
    mutate(ann_7 = annualized_cost(control_lifetime, .07, capital_cost, annual_cost)) %>%
    
    # calculate annualized costs w revenue at 3% and 7% 
    mutate(ann_3_wgas = annualized_cost(control_lifetime, .03, capital_cost, annual_cost, gas_revenue)) %>%
    mutate(ann_7_wgas = annualized_cost(control_lifetime, .07, capital_cost, annual_cost, gas_revenue))
  
  proj_extra_4 <- proj_extra_3 %>%
    mutate(capital_cost = case_when(
      year == vintage ~ capital_cost,
      year > vintage & ((vintage - year) %% control_lifetime == 0) ~ capital_cost,
      year > vintage ~ 0,
      TRUE ~ NA_real_
    ))
  
  result <- proj_extra_4
  result
}

#' Takes two scenario projections and produces comparison results
#' 
#' Scenario projections are mp-fate projections along with additional calculations
#' Comparisons can only be made for metrics which are available in both scenarios
#' In order to be comparable, the two scenarios must be related to the same activity
#' data projection and have data on the same scope of facilities.
compare_two_scenarios <- function(scn_bau, scn_policy) {
  
  # variables of interest (summing over fate...)
  mp_char_vars <- rlang::syms(list("mp", "attrs", "location", "vintage"))
  
  var_list <- rlang::syms(list(#"fac_count", 
                        "Methane", "VOC", "HAP", "CH4_CO2e",
                        "flare_wholegas", "gas_capture", "gas_revenue",
                        "capital_cost", "annual_cost", 
                        "ann_3", "ann_7", "ann_3_wgas", "ann_7_wgas"))
  
  
  # combine data from the 2 scenarios
  scn_both <- bind_rows(
    mutate(scn_bau, scn = "bau"),
    mutate(scn_policy, scn = "policy")
  )
    
  # to calculate change, negate the "bau" values, so a reduction would be negative
  scn_comp <- scn_both %>% 
    mutate(fac_diff = fac_count) %>%
    mutate(fac_count = if_else(scn == "bau", fac_count, 0)) %>%
    
    mutate_at(.vars = vars(fac_diff, !!!var_list),
              .funs = funs(if_else(scn == "bau", . * -1, .))) %>%
  
  # sum over scenarios
    group_by(!!!mp_char_vars, fate, year) %>%
    summarize_at(.vars = vars(fac_count, fac_diff, !!!var_list),
                 .funs = funs(sum(., na.rm = TRUE))) %>%
    ungroup() %>%
    
  # affected facilities are mp's w/ different fate in policy than bau
    mutate(fac_affected = pmax(fac_diff, 0)) %>%
  
  # sum over fates
    group_by(!!!mp_char_vars, year) %>%
    summarize_at(.vars = vars(fac_count, fac_diff, fac_affected, !!!var_list),
                 .funs = funs(sum(., na.rm = TRUE))) %>%
    ungroup() %>%
    
  # convert year variables to character
    mutate(year = as.character(year),
           vintage = as.character(vintage))
    
  scn_comp
}


#' Calculate annualized costs for a vector of ONG projects
#' 
#' Calculates annualized costs (optionally with revenue from gas sales)
#' Assumes that capital costs occur in the initial period and that operating costs occur starting after 1 period
#' 
#' @param contr_life the number of periods over which to annualize costs (numeric vector)
#' @param r the discount rate (numeric vector)
#' @param cap_cost capital costs (numeric vector)
#' @param annual_cost annual costs, the first of which is one period out (unless contr_life is 0) (numeric vector)
#' @param gas_rev revenue from gas sales, the first of which is one period out (unless contr_life is 0) (numeric vector)
annualized_cost <- function(contr_life, r, cap_cost, annual_cost, gas_rev = NA_real_) {
  
  if (! is.numeric(contr_life) & 
      is.numeric(r) &
      is.numeric(cap_cost) & 
      is.numeric(annual_cost) & 
      is.numeric(gas_rev)
  ) stop("Arguments to `annualized cost` must be numeric")
  
  if (any(is.infinite(cap_cost)) |
      any(is.infinite(annual_cost)) |
      any(is.infinite(gas_rev))
  ) stop("Arguments to `annualized cost` must be finite.")
  
  if (isTRUE(min(contr_life) < 0)) stop("Argument control lifetime must be >= 0")
  
  gas_rev <- if_else(is.na(gas_rev), 0, gas_rev)
  
  result <- case_when(
    is.na(contr_life) ~ NA_real_,  
    contr_life == 0 ~ cap_cost + annual_cost - gas_rev,
    TRUE ~ 
      equiv_annualized_value(cap_cost + 
                               npv_fixed_pmts(annual_cost - gas_rev, contr_life, r), contr_life, r)
    
  )
  
  result
}

