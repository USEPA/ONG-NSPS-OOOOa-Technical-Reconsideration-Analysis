# 2018 O&G Technical Reconsideration of OOOOa
# Analysis Script


# This script loads and processes information for the technical reconsideration RIA

# load libraries and source functions
source("scripts/setup.R")

# load policy scenario functions
source("scripts/policy_2018techreconsideration.R")

# set proposed option:
prop_opt <- "opt3"
proposed_option <- "3.Option 3"
policy_2018_proposed <- policy_2018_opt3


# load ONG analysis parameters:
# Includes model plant types, control fates, and lists for consistent table displays.
source("scripts/read_parameter_lists.R")


# unit-level costs and emissions of model plants with different controls
mp_fates_2018_tech <- read_excel("data-raw/params_2018_tech_recon.xlsx", 
                            sheet = "mp_fates_2018_tech", 
                            na = c("", "NA")) 


# activity data (AD)
# these are projections of new and modified facilities for each model plant
# activity data projections are held constant across policy scenarios

source(here('scripts/2018_technical_activity-data-calcs.R'))
#activity data script returns ad_proj_2018_tech




# policy scenarios
# policy scenario functions define how model plants AD projections are mapped to emissions controls under a particular policy scenario based on a combination of federal and state regulations. They result in mp-fate projections, which are similar to the AD projections, but with the addition of information on emissions controls, in the `fate` column.

# (ad_proj, policy scenarios) -> (mp-fate-projections)
# e.g, each of form:
# mpf_bau <- ad_proj_2018_tech %>%
#   mutate(fate = policy_2018_baseline(.))

mpf_proj <- ad_proj_2018_tech %>% {
  list(
    "pre2016"        = mutate(., fate = pre_2016_baseline(.)),
    "baseline"       = mutate(., fate = policy_2018_baseline(.)),
    "opt1"           = mutate(., fate = policy_2018_opt1(.)),
    "opt2"           = mutate(., fate = policy_2018_opt2(.)),
    "opt3"           = mutate(., fate = policy_2018_opt3(.)),
    "ann_comps"      = mutate(., fate = policy_2018_ann_comps(.)),
    "quarterly_comps"  = mutate(., fate = policy_2018_quarterly_comps(.))
  )
}

# Calculate scenario metrics using the unit-level cost and emissions information for each model plant and control fate combination

# (mpf_proj, mp_fates_chars) -> (scn_proj)
# e.g., each of form:
# scn_bau <- add_scenario_calcs_v(mpf_bau, mp_fates_2018_tech)

scn_proj <- mp_fates_2018_tech %>% {
  list(
    "pre2016"        = add_scenario_calcs_v(mpf_proj[["pre2016"]], .),
    "baseline"       = add_scenario_calcs_v(mpf_proj[["baseline"]], .),
    "opt1"           = add_scenario_calcs_v(mpf_proj[["opt1"]], .),
    "opt2"           = add_scenario_calcs_v(mpf_proj[["opt2"]], .),
    "opt3"           = add_scenario_calcs_v(mpf_proj[["opt3"]], .),
    "ann_comps"      = add_scenario_calcs_v(mpf_proj[["ann_comps"]], .),
    "quarterly_comps"  = add_scenario_calcs_v(mpf_proj[["quarterly_comps"]], .)
  )
}

# Scenario Comparisons
# Calculate changes in metrics from moving from one policy scenario to another. In most cases policy option scenarios are compared to the requirements in the baseline scenario, but in principle other comparisons could be made. For example, some calculations are performed both relative to the current 2018 baseline, and the requirements that were in place prior to the 2016 NSPS.

# (scn_proj, scn_proj) -> (scn_comp)
# e.g., each of form:
# scn_comp <- compare_two_scenarios(scn_bau, scn_policy) %>%
#   mutate(year = as.character(year),
#          vintage = as.character(vintage))

scn_comps <- list(
  "opt1"           = compare_two_scenarios(scn_proj[["baseline"]], scn_proj[["opt1"]]),
  "opt2"           = compare_two_scenarios(scn_proj[["baseline"]], scn_proj[["opt2"]]),
  "opt3"           = compare_two_scenarios(scn_proj[["baseline"]], scn_proj[["opt3"]]),
  
  "ann_comps"      = compare_two_scenarios(scn_proj[["baseline"]], scn_proj[["ann_comps"]]),
  "quarterly_comps"  = compare_two_scenarios(scn_proj[["baseline"]], scn_proj[["quarterly_comps"]])
)

scn_combo <- bind_rows(
  mutate(scn_comps[["opt1"]], pol_scn = "1.Option 1"),
  mutate(scn_comps[["opt2"]], pol_scn = "2.Option 2"),
  mutate(scn_comps[["opt3"]], pol_scn = "3.Option 3"),
  
  mutate(scn_comps[["ann_comps"]], pol_scn = "5.AnnComps"),
  mutate(scn_comps[["quarterly_comps"]], pol_scn = "6.QuarterlyComps")
)

#### additional context comparisons

scn_comps_pre2016 <- list(
  "baseline"   = compare_two_scenarios(scn_proj[["pre2016"]], scn_proj[["baseline"]]),
  "opt1"       = compare_two_scenarios(scn_proj[["pre2016"]], scn_proj[["opt1"]]),
  "opt2"       = compare_two_scenarios(scn_proj[["pre2016"]], scn_proj[["opt2"]]),
  "opt3"       = compare_two_scenarios(scn_proj[["pre2016"]], scn_proj[["opt3"]])
  )

scn_pre2016_combo <-  bind_rows(
  mutate(scn_comps_pre2016[["baseline"]], pol_scn = "0.Pre2016 to Current Law"),
  mutate(scn_comps_pre2016[["opt1"]], pol_scn = "1.Pre2016 to Option 1"),
  mutate(scn_comps_pre2016[["opt2"]], pol_scn = "2.Pre2016 to Option 2"),
  mutate(scn_comps_pre2016[["opt3"]], pol_scn = "3.Pre2016 to Option 3")
)



####### Excel output scenario summaries --------

scn_summ_tab <- function(comp_dat) {
  scn_comp_table(comp_dat,
                 var_list = list("Methane", "VOC", "HAP", "CH4_CO2e", 
                                 "capital_cost", "annual_cost", "gas_revenue",
                                 "ann_7", "ann_7_wgas", "ann_3", "ann_3_wgas"),
                 row_detail = "total",
                 show_years = 2014:2025)
}


scn_summ_list <- map(
  list(scn_comps[["opt1"]], 
       scn_comps[["opt2"]],
       scn_comps[["opt3"]],
       scn_comps[["ann_comps"]],
       scn_comps[["quarterly_comps"]]
       ),
  .f = scn_summ_tab
) %>%
  set_names(c("opt1", "opt2", "opt3", "ann_comps", "quarterly_comps"
              ))


##### Text output for informal change detection ----------

# (scn_comp, table_specs) -> (table_data)

detail_tables <- scn_comp_table(scn_comps[[prop_opt]],
                                var_list = list("fac_affected", "Methane", "VOC", "HAP", "CH4_CO2e", "capital_cost", "annual_cost", "gas_revenue",  "ann_7_wgas"),
                                row_detail = "detail",
                                show_years = c(2020, 2025))

write_csv(detail_tables, "data/2018tech_detail_tables.csv")

write_csv(bind_rows(scn_summ_list, .id = "id"), "output/csv/tech_scn_summ_list.csv")


#### Ask the user whether to update xlsx outputs
# these are not automatically updated because they don't work well w/version control

switch(menu(c("Yes, Update RIA xlsx outputs.", 
         "No, do not update."),
         title = "Do you want to update RIA xlsx outputs?"), {
           
           # Update RIA xlsx outputs by rendering Rmarkown file
           rmarkdown::render("scripts/2018_tech_recon_RIA_xlsx_output.Rmd",
                             output_dir = "output/docs")
         }, "not updated")



