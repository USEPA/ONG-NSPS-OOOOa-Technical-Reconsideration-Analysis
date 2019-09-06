# 2018_technical_activity-data-calcs.R
# Calculation of activity data projections for the 2018 Technical Reconsideration Proposal RIA

######

# Activity data projections datasets include the following information:
# mp -- the model plant type being tracked
# attrs -- attributes on mp
# location -- state or region where the facility is located (for state program applicability)
# vintage -- when the plant was established/created/modified for regulatory applicability purposes
# year -- the projection year
# fac_count -- count of facilities




#### Activity data projection options ----------------
base_year <- 2014
year_range <- 2014:2025

#### READ IN summarized 2014 Completions Data from DrillingInfo wells analysis -------------
by_welltype_loc <- read_csv("data/comps-base-year.csv") %>%
  rename(by_pct = percent,
         by_count = n) %>%

  # translate into simpler mp-like names
  mutate(mp = case_when(
    WELLTYPE6 == "gas (GOR > 100,000)" ~ "gaswellcomp",
    WELLTYPE6 == "oil w/ assoc gas" ~ "oilassocgaswellcomp",
    WELLTYPE6 == "black oils (GOR < 300)" ~ "oilwellcomp",
    WELLTYPE6 == "low production -- gas" ~ "lp_gaswellcomp",
    WELLTYPE6 == "low production -- oil w/assoc gas" ~ "lp_oilassocgaswellcomp",
    WELLTYPE6 == "low production -- oil only" ~ "lp_oilwellcomp"
  )) %>%

  rename(location = LOCATION) %>%
  mutate(attrs = NA_character_) %>%
  select(mp, attrs, location, by_count)


#### READ In AEO 2018 Drilling Series -----------
aeo18_drilling <- read_excel("data-raw/aeo2018_tab14_ongsupply.xlsx") %>%
  filter(`...1` == "OGS000:la_TotalLower48W") %>%
  select(`2014`:`2025`) %>%
  gather(YEAR, wells_drilled) %>%

  mutate(YEAR = as.numeric(YEAR),
         wells_drilled = as.numeric(wells_drilled) * 1000) %>% # AEO reports in thousands

  rename(aeo_wells_drilled = wells_drilled,
         year = YEAR)



#### Projection of Completions ---------------
ad_comps_proj <- expand(by_welltype_loc,
               nesting(mp, location),
               crossing(year_range)) %>%
  rename(year = year_range) %>%

  # Join AEO and 2014 completions data to the above structure
  arrange(year, mp, location) %>%
  left_join(by_welltype_loc, by = c("mp", "location")) %>%
  left_join(aeo18_drilling, by = "year") %>%

  # Multiply change in AEO wells drilled by 2014 completions information
  group_by(mp, location) %>%
  mutate(count = aeo_wells_drilled / (aeo_wells_drilled[year==2014] %ifempty% NA_real_) * by_count) %>%
  ungroup() %>%

  # Delete unnecessary columns used in prior calculation
  select(-by_count, -aeo_wells_drilled) %>%
  mutate(attrs = case_when(
    location == "AK-NS" ~ "AK-NS",
    TRUE ~ NA_character_)) %>%

  # Normalize column structure
  select(mp, attrs, location, year, count)



##### Calculate wellsites projection from completions projection ------------
wellsites_proj <- expand(ad_comps_proj,
                         nesting(mp, attrs, location, year),
                         crossing(year_index = year_range)) %>%
  rename(vintage = year) %>%
  left_join(ad_comps_proj, by = c("mp", "attrs", "location", "vintage"="year")) %>%
  filter(year_index >= vintage) %>%

  # change e.g., oilassocgascomp --> oilassocgaswellsite
  mutate(mp = str_replace(mp, "comp", "site")) %>%

  # Divide completions by 2 to get wellsites (average 2 wells per site)
  mutate(fac_count = count / 2) %>%
  rename(year = year_index) %>%
  select(mp, attrs, location, vintage, year, fac_count) %>%
  mutate(vintage = as.numeric(vintage))


####### Calculate compressor station projections --------------

GHGI_stations <- tibble(
  mp = c("gbstation", "transstation", "storstation"),
  attrs = rep(NA_character_, 3),
  location = rep(NA_character_, 3),
  count_per_year = c(212, 36, 2)
)

new_stations_proj <- expand(GHGI_stations,
                                nesting(mp, attrs, location),
                                crossing(vintage = year_range)) %>%
  left_join(GHGI_stations, by = c("mp", "attrs", "location")) %>%
  rename(fac_count = count_per_year)

stations_proj <- expand(new_stations_proj,
                        nesting(mp, attrs, location),
                        crossing(vintage = year_range,
                                 year = year_range)) %>%
  left_join(new_stations_proj, by = c("mp", "attrs", "location", "vintage")) %>%
  filter(year >= vintage)

###### Calculate Additional activity data projections ----------


###### Closed Vent Systems projections ----------------

# CVS design / infeasibility certification counts are estimated by a combination of estimates of storage tanks (which are driven by well counts that change each year) and a fixed addition of CVS associated with pumps and compressors.

base_cvs <- tibble(
  mp = c("cert"),
  attrs = c(NA_character_),
  location = c(NA_character_),
  base_year_count = 18616, # storage tanks ~ wells, change per year
  fixed_count = 3794 + 8 + 18 # from 2018 GHGI; 87.5% of pumps, 10% of total recip compressors, 100% of wet seal centrifugal compressors ~ fixed per year
)

new_cvs_proj <- expand(base_cvs,
                  nesting(mp, attrs, location),
                  crossing(vintage = year_range)) %>%

  # Join base year count and AEO drilling projection
  left_join(base_cvs, by = c("mp", "attrs", "location")) %>%
  left_join(aeo18_drilling, by = c("vintage"="year")) %>%
  mutate(year = vintage) %>%

  # Multiply change in AEO wells drilled by 2014 completions information
  group_by(mp, attrs, location) %>%
  mutate(fac_count = aeo_wells_drilled / (aeo_wells_drilled[vintage==2014] %ifempty% NA_real_) * base_year_count + fixed_count) %>%
  ungroup() %>%

  # Delete unnecessary columns used in prior calculation
  select(-base_year_count, -aeo_wells_drilled) %>%
  select(mp, attrs, location, vintage, year, fac_count)


cvs_proj <- expand(new_cvs_proj,
                           nesting(mp, attrs, location),
                           crossing(vintage = year_range,
                                    year = year_range)) %>%
  left_join(select(new_cvs_proj, -year), by = c("mp", "attrs", "location", "vintage")) %>%
  filter(year >= vintage)

##### Drivers Summary -----

drivers_summary <- bind_rows(
  aeo18_drilling %>% mutate(mp = "AEO18 wells drilled") %>% rename(count = aeo_wells_drilled),
  ad_comps_proj %>% group_by(year) %>% summarize(count = sum(count)) %>% mutate(mp = "D.I. completions"),
  new_stations_proj %>% select(mp, year=vintage, count = fac_count),
  new_cvs_proj %>% select(mp, year, count=fac_count)
)


##### Assemble overall activity data projection of relevant facilities ----------------

ad_proj_2018_tech <- bind_rows(wellsites_proj,
                             stations_proj,
                             new_cvs_proj)




  rm(ad_comps_proj, aeo18_drilling, wellsites_proj,
     GHGI_stations, new_stations_proj, stations_proj,
     base_csv, new_cvs_proj, cvs_proj )


