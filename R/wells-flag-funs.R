#wells-flag-funs.R

#functions to categorize wells for base year analysis


well_is_producing <- function(data) {
  
  if(! all(has_name(data, c("GOR", "GOR_QUAL", "FIPS_CODE")))) stop("`data` must contain columns GOR, GOR_QUAL, and FIPS_CODE")
  
  data %>%
    mutate(result = if_else(
      # onshore
      (!is.na(FIPS_CODE) & FIPS_CODE != "OFFSHORE") &
      
      # oil, gas, or oil w/ associated gas
      (!is.na(GOR)) & (GOR_QUAL %in% c("Gas only", "Liq only", "Liq+Gas"))
        
      , TRUE, FALSE)) %>%
    pull(result)
}

well_has_comp <- function(data) {
  
  if(! all(has_name(data, c("GOR", "GOR_QUAL", "FIPS_CODE", "HF", "COMPLETION_YEAR")))) stop("`data` must contain columns GOR, GOR_QUAL, FIPS_CODE, HF, and COMPLETION_YEAR")
  
  data %>%
    
    mutate(result = if_else( 
      # onshore
      (!is.na(FIPS_CODE) & FIPS_CODE != "OFFSHORE") &
      
      # oil, gas, or oil w/ associated gas 
      (!is.na(GOR)) & (GOR_QUAL %in% c("Gas only", "Liq only", "Liq+Gas")) & 
              
      (COMPLETION_YEAR == YEAR), TRUE, FALSE)) %>%
    pull(result)
}

well_has_hf_comp <- function(data) {
  
  if(! all(has_name(data, c("GOR", "GOR_QUAL", "FIPS_CODE", "HF", "COMPLETION_YEAR")))) stop("`data` must contain columns GOR, GOR_QUAL, and FIPS_CODE")
  
  data %>%
    
    mutate(result = if_else( 
      # onshore
      (!is.na(FIPS_CODE) & FIPS_CODE != "OFFSHORE") &
      
      # oil, gas, or oil w/ associated gas 
      (!is.na(GOR)) & (GOR_QUAL %in% c("Gas only", "Liq only", "Liq+Gas")) &
      
      #HF flag
      HF == "Yes" &
      COMPLETION_YEAR == YEAR, TRUE, FALSE)) %>%
    pull(result)
  
}

