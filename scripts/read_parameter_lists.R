# read_parameter_lists.R

# model plants
mp_list <- read_excel("data-raw/parameters.xlsx", sheet = "mp_list")

# model plant attributes
# mp_attrs_list must be consistent with mp_list
mp_attrs_list <- read_excel("data-raw/parameters.xlsx", sheet = "mp_attrs_list")

# mp-fates
# list of model plant/control fate combinations 
mp_fates_list <- read_excel("data-raw/parameters.xlsx", 
                            sheet = "mp_fates_list", 
                            na = c("", "NA"))

# derived lists for consistent ordering in tables
mp_order <- mp_list$mp %>% unique()
src_order <- mp_list$source %>% unique()
det1_order <- mp_attrs_list$detail_1 %>% unique()
det2_order <- mp_attrs_list$detail_2 %>% unique()
det3_order <- mp_attrs_list$detail_3 %>% unique()
fate_order <- mp_fates_list$fate %>% unique()

src_labels <- unique(mp_list$source_detail) %>% set_names(unique(mp_list$source))


