
# clean environment -------------------------------------------------------

rm(list = ls(all.names = TRUE))
graphics.off()
cat("\014") 
#Alt + Shift + K  == gets you all shortcuts

# load some packages ------------------------------------------------------

library(tidyverse)
library(lubridate)
library(tigris)
library(sf)

# load geospatial data ----------------------------------------------------

# NYC borough boundaries. Suggested EPSG = 6539. Default shp EPSG should be 2263

borough_boundries <-
  st_read('data/raw/shapefiles/borough_boundries/geo_export_830dcae7-8918-4bbd-9062-5cfe61a5a1ac.shp') %>%
  st_transform(crs = 2263)

# dycd programs of interest -----------------------------------------------

# final DYCD program type list
programs_of_interest <- 
  c("Adolescent Literacy",
    "Advance and Earn",
    "Beacon",
    "COMPASS Elementary",
    "COMPASS High School",
    "COMPASS Middle School",
    "COMPASS Explore",
    "COMPASS Horizon",
    "COMPASS SONYC Pilot",
    "Cornerstone",
    "Fatherhood Prior Involvement",
    "Fathers aged 18 and over",
    "General NYCHA",
    "High School",
    "Ladders for Leaders (LFL)",
    "Learn and Earn",
    "Learning Labs",
    "NYCHA MAP",
    "Older Youth (OY)",
    "Opportunity Youth Support",
    "Over Threshold",
    "SYEP School-Based",
    "Train and Earn",
    "Under Threshold",
    "Vulnerable Youth (VY)",
    "WLG (OY)",
    "YAIP", 
    "YAIP Plus",
    "Year Round Sector Focus",
    "Young Adult Literacy Program (YALP)",
    "Younger Youth (YY)")

# load dycd data ----------------------------------------------------------

# in the case the aggregate dycd dataset needs to be created, run the following:
#source('scripts/wrangling/create_aggregate_dycd_file.R')

# loads 2019, 2020, and 2021 dycd dataset we made above
dycd_program_data <- 
  read_csv('data/raw/dycd/dycd_program_sites_2019_2020_2021.csv') %>% 
  filter(!is.na(latitude) & !is.na(longitude),
         program_type %in% programs_of_interest) %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326) %>% 
  st_transform(
    st_crs(borough_boundries)) %>% 
  st_filter(borough_boundries) %>% 
  rename(year = fiscal_year) %>% 
  # for subgroup analyses
  mutate(
    all_site_type_indicator = case_when(
      # all sites types indicator - case_when is used to avoid NAs
      (!is.na(year)) ~ 1,
      TRUE ~ 0),
    # Community Services and Family Support indicator 
    # comm_serv_fam_supp_indicator = case_when(
    #   (grepl("Community Services", service_category) | 
    #      grepl("Family Support", service_category)) ~ 1,
    #   TRUE ~ 0),
    # SYEP indicator - case_when is used to avoid NAs
    syep_site_type_indicator = case_when(
      (program_area == "SYEP" | 
         grepl("Jobs and Internships", service_category) &
         !(grepl("Afterschool Programs", service_category))) ~ 1,
      TRUE ~ 0),
    # ASP indicator - case_when is used to avoid NAs
    asp_site_type_indicator = case_when(
      (program_area == "Compass" | 
         grepl("Afterschool Programs", service_category) &
         !(grepl("Jobs and Internships", service_category))) ~ 1,
      TRUE ~ 0))

# crime types -------------------------------------------------------------

# violent and property crime specifications are loaded here
source('scripts/wrangling/crimes_violent_property.R')

# load crime data ---------------------------------------------------------

# in the case the crime dataset needs to be pulled, run the following:
# source('scripts/wrangling/nypd_crime_api_pull.R')

# load nypd data and clean the names to make future analyses easier
nypd_complaint_data <- read_rds('data/raw/crime/nypd_all_complaints_2023-02-08.rds') %>% 
  mutate(cmplnt_fr_dt = ymd(cmplnt_fr_dt),
    year = year(cmplnt_fr_dt)) %>% 
  filter(
    !is.na(cmplnt_fr_dt),
    year(cmplnt_fr_dt) %in% 2019:2022,
    !is.na(longitude) & !is.na(latitude),
    # We're only interested in suspects in our age group of interest
     # vic_age_group %in% c('<18', '18-24') | 
      susp_age_group %in% c('<18', '18-24')) %>% 
  # adding our violent crime variable
  mutate(
    violent_crime = if_else(
      pd_desc %in% violent_crimes,
      1,
      0),
    property_crime = if_else(
      pd_desc %in% property_crimes,
      1,
      0)) %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'),
    crs = 4326) %>% 
  st_transform(
    st_crs(borough_boundries)) %>% 
  st_filter(borough_boundries) 

# checking annual frequencies: how many per-year complaints do we have?
# yearly_complaint_freq <-
#   nypd_complaint_data %>%
#   as_tibble() %>% 
#   count(year) %>% 
#   ggplot() + 
#   geom_col(mapping = aes(x = year, y = n))

# load acs data -----------------------------------------------------------

# run the line below or visit the file in case we need to recreate an .rds
# source('scripts/wrangling/acs_wrangle.R')

# the following lines imputes median hh income for our interaction terms
# load our 2019-2021 acs data
acs_data1 <-
  read_rds('data/processed/acs_data_v2.rds') %>% 
  mutate(
    geoid = as.numeric(geoid))

# # library(haven)
# acs_data %>%
#   st_drop_geometry() %>%
#   write_dta('data/processed/data_to_be_imputed_2023-03-06.dta')

acs_data_imputed <- 
  read_csv('data/processed/imputed_data_2023-03-06.csv')

acs_data <- 
  acs_data_imputed %>% 
  group_by(year) %>%
  mutate(
    # making our imputed median hh income our regular median hh income variable
    median_hh_inc = imputed_mhhi,
    # calculating annual median HH inc for our threshold variable
    frst_quart = quantile(median_hh_inc, .25, na.rm = TRUE),
    # calculating annual young bgs for our threshold variable
    mean_youth = mean(prcnt_yth_yng_adlt, na.rm = TRUE),
    stdev_youth = sd(prcnt_yth_yng_adlt, na.rm = TRUE),
    yth_prop_thresh = mean_youth + stdev_youth,
    # .5 stdevs below income indicator
    below_inc_theshold = if_else(
      median_hh_inc < frst_quart,
      1,
      0),
    # 1 stdev above average youth concentration 
    above_yth_prop_threshold = if_else(
      prcnt_yth_yng_adlt > yth_prop_thresh,
      1,
      0)) %>%
  ungroup() %>% 
  select(-imputed_mhhi) %>% 
  left_join(
    acs_data1 %>% 
      select(geoid, year, geometry),
    .,
    by = c('geoid', 'year')
  )

# adding pseudo 2022 data for lagged crime data
acs_data_2019_2022 <- 
  rbind(
    acs_data,
    acs_data %>% 
      filter(year == 2021) %>% 
      mutate(year = 2022))

# acs_data %>% 
#   filter(year == 2019) %>% 
#   ggplot() +
#   geom_sf() +
#   theme_void()
# 
# ggsave(filename = 'blank_nyc.png',
#        path = 'output/plots/',
#        width = 8,
#        height = 8,
#        units = 'in')

# cleaning dycd data ------------------------------------------------------

# iterating through and creating a df of all dycd site + ACS data by year
cleaned_dycd_programs <- 
  map_df(
    2019:2022,
    ~ dycd_program_data %>% 
      filter(year == .x) %>% 
      st_join(
        acs_data_2019_2022 %>% 
          filter(year == .x),
        .)) %>% 
  st_drop_geometry() %>% 
  rename(year = year.x) %>% 
  # collapsing by geoid and year
  group_by(geoid, year) %>% 
  summarize(
    all_site_type = if_else(
      # all sites types - using sum to help collapse rows
      sum(all_site_type_indicator, na.rm = TRUE) > 0,
      1,
      0),
    # SYEP, ASP, or a both
    syep_asp_othr_site_type = case_when(
      # coding for other
      (sum(syep_site_type_indicator) > 0 &
         sum(asp_site_type_indicator) > 0) ~ 3,
      # coding for syep only
      (sum(syep_site_type_indicator) > 0 & 
        sum(asp_site_type_indicator) == 0) ~ 1,
      # coding for asp only
      (sum(asp_site_type_indicator) > 0 & 
      sum(syep_site_type_indicator) == 0) ~ 2,
      # coding for community/family centers
      # (sum(comm_serv_fam_supp_indicator) > 0 & 
      #   sum(asp_site_type_indicator) == 0 & 
      #    sum(syep_site_type_indicator) == 0) ~ 4,
      TRUE ~ 0)) %>% 
  mutate(
    # dummying out SYEPs
    syep_site_type = if_else(
      syep_asp_othr_site_type == 1,
      1,
      0),
    # dummying out ASPs
    asp_site_type = if_else(
      syep_asp_othr_site_type == 2,
      1,
      0)
    # ,
    # comm_fam_site_type = if_else(
    #   syep_asp_othr_site_type == 4,
    #   1,
    #   0)
    ) %>% 
  ungroup() 



# cleaning nypd complaint data --------------------------------------------

# iterating through and creating a df of all crime data from 2019 to 2021 + ACS
cleaned_complaint_data <- 
  map_df(
    2019:2022,
    ~ nypd_complaint_data %>% 
      filter(year == .x) %>% 
      st_join(
        acs_data_2019_2022 %>% 
          filter(year == .x),
        .) %>% 
      st_drop_geometry()) %>% 
  select(!year.y) %>% 
  rename(year = year.x) %>% 
  # collapsing crime data down to geoid and year (getting counts)
  # issues? Try the 'detach(...' at the end of this section
  group_by(geoid, year) %>% 
  summarize(
    num_cmplnt_ttl = sum(!is.na(cmplnt_num)),
    num_cmplnt_vlnt = sum(violent_crime, na.rm = TRUE),
    num_cmplnt_pprty = sum(property_crime, na.rm = TRUE))

# detach("package:Hmisc", unload=TRUE)

# create regression dataset -----------------------------------------------

# mashing the nypd and dycd lists together
dycd_nypd_2019_2022 <- 
  full_join(
    cleaned_dycd_programs,
    cleaned_complaint_data,
    by = c('geoid', 'year'))

# adding acs data, calculating crime rate, and identifying usable tracts
reg_dataset <- 
  full_join(
    acs_data_2019_2022,
    dycd_nypd_2019_2022,
    by = c('geoid', 'year')) %>% 
  # calculating crime rate and making ACS data percents (not proportions)
  mutate(
    crime_rate = (num_cmplnt_ttl/total_pop)*100000,
    # addressing zeros for logging crime rate
    crime_rate = if_else(
      crime_rate == 0,
      0.000000000000000000000000000001,
      crime_rate),
    violent_crime_rate = (num_cmplnt_vlnt/total_pop)*100000,
    # addressing zeros for logging violent crime rate
    violent_crime_rate = if_else(
      violent_crime_rate == 0,
      0.000000000000000000000000000001,
      violent_crime_rate),
    property_crime_rate = (num_cmplnt_pprty/total_pop)*100000,
    # addressing zeros for logging property crime rate
    property_crime_rate = if_else(
      property_crime_rate == 0,
      0.000000000000000000000000000001,
      property_crime_rate),
    logd_crime_rate = log(crime_rate),
    logd_violent_crime_rate = log(violent_crime_rate),
    logd_property_crime_rate = log(property_crime_rate),
    across(
      .cols = prcnt_unemp:prcnt_ba_or_hghr_deg,
      .fns = ~ . * 100),
    # taking the log of all variables
    # across(
    #   .cols = median_hh_inc:prcnt_ba_or_hghr_deg,
    #   .fns = ~ log(.),
    #   .names = '{.col}_log')
    )

# leading crime -----------------------------------------------------------

crime_data_4_lagging <- 
  reg_dataset %>% 
  st_drop_geometry() %>% 
  # selecting only relevant columns
  select(geoid, year, num_cmplnt_ttl:logd_property_crime_rate) %>% 
  # removing 2019 observations
  filter(year != 2019) %>% 
  # renaming year values such that everything gets pseudo-shifted
  mutate(
    year = case_when(
    year == 2020 ~ 2019,
    year == 2021 ~ 2020,
    year == 2022 ~ 2021)) %>% 
  # renaming crime columns for when we add it to the dataset
  rename_with(
    .fn = ~ paste0('lead_', .),
    .cols = num_cmplnt_ttl:logd_property_crime_rate)

# adding lagged/lead crime
reg_dataset_lagged <- 
  reg_dataset %>% 
  filter(year %in% 2019:2021) %>% 
  left_join(
    .,
    crime_data_4_lagging,
    by = c('geoid', 'year'))

# removing unhelpful block groups -----------------------------------------

central_prk <- '360610277001'
rikers <- '360050001001'
mnt_olv_cem <- '360810607011' # pop = 14
cyp_hill_cem <- '360810561001' # pop = 17
flyd_bnt_fld <- '360470702021' # pop = 8
hly_crss_cem <- '360470852001' # pop = 26
sth_frsh_klls_prk <- '360850228011' # pop = 21
innwd_park <- '360610297001' # pop = 22
# high_pop_outlier <- '360470555001' # pop = 7696
high_crime_1 <- '360810179021' # pop = 5; crime = 436555.290
high_crime_2 <- '360610109001' # pop = 127; crime = 128488.372
brnx_zoo <- '360050334001' # pop = 15

# removing geoids with no population, NAs, and irrelevant block groups
reg_dataset_testing <-
  reg_dataset_lagged %>%
  select(-frst_quart, -mean_youth, -stdev_youth, -yth_prop_thresh) %>% 
  # st_drop_geometry() %>%
  group_by(geoid) %>% 
  # booted is when all geoids are knocked out if even a single one has pop == 0
  mutate(
    booted = if_else(
      any(total_pop == 0),
      1,
      0),
    geoid = as.numeric(geoid)) %>%
  filter(
    total_pop > 0,
    booted == 0,
    geoid != central_prk,
    geoid != rikers,
    geoid != mnt_olv_cem,
    geoid != cyp_hill_cem,
    geoid != flyd_bnt_fld,
    geoid != hly_crss_cem,
    geoid != sth_frsh_klls_prk,
    geoid != innwd_park,
    # geoid != high_pop_outlier,
    # geoid != high_crime_1,
    # geoid != high_crime_2,
    geoid != brnx_zoo) %>% 
  ungroup() %>% 
  # I want the geometry at the end of the dataframe
  relocate(geometry, .after = last_col()) 

# saving data files -------------------------------------------------------

# create a .dta
# library(haven)
# reg_dataset_testing %>%
#   st_drop_geometry() %>%
#   write_dta('data/processed/thesis_dataset_2023-03-28.dta')

# library(haven)
# create an .rds
# reg_dataset_testing %>%
#   st_drop_geometry() %>%
#   write_rds('data/processed/thesis_dataset_2023-03-07.rds')

# manipulation checks (delete later) --------------------------------------

# library(Hmisc)

# CHECKING FREQUENCIES
# first set of frequencies
# reg_dataset_testing_4_hist_1 <- 
#   reg_dataset_testing %>% 
#   as_tibble() %>% 
#   select(median_hh_inc:prcnt_all_other)
# hist.data.frame(reg_dataset_testing_4_hist_1)
# 
# # second set of frequencies
# reg_dataset_testing_4_hist_2 <- 
#   reg_dataset_testing %>% 
#   as_tibble() %>% 
#   select(prcnt_yth_yng_adlt:prcnt_ba_or_hghr_deg, all_site_type:property_crime_rate)
# hist.data.frame(reg_dataset_testing_4_hist_2)

# general frequencies over time
# yearly_freqs <- reg_dataset_testing %>%
#   as_tibble() %>%
#   group_by(year) %>%
#   summarise(
#     total_pop = sum(total_pop),
#     n = n(),
#     num_cmplnt_ttl = sum(num_cmplnt_ttl),
#     num_cmplnt_vlnt = sum(num_cmplnt_vlnt),
#     avg_crime_rate = mean(crime_rate),
#     avg_violent_crime_rate = mean(violent_crime_rate),
#     avg_pprty_crime_rate = mean(property_crime_rate),
#     num_bgs_w_sites = sum(all_site_type),
#     num_bgs_w_syep = sum(syep_site_type),
#    num_bgs_w_asp = sum(asp_site_type))
  
# here we can get an interactive visual of the unit of analysis overlayed on NYC
# library(tmap)
# tmap_mode('view')
# tm_basemap('OpenStreetMap') +
#   reg_dataset_lagged %>%
#   filter(year == 2019) %>%
#   tm_shape() +
#   tm_polygons(col = 'median_hh_inc') +
#   tmap_options(check.and.fix = TRUE)

# updating data with imputed stata values ---------------------------------

















