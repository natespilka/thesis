# clean environment -------------------------------------------------------

# rm(list = ls(all.names = TRUE))
# graphics.off()
# cat("\014") 
#Alt + Shift + K  == gets you all shortcuts

# load some packages ------------------------------------------------------

# library(tidyverse)
# library(tigris)
# library(sf)

# load geospatial data ----------------------------------------------------

# NYC borough boundaries. Suggested EPSG = 6539. Default shp EPSG should be 2263

borough_boundries <-
  st_read('data/raw/shapefiles/borough_boundries/geo_export_830dcae7-8918-4bbd-9062-5cfe61a5a1ac.shp') %>%
  st_transform(crs = 2263)

# identify acs variables --------------------------------------------------

# pulling five-year ACS data from 2019
# census_variables <-
#   tidycensus::load_variables(
#     year = 2019,
#     dataset = 'acs5')

# economic variables
econ_vars <- c(
  # median household income
  med_hh_inc = 'B19013_001',
  # number of civilians unemployed (raw numbers)
  unemployed_people_num = 'B23025_005',
  # number of people in civilian labor force (raw numbers)
  civ_labor_force_num = 'B23025_003',
  # number of people employed (raw numbers)
  employed_people_num = 'B23025_004')

# demographic variables
demo_vars <- c(
  # total population for race/ethnicity variables (raw numbers)
  total_race_ethn_pop = 'B03002_001',
  # number of white people (raw numbers)
  white_num = 'B03002_003',
  # number of Black people (raw numbers)
  black_num = 'B03002_004',
  # number of Hispanic/Latino people (raw numbers)
  hisp_num = 'B03002_012',
  # number of Asian people (raw numbers)
  asian_num = 'B03002_006',
  # number of AIAN people (raw numbers)
  aian_num = 'B03002_005',
  # number of nhopi people (raw numbers)
  nhopi_num = 'B03002_007',
  # number of "Other" people (raw numbers)
  other_num = 'B03002_008',
  # number of "two or more races" people (raw numbers)
  two_or_more_num = 'B03002_009',
  # number of "two or more races + some other race" people (raw numbers)
  two_or_more_more_num = 'B03002_010',
  # number of "two/three or more races + some other race" people (raw numbers)
  two_or_three_or_more_more_num = 'B03002_011',
  # total sex by age population (raw numbers)
  total_pop = 'B01001_001',
  # # population Male Under 5 Years	B01001003
  # male_undr_5 = 'B01001_003',
  # # population Male 5 To 9 Years	B01001004
  # male_5_to_9 = 'B01001_004',
  # population Male 10 To 14 Years	B01001005
  male_10_to_14 = 'B01001_005',
  # population Male 15 To 17 Years	B01001006
  male_15_to_17 = 'B01001_006',
  # population Male 18 And 19 Years	B01001007
  male_18_to_19 = 'B01001_007',
  # population Male 20 Years	B01001008
  male_20 = 'B01001_008',
  # population Male 21 Years	B01001009
  male_21 = 'B01001_009',
  # population Male 22 To 24 Years	B01001010
  male_22_to_24 = 'B01001_010',
  # # population Female Under 5 Years	B01001027
  # female_undr_5 = 'B01001_027',
  # # population Female 5 To 9 Years	B01001028
  # female_5_to_9 = 'B01001_028',
  # population Female 10 To 14 Years	B01001029
  female_10_to_14 = 'B01001_029',
  # population Female 15 To 17 Years	B01001030
  female_15_to_17 = 'B01001_030',
  # population Female 18 And 19 Years	B01001031
  female_18_to_19 = 'B01001_031',
  # population Female 20 Years	B01001032
  female_20 = 'B01001_032',
  # population Female 21 Years	B01001033
  female_21 = 'B01001_033',
  # population Female 22 To 24 Years	B01001034
  female_22_to_24 = 'B01001_034')

# education variables
edu_vars <- c(
  # total population for 'EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER'
  total_edu_pop = 'B15003_001',
  # number of people: no school completed without a high school degree
  no_hs_ns_num = 'B15003_002',
  # number of people: Nursery school without a high school degree
  no_hs_nurs_num = 'B15003_003',
  # number of people: Kindergarten without a high school degree
  no_hs_k_num = 'B15003_004',
  # number of people: 1th grade without a high school degree
  no_hs_1_num = 'B15003_005',
  # number of people: 2th grade without a high school degree
  no_hs_2_num = 'B15003_006',
  # number of people: 3th grade without a high school degree
  no_hs_3_num = 'B15003_007',
  # number of people: 4th grade without a high school degree
  no_hs_4_num = 'B15003_008',
  # number of people: 5th grade without a high school degree
  no_hs_5_num = 'B15003_009',
  # number of people: 6th grade without a high school degree
  no_hs_6_num = 'B15003_010',
  # number of people: 7th grade without a high school degree
  no_hs_7_num = 'B15003_011',
  # number of people: 8th grade without a high school degree
  no_hs_8_num = 'B15003_012',
  # number of people: 9th grade without a high school degree
  no_hs_9_num = 'B15003_013',
  # number of people: 10th grade without a high school degree
  no_hs_10_num = 'B15003_014',
  # number of people: 11th grade without a high school degree
  no_hs_11_num = 'B15003_015',
  # number of people: 12th grade without a high school degree
  no_hs_12_num = 'B15003_016',
  # number of people: with a high school diploma
  hs_diploma_num = 'B15003_017',
  # number of people: with a GED or equivalent
  ged_num = 'B15003_018',
  # number of people: Some college, less than 1 year
  some_college_1_num = 'B15003_019',
  # number of people: Some college, 1 or more years, no degree
  some_college_no_deg_num = 'B15003_020',
  # number of people: Associate's degree
  associates_num = 'B15003_021',
  # number of people with a bachelor's degree
  ba_deg_num = 'B15003_022',
  # number of people with a master's degree
  ma_deg_num = 'B15003_023',
  # number of people with a Professional school degree (e.g., jd)
  prfsnl_deg_num = 'B15003_024',
  # number of people with a Doctorate degree
  dctrl_deg_num = 'B15003_025')

# tidycensus pull ---------------------------------------------------------

years <- list(2019, 2020, 2021) 

# pulling acs data
acs_data <-
  map_dfr(
    years,
    ~ tidycensus::get_acs(
      geography = 'block group',
      variables = c(
        econ_vars,
        demo_vars,
        edu_vars),
      year = .x,
      state = 'NY',
      county = c('Queens', 
                 'Richmond',
                 'New York',
                 'Kings',
                 'Bronx'),
      survey = 'acs5',
      output = 'wide',
      geometry = TRUE),
    .id = "year") %>% 
  janitor::clean_names() %>% 
  mutate(
    year = case_when(
      year == 1 ~ 2019,
      year == 2 ~ 2020,
      year == 3 ~ 2021))

# crosswalk 2019 acs bgs to 2020 acs bgs ----------------------------------

# load in crosswalk dataset and only use block-groups that are in 2020 census
crosswalk <- 
  read_csv('data/raw/nhgis_crosswalk/nhgis_bg2010_bg2020_36/nhgis_bg2010_bg2020_36.csv') %>%
  # we must mutate since the BG identifiers are numeric (not character)
  mutate(geoid = as.character(bg2020ge)) %>%
  left_join(
    acs_data %>%
      st_drop_geometry() %>%
      filter(year == 2020),
    .,
    by = 'geoid') %>%
  select(bg2010gj:last_col()) %>%
  mutate(geoid = as.character(bg2010ge))

#THERE ARE SOME NANS HERE - unsure exactly what's going on  but I think it's ok
crossed_2019 <-
  acs_data %>%
  st_drop_geometry() %>%
  filter(year == 2019) %>% 
  left_join(
    crosswalk, 
    ., 
    by = 'geoid') %>%
  mutate(geoid = as.character(bg2020ge))

# manipulation check
# temp <- crossed_2019 %>%
#   group_by(geoid) %>%
#   summarise(
#     n = n())

# apply weights to appropriate columns
crossed_2019_weighted <-
  crossed_2019 %>%
  mutate(
    # this conditional turns income into an NA if wt_pop == 0
    med_hh_inc_e = if_else(
      wt_pop == 0,
      NA_real_,
      med_hh_inc_e),
    # apply weights to all columns except income
    across(
      .cols = unemployed_people_num_e:last_col(),
      .fns = ~ . * wt_pop)) %>%
  select(year, geoid, name:last_col())

# collapsing BGs using sum (median hh income is not included)
crossed_2019_weighted_summed <- crossed_2019_weighted %>% 
  group_by(geoid) %>% 
  summarise(
    across(
      .cols = unemployed_people_num_e:last_col(),
      .fns = ~ sum(.x, na.rm = TRUE))) %>% 
  ungroup()

# manipulation check: checking for NAs in the previous dataset
# crossed_2019_weighted_summed %>% 
#   filter(
#     if_any(
#       everything(),
#       ~ is.na(.)))

# adjusting/weighting income by employed population
weighted_inc <- crossed_2019_weighted %>% 
  select(year, geoid, name, med_hh_inc_e, med_hh_inc_m, total_race_ethn_pop_e) %>% 
  group_by(geoid) %>% 
  filter(
    !is.na(name)) %>%
  # adding all employed people in a given BG
  # finding the proportion of people for that given row in the given BG
  mutate(
    sum_of_people = sum(total_race_ethn_pop_e, na.rm = TRUE),
    relative_income_weight = total_race_ethn_pop_e / sum_of_people) %>% 
  # collapsing income down by BG - weighted by relative_income_weight
  summarise(
    med_hh_inc_e = sum(med_hh_inc_e * relative_income_weight, na.rm = TRUE),
    across()) %>% 
  distinct(geoid, .keep_all = TRUE) %>% 
  # convert all 0's into NAs
  na_if(., 0) %>% 
  ungroup()

wghted_adjstd_2019_acs_data <- weighted_inc %>% 
  select(year, geoid, name, med_hh_inc_e, med_hh_inc_m) %>% 
  left_join(
    .,
    crossed_2019_weighted_summed,
    by = 'geoid') %>% 
  left_join(
    acs_data %>% 
      filter(year == 2020) %>% 
      select(geoid),
    .,
    by = 'geoid') %>% 
  select(year, geoid, name:last_col())

# cleaning up 2021 ACS scraps (the scrap geometries)
acs_data_2021 <- 
  left_join(
    acs_data %>%
      filter(year == 2020) %>%
      select(geoid),
    acs_data %>% 
      st_drop_geometry() %>%
      filter(year == 2021),
    by = 'geoid')

# final acs dataset before the fancy treatment below
acs_data_adjusted <- 
  bind_rows(
    wghted_adjstd_2019_acs_data,
    acs_data %>% 
      filter(year == 2020),
    acs_data_2021) %>% 
  group_by(geoid) %>% 
  # interpolation
  fill(
    med_hh_inc_e, unemployed_people_num_e, civ_labor_force_num_e,
    .direction = 'downup') %>% 
  ungroup()

# convert counts into percents --------------------------------------------

# double check this
acs_data_v2 <- 
  acs_data_adjusted %>%
  # double check
  mutate(
    # economic variables
    # adjusting income levels to be for 2019
    median_hh_inc = case_when(
      year == 2019 ~ med_hh_inc_e,
      year == 2020 ~ med_hh_inc_e * 0.98,
      year == 2021 ~ med_hh_inc_e * 0.96),
    
    total_pop = total_race_ethn_pop_e,
    prcnt_unemp = if_else(
      civ_labor_force_num_e == 0,
      0,
      (unemployed_people_num_e / civ_labor_force_num_e)),
    
    # demographic variables: race/ethnicity
    prcnt_white = (white_num_e / total_race_ethn_pop_e),
    prcnt_black = (black_num_e / total_race_ethn_pop_e),
    prcnt_hisp = (hisp_num_e / total_race_ethn_pop_e),
    prcnt_asian = (asian_num_e / total_race_ethn_pop_e),
    # adding together all other races to create the "other" category
    prcnt_all_other = ((aian_num_e + nhopi_num_e + other_num_e) /
                         # +
                         #   two_or_more_num_e + two_or_more_more_num_e +
                         #   two_or_three_or_more_more_num_e) /
                         total_race_ethn_pop_e),
    
    # demographic variables: calculation for those ≤ 24yo
    prcnt_yth_yng_adlt = ((male_10_to_14e + male_15_to_17e + male_18_to_19e +
                             male_20e + male_21e + male_22_to_24e +
                             female_10_to_14e + female_15_to_17e +
                             female_18_to_19e + female_20e + female_21e +
                             female_22_to_24e) / total_pop_e),
    
    # education variables: no high school degree
    prcnt_no_hs_deg = ((no_hs_ns_num_e + no_hs_nurs_num_e + no_hs_k_num_e +
                          no_hs_1_num_e + no_hs_2_num_e + no_hs_3_num_e +
                          no_hs_4_num_e + no_hs_5_num_e + no_hs_6_num_e +
                          no_hs_7_num_e + no_hs_8_num_e + no_hs_9_num_e +
                          no_hs_10_num_e + no_hs_11_num_e + no_hs_12_num_e) /
                         total_edu_pop_e),
    
    # education variables: everything between HS and a BA
    prcnt_hs_no_ba_deg = ((hs_diploma_num_e + ged_num_e + some_college_1_num_e +
                             some_college_no_deg_num_e + associates_num_e) /
                            total_edu_pop_e),
    
    # education variables: BA or higher
    prcnt_ba_or_hghr_deg = ((ba_deg_num_e + ma_deg_num_e + prfsnl_deg_num_e +
                               dctrl_deg_num_e) / total_edu_pop_e)) %>%
  select(geoid, year, name,
         median_hh_inc:prcnt_ba_or_hghr_deg) %>%
  st_transform(
    st_crs(borough_boundries)) %>%
  st_filter(borough_boundries) %>%
  erase_water(area_threshold = .25)

# save file as rds --------------------------------------------------------

# write_rds(acs_data_v2, 'data/processed/acs_data_v2.rds')

# manipulation checks -----------------------------------------------------

# 360810638005 helped me correct the wrangling further upstream

# mani_check_acs_2019 <-
#   acs_data_v2 %>%
#   filter(year == 2019)
# 
# mani_check_acs_2020 <-
#   acs_data_v2 %>%
#   filter(year == 2020)
# 
# mani_check_acs_2021 <-
#   acs_data_v2 %>%
#   filter(year == 2021)
# 
# # seeing the issues with 2021 data
# outliers_2020_2021 <- 
#   anti_join(
#     mani_check_acs_2021 %>% 
#       as_tibble(),
#     mani_check_acs_2020 %>% 
#       as_tibble(),
#     by = 'geoid') %>% 
#   select(geoid)
# 
# # 2021 geometry craps
# outliers_2020_2021 %>% 
#   ggplot() +
#   geom_sf() +
#   theme_void()
# 
# acs_data_v2 %>%
#   filter(year == 2019) %>% 
#   ggplot() +
#   geom_sf() +
#   theme_void()
# 
# acs_data_v2 %>%
#   filter(year == 2020) %>% 
#   ggplot() +
#   geom_sf() +
#   theme_void()
# 
# acs_data_v2 %>%
#   filter(year == 2021) %>% 
#   ggplot() +
#   geom_sf() +
#   theme_void()















