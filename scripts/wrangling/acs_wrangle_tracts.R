# this script will do the following:
# (1) identify ACS variables of interest
# (2) crosswalk 2019 acs data to 2020 block-groups
# (3) 

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

# tidycensus 2019 BG pull -------------------------------------------------

# pulling acs data
acs_data_2019_bg <-
tidycensus::get_acs(
      geography = 'block group',
      variables = c(
        econ_vars,
        demo_vars,
        edu_vars),
      year = 2019,
      state = 'NY',
      county = c('Queens', 
                 'Richmond',
                 'New York',
                 'Kings',
                 'Bronx'),
      survey = 'acs5',
      output = 'wide',
      geometry = TRUE) %>% 
  janitor::clean_names() %>% 
  mutate(year = 2019)

# tidycensus 2020 and 2021 tract pull -------------------------------------

years <- list(2020, 2021) 

# pulling acs data
acs_data_2020_2021 <-
  map_dfr(
    years,
    ~ tidycensus::get_acs(
      geography = 'tract',
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
      year == 1 ~ 2020,
      year == 2 ~ 2021))

# crosswalk 2019 BGs to 2020 tracts ---------------------------------------

# load in crosswalk dataset and only use tracts that are in 2020 census
crosswalk <- 
  read_csv('data/raw/nhgis_crosswalk/nhgis_bg2010_tr2020_36/nhgis_bg2010_tr2020_36.csv') %>%
  # we must mutate since the tract/BG identifiers are numeric (not character)
  mutate(geoid = as.character(tr2020ge)) %>%
  left_join(
    acs_data_2020_2021 %>%
      st_drop_geometry() %>%
      filter(year == 2020),
    .,
    by = 'geoid') %>%
  select(bg2010gj:last_col()) %>%
  mutate(geoid = as.character(bg2010ge))

# combining our acs_data_2019_bg dataset with our crosswalk
crossed_2019 <-
  acs_data_2019_bg %>%
  st_drop_geometry() %>%
  left_join(
    crosswalk, 
    ., 
    by = 'geoid') %>%
  mutate(geoid = as.character(tr2020ge))

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
      .fns = sum))

# manipulation check: checking for NAs in the previous dataset
# crossed_2019_weighted_summed %>% 
#   filter(
#     if_any(
#       everything(),
#       ~ is.na(.)))


# adjusting/weighting income by employed population
weighted_inc <- crossed_2019_weighted %>% 
  select(year, geoid, name, med_hh_inc_e, med_hh_inc_m, employed_people_num_e) %>% 
  group_by(geoid) %>% 
  # adding all employed people in a given BG
  # finding the proportion of employed for that given row in the given BG
  mutate(
    sum_of_employed = sum(employed_people_num_e, na.rm = TRUE),
    relative_income_weight = employed_people_num_e / sum_of_employed) %>% 
  # collapsing income down by BG - weighted by relative_income_weight
  summarise(
    med_hh_inc_e = sum(med_hh_inc_e * relative_income_weight, na.rm = TRUE),
    across()) %>% 
  distinct(geoid, .keep_all = TRUE) %>% 
  # convert all 0's into NAs
  na_if(., 0)

wghted_adjstd_2019_acs_data <- weighted_inc %>% 
  select(year, geoid, name, med_hh_inc_e, med_hh_inc_m) %>% 
  left_join(
    .,
    crossed_2019_weighted_summed,
    by = 'geoid') %>% 
  left_join(
    acs_data_2020_2021 %>% 
      filter(year == 2020) %>% 
      select(geoid),
    .,
    by = 'geoid') %>% 
  select(year, geoid, name:last_col())

# final acs dataset before the fancy treatment below
acs_data_adjusted <- acs_data_2020_2021 %>% 
  rbind(wghted_adjstd_2019_acs_data, .) 




















