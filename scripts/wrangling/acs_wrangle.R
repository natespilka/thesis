# Nate Spilka
# Date started: 2023-01-03
# Description: This crosswalks, wrangles/cleans, interpolates, and imputes 
# ACS data. Specifically, 2019 ACS data are crosswalked with 2020 due to the  
# decennial census update. For more information about this step, visit: 
# https://www.nhgis.org/geographic-crosswalks. Missing median household income 
# values are interpolated with temporally neighboring values (of the same block
# group). For values that were not interpolated, mhhi values were imputed using 
# tidymodels. Regression, knn, and random forest models were used to identify 
# the optimal strategy for imputation. Random forest took gold with the lowest 
# rmse (justifying its usage for imputation). 

# Note: For the purpose of my thesis, I only used my control variables when 
# imputing median household income. 

# clean environment -------------------------------------------------------

rm(list = ls(all.names = TRUE))
graphics.off()
cat("\014")

# load some packages ------------------------------------------------------

library(tidyverse)
library(sf)

# load NYC borough boundaries ---------------------------------------------

# url for API
# url_borough_boundaries <- 'https://data.cityofnewyork.us/resource/7t3b-ywvw.json'

# API query for NYC borough boundaries data
# borough_boundries <- RSocrata::read.socrata(
#   url_borough_boundaries,
#   app_token = "###",
#   email     = "nhs40@georgetown.edu",
#   password  = "###")

# NYC borough boundaries data. 
# using an EPSG of 2263 since it's recommended for NY (https://epsg.io/2263). 
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

# acs years of interest
years <- list(2019, 2020, 2021) 

# pulling NYC block group (bg) level ACS data (acs5) from 2019 to 2021
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

# crosswalk 2019 ACS bgs to 2020 ACS bgs ----------------------------------

# crosswalk data can be found here: https://www.nhgis.org/geographic-crosswalks
# Specifically, New York "2010 → 2020" "Block Groups → Block Groups" were used

# load in crosswalk dataset and only use block-groups that are in 2020 census
crosswalk <- 
  read_csv('data/raw/nhgis_crosswalk/nhgis_bg2010_bg2020_36/nhgis_bg2010_bg2020_36.csv') %>%
  # mutate since the bg identifiers are numeric (not character)
  mutate(geoid = as.character(bg2020ge)) %>%
  left_join(
    acs_data %>%
      st_drop_geometry() %>%
      filter(year == 2020),
    .,
    by = 'geoid') %>%
  select(bg2010gj:last_col()) %>%
  mutate(geoid = as.character(bg2010ge))

# only using 2019 acs data when we join it to our crosswalk above
crossed_2019 <-
  acs_data %>%
  st_drop_geometry() %>%
  filter(year == 2019) %>% 
  left_join(
    crosswalk, 
    ., 
    by = 'geoid') %>%
  mutate(geoid = as.character(bg2020ge))

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

# collapsing bgs using sum (median hh income is not included)
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
  # adding all employed people in a given bg
  # finding the proportion of people for that given row in the given BG
  mutate(
    sum_of_people = sum(total_race_ethn_pop_e, na.rm = TRUE),
    relative_income_weight = total_race_ethn_pop_e / sum_of_people) %>% 
  # collapsing income down by bg - weighted by relative_income_weight
  summarise(
    med_hh_inc_e = sum(med_hh_inc_e * relative_income_weight, na.rm = TRUE),
    across()) %>% 
  distinct(geoid, .keep_all = TRUE) %>% 
  # convert all 0's into NAs
  na_if(., 0) %>% 
  ungroup()

# combining our weighted income data and other crosswalked acs 2019 data
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

# final acs dataset before the fancy treatment below (interpolation at end)
acs_data_adjusted <- 
  bind_rows(
    wghted_adjstd_2019_acs_data,
    acs_data %>% 
      filter(year == 2020),
    acs_data_2021) %>% 
  group_by(geoid) %>% 
  mutate(
    med_hh_inc_e = case_when(
      # interpolating from 2019 med_hh_inc_e
      (!is.na(first(med_hh_inc_e)) & 
         is.na(nth(med_hh_inc_e, 2)) & 
         is.na(last(med_hh_inc_e))) ~ first(med_hh_inc_e),
      # interpolating from 2020 med_hh_inc_e
      (is.na(first(med_hh_inc_e)) & 
         !is.na(nth(med_hh_inc_e, 2)) & 
         is.na(last(med_hh_inc_e))) ~ nth(med_hh_inc_e, 2),
      # interpolating from 2021 med_hh_inc_e
      (is.na(first(med_hh_inc_e)) & 
         is.na(nth(med_hh_inc_e, 2)) & 
         !is.na(last(med_hh_inc_e))) ~ last(med_hh_inc_e),
      # interpolating from 2019 and 2021 med_hh_inc_e - (creating the mean)
      (!is.na(first(med_hh_inc_e)) &
         is.na(nth(med_hh_inc_e, 2)) &
         !is.na(last(med_hh_inc_e))) ~ if_else(
           year == 2020, 
           (first(med_hh_inc_e) + last(med_hh_inc_e))/2,
           med_hh_inc_e),
      # all other cases just throw in its own med_hh_inc_e
      TRUE ~ med_hh_inc_e)) %>% 
  # interpolation 2019 and 2021 from neighbors
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

# imputation: prepping data for splits ------------------------------------

library(tidymodels)

# bgs outside the scope of the study
central_prk <- '360610277001'
rikers <- '360050001001'
mnt_olv_cem <- '360810607011' # pop = 14
cyp_hill_cem <- '360810561001' # pop = 17
flyd_bnt_fld <- '360470702021' # pop = 8
hly_crss_cem <- '360470852001' # pop = 26
sth_frsh_klls_prk <- '360850228011' # pop = 21
innwd_park <- '360610297001' # pop = 22
brnx_zoo <- '360050334001' # pop = 15

acs_data_ml <- acs_data_v2 %>% 
  st_drop_geometry() %>%
  group_by(geoid) %>% 
  # booted = all geoids are knocked even if a single bg has pop == 0
  mutate(
    booted = if_else(
      any(total_pop == 0),
      1,
      0),
    geoid = as.numeric(geoid)) %>%
  filter(
    !is.na(median_hh_inc),
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
    geoid != brnx_zoo) %>% 
  # setting our id variable (entity_time)
  ungroup() %>% 
  mutate(
    id = paste0(geoid, '_', year)) %>% 
  select(id, year, median_hh_inc:prcnt_ba_or_hghr_deg)

set.seed(20211101)
# splitting data and trying to keep year relatively balanced
acs_data_testing_split <- initial_split(
  acs_data_ml, 
  prop = .8,
  strata = year) 

# training and testing data split
acs_data_train <- training(acs_data_testing_split) %>% 
  select(-year)
acs_data_test <- testing(acs_data_testing_split) %>% 
  select(-year)
  
# recipe shenanigans ------------------------------------------------------

# making our recipe: 
acs_data_recipe <- recipe(median_hh_inc ~ ., data = acs_data_train) %>%
  # setting our id variable (entity_time)
  update_role(id, new_role = "id variable") %>%
  #removes variables that are sparce and unbalanced
  step_nzv(all_predictors()) %>%
  #centers our variables
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) 

# checking the recipe
check_recipe <- 
  bake(
    prep(
      acs_data_recipe, 
      training = acs_data_train), 
    new_data = acs_data_train)

# set up resampling using 10-fold cross validation
set.seed(20211102)
folds <- vfold_cv(data = acs_data_train, v = 10, repeats = 1)
  
# regression model --------------------------------------------------------

# setting linear regression model
model_lm <- linear_reg() %>%
  set_engine("lm")

# setting workflow
workflow_lm <- workflow() %>%
  add_recipe(acs_data_recipe) %>%
  add_model(model_lm) 

# using the workflow, fit the model + folds
resamples_lm <- workflow_lm %>%
  fit_resamples(resamples = folds)

# select the best model
best_lm <- resamples_lm %>%
  select_best("rmse")

# finalizing the workflow using the best lm
final_lm <- workflow_lm %>% 
  finalize_workflow(parameters = best_lm)

# fit to the training data and extract coefficients
fit_lm <- final_lm %>%
  fit(data = acs_data_train)

# lm performance
collect_metrics(resamples_lm) %>%
  filter(.metric == "rmse") %>%
  select(mean) %>%
  pull(mean)

# knn ---------------------------------------------------------------------

# create a tuning grid for our knn
grid_knn <- grid_regular(
  neighbors(range = c(10, 25)), levels = 8)

# create the knn model
model_knn <- nearest_neighbor(neighbors = tune()) %>%
  set_engine(engine = "kknn") %>%
  set_mode(mode = "regression")

# initiate the workflow
workflow_knn <- workflow() %>%
  add_model(spec = model_knn) %>%
  add_recipe(recipe = acs_data_recipe)

# we may run into issues here with conflicting libraries
resampling_knn <- workflow_knn %>% 
  tune_grid(
    resamples = folds,
    grid = grid_knn,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(rmse))

# identifying the best hyper-parameter
hyper_knn_best <- resampling_knn %>%
  select_best("rmse")

# finalize the workflow using the best model
final_knn <- workflow_knn %>%
  finalize_workflow(parameters = hyper_knn_best)

# fitting the workflow with the original data
fit_knn <- final_knn %>%
  fit(acs_data_train)

# applying the finalized workflow to the folds
knn_fit_rs <- final_knn %>%
  fit_resamples(resamples = folds)

# model performance
collect_metrics(knn_fit_rs)  
  
# random forests ----------------------------------------------------------

# I would add more trees but the performance levels out at ~100
# setting the number of trees/grid
tree_nmbr <- 1
grid_nmbr <- 1

#since random forests can be computationally heavier, we use parallel processing
cores <- parallel::detectCores()

# rf with 1000 iterations, using parallel processing
model_rf <- rand_forest(
  mtry = tune(), 
  min_n = tune(), 
  trees = tree_nmbr) %>% 
  set_engine(
    engine = "ranger", 
    num.threads = cores, 
    importance = "impurity") %>%
  set_mode("regression")

# rf workflow
workflow_rf <- workflow() %>% 
  add_recipe(
    recipe = acs_data_recipe) %>%
  add_model(
    spec = model_rf)

# tune hyper-parameters using folds and the grid_nmbr above (this takes time)
hyper_rf <- workflow_rf %>% 
  tune_grid(
    resamples = folds, 
    grid = grid_nmbr,
    control = control_grid(save_pred = TRUE))

# identifying the best hyper-parameters (mtry = , min_n = )
hyper_rf_best <- hyper_rf %>% 
  select_best(metric = "rmse")

# applying the best hyper-parameters
hyper_rf_best_applied <- workflow_rf %>%
  finalize_workflow(parameters = hyper_rf_best)

# apply the worlflow when fitting the data
fit_rf <- hyper_rf_best_applied %>%
  last_fit(acs_data_testing_split)

#fitting the workflow with the original data
rf_final_fit <- hyper_rf_best_applied %>% 
  fit(acs_data_train)

# applying the updated workflow to the fit
fit_rs_rf <- hyper_rf_best_applied %>%
  fit_resamples(resamples = folds)

# mean RMSE across the 10 samples
collect_metrics(fit_rs_rf) %>%
  filter(.metric == "rmse") %>%
  select(mean) %>%
  pull(mean)

# these are the top ten variables of highest importance
fit_rf %>%
  extract_fit_parsnip() %>%
  vip::vip(num_features = 10)

# apply the best model to the testing data --------------------------------

# using the random forest model since it has the best performance
predictions_test <- bind_cols(
  acs_data_test, 
  predict(
    object = rf_final_fit, 
    new_data = acs_data_test))

# checking the performance with our testing data
rmse(
  data = predictions_test, 
  truth = acs_data_test$median_hh_inc, 
  estimate = .pred)  

# applying the model to our NAs: imputation -------------------------------

# all median incomes that will be imputed
to_be_imputed <- acs_data_v2 %>% 
  st_drop_geometry() %>%
  filter(
    is.na(median_hh_inc),
    total_pop > 0) %>% 
  mutate(
    id = paste0(geoid, '_', year),
    # two observations have zeros for education
    prcnt_no_hs_deg = ifelse(
      is.na(prcnt_no_hs_deg),
      0,
      prcnt_no_hs_deg),
    prcnt_hs_no_ba_deg = ifelse(
      is.na(prcnt_hs_no_ba_deg),
      0,
      prcnt_hs_no_ba_deg),
    prcnt_ba_or_hghr_deg = ifelse(
      is.na(prcnt_ba_or_hghr_deg),
      0,
      prcnt_ba_or_hghr_deg)) %>% 
  select(id, median_hh_inc:prcnt_ba_or_hghr_deg)

# imputing the mhhi for those with NAs
imputed_mhhi <- bind_cols(
  to_be_imputed, 
  predict(
    object = rf_final_fit, 
    new_data = to_be_imputed))

# converting the id variable to geoid/year for the left_join below
imputed_mhhi_2 <- bind_cols(
  imputed_mhhi,
  geoid = str_sub(imputed_mhhi$id, 1, 12),
  year = as.numeric(
    str_sub(imputed_mhhi$id, -4)))

# merging/filling in appropriate mhhi NAs with the imputed values
acs_data_v3 <- left_join(
  acs_data_v2,
  imputed_mhhi_2 %>% 
    select(geoid, year, .pred),
  by = c('geoid', 'year')) %>% 
  mutate(
    median_hh_inc = ifelse(
      is.na(median_hh_inc) & !is.na(.pred),
      .pred,
      median_hh_inc)) %>% 
  select(-.pred)

# outcome -----------------------------------------------------------------

# 1571 interpolated median household income values
# 1 interpolated unemployment rate value
# 400 imputed median household income values

# save file as rds --------------------------------------------------------

# write_rds(acs_data_v3, 'data/processed/acs_data_v3.rds')






