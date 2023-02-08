
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
  rename(year = fiscal_year)

# crime types -------------------------------------------------------------

violent_crimes <- c(
  'AGGRAVATED SEXUAL ASBUSE',
  'ASSAULT 2,1,UNCLASSIFIED',
  'ASSAULT 3 & RELATED OFFENSES',
  'ASSAULT 3',
  'ASSAULT OTHER PUBLIC SERVICE EMPLOYEE',
  'ASSAULT POLICE/PEACE OFFICER',
  'ASSAULT SCHOOL SAFETY AGENT',
  'ASSAULT TRAFFIC AGENT',
  'FELONY ASSAULT',
  'FELONY SEX CRIMES',
  'HOMICIDE-NEGLIGENT,UNCLASSIFIE',
  'HOMICIDE-NEGLIGENT-VEHICLE',
  'KIDNAPPING & RELATED OFFENSES',
  'HOMICIDE, NEGLIGENT, VEHICLE,',
  'HOMICIDE,NEGLIGENT,UNCLASSIFIE',
  'KIDNAPPING',
  'KIDNAPPING 1',
  'KIDNAPPING 2',
  'MURDER & NON-NEGL. MANSLAUGHTER',
  'ROBBERY',
  'RAPE',
  'RAPE 1',
  'RAPE 1,ATTEMPT',
  'RAPE 2',
  'RAPE 3',
  'ROBBERY, CHAIN STORE',
  'ROBBERY, PAYROLL',
  'ROBBERY,ATM LOCATION',
  'ROBBERY,BANK',
  'ROBBERY,BAR/RESTAURANT',
  'ROBBERY,BEGIN AS SHOPLIFTING',
  'ROBBERY,BICYCLE',
  'ROBBERY,BODEGA/CONVENIENCE STORE',
  'ROBBERY,CAR JACKING',
  'ROBBERY,CHECK CASHING BUSINESS',
  'ROBBERY,CLOTHING',
  'ROBBERY,COMMERCIAL UNCLASSIFIED',
  'ROBBERY,DELIVERY PERSON',
  'ROBBERY,DWELLING',
  'ROBBERY,GAS STATION',
  'ROBBERY,HOME INVASION',
  'ROBBERY,LICENSED FOR HIRE VEHICLE',
  'ROBBERY,LICENSED MEDALLION CAB',
  'ROBBERY,NECKCHAIN/JEWELRY',
  'ROBBERY,ON BUS/ OR BUS DRIVER',
  'ROBBERY,OPEN AREA UNCLASSIFIED',
  'ROBBERY,PERSONAL ELECTRONIC DEVICE',
  'ROBBERY,PHARMACY',
  'ROBBERY,POCKETBOOK/CARRIED BAG',
  'ROBBERY,PUBLIC PLACE INSIDE',
  'ROBBERY,RESIDENTIAL COMMON AREA',
  'ROBBERY,UNLICENSED FOR HIRE VEHICLE',
  'SEXUAL ABUSE',
  'SEXUAL ABUSE 3,2',
  'SEX CRIMES',
  'STRANGULATION 1ST')

# load crime data ---------------------------------------------------------

# in the case the crime dataset needs to be pulled, run the following:
# source('scripts/wrangling/nypd_crime_api_pull.R')

# load nypd data and clean the names to make future analyses easier
nypd_complaint_data <- read_rds('data/raw/crime/nypd_all_complaints_2023-02-08.rds') %>% 
  mutate(cmplnt_fr_dt = ymd(cmplnt_fr_dt),
    year = year(cmplnt_fr_dt)) %>% 
  filter(
    !is.na(cmplnt_fr_dt),
    year(cmplnt_fr_dt) %in% 2019:2021,
    !is.na(longitude) & !is.na(latitude),
    # We're only interested in suspects in our age group of interest
     # vic_age_group %in% c('<18', '18-24') | 
      susp_age_group %in% c('<18', '18-24')) %>% 
  # adding our violent crime variable
  mutate(
    violent_crime = if_else(
      pd_desc %in% violent_crimes,
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

# load our 2019-2021 acs data
acs_data <- 
  read_rds('data/processed/acs_data_v2.rds')

# manipulation check and checking frequencies
# library(Hmisc)
# acs_data_4_hist <- 
#   acs_data %>% 
#   as_tibble() %>% 
#   select(median_hh_inc:prcnt_ba_or_hghr_deg)
# hist.data.frame(acs_data_4_hist)

# potential issues:
# total_pop variable is using decimals due to the weights used in the crosswalk

# cleaning dycd data ------------------------------------------------------

# iterating through and creating a list of dycd site + ACS data by year
cleaned_dycd_programs <- 
  map(
    2019:2021,
    ~ dycd_program_data %>% 
      filter(year == .x) %>% 
      st_join(
        acs_data %>% 
          filter(year == .x) %>% 
          select(!year),
        .)) %>% 
  set_names('dycd_2019','dycd_2020','dycd_2021')

# names for dycd columns
site_yr_names <- 
  c('dycd_site_2019','dycd_site_2020','dycd_site_2021')

# count of all DYCD programs by geoid and create tract indicators
dycd_count <- 
  map2(
    cleaned_dycd_programs,
    site_yr_names,
    ~ .x %>% 
      st_drop_geometry() %>% 
      group_by(geoid) %>% 
      # counting the number of sites per unit of analysis
      summarise(
        num_dycd_programs = sum(!is.na(year))) %>% 
      mutate(
        # iterating through our site_yr_names object
        # creating an indicator variable (1/0 whether there's a site)
        !!.y := if_else(
          num_dycd_programs > 0,
          1,
          0)) %>% 
      select(!num_dycd_programs)) %>% 
  # mashing all dycd datasets in the list
  reduce(
    full_join,
    by = 'geoid') %>% 
  # identifying tracts that had a site in only 2020 and 2021
  mutate(
    site_for_0_1_2_3_yrs = case_when(
      ((dycd_site_2019 == 0) & 
         (dycd_site_2020 == 0) & 
         (dycd_site_2021 == 0)) ~ 0,
      ((dycd_site_2019 == 0) & 
         (dycd_site_2020 == 1) & 
         (dycd_site_2021 == 0)) ~ 1,
      ((dycd_site_2019 == 0) & 
         (dycd_site_2020 == 1) & 
         (dycd_site_2021 == 1)) ~ 2,
      ((dycd_site_2019 == 1) & 
         (dycd_site_2020 == 1) & 
         (dycd_site_2021 == 1)) ~ 3,
      ((dycd_site_2019 == 1) & 
         (dycd_site_2020 == 1) & 
         (dycd_site_2021 == 0)) ~ 4,
      ((dycd_site_2019 == 1) & 
         (dycd_site_2020 == 0) & 
         (dycd_site_2021 == 0)) ~ 5,
      ((dycd_site_2019 == 0) & 
         (dycd_site_2020 == 0) & 
         (dycd_site_2021 == 1)) ~ 6)) %>% 
  pivot_longer(
    cols = dycd_site_2019:dycd_site_2021,
    names_to = 'year',
    values_to = 'dycd_site',
    names_prefix = 'dycd_site_') %>% 
  # dummying out our different site types
  mutate(
    year = as.numeric(year),
    sites_for_0yrs = if_else(
      site_for_0_1_2_3_yrs == 0,
      1,
      0),
    sites_for_2nd_yr = if_else(
      site_for_0_1_2_3_yrs == 1,
      1,
      0),
    sites_for_2nd_3rd_yrs = if_else(
      site_for_0_1_2_3_yrs == 2,
      1,
      0),
    sites_for_3_yrs = if_else(
      site_for_0_1_2_3_yrs == 3,
      1,
      0),
    sites_for_1st_2nd_yrs = if_else(
      site_for_0_1_2_3_yrs == 4,
      1,
      0),
    sites_for_1st_yr = if_else(
      site_for_0_1_2_3_yrs == 5,
      1,
      0),
    sites_for_last_yr = if_else(
      site_for_0_1_2_3_yrs == 6,
      1,
      0))

# cleaning nypd complaint data --------------------------------------------

# iterating through and segmenting crime data from 2019 to 2021
cleaned_complaint_data <- 
  map_df(
    2019:2021,
    ~ nypd_complaint_data %>% 
      filter(year == .x) %>% 
      st_join(
        acs_data %>% 
          filter(year == .x),
        .) %>% 
      st_drop_geometry()) %>% 
  select(!year.y) %>% 
  rename(year = year.x)

# %>% 
#   set_names('nypd_2019','nypd_2020','nypd_2021')

# rm(nypd_complaint_data)

# names for complaint columns from 2019 to 2021
# complaints_names <- 
#   c('num_complaints_2019','num_complaints_2020','num_complaints_2021')

# count of all NYPD complaints by geoid
# complaint_count <- 
#   map2(
#     cleaned_complaint_data,
#     complaints_names,
#     ~ .x %>% 
#       group_by(geoid) %>% 
#       summarise(
#         # iterating through our complaints_names object
#         # adding up all complaints
#         !!.y := sum(!is.na(cmplnt_num)))) %>% 
#   # mashing all dycd datasets in the list
#   reduce(
#     full_join,
#     by = 'geoid') %>% 
#   pivot_longer(
#     cols = num_complaints_2019:num_complaints_2021,
#     names_to = 'year',
#     values_to = 'num_complaints',
#     names_prefix = 'num_complaints_') %>% 
#   mutate(year = as.numeric(year)) 

# if I'm running into issues here try the following:
# detach("package:Hmisc", unload=TRUE)
complaint_count <- 
  cleaned_complaint_data %>% 
  group_by(geoid, year) %>% 
  summarize(
    num_cmplnt_ttl = sum(!is.na(cmplnt_num)),
    num_cmplnt_vlnt = sum(violent_crime, na.rm = TRUE))
  
# create regression dataset -----------------------------------------------

# mashing the nypd and dycd lists together
dycd_nypd_2019_2020_2021 <- 
  full_join(
    dycd_count,
    complaint_count,
    by = c('geoid', 'year'))

col_rate_names <- 
  c('complaint_rate_2019','complaint_rate_2020','complaint_rate_2021')

# adding acs data, calculating crime rate, and identifying usable tracts
reg_dataset <- 
  full_join(
    acs_data,
    dycd_nypd_2019_2020_2021,
    by = c('geoid', 'year')) %>% 
  # calculating crime rate and making ACS data percents (not proportions)
  mutate(
    crime_rate = (num_cmplnt_ttl/total_pop)*100000,
    violent_crime_rate = (num_cmplnt_vlnt/total_pop)*100000,
    across(
      .cols = prcnt_unemp:prcnt_ba_or_hghr_deg,
      .fns = ~ . * 100),
    across(
      .cols = median_hh_inc:prcnt_ba_or_hghr_deg,
      .fns = ~ log(.),
      .names = '{.col}_log'))

# removing unhelpful block groups -----------------------------------------

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
  reg_dataset %>%
  # as_tibble() %>% 
  group_by(geoid) %>% 
  # booted is when all geoids are knocked out if even a single one has pop == 0
  mutate(
    booted = if_else(
      any(total_pop == 0),
      1,
      0)) %>% 
  filter(
    total_pop > 0,
    booted == 0,
    !is.na(site_for_0_1_2_3_yrs),
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
    geoid != brnx_zoo)

# NOTE THE LAST FILTERED OPTION
# create a .dta
# library(haven)
# reg_dataset_testing %>%
#   st_drop_geometry() %>%
#   write_dta('data/processed/thesis_dataset_2023-02-07.dta')

# library(haven)
# create an .rds
# reg_dataset_testing %>%
#   st_drop_geometry() %>%
#   write_rds('data/processed/thesis_dataset_2023-02-02.rds')
 
# manipulation checks (delete later) --------------------------------------

# library(Hmisc)

# CHECKING FREQUENCIES
# first set of frequencies
reg_dataset_testing_4_hist_1 <- 
  reg_dataset_testing %>% 
  as_tibble() %>% 
  select(median_hh_inc:prcnt_all_other)

hist.data.frame(reg_dataset_testing_4_hist_1)

# second set of frequencies
reg_dataset_testing_4_hist_2 <- 
  reg_dataset_testing %>% 
  as_tibble() %>% 
  select(prcnt_yth_yng_adlt:prcnt_ba_or_hghr_deg, crime_rate, violent_crime_rate)

hist.data.frame(reg_dataset_testing_4_hist_2)

# what years are dycd sites around in each of the units of analysis
check_1 <- reg_dataset_testing %>% 
  as_tibble() %>% 
  filter(year == 2019) %>% 
  summarise(sites_for_0yrs = sum(site_for_0_1_2_3_yrs == 0, na.rm = TRUE),
            sites_for_2nd_yr = sum(site_for_0_1_2_3_yrs == 1, na.rm = TRUE),
            sites_for_2nd_3rd_yrs = sum(site_for_0_1_2_3_yrs == 2, na.rm = TRUE),
            sites_for_3_yrs = sum(site_for_0_1_2_3_yrs == 3, na.rm = TRUE),
            sites_for_1st_2nd_yrs = sum(site_for_0_1_2_3_yrs == 4, na.rm = TRUE),
            sites_for_1st_yr = sum(site_for_0_1_2_3_yrs == 5, na.rm = TRUE),
            sites_for_last_yr = sum(site_for_0_1_2_3_yrs == 6, na.rm = TRUE))

check_2 <- reg_dataset_testing %>% 
  as_tibble() %>% 
  filter(year == 2019) %>% 
  summarise(
    across(
      .cols = sites_for_0yrs:sites_for_last_yr,
      .fns = ~ sum(., na.rm = TRUE)
    ))
    

# NYC total population checks out
# reg_dataset_testing %>%
#   as_tibble() %>%
#   filter(year == 2019) %>%
#   summarise(sum(total_pop))

# here we can get an interactive visual of the unit of analysis overlayed on NYC
# library(tmap)
# tmap_mode('view')
# tm_basemap('OpenStreetMap') +
#   reg_dataset_testing %>%
#   filter(year == 2019) %>%
#   tm_shape() +
#   tm_polygons(col = 'total_pop') +
#   tmap_options(check.and.fix = TRUE)

# static map
# reg_dataset_testing %>%
#   filter(year == 2019) %>%
#   ggplot() +
#   geom_sf() +
#   theme_void()

# compare each year of our dataset individually
# temp_2019 <- reg_dataset_testing %>% 
#   filter(year == 2019)
# temp_2020 <- reg_dataset_testing %>% 
#   filter(year == 2020)
# temp_2021 <- reg_dataset_testing %>% 
#   filter(year == 2021)

# in the case we want to compare two datasets and see what's different
# anti_joined <- 
#   anti_join(
#     temp_2020,
#     temp_2019 %>% 
#       as_tibble(),
#     by = 'geoid')

model_1 <- 
  lm(crime_rate ~ reg_dataset_testing_2019$prcnt_hs_no_ba_deg,
     data = reg_dataset_testing_2019)

summary(model_1)





# creating a table for the main figure (across different DYCD BG types)
crime_dycd_summary <- 
  reg_dataset_testing %>% 
  as_tibble() %>% 
  group_by(year, site_for_0_1_2_3_yrs) %>% 
  summarise(
    crime_count = sum(num_complaints, na.rm = TRUE),
    crime_mean = round(
      mean(num_complaints, na.rm = TRUE), 2),
    crime_rate_mean = round(
      mean(crime_rate, na.rm = TRUE), 2),
    n =n())

crime_dycd_summary %>% 
  ggplot(
    mapping = aes(
      x = factor(year), 
      y = crime_rate_mean,
      group = factor(site_for_0_1_2_3_yrs))) +
  geom_line(
    mapping = aes(
      color = factor(site_for_0_1_2_3_yrs)),
    linewidth = 1.25) +
  labs(
    title = 'Average Crime Rate across NYC block groups \nfrom 2019 to 2021',
    x = '',
    y = 'Average crime rate per 100,000',
    color = '',
    caption = 'Source: NYC OpenData') +
  scale_color_hue(labels = c('Sites for 0 years',
                             'Sites during 2nd year only',
                             'Sites during 2nd & 3rd years',
                             'Sites during all 3 years',
                             'Sites during 1st & 2nd years',
                             'Sites during 1st year only',
                             'Sites during 3rd year only')) +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.8),
    axis.text.x = element_text(
      face = "bold", 
      size = 14),
    axis.text.y = element_text(
      face = "bold", 
      size = 14),
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 15),
    plot.title = element_text(size = 20))




