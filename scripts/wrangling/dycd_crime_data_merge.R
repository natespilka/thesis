
# clean workspace ---------------------------------------------------------

rm(list = ls(all.names = TRUE))
graphics.off()
cat("\014") 
#Alt + Shift + K  == gets you all shortcuts

# load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)
library(crsuggest)
library(tigris)
library(sf)

# load dycd data ----------------------------------------------------------

# MAKE SURE TO DO THIS WITH APIS

# limitation: too few sites
# asp_syep_data <- 
#   read_csv('data/raw/dycd/DYCD_after-school-programs__SYEP_Summer_Youth_Employment_Programs.csv')

# no geospatial data
# asp_contractor_data <-
#   read_csv('data/raw/dycd/DYCD_Contractors.csv')

# dycd_program_site_data <- 
#   read_csv('data/raw/dycd/DYCD_Program_Sites.csv')

# this dataset allows us to date back to 2019
# dycd_funded_geo_data <- 
#   read_csv('data/raw/dycd/DYCD-funded_Program_Sites_-_Geocoded_Data.csv')

# this dataset allows us to date back to 2019
dycd_eval_data <- 
  read_csv('data/raw/dycd/Evaluation_and_Monitoring_Reports_for_Program_Sites.csv') 

# load crime data ---------------------------------------------------------

complaint_historic_data <-
  read_csv('data/raw/crime/NYPD_Complaint_Data_Historic.csv') %>%
  clean_names() %>%
  mutate(cmplnt_fr_dt = mdy(cmplnt_fr_dt))

arrest_historic_data <- 
  read_csv('data/raw/crime/NYPD_Arrests_Data__Historic_2.csv') 

# load geospatial data ----------------------------------------------------

borough_boundries <-
  st_read('data/raw/geo/borough_boundries/geo_export_88fe9910-a630-4e7b-a5a8-c49dd83de4f1.shp')

# 6539 - 6318
# suggest_crs(borough_boundries)

nyc_census_tracts <-
  st_read('data/raw/geo/nyct2020_22b/nyct2020.shp')

nyc_census_tracts <- 
  st_read('data/raw/geo/nyct2010/geo_export_7b139b00-a5fd-4f93-8a91-afcd932e4fe3.shp')

# 6539 - 6318
# suggest_crs(nyc_census_tracts)


# acs ---------------------------------------------------------------------

library(tidycensus)

variables <- 
  load_variables(2019, "acs1")

acs_data <- 
  get_acs(
    geography = "tract",
    variables = c(ttl_male = "B01001_002",
                  ttl_female = "B01001_026"),
    state = "New York",
    county = "New York",
    year = 2019, 
    output = "wide",
    survey = "acs1")



# clean crime data --------------------------------------------------------

crime_data_cleaned <- 
  arrest_historic_data %>% 
  clean_names() %>% 
  mutate(
    arrest_date = mdy(arrest_date)) %>%
  filter(
    !is.na(arrest_date),
    age_group %in% c('<18', '18-24'),
    between(
      arrest_date, 
      as.Date('2019-01-01'), 
      as.Date('2019-12-31')))

# clean dycd data ---------------------------------------------------------

# programs in question:
# "High School", "NDA Healthy Families", "Over Threshold", "Street Outreach" (this is just a two-day event), "Under Threshold", "Younger Youth (YY)" , "Older Youth (OY)", "Fathers aged 18 and over",

# final DYCD program type list
programs_of_interest <- 
  c("Adolescent Literacy",
    "Beacon",
    "COMPASS Elementary",
    "COMPASS High School",
    "COMPASS Middle School",
    "COMPASS Explore",
    "COMPASS Horizon",
    "COMPASS SONYC Pilot",
    "Cornerstone",
    "Ladders for Leaders (LFL)",
    "Learn and Earn",
    "Older Youth (OY)",
    "SYEP School-Based",
    "Train and Earn",
    "WLG (OY)",
    "YAIP", 
    "YAIP Plus",
    "Year Round Sector Focus",
    "Young Adult Literacy Program (YALP)",
    "Younger Youth (YY)"
  )

# cleaning up the data (only 2019, the program types specified above, and only one instance at each site)
# we'll need to match the dataset with the 2019 program site dataset

# if we run yearly, we're limited by 725 - maybe - depends if we go by program site or number of programs in a given census tract
# dycd_eval_data0 <- dycd_eval_data %>%
#   mutate(
#     eval_year = year(evaluation_date)) %>%
#   filter(
#     eval_year == 2017,
#     program_type %in% programs_of_interest) %>% 
#   distinct(bin, .keep_all = TRUE)

dycd_eval_data1 <- 
  dycd_eval_data %>%
  # clean_names() %>%
  # mutate(
  #   evaluation_date = mdy(evaluation_date))%>% 
  filter(
    # we don't need this specific date/time filter if we're going by eval - actually i dont know
    between(evaluation_date,
    as.Date('2017-01-01'),
    as.Date('2018-06-01')),
    program_type %in% programs_of_interest) %>%
  distinct(bin, .keep_all = TRUE)

#write_csv(dycd_eval_data1, 'data/processed/dycd_eval_data1.csv')

# dycd_eval_data2 <- dycd_eval_data %>%
#   mutate(
#     eval_year = year(evaluation_date)) %>%
#   filter(
#     program_type %in% programs_of_interest)


# we need to do this by year if we do an annual analysis:  distinct(bin, .keep_all = TRUE)

# organizing geo data -----------------------------------------------------

# applying the lon and lat to yield our point geodata
# QUESTION - IS THIS THE RIGHT CRS? we need to consider the buffering component because there is a difference with distance and presentation. use st_crs to help understand this (and suggest)
# suggest_crs(nyc_census_tracts)

dycd_eval_data_sf <- 
  dycd_eval_data1 %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'), 
    na.fail = FALSE) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = 2263)

crime_data_cleaned_sf <- 
  crime_data_cleaned %>% 
  st_as_sf(
    coords = c('longitude', 'latitude'), 
    na.fail = FALSE) %>% 
  st_set_crs(4326) %>% 
  st_transform(crs = 2263)

nyc_census_tracts_sf <- 
  nyc_census_tracts %>% 
  clean_names() %>%
  select(
    geoid, 
    geometry,
    boro_ct2020,
    boro_code,
    ct_label,
    boro_name,
    nta_name) %>% 
  st_as_sf(value = 2263) %>% 
  st_transform(crs = 2263)

borough_boundries_sf <- 
  borough_boundries %>% 
  st_as_sf(value = 2263) %>% 
  st_transform(crs = 2263)

# makeing sure everything is within NYC boundaries  -----------------------

# MAKE SURE THAT USING NYC BOUNDARIES AND CENSUS TRACTS SEPARATLY IS OK 

crime_data_sf_bounded <- 
  st_join(crime_data_cleaned_sf, 
          borough_boundries_sf, 
          left = FALSE)

dycd_eval_sf_bounded <- 
  st_join(dycd_eval_data_sf, 
          borough_boundries_sf, 
          left = FALSE)

# mashing crime, dycd, and census tract data ------------------------------

# it looks like we lose some arrest data when we join the two (order by boro_ct2020)?
census_tract_crime <- 
  st_join(nyc_census_tracts_sf, 
          crime_data_sf_bounded, 
          join = st_contains) %>% 
  select(geoid:perp_race) %>% 
  st_drop_geometry()

census_tract_dycd <- 
  st_join(nyc_census_tracts_sf,
          dycd_eval_sf_bounded,
          join = st_contains) %>% 
  select(geoid:bbl) %>% 
  st_drop_geometry()

all_mashed <- 
  right_join(census_tract_crime,
          census_tract_dycd,
          copy = FALSE,
          by = 'geoid')

# variable selection for thesis dataset -----------------------------------

thesis_dataset <- 
  all_mashed %>% 
  select(
    geoid,
    boro_ct2020.x,
    census_tract_2010,
    postcode,
    arrest_key,
    arrest_date,
    ofns_desc,
    age_group,
    evaluation_id,
    program_type,
    evaluation_date,
    overall_rating,
    boro_name.x.x,
    arrest_boro,
    boro_name.x.y)

# manipulation checks -----------------------------------------------------


# make sure all boros line up
# check census tracts (lined up?)
# 



# condense thesis dataset -------------------------------------------------

temp <- 
  thesis_dataset %>% 
  group_by(boro_ct2020.x) %>% 
  summarise(arrests = n_distinct(na.omit(arrest_key)),
            dycd_program_sites = n_distinct(na.omit(evaluation_id))) %>% 
  ungroup()

# as a manipulation check, sum the columns to get the number of observations in their respective datasets
# buffer/misc geospatial creation -----------------------------------------

# program_site_buffer <-
#   dycd_eval_data_sf %>% 
#   st_buffer(dist = units::set_units(160 ,"m"))

# geospatial plots --------------------------------------------------------

# we need to remove all program sites that fall outside of the NYC boundaries
 ggplot() +
  geom_sf(data = nyc_census_tracts_sf,
          size = .1) +
  geom_sf(data = crime_data_sf_bounded,
          size = .5,
          color = 'red',
          alpha = 1/10) +
  geom_sf(data = dycd_eval_sf_bounded,
          size = .3,
          color = 'blue') +
  theme_void()
# 
# ggsave(filename = 'tract_crime_dycd_heatmap.png',
#        path = 'output/plots/')


# ggplot() +
#   geom_sf(data = nyc_census_tracts_sf,
#           size = .1) +
#   geom_sf(data = dycd_eval_sf_bounded, 
#           size = 0.3,
#           color = 'blue') +
#   theme_void()
# 
# ggsave(filename = 'dycd_census_tracts.png',
#        path = 'output/plots/')

# manipulation check: the nyc boundries data is 1:1 with the census tracts data
# ggplot() +
#   geom_sf(data = borough_boundries_sf,
#           color = 'blue',
#           size = 1.2) +
#   geom_sf(data = nyc_census_tracts_union,
#           color = 'red') +
#   theme_void()


# to do -------------------------------------------------------------------

# look at the st_ cheat sheet!
# group_by(tract) %>% st_distance(program_site, centroid) st_centroid
# make sure that spatial autocorrelation is not an issue with my work





