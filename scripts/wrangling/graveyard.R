# cleaning dycd data ------------------------------------------------------

# iterating through and creating a list of dycd site + ACS data by year
cleaned_dycd_programs <- 
  map_df(
    2019:2021,
    ~ dycd_program_data %>% 
      filter(year == .x) %>% 
      st_join(
        acs_data %>% 
          filter(year == .x),
        .)) %>% 
  st_drop_geometry() %>% 
  rename(year = year.x) %>% 
  mutate(
    all_site_type_indicator = case_when(
      # all sites types indicator
      (!is.na(year.y) > 0) ~ 1,
      TRUE ~ 0),
    # SYEP indicator
    syep_site_type_indicator = case_when(
      (program_area == "SYEP" | 
         grepl("Jobs and Internships", service_category)) ~ 1,
      TRUE ~ 0),
    # ASP indicator
    asp_site_type_indicator = case_when(
      grepl("Afterschool Programs", service_category) ~ 1,
      TRUE ~ 0))

# %>% 
#   set_names('dycd_2019','dycd_2020','dycd_2021')

dycd_count <- 
  cleaned_dycd_programs %>% 
  group_by(geoid, year) %>% 
  summarise(
    all_site_type = if_else(
      # all sites types
      sum(all_site_type_indicator) > 0,
      1,
      0),
    # SYEP dummy
    syep_site_type = if_else(
      sum(syep_site_type_indicator) > 0,
      1,
      0),
    # ASP dummy
    asp_site_type = if_else(
      sum(asp_site_type_indicator) > 0,
      1,
      0))

# # names for dycd columns
# site_yr_names <- 
#   c('dycd_site_2019','dycd_site_2020','dycd_site_2021')
# 
# # count of all DYCD programs by geoid and create tract indicators
# dycd_count <- 
#   map2(
#     cleaned_dycd_programs,
#     site_yr_names,
#     ~ .x %>% 
#       st_drop_geometry() %>% 
#       group_by(geoid) %>% 
#       # counting the number of sites per unit of analysis
#       summarise(
#         num_dycd_programs = sum(!is.na(year))) %>% 
#       mutate(
#         # iterating through our site_yr_names object
#         # creating an indicator variable (1/0 whether there's a site)
#         !!.y := if_else(
#           num_dycd_programs > 0,
#           1,
#           0)) %>% 
#       select(!num_dycd_programs)) %>% 
#   # mashing all dycd datasets in the list
#   reduce(
#     full_join,
#     by = 'geoid') %>% 
#   # identifying tracts that had a site in only 2020 and 2021
#   mutate(
#     site_for_0_1_2_3_yrs = case_when(
#       ((dycd_site_2019 == 0) & 
#          (dycd_site_2020 == 0) & 
#          (dycd_site_2021 == 0)) ~ 0,
#       ((dycd_site_2019 == 0) & 
#          (dycd_site_2020 == 1) & 
#          (dycd_site_2021 == 0)) ~ 1,
#       ((dycd_site_2019 == 0) & 
#          (dycd_site_2020 == 1) & 
#          (dycd_site_2021 == 1)) ~ 2,
#       ((dycd_site_2019 == 1) & 
#          (dycd_site_2020 == 1) & 
#          (dycd_site_2021 == 1)) ~ 3,
#       ((dycd_site_2019 == 1) & 
#          (dycd_site_2020 == 1) & 
#          (dycd_site_2021 == 0)) ~ 4,
#       ((dycd_site_2019 == 1) & 
#          (dycd_site_2020 == 0) & 
#          (dycd_site_2021 == 0)) ~ 5,
#       ((dycd_site_2019 == 0) & 
#          (dycd_site_2020 == 0) & 
#          (dycd_site_2021 == 1)) ~ 6)) %>% 
#   pivot_longer(
#     cols = dycd_site_2019:dycd_site_2021,
#     names_to = 'year',
#     values_to = 'dycd_site',
#     names_prefix = 'dycd_site_') %>% 
#   # dummying out our different site types
#   mutate(
#     year = as.numeric(year),
#     sites_for_0yrs = if_else(
#       site_for_0_1_2_3_yrs == 0,
#       1,
#       0),
#     sites_for_2nd_yr = if_else(
#       site_for_0_1_2_3_yrs == 1,
#       1,
#       0),
#     sites_for_2nd_3rd_yrs = if_else(
#       site_for_0_1_2_3_yrs == 2,
#       1,
#       0),
#     sites_for_3_yrs = if_else(
#       site_for_0_1_2_3_yrs == 3,
#       1,
#       0),
#     sites_for_1st_2nd_yrs = if_else(
#       site_for_0_1_2_3_yrs == 4,
#       1,
#       0),
#     sites_for_1st_yr = if_else(
#       site_for_0_1_2_3_yrs == 5,
#       1,
#       0),
#     sites_for_last_yr = if_else(
#       site_for_0_1_2_3_yrs == 6,
#       1,
#       0))

# SYEP dummy
syep_site_type = if_else(
  sum(syep_site_type_indicator) > 0,
  1,
  0),
# ASP dummy
asp_site_type = if_else(
  sum(asp_site_type_indicator) > 0,
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



# original distance measure -----------------------------------------------

# USE FILTER TO GET WHAT WE WANT
# calculating average centroid distance 
# dycd_programs_dist <- 
#   map(
#     2019:2022,
#     ~ cleaned_dycd_programs %>% 
#       filter(year == .x) %>% 
#       # head(5) %>% 
#       mutate(
#         dist = 
#           st_distance(
#             st_centroid(.),
#             dycd_program_data %>% 
#               filter(year == .x)) %>% 
#               # head(5)) %>%
#           units::set_units('m')) %>% 
#       st_drop_geometry() %>% 
#   rowwise() %>% 
#   mutate(
#     mean_dist = mean(dist, na.rm = TRUE)) %>% 
#   select(geoid:asp_site_type, mean_dist)) %>% 
#   bind_rows()

# fancy distance measures -------------------------------------------------

# %>% 
#   # adding the geometry for the centroid work below
#   left_join(
#     acs_data_2019_2022 %>% 
#       select(geoid, year, geometry),
#     .,
#     by = c('geoid', 'year'))

# calculating distances for all DYCD centers, SYEPs, and ASPs
# dycd_programs_dist <- 
#   # creating a list initially because matracies are of different sizes
#   map(
#     2019:2022,
#     ~ cleaned_dycd_programs %>% 
#       filter(year == .x) %>% 
#       # head(5) %>%
#       mutate(
#         # calculating bg centroid distances from all center types
#         dist_all = st_distance(
#           st_centroid(.),
#           dycd_program_data %>% 
#             filter(year == .x)) %>% 
#             # head(5)) %>%
#           units::set_units('m'),
#         # calculating bg centroid distances from SYEPs
#         dist_syep = st_distance(
#           st_centroid(.),
#           dycd_program_data %>% 
#             filter(
#               year == .x,
#               (syep_site_type_indicator == 1 & 
#                  asp_site_type_indicator == 0))) %>% 
#             # head(5)) %>%
#           units::set_units('m'),
#         # calculating bg centroid distances from ASPs
#         dist_asp = st_distance(
#           st_centroid(.),
#           dycd_program_data %>% 
#             filter(
#               year == .x,
#               (asp_site_type_indicator == 1 & 
#                  syep_site_type_indicator == 0))) %>% 
#             # head(5)) %>%
#           units::set_units('m')
#         ) %>% 
#       # this lowers computational burden
#       st_drop_geometry() %>% 
#       rowwise() %>% 
#       # calculating mean distances for all centers, SYEPs, and ASPs
#       mutate(
#         mean_dist_all = mean(dist_all, na.rm = TRUE),
#         mean_dist_syep = mean(dist_syep, na.rm = TRUE),
#         mean_dist_asp = mean(dist_asp, na.rm = TRUE),
#         indx_dist_all = sum(
#           (1/dist_all)^2, na.rm = TRUE),
#         indx_dist_syep = sum(
#           (1/dist_syep)^2, na.rm = TRUE),
#         indx_dist_asp = sum(
#           (1/dist_asp)^2, na.rm = TRUE)) %>% 
#       select(geoid:asp_site_type, mean_dist_all:indx_dist_asp)) %>% 
#   bind_rows() %>% 
#   # theres one observation where the value is > 1
#   mutate(indx_dist_all = if_else(
#     indx_dist_all > 1 %>% units::set_units('1/m^2'),
#     1 %>% units::set_units('1/m^2'),
#     indx_dist_all
#   ))

# manipulation check
# tmap_mode('view')
# tm_basemap('OpenStreetMap') +
#   tm_shape(acs_data_2019_2022 %>%
#              filter(year == 2019)) +
#   tm_polygons() +
#   tm_shape(dycd_program_data %>% 
#              filter(
#                year == 2019,
#                (syep_site_type_indicator == 1 & 
#                   asp_site_type_indicator == 0))) +
#   tm_dots() +
#   tmap_options(check.and.fix = TRUE)















