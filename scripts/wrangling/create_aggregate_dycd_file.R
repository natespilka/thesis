# this mashes 2019, 2020, and 2021 DYCD site data 

# load dycd data ----------------------------------------------------------

# 2019 DYCD data loaded in to be mashed with 2020/2021 dycd data

# dycd_programs_2019 <-
#   read_csv('data/raw/dycd/dycd_program_sites_2019.csv') %>% 
#   janitor::clean_names() %>%
#     mutate(date = ymd(date)) %>%
#     mutate(
#       fiscal_year = year(date)) %>% 
#   select(fiscal_year, program_area, program_type, service_category, 
#          provider, program_site_name, borough, street_address, 
#          postcode, latitude, longitude, census_tract_2010, bin, bbl) 

# 2020 and 2021 DYCD data is loaded in to be mashed with the previous dataset

# dycd_programs_2020_2021 <-
#   read_csv('data/raw/dycd/dycd_program_sites_2020_2021.csv')%>% 
#   janitor::clean_names() %>%
#   mutate(fiscal_year = year(
#     ymd(
#       fiscal_year, 
#       truncated = 2L))) %>% 
#   select(fiscal_year:street_address, 
#          postcode:longitude,
#          census_tract_2010,
#          bin,
#          bbl) 

# combining the previous two dycd datasets
# dycd_programs <- 
#   rbind(dycd_programs_2019, 
#         dycd_programs_2020_2021)

# creating a csv to make future analyses easier
# dycd_programs %>% 
#   write_csv('data/raw/dycd/dycd_program_sites_2019_2020_2021.csv')