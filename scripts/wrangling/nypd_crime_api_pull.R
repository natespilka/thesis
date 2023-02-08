# NYPD complaint data API query


# load relevant libraries -------------------------------------------------

library(tidyverse)
library(RSocrata)

# nypd historic data ------------------------------------------------------

# trimming the data down so the pull doesn't take so long
url1 <- str_glue(
  paste0(
    "https://data.cityofnewyork.us/resource/qgea-i56i.json?",
    "$select=cmplnt_num,cmplnt_fr_dt,pd_cd,pd_desc,law_cat_cd,boro_nm,susp_age_group,susp_race,susp_sex,vic_age_group,vic_race,vic_sex,latitude,longitude&",
    "$where= susp_age_group='<18' or susp_age_group='18-24'"))

# API query for NYPD complaint data
complaints_historic <- read.socrata(
  url1,
  app_token = "this will need to be updated",
  email     = "nhs40@georgetown.edu",
  password  = "###")

# saving the data for future ease of use
# nypd_complaint_data <- as_tibble(complaints_historic)
# write_rds(nypd_complaint_data, 'data/raw/crime/nypd_complaint_data_historic_2023-02-07.rds')

# nypd year to date data --------------------------------------------------

url2 <- str_glue(
  paste0(
    "https://data.cityofnewyork.us/resource/5uac-w243.json?",
    "$select=cmplnt_num,cmplnt_fr_dt,pd_cd,pd_desc,law_cat_cd,boro_nm,susp_age_group,susp_race,susp_sex,vic_age_group,vic_race,vic_sex,latitude,longitude&",
    "$where= susp_age_group='<18' or susp_age_group='18-24'"))

# API query for NYPD complaint data
complaints_year_to_date <- read.socrata(
  url2,
  app_token = "SpjmCnwSgGsKCjZgLeYGYD8Yb",
  email     = "nhs40@georgetown.edu",
  password  = "###")

# saving the data for future ease of use
# nypd_complaint_data_year_to_date <- as_tibble(complaints_year_to_date)
# write_rds(nypd_complaint_data_year_to_date, 
#           'data/raw/crime/nypd_complaint_data_ytd_2023-02-07.rds')

# mash the two datasets ---------------------------------------------------

all_complaints <- 
  rbind(
    complaints_historic,
    complaints_year_to_date)

# nypd_all_complaints <- as_tibble(all_complaints)
# write_rds(nypd_all_complaints,
#           'data/raw/crime/nypd_all_complaints_2023-02-08.rds')

# manipulation checks -----------------------------------------------------

data_cleaned <- nypd_all_complaints %>% 
  mutate(cmplnt_fr_dt = ymd(cmplnt_fr_dt),
         year = year(cmplnt_fr_dt)) 

yearly_complaint_freq <-
  data_cleaned %>%
  filter(year > 2005) %>% 
    as_tibble() %>%
    count(year) %>%
    ggplot() +
    geom_col(mapping = aes(x = year, y = n))







