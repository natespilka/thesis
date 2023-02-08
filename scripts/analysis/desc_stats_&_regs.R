

# load data ---------------------------------------------------------------

thesis_dataset <- 
  read_rds('data/processed/thesis_dataset_2023-01-30.rds')


# load packages -----------------------------------------------------------

library(vtable)

# descriptive statistics --------------------------------------------------

thesis_dataset %>% 
  st_drop_geometry() %>% 
  select(geoid, year, 
         # dependent variable
         crime_rate, num_complaints, 
         # independent variable
         sites_for_2nd_3rd_yrs, 
         # economic control variables
         inc_2019_dlrs, prcnt_unemp,
         # demographic control variables
         total_pop, prcnt_white, prcnt_black,
         prcnt_hisp, prcnt_asian, prcnt_all_other,
         prcnt_yth_yng_adlt,
         # education control variables
         prcnt_no_hs_deg, prcnt_hs_no_ba_deg, 
         prcnt_ba_or_hghr_deg) %>% 
  sumtable(
    group = 'year',
    summ = c('mean(x)',
             'sd(x)', 
             'min(x)', 
             'max(x)'))

# regressions -------------------------------------------------------------





