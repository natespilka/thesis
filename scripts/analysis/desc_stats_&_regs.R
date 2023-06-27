

# load data ---------------------------------------------------------------

thesis_dataset <- 
  read_rds('data/processed/thesis_dataset_2023-03-07.rds')

# descriptive statistics --------------------------------------------------

reg_dataset_testing %>% 
  # st_drop_geometry() %>% 
  select(geoid, year, 
         # dependent variable
         crime_rate, num_cmplnt_ttl, 
         # independent variable
         all_site_type, 
         # economic control variables
         median_hh_inc, prcnt_unemp,
         # demographic control variables
         total_pop, prcnt_white, prcnt_black,
         prcnt_hisp, prcnt_asian, prcnt_all_other,
         prcnt_yth_yng_adlt,
         # education control variables
         prcnt_no_hs_deg, prcnt_hs_no_ba_deg, 
         prcnt_ba_or_hghr_deg) %>% 
  vtable::sumtable(
    group = 'year',
    summ = c('mean(x)',
             'sd(x)', 
             'min(x)', 
             'max(x)'))

# regressions -------------------------------------------------------------


thesis_dataset %>% 
  group_by(year) %>% 
  summarise(
    avg = mean(all_site_type, na.rm = TRUE)
  )


