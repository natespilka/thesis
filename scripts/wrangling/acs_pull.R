
# load libraries ----------------------------------------------------------

library(tidyverse)
library(sf)

# load nyc's acs ----------------------------------------------------------

years <- list(2020, 2021) 

acs_data <-
  map_dfr(
    years,
    ~ tidycensus::get_acs(
      # change 'tract' to 'block group' to see the shift for BGs
      geography = 'block group',
      # using median hh income as a random variable
      variables = 'B19013_001',
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

# manipulation checks -----------------------------------------------------

# despite the shift/changes, goeids are consistent
outliers <- 
  anti_join(
    acs_data %>%
      filter(year == 2020), 
    acs_data %>%
      as_tibble() %>%
      filter(year == 2021), 
    by = 'geoid')

# you can see the shift visually after running the following two ggplots
acs_data %>%
  filter(year == 2020) %>%
  ggplot() +
  geom_sf() +
  theme_void()

acs_data %>%
  filter(year == 2021) %>%
  ggplot() +
  geom_sf() +
  theme_void()



