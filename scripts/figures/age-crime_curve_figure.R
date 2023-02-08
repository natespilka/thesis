# clean environment -------------------------------------------------------

rm(list = ls(all.names = TRUE))
graphics.off()
cat("\014") 
#Alt + Shift + K  == gets you all shortcuts

# load some packages ------------------------------------------------------

library(tidyverse)
library(janitor)
library(stringr)

# data cleaning -----------------------------------------------------------

crime_data <- 
  read_csv('data/raw/crime/table-38.csv',
           col_names = 
           skip = 4) %>% 
  clean_names() %>% 
  rename_all(
    ~str_replace(.,'^x','ages_')) %>% 
  filter(
    offense_charged == 'TOTAL') %>% 
  select(
    total_all_ages:ages_65_and_over) %>% 
  mutate_at(
    c(names(.)), as.numeric) %>% 
  mutate(
    ages_18_24 = sum(
      c_across(ages_18:ages_24))) %>% 
  select(
    total_all_ages,
    ages_17_under = ages_under_18,
    ages_18_24,
    ages_25_29:ages_60_64,
    ages_65_over = ages_65_and_over) %>% 
  pivot_longer(
    cols = total_all_ages:ages_65_over,
    names_to = 'age_range',
    values_to = 'arrest_count') %>% 
  mutate(
    prop_arrest_count = arrest_count/arrest_count[1],
    age_range = recode(age_range,
                       `ages_17_under` = '<18',
                       `ages_18_24` = '18-24',
                       `ages_25_29` = '25-29',
                       `ages_30_34` = '30-34',
                       `ages_35_39` = '35-39',
                       `ages_40_44` = '40-44',
                       `ages_45_49` = '45-49',
                       `ages_50_54` = '50-54',
                       `ages_55_59` = '55-59',
                       `ages_60_64` = '60-64',
                       `ages_65_over` = '65+'))

# figure creation ---------------------------------------------------------

crime_data %>% 
  filter(
    age_range != 'total_all_ages') %>% 
  ggplot() +
  geom_col(
    mapping = aes(
      x = age_range, y = prop_arrest_count)) +
  scale_color_manual(
    values = "#0080ff") +
  scale_y_continuous(
    limits = c(0, .25),
    expand = c(0, 0)) +
  scale_y_continuous(
    labels = scales::percent) +
  labs(
    title = 'Age-Crime Curve (arrests by age-group in 2018)',
    x = 'Age ranges',
    y = 'Percentage of arrests',
    caption =  'Source: FBI, Crime in the United States 2018, Table 38') +
  theme(
    axis.title = element_text(size = 20),
    plot.title = element_text(size  = 24),
    axis.text = element_text(size = 12),
    panel.background = element_rect(fill = 'white'),
    panel.grid = element_line(color = '#e9e9e9'),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(color = 'black'))

# save figure -------------------------------------------------------------

ggsave(
  filename = 'age-crime_curve.png',
  path = 'output/plots',
  width = 8,
  height = 5,
  units = 'in')
