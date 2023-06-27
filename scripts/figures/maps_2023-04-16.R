
# load data ---------------------------------------------------------------

# source('scripts/wrangling/data_wrangling_v4.R')

# cleaning data for the maps ----------------------------------------------

# specific block groups to be grayed out (not within the scope of the study)
central_prk <- '360610277001'
rikers <- '360050001001'
mnt_olv_cem <- '360810607011' # pop = 14
cyp_hill_cem <- '360810561001' # pop = 17
flyd_bnt_fld <- '360470702021' # pop = 8
hly_crss_cem <- '360470852001' # pop = 26
sth_frsh_klls_prk <- '360850228011' # pop = 21
innwd_park <- '360610297001' # pop = 22
high_pop_outlier <- '360470555001' # pop = 7696
high_crime_1 <- '360810179021' # pop = 5; crime = 436555.290
high_crime_2 <- '360610109001' # pop = 127; crime = 128488.372
brnx_zoo <- '360050334001' # pop = 15

# turning bgs outside of the study's scope into NAs (to be greyed out)
map_data_1 <-
  reg_dataset_lagged %>%
  group_by(geoid) %>% 
  # booted is when all geoids are knocked out if even a single one has pop == 0
  mutate(
    num_cmplnt_ttl = if_else(
      (any(total_pop == 0) |
         total_pop == 0 |
         geoid == central_prk |
         geoid == rikers |
         geoid == mnt_olv_cem |
         geoid == cyp_hill_cem |
         geoid == flyd_bnt_fld |
         geoid == hly_crss_cem |
         geoid == sth_frsh_klls_prk |
         geoid == innwd_park |
         # geoid != high_pop_outlier,
         # geoid == high_crime_1 |
         # geoid == high_crime_2 |
         geoid == brnx_zoo),
      NA_integer_,
      num_cmplnt_ttl)) %>%
  ungroup() %>% 
  # I want the geometry at the end of the dataframe
  relocate(geometry, .after = last_col()) 

# map 1 -------------------------------------------------------------------

# 2019 DYCD and crime
ggplot() +
  # NYC crime map (fill)
  geom_sf(
    data = map_data_1 %>% 
      filter(
        year == 2019),
    mapping = aes(fill = num_cmplnt_ttl), 
    show.legend = 'fill') +
  # DYCD point data
  geom_sf(
    data = dycd_program_data %>% 
      filter(
        year == 2019),
    mapping = aes(color = 'DYCD Program Site Location'),
    size = .25,
    show.legend = 'point') +
  theme_void() +
  scale_fill_gradient2(low = '#ffffff',
                       mid = '#FF0000',
                       high = '#030200',
                       midpoint = 113,
                       na.value = '#d3d3d3') +
  scale_color_manual(values = c('DYCD Program Site Location' = '#0000ff'),
                     labels = c('DYCD Program Site Location'),
                     name = '') +
  labs(
    # title = 'Figure A1. DYCD centers and NYC crime (2019)',
    # subtitle = 'DYCD centers are concentrated around higher crime areas',
    caption = 'Data source: NYC OpenData and ACS',
    fill = 'Number of Reported Crimes') +
  theme(
    plot.title = element_text(
      size = 14, family = 'Times', face = 'bold', vjust = -16),
    plot.subtitle = element_text(
      size = 11, family = 'Times', vjust = -21),
    plot.caption = element_text(
      size = 9.5, family = 'Times', vjust = 20, hjust = 0.9),
    legend.position = c(0.20, 0.84),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(1.15, 'cm'),
    legend.text = element_text(size = 10, family = 'Times'),
    legend.title = element_text(size = 10, family = 'Times'),
    legend.spacing.x = unit(0, 'cm'),
    legend.spacing = unit(0.5, 'cm'),
    legend.title.align = 0,
    legend.text.align = 0) +
  guides(
    fill = guide_colorbar(
      title.position = 'top',
      frame.color = NULL,),
    color = guide_legend(
      override.aes = list(
        fill = NA, size = 1.7, linetype = 0)))

ggsave(filename = 'dycd_crime_2019_black.png',
       path = 'output/plots/',
       width = 8,
       height = 8,
       units = 'in')

# map 2 -------------------------------------------------------------------

# 2019 DYCD and 2020 crime (lag crime)
ggplot() +
  # 2020 NYC crime map (fill)
  geom_sf(
    data = map_data_1 %>% 
      filter(
        year == 2020),
    mapping = aes(fill = num_cmplnt_ttl), 
    show.legend = 'fill') +
  # DYCD point data
  geom_sf(
    data = dycd_program_data %>% 
      filter(
        year == 2019),
    mapping = aes(color = 'DYCD Program Site Location'),
    size = .25,
    show.legend = 'point') +
  theme_void() +
  scale_fill_gradient(low = '#ffffff',
                      high = '#FF0000',
                      na.value = '#d3d3d3') +
  scale_color_manual(values = c('DYCD Program Site Location' = '#0000ff'),
                     labels = c('DYCD Program Site Location'),
                     name = '') +
  labs(
    # title = 'Figure A2. 2019 DYCD centers and 2020 NYC crime',
    # subtitle = 'Crime persists around DYCD centers despite fewer reported crimes',
    caption = 'Data source: NYC OpenData and ACS',
    fill = 'Number of Reported Crimes') +
  theme(
    plot.title = element_text(
      size = 14, family = 'Times', face = 'bold', vjust = -16),
    plot.subtitle = element_text(
      size = 11, family = 'Times', vjust = -21),
    plot.caption = element_text(
      size = 9.5, family = 'Times', vjust = 20, hjust = 0.9),
    legend.position = c(0.20, 0.84),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(1.15, 'cm'),
    legend.text = element_text(size = 10, family = 'Times'),
    legend.title = element_text(size = 10, family = 'Times'),
    legend.spacing.x = unit(0, 'cm'),
    legend.spacing = unit(0.5, 'cm'),
    legend.title.align = 0,
    legend.text.align = 0) +
  guides(
    fill = guide_colorbar(
      title.position = 'top', order = 1,
      frame.color = NULL,),
    color = guide_legend(
      override.aes = list(
        fill = NA, size = 1.7, linetype = 0),
      order = 2))

ggsave(filename = 'dycd_crime_2020.png',
       path = 'output/plots/',
       width = 8,
       height = 8,
       units = 'in')

# map 3 -------------------------------------------------------------------

# 2019 DYCD and crime
ggplot() +
  # NYC crime map (fill)
  geom_sf(
    data = map_data_1 %>% 
      filter(
        year == 2019),
    mapping = aes(fill = num_cmplnt_ttl), 
    show.legend = 'fill') +
  # DYCD point data
  geom_sf(
    data = dycd_program_data %>% 
      filter(
        year == 2019),
    mapping = aes(color = 'DYCD Program Site Location'),
    size = .25,
    show.legend = 'point') +
  theme_void() +
  scale_fill_gradient2(low = '#ffffff',
                       mid = '#FF0000',
                       high = '#030200',
                       midpoint = 113,
                       na.value = '#d3d3d3') +
  scale_color_manual(values = c('DYCD Program Site Location' = '#0000ff'),
                     labels = c('DYCD Program Site Location'),
                     name = '') +
  labs(
    title = 'Figure A1. DYCD centers and NYC crime (2019)',
    subtitle = 'DYCD centers are concentrated around higher crime areas',
    caption = 'Data source: NYC OpenData and ACS',
    fill = 'Number of Reported Crimes') +
  theme(
    plot.title = element_text(
      size = 14, family = 'Times', face = 'bold', vjust = -16),
    plot.subtitle = element_text(
      size = 11, family = 'Times', vjust = -21),
    plot.caption = element_text(
      size = 9.5, family = 'Times', vjust = 20, hjust = 0.9),
    legend.position = c(0.17, 0.81),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(1.15, 'cm'),
    legend.text = element_text(size = 10, family = 'Times'),
    legend.title = element_text(size = 10, family = 'Times'),
    legend.spacing.x = unit(0, 'cm'),
    legend.spacing = unit(0.5, 'cm'),
    legend.title.align = 0,
    legend.text.align = 0) +
  guides(
    fill = guide_colorbar(
      title.position = 'top',
      frame.color = NULL,),
    color = guide_legend(
      override.aes = list(
        fill = NA, size = 1.7, linetype = 0)))

ggsave(filename = 'dycd_crime_2019_black_2.png',
       path = 'output/plots/',
       width = 8,
       height = 8,
       units = 'in')

# map 4 -------------------------------------------------------------------

# 2019 DYCD and 2020 crime (lag crime)
ggplot() +
  # 2020 NYC crime map (fill)
  geom_sf(
    data = map_data_1 %>% 
      filter(
        year == 2020),
    mapping = aes(fill = num_cmplnt_ttl), 
    show.legend = 'fill') +
  # DYCD point data
  geom_sf(
    data = dycd_program_data %>% 
      filter(
        year == 2019),
    mapping = aes(color = 'DYCD Program Site Location'),
    size = .25,
    show.legend = 'point') +
  theme_void() +
  scale_fill_gradient(low = '#ffffff',
                      high = '#FF0000',
                      na.value = '#d3d3d3') +
  scale_color_manual(values = c('DYCD Program Site Location' = '#0000ff'),
                     labels = c('DYCD Program Site Location'),
                     name = '') +
  labs(
    title = 'Figure A2. 2019 DYCD centers and 2020 NYC crime',
    subtitle = 'Crime persists around DYCD centers despite fewer reported crimes',
    caption = 'Data source: NYC OpenData and ACS',
    fill = 'Number of Reported Crimes') +
  theme(
    plot.title = element_text(
      size = 14, family = 'Times', face = 'bold', vjust = -16),
    plot.subtitle = element_text(
      size = 11, family = 'Times', vjust = -21),
    plot.caption = element_text(
      size = 9.5, family = 'Times', vjust = 20, hjust = 0.9),
    legend.position = c(0.17, 0.81),
    legend.direction = 'horizontal',
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(1.15, 'cm'),
    legend.text = element_text(size = 10, family = 'Times'),
    legend.title = element_text(size = 10, family = 'Times'),
    legend.spacing.x = unit(0, 'cm'),
    legend.spacing = unit(0.5, 'cm'),
    legend.title.align = 0,
    legend.text.align = 0) +
  guides(
    fill = guide_colorbar(
      title.position = 'top', order = 1,
      frame.color = NULL,),
    color = guide_legend(
      override.aes = list(
        fill = NA, size = 1.7, linetype = 0),
      order = 2))

ggsave(filename = 'dycd_crime_2020_2.png',
       path = 'output/plots/',
       width = 8,
       height = 8,
       units = 'in')





