clim_trends <- read.csv('./data/output/era5_met_summary_stats_trends.csv')

sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv') %>% 
  filter(!is.na(district))

##################
# ADD REGION AS COLUMN, SPLIT DISTRICT
sen <- sen %>% 
  mutate(region = sub(".*\\((.*)\\)", "\\1", district),
         city = sub(" \\(.*\\)", "", district)) %>% 
  mutate(island = ifelse(region %in% c('Northland',
                                       'Auckland', 'Waikato',
                                       'Bay of Plenty',
                                       'Gisborne', "Hawke's Bay", 
                                       'Manawatu-Whanganui', 'Taranaki', 
                                       'Wellington'), 
                         'North Island', 'South Island')) %>% 
  arrange(region, city)


sen$region <- factor(sen$region, levels = c('Northland',
                                            'Auckland', 'Waikato',
                                            'Bay of Plenty',
                                            'Gisborne', "Hawke's Bay", 
                                            'Manawatu-Whanganui', 'Taranaki', 
                                            'Wellington',
                                            'Nelson', "Marlborough",
                                            'West Coast','Canterbury', 
                                            'Otago',
                                            'Southland'))

# combine climate and lake trend datasets
sen <- sen %>% 
  select(region, LID, sen_slope) %>% 
  rename(sen_lswt_annual = sen_slope)

clim_wide <- clim_trends %>% 
  select(LID, variable, season, sen_season) %>% 
  filter(season=='annual') %>% 
  select(-season) %>% 
  pivot_wider(names_from = 'variable', values_from = 'sen_season') %>% 
  rename(sen_atemp_annual = 'MET_tmpair',
         sen_rain_annual = 'MET_pprain')

sen_clim <- left_join(sen, clim_wide, by = 'LID')

a <- ggplot(sen_clim, aes(x = sen_atemp_annual, y = sen_lswt_annual)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab('Air temperature trend') +
  ylab('LSWT trend')
a
b <- ggplot(sen_clim, aes(x = sen_rain_annual, y = sen_lswt_annual)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw()+
  xlab('Precipitation trend') +
  ylab('LSWT trend')

p <- ggarrange(a, b, common.legend = TRUE)

ggsave('./figures/landsat_7/LSWT_vs_atemp_precip.png', p, 
       dpi = 300, units = 'mm', height = 200, width = 450, scale = 0.5)
