################################################################################
# compare LSWT to in situ observations
atemp_season <- read.csv('./data/average-seasonal-temperature-trends-1972-2022.csv')

atemp_season$season <- factor(atemp_season$season, levels = c('Winter', 'Autumn', 'Summer', 'Spring'))

ggplot(atemp_season, aes(y = fct_rev(season), x = slope_decade, fill = season)) +
  geom_boxplot() +
  geom_vline(xintercept = 0) +
  theme_bw() +
  scale_fill_manual(values = c("#96C0B7", "#FFB84D", "#EE6C4D", "#A8D08D")) +
  xlab('Change in air temp (1972-2022)') +
  ylab('Season')

atemp <- read.csv('./data/average-annual-temperature-trends-1972-2022.csv')

ggplot(atemp, aes(y = site, x = slope_decade, color = site)) +
  geom_point(size = 4) +
  geom_vline(xintercept = 0) +
  xlab('Change in air temp (1972-2022)') +
  theme_bw() +
  theme(legend.position = 'none') +
  geom_vline(xintercept = 0.177, color = 'red') 

mean(atemp$slope_decade)

ann_temp <- read.csv('./data/temperature-data-to-2022/annual-and-seasonal-temperature-at-30-sites-to-2022.csv')

ann_temp %>% 
  filter(season=='Annual') %>% 
  ggplot(aes(x = year, y = temperature, color = site)) +
  geom_point() +
  facet_wrap(~statistic) +
  theme(legend.position = 'none')

# compare to annual ai./data/temperature-data-to-2022/annual-and-seasonal-temperature-at-30-sites-to-2022.csv# compare to annual air temperature trends