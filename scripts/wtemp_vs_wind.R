#######################################################################
## read in wind data
wnd <- read.csv('./data/extreme-wind-data-to-2022/extreme-wind-trends-1980-2022.csv') %>% 
  filter(method=="Sen's slope") %>% 
  select(site, statistic, slope)

sen <- read.csv('./data/output/sen_slope_LWST_annual_mean_17_sites.csv')

b <- left_join(wnd, sen, by = 'site') %>% 
  rename(wind_slope = slope)

wind_wtemp <- b %>% 
  ggplot(aes(x = wind_slope, y = sen_slope, fill = site, color = site)) +
  # geom_boxplot(position = 'identity', width = 0.4, alpha = 0.7) +
  geom_point() +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_point() +
  theme_bw() +
  facet_wrap(~statistic) +
  ylab('Rate of change in surface temp') +
  xlab('Rate of change in wind per decade (mm)') +
  theme(legend.position = 'none')

wind_wtemp

ggarrange(wtemp_atemp, wtemp_rain, wind_wtemp)

ggplot(df, aes(x = slope_rain, y = sen_slope, fill = district, color = district)) +
  geom_boxplot(position = 'identity', width = 0.4, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_point() +
  theme_bw() +
  ylab('Rate of change in surface temp') +
  xlab('Rate of change in precip per decade (mm)') +
  theme(legend.position = 'none')


length(unique(b$site))

# run linear model for both statistics
avg <- b %>% 
  filter(statistic=='Average max gust')
mod1 <- lm(avg$sen_slope ~ avg$wind_slope, data = avg)
summary(mod1)

maxg <- b %>% 
  filter(statistic=='Highest max gust')
mod2 <- lm(maxg$sen_slope ~ maxg$wind_slope, data = maxg)
summary(mod2)

# add in northing and easting
d <- readRDS('./data/lernzmp_lakes_master.rds')
geo <- d$updated %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  select(-char)

b <- left_join(b, geo)

wind <- ggplot(b, aes(x = easting_NZTM, y = northing_NZTM, color = wind_slope)) +
  geom_point() +
  theme_bw() +
  scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                        midpoint = 0) +
  labs(color = 'Slope') +
  ggtitle('Wind') +
  xlab('Easting') +
  ylab('Northing')
wind