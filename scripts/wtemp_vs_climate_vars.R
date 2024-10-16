##################################################################################

temp <- read.csv('./data/average-annual-temperature-trends-1972-2022.csv')
temp <- temp %>% 
  #separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(district = site) %>% 
  rename(slope_temp = slope_decade) %>% 
  group_by(district) %>% 
  mutate(slope_temp = mean(slope_temp)) %>% 
  distinct(district, .keep_all = TRUE)

rain <- read.csv('./data/annual-maximum-one-day-rainfall-trends-1960-2022.csv') %>% 
  #separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(district = site) %>% 
  rename(slope_rain = slope_decade) %>% 
  group_by(district) %>% 
  mutate(slope_rain = mean(slope_rain)) %>% 
  distinct(district, .keep_all = TRUE)

drought_freq <- read.csv('./data/meteorological-drought-frequency-trends-1972-2022.csv') %>% 
  separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) 


df <- left_join(sen, temp, by = 'district')
df <- left_join(df, rain, by = 'district')
df <- left_join(df, geo, by = 'LID')

df <- df %>% 
  select(district:n_lakes, slope_temp, slope_rain, area:GeomorphicType) %>% 
  group_by(district) %>% 
  mutate(temp_rain_effect = slope_temp*slope_rain)

df %>% 
  filter(area < 1e+08) %>% 
  ggplot(aes(x = abs(temp_rain_effect), y = sen_slope)) +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = 1))

a <- ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = temp_rain_effect)) + 
  geom_point() +
  theme_bw() +
  scale_color_gradient2(low = 'yellow', mid = 'grey', high = 'purple') 


df %>% 
  group_by(district) %>% 
  mutate(n_lakes = n()) %>% 
  ggplot(aes(x = n_lakes, y = district)) +
  geom_col()


##################################
## air temp v surface water temp
wtemp_atemp <- ggplot(df, aes(x = slope_temp, y = sen_slope, fill = district, color = district)) +
  geom_boxplot(position = 'identity', width = 0.01, alpha = 0.7) +
  geom_point() +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  theme_bw() +
  ylab('Rate of change in surface temp (C/year)') +
  xlab('Rate of change in air temp per decade (C)') +
  theme(legend.position = 'none')
wtemp_atemp

# run linear model
lm_temp <- df %>% 
  do(mod = lm(sen_slope ~ slope_temp, data = df)) %>% 
  summarize(p_value = summary(mod)$coefficients[2, 4],
            adj_r_squared = summary(mod)$adj.r.squared) %>% 
  distinct(p_value, .keep_all = TRUE)
lm_temp  

##################################
## rain v surface water temp
wtemp_rain <- ggplot(df, aes(x = slope_rain, y = sen_slope, fill = district, color = district)) +
  geom_boxplot(position = 'identity', width = 0.4, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_point() +
  theme_bw() +
  ylab('Rate of change in surface temp') +
  xlab('Rate of change in precip per decade (mm)') +
  theme(legend.position = 'none')

lm_rain <- df %>% 
  do(mod = lm(sen_slope ~ slope_rain, data = df)) %>% 
  summarize(p_value = summary(mod)$coefficients[2, 4],
            adj_r_squared = summary(mod)$adj.r.squared) %>% 
  distinct(p_value, .keep_all = TRUE)
lm_rain

ggarrange(wtemp_atemp, wtemp_rain, common.legend = TRUE)
################################################################
# other descriptive variables to explain change in surface temp
maxd <- ggplot(df, aes(x = max_depth, y = sen_slope)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ylab('Rate of change in surface temp')

meand <- ggplot(df, aes(x = mean_depth, y = sen_slope)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()+
  ylab('Rate of change in surface temp')

sa <- ggplot(df, aes(x = area, y = sen_slope)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()+
  ylab('Rate of change in surface temp')

ggarrange(maxd, meand, sa, common.legend = TRUE)


################################################################################
## look across geomorphic type

lm_temp_geo <- df %>%
  group_by(GeomorphicType) %>%
  do(tidy(lm(sen_slope ~ slope_temp, data = .))) %>%
  filter(term == "slope_temp") %>%
  select(GeomorphicType, p.value) %>%
  rename(p_value = p.value)
lm_temp_geo

temp_geo <-  df %>% 
  filter(GeomorphicType!='Geothermal' & GeomorphicType!='Tectonic') %>% 
  ggplot(aes(x = slope_temp, y = sen_slope, fill = district, color = district)) +
  geom_boxplot(position = 'identity', width = 0.01, alpha = 0.7) +
  geom_point() +
  theme_bw() +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_hline(yintercept = 0) +
  facet_wrap(~GeomorphicType) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Rate of change in air temp per decade (C)') +
  theme(legend.position = 'none')

lm_rain_geo <- df %>%
  group_by(GeomorphicType) %>%
  do(tidy(lm(sen_slope ~ slope_rain, data = .))) %>%
  filter(term == "slope_rain") %>%
  select(GeomorphicType, p.value) %>%
  rename(p_value = p.value)
lm_rain_geo 

rain_geo <- df %>% 
  filter(GeomorphicType!='Geothermal' & GeomorphicType!='Tectonic') %>% 
  ggplot(aes(x = slope_rain, y = sen_slope, fill = district, color = district)) +
  geom_boxplot(position = 'identity', width = 0.01, alpha = 0.7) +
  geom_point() +
  theme_bw() +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_hline(yintercept = 0) +
  facet_wrap(~GeomorphicType) +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Rate of change in rain (mm/decade)') +
  theme(legend.position = 'none')

ggarrange(temp_geo, rain_geo, common.legend = TRUE)
################################################################################
## change in surf temp by east-west and north-south gradients
e_wtemp <- ggplot(df, aes(x = easting_NZTM, y = sen_slope, color = district)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  theme_bw() +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Easting')

n_wtemp <- ggplot(df, aes(x = northing_NZTM, y = sen_slope, color = district)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Northing')

ggarrange(e_wtemp, n_wtemp, common.legend = TRUE)

################################################################################
# change in climate across e/w n/s gradients
nt <- ggplot(df, aes(x = northing_NZTM, y = slope_temp)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Rate of change in air temp (C/decade)') +
  xlab('Northing')

et <- ggplot(df, aes(x = easting_NZTM, y = slope_temp)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Rate of change in air temp (C/decade)') +
  xlab('Easting')

er <- ggplot(df, aes(x = easting_NZTM, y = slope_rain)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Rate of change in rain (mm/decade)') +
  xlab('Easting')

nr <- ggplot(df, aes(x = northing_NZTM, y = slope_rain)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Rate of change in rain (mm/decade)') +
  xlab('Northing')
ggarrange(nt, et, nr, er)

rain <- ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = slope_rain)) +
  geom_point() +
  #scale_color_distiller(palette = 'RdYlGn', direction = 1) +
  scale_color_gradientn(colors = c('red', 'darkgoldenrod1', 'darkgreen')) +
  theme_bw() +
  labs(color = 'Slope') +
  ggtitle('Precipitation') +
  xlab('Easting') +
  ylab('Northing')

atemp <- ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = slope_temp)) +
  geom_point() + 
  scale_color_viridis_c(option = 'plasma') +
  theme_bw()+
  labs(color = 'Slope') +
  ggtitle('Air Temperature') +
  xlab('Easting') +
  ylab('Northing')

wtemp <- ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = sen_slope)) +
  geom_point(size = 2) +
  scale_color_viridis_b() +
  theme_bw() +
  labs(color = 'Slope') +
  ggtitle('Water Temperature') +
  xlab('Easting') +
  ylab('Northing')

ggarrange(rain, atemp, wind, wtemp)
