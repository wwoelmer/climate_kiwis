# look at annual air temperature in each region
library(tidyverse)
library(RColorBrewer)
library(lubridate)

temp <- read.csv('./data/temperature-data-to-2022/annual-and-seasonal-temperature-at-30-sites-to-2022.csv')
temp <- temp %>% 
  filter(season=='Annual') %>% 
  rename(district = site)

ggplot(temp, aes(x = year, y = temperature, color = district)) +
  geom_point()

temp_annual <- temp %>% 
  group_by(district, statistic, lat, lon) %>% 
  summarise(mean_temp = mean(temperature, na.rm = TRUE))

ggplot(temp_annual, aes(x = lon, y = lat, color = mean_temp)) +
  geom_point() +
  scale_color_gradientn(colors = c('red', 'darkgoldenrod1', 'darkgreen')) 
  
# combine with LSWT rate of change
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts.csv')

df <- left_join(sen, temp_annual)

ggplot(df, aes(x = mean_temp, y = sen_slope, color = district)) +
  geom_point() +
  theme(legend.position = 'none') +
  geom_smooth(method = 'lm', aes(group = district)) +
  facet_wrap(~statistic, scales = 'free')

##########################################################################################
# rainfall
rain <- read.csv('./data/extreme-rainfall-data-to-2022/extreme-rainfall-1960-to-2022.csv')

rain <- rain %>% 
  mutate(year = year(period_start)) %>% 
  select(site, year, parameter, data_value, lat, lon) %>% 
  rename(district = site,
         statistic = parameter,
         value = data_value)


ggplot(rain, aes(x = year, y = value, color = district)) +
  geom_point() +
  facet_wrap(~statistic, scales = 'free')

rain_annual <- rain %>% 
  group_by(district, statistic, lat, lon) %>% 
  summarise(mean_rain = mean(value, na.rm = TRUE))


df <- left_join(sen, rain_annual)

ggplot(df, aes(x = mean_rain, y = sen_slope, color = district)) +
  geom_point() +
  theme(legend.position = 'none') +
  geom_smooth(method = 'lm', aes(group = district)) +
  facet_wrap(~statistic, scales = 'free')
