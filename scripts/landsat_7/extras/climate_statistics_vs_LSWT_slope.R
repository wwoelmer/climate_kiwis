# look at annual air temperature in each region
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(Hmisc)
library(corrplot)

temp <- read.csv('./data/temperature-data-to-2022/annual-and-seasonal-temperature-at-30-sites-to-2022.csv')
temp <- temp %>% 
  rename(district = site)

ggplot(temp, aes(x = year, y = temperature, color = district)) +
  geom_point() + facet_wrap(~season)

temp_annual <- temp %>% 
  group_by(district, statistic, season, lat, lon) %>% 
  summarise(mean_temp = mean(temperature, na.rm = TRUE))

ggplot(temp_annual, aes(x = lon, y = lat, color = mean_temp)) +
  geom_point() +
  scale_color_gradientn(colors = c('red', 'darkgoldenrod1', 'darkgreen')) +
  facet_wrap(~season)
  

temp_wide <- temp_annual %>% 
  pivot_wider(names_from = c(statistic, season), values_from = mean_temp, names_glue = "{statistic}_{season}_temp")


#write.csv(temp_wide, './data/drivers/seasonal_temperature_statistics.csv', row.names = FALSE)

# combine with LSWT rate of change
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv')

df <- left_join(sen, temp_wide)

cor_df <- df %>% 
  select(-c(district, LID, lat, lon, sen_signif, n))
cor_out <- rcorr(as.matrix(cor_df)) # default is pearson
round(cor_out$r[1,],2)
corrplot(cor_out$r, type = 'upper', 
         sig.level = 0.01, insig = 'blank', p.mat = cor_out$P)


##########################################################################################
# rainfall
rain <- read.csv('./data/extreme-rainfall-data-to-2022/extreme-rainfall-1960-to-2022.csv')

rain <- rain %>% 
  mutate(year = year(period_start)) %>% 
  select(site, year, parameter, data_value, lat, lon) %>% 
  rename(district = site,
         statistic = parameter,
         value = data_value)

rain_annual <- rain %>% 
  group_by(district, statistic, lat, lon) %>% 
  summarise(mean_rain = mean(value, na.rm = TRUE))


df <- left_join(sen, rain_annual)

ggplot(df, aes(x = mean_rain, y = sen_slope, color = district)) +
  geom_point() +
  theme(legend.position = 'none') +
  geom_smooth(method = 'lm', aes(group = district)) +
  facet_wrap(~statistic, scales = 'free')

rain_wide <- rain_annual %>% 
  pivot_wider(names_from = statistic, values_from = mean_rain, names_glue = "{statistic}_rain")

df <- left_join(sen, rain_wide)

cor_df <- df %>% 
  select(-c(district, LID, n, lat, lon, sen_signif))

cor_out <- rcorr(as.matrix(cor_df), type = 'spearman') # default is pearson
round(cor_out$r[1,],2)
corrplot(cor_out$r, type = 'upper', method = 'number',
         sig.level = 0.01, insig = 'blank', p.mat = cor_out$P)
