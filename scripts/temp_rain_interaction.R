# combined effect of temp and rain
library(tidyverse)

# combine with climate data
clim <- read.csv('./data/average-annual-temperature-trends-1972-2022.csv')
clim <- clim %>% 
  separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(slope_temp = slope_decade) %>% 
  group_by(region) %>% 
  mutate(slope_temp = mean(slope_temp)) %>% 
  distinct(region, .keep_all = TRUE)

rain <- read.csv('./data/annual-maximum-one-day-rainfall-trends-1960-2022.csv') %>% 
  separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(slope_rain = slope_decade) %>%
  group_by(region) %>% 
  mutate(slope_rain = mean(slope_rain)) %>% 
  distinct(region, .keep_all = TRUE)

df <- left_join(sen, clim, by = 'region')
df <- left_join(df, rain, by = 'region')

df <- df %>% 
  select(region:n_lakes, slope_temp, slope_rain, area:GeomorphicType) %>% 
  group_by(region) %>% 
  mutate(temp_rain_effect = slope_temp*slope_rain)

df %>% 
  filter(area < 1e+08) %>% 
  ggplot(aes(x = abs(temp_rain_effect), y = sen_slope)) +
  geom_point() +
  geom_smooth(method = 'lm', aes(group = 1))

ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = temp_rain_effect)) + 
  geom_point() +
  theme_bw() +
  scale_color_gradient2(low = 'yellow', mid = 'grey', high = 'purple') 

################################################################################
## test some generalized linear models
df_glm <- df %>% 
  select(sen_slope, slope_temp, slope_rain)

m1 <- lm(sen_slope ~ slope_temp + slope_rain, data = df_glm)
summary(m1)

m_interact <- lm(sen_slope ~ slope_temp*slope_rain, data = df_glm)
summary(m_interact)
