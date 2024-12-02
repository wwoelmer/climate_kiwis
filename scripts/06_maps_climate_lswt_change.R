##################################################################################
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(ggpubr)

# read LSWT output
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts.csv')

# read lake x-y locations to match with output
geo <- readRDS('./data/lernzmp_lakes_master.rds')
geo <- geo$updated 
geo <- geo %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  select(-char)
geo$LID <- as.numeric(geo$LID)

temp <- read.csv('./data/average-annual-temperature-trends-1972-2022.csv')
temp_loc <- read.csv('./data/temperature-data-to-2022/annual-and-seasonal-temperature-at-30-sites-to-2022.csv') %>% 
  select(site, lat, lon)
temp <- left_join(temp, temp_loc)

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


df <- left_join(sen, temp, by = 'district')
df <- left_join(df, rain, by = 'district')
df <- left_join(df, geo, by = 'LID')

df <- df %>% 
  select(district:n_lakes, lat, lon, slope_temp, slope_rain, area:GeomorphicType) %>% 
  group_by(district) %>% 
  mutate(temp_rain_effect = slope_temp*slope_rain)

df <- df %>% 
  mutate(region = sub(".*\\((.*)\\)", "\\1", district),
         city = sub(" \\(.*\\)", "", district)) %>% 
  mutate(island = ifelse(region %in% c('Northland',
                                       'Auckland', 'Waikato',
                                       'Bay of Plenty',
                                       'Gisborne', "Hawke's Bay", 
                                       'Manawatu-Whanganui', 'Taranaki', 
                                       'Wellington'), 
                         'North', 'South')) %>% 
  arrange(region, city)


# add wind
wnd <- read.csv('./data/extreme-wind-data-to-2022/extreme-wind-trends-1980-2022.csv') 
wind_loc <- read.csv('./data/extreme-wind-data-to-2022/extreme-wind-1972-2022.csv') %>% 
  select(site, lat, lon) %>% 
  distinct(site, .keep_all = TRUE)
wnd <- left_join(wnd, wind_loc, by = 'site')

wnd <- wnd %>% 
  filter(method=="Sen's slope") %>% 
  select(site, statistic, slope) %>% 
  rename(wind_slope = slope,
         city = site)

b <- left_join(wnd, df) 


df %>% 
  group_by(district) %>% 
  mutate(n_lakes = n()) %>% 
  ggplot(aes(x = n_lakes, y = district)) +
  geom_col()


################################################################################
# display maps of changes in climate vars and lswt
rain <- ggplot(df, aes(x = lon, y = lat, color = slope_rain)) +
  geom_point(size = 4) +
  #scale_color_distiller(palette = 'RdYlGn', direction = 1) +
  scale_color_gradientn(colors = c('red', 'darkgoldenrod1', 'darkgreen')) +
  theme_bw() +
  labs(color = 'Slope') +
  ggtitle('Precipitation') +
  xlab('Easting') +
  ylab('Northing')
rain

atemp <- ggplot(df, aes(x = lon, y = lat, color = slope_temp)) +
  geom_point(size = 4) + 
  scale_color_viridis_c(option = 'plasma') +
  theme_bw()+
  labs(color = 'Slope') +
  ggtitle('Air Temperature') +
  xlab('Easting') +
  ylab('Northing')
atemp

wtemp <- ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = sen_slope)) +
  geom_point(size = 2) +
  scale_color_gradient2(
    high = brewer.pal(9, "RdBu")[1],   # Cold color
    mid = "ivory",                    # Midpoint (neutral color)
    low = brewer.pal(9, "RdBu")[9],  # Hot color
    midpoint = 0                      # Set midpoint to 0 for diverging effect
  ) +  #scale_color_viridis_b() +
  theme_bw() +
  labs(color = 'Slope') +
  ggtitle('Water Temperature') +
  xlab('Easting') +
  ylab('Northing')
wtemp

wind <- b %>% 
  filter(statistic=='Average max gust') %>% 
  ggplot(aes(x = lon, y = lat, color = wind_slope)) +
  geom_point(size = 4) +
  theme_bw() +
  scale_color_gradient2(low = 'blue', mid = 'yellow', high = 'red',
                        midpoint = 0) +
  labs(color = 'Slope') +
  ggtitle('Average Max Gust') +
  xlab('Easting') +
  ylab('Northing')
wind

maps <- ggarrange(rain, atemp, wind, wtemp, labels = 'auto')
maps

ggsave('./figures/maps_climate_vars_LSWT.png', maps,
       dpi = 300, units = 'mm', height = 500, width = 575, scale = 0.45)
