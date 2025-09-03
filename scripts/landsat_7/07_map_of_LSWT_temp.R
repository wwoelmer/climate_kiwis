##################################################################################
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(ggpubr)
library(sf)
library(aemetools)
#install.packages('ggExtra')
library(ggExtra)

# read LSWT output
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv') %>% 
  filter(!is.na(district))

# read lake x-y locations to match with output
geo <- readRDS('./data/lernzmp_lakes_master.rds')
geo <- geo$updated 
geo <- geo %>% 
  dplyr::select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  dplyr::select(-char)
geo$LID <- as.numeric(geo$LID)

temp <- read.csv('./data/average-annual-temperature-trends-1972-2022.csv')
temp_loc <- read.csv('./data/temperature-data-to-2022/annual-and-seasonal-temperature-at-30-sites-to-2022.csv') %>% 
  dplyr::select(site, lat, lon)
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
  dplyr::select(district:n, lat, lon, slope_temp, slope_rain, area:GeomorphicType) %>% 
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
  dplyr::select(site, lat, lon) %>% 
  distinct(site, .keep_all = TRUE)
wnd <- left_join(wnd, wind_loc, by = 'site')

wnd <- wnd %>% 
  filter(method=="Sen's slope") %>% 
  dplyr::select(site, statistic, slope) %>% 
  rename(wind_slope = slope,
         city = site)

b <- left_join(wnd, df) 


df %>% 
  group_by(district) %>% 
  mutate(n_lakes = n()) %>% 
  ggplot(aes(x = n_lakes, y = district)) +
  geom_col()

# format easting and northing into lat/long
df_wtemp <- df %>% 
  dplyr::select(-lat, -lon) %>% 
  sf::st_as_sf(coords = c("easting_NZTM", "northing_NZTM"), crs = 2193)   # NZGD2000 / New Zealand Transverse Mercator 2000

# Transform to WGS84 (latitude/longitude)
sf_data_latlon <- st_transform(df_wtemp, crs = 4326)

# Add lat/lon columns to the original dataframe
df_wtemp$lat <- st_coordinates(sf_data_latlon)[, 2]
df_wtemp$lon <- st_coordinates(sf_data_latlon)[, 1]

#################################################################################
# get country shapefile
# read from LINZ
url <- "https://data.linz.govt.nz/"
layer_id <- 51560
key <- Sys.getenv("LINZ_API_KEY") # LINZ API key

# Read the shapefile data
nz_shapefile <- read_web_sf(url = url, layer_id = layer_id, key = key)
nz_shapefile

wtemp <-  ggplot() +
  geom_sf(data = nz_shapefile, fill = 'darkgrey', color = 'black') +
  theme_bw() +
  geom_jitter(data = df_wtemp, 
             aes(x = lon, y = lat, fill = sen_slope), 
             shape = 21, color = 'black', size = 2)  +
  scale_fill_gradient2(high = '#ca0020',
                       mid = "white",         
                       low = '#00316E', 
                       midpoint = 0) + 
  theme_bw() +
  labs(fill = 'Rate of Change') +
  xlab('Longitude') +
  ylab('Latitude') +
  guides(size = 'none') +
  theme(text = element_text(size = 12),
        legend.position = "left",
        legend.direction = "vertical",
        legend.box = "vertical") 
wtemp

library(plotly)
ggplotly(wtemp)

wtemp_hist <- ggMarginal(wtemp, 
           type = "histogram", 
           margins = "both", 
           size = 4, 
           fill = "gray", 
           color = "black")
wtemp_hist

ggsave('./figures/landsat_7/map_LSWT.png', wtemp_hist,
       dpi = 300, units = 'mm', height = 400, width = 350, scale = 0.4)

# discrete slope categories
df_wtemp <- df_wtemp %>% 
  mutate(slope_cat = case_when(
    sen_slope <= -0.1 ~ "Strong Cooling",
    sen_slope > -0.1 & sen_slope < -0.01 ~ "Mild Cooling",
    sen_slope >= -0.01 & sen_slope <= 0.01 ~ "No Change",
    sen_slope > 0.01 & sen_slope < 0.1 ~ "Mild Warming",
    sen_slope >= 0.1 ~ "Strong Warming"))

df_wtemp$slope_cat <- factor(df_wtemp$slope_cat, levels = c('Strong Warming',
                                                            'Mild Warming',
                                                            'No Change',
                                                            'Mild Cooling',
                                                            'Strong Cooling'),
                             labels = c('Strong Warming: >= 0.1',
                                        'Mild Warming: 0.01 to 0.1',
                                        'No Change: -0.01 to 0.01',
                                        'Mild Cooling: -0.1 to -0.01',
                                        'Strong Cooling: <= -0.1'))

map_categ <-  ggplot() +
  geom_sf(data = nz_shapefile, fill = '#4D4D4D', color = 'black') +
  theme_bw() +
  geom_point(data = df_wtemp, 
             aes(x = lon, y = lat, color = slope_cat)) +
  geom_point(data = df_wtemp, 
             aes(x = lon, y = lat, fill = slope_cat, group = slope_cat), 
             shape = 21, color = 'black', size = 2)  +
  theme_bw() +
  scale_fill_manual(values = c('Strong Cooling: <= -0.1' = '#00316E',
                               'Mild Cooling: -0.1 to -0.01' = "#8098B7",
                               'No Change: -0.01 to 0.01' = 'white',
                               'Mild Warming: 0.01 to 0.1' = "#F299A3",
                               'Strong Warming: >= 0.1' = '#ca0020')) +
  scale_color_manual(values = c('Strong Cooling: <= -0.1' = '#00316E',
                                'Mild Cooling: -0.1 to -0.01' = "#8098B7",
                                'No Change: -0.01 to 0.01' = 'white',
                                'Mild Warming: 0.01 to 0.1' = "#F299A3",
                                'Strong Warming: >= 0.1' = '#ca0020')) +
  xlab('Longitude') +
  ylab('Latitude') +
  labs(fill = 'Rate of Change') +
  guides(size = 'none',
         color = 'none') +
  theme(text = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "left", # or left
        legend.direction = "vertical")
        #legend.box = "vertical") 
map_categ

map_cat_hist <- ggMarginal(map_categ, 
           type = "histogram", 
           margins = "both", 
           groupFill = TRUE,
           size = 4, 
           color = "black")
map_cat_hist

ggsave('./figures/landsat_7/map_categories_LSWT.png', map_cat_hist,
       dpi = 300, units = 'mm', height = 400, width = 400, scale = 0.4)

a <- ggplot(df_wtemp, aes(y = sen_slope)) +
  geom_density(size = 2, fill = 'black', alpha = 0.7) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  ylab('Rate of change in LSWT (°C/year)') +
  xlab('Density') +
  theme(text = element_text(size = 16))
        
a
ggsave('./figures/landsat_7/density_all_lakes.png', a,
       dpi = 300, units = 'mm', height = 400, width = 250, scale = 0.34)

