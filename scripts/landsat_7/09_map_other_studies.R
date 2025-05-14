# make map of other studies
library(tidyverse)
library(aemetools)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(scales)
library(MetBrewer)
library(viridis)

gps <- read.csv('./data/LSWT_rates_of_change_literature_8May2025.csv',
                fileEncoding = 'latin1')
mean(gps$rate_C_year)
sum(gps$n_lakes)

gps$spatial.extent <- factor(gps$spatial.extent, levels = c('point', 'country', 'region', 'global'))
gps <- gps %>% 
  filter(!is.na(rate_C_year))

# filter to lakes that we can directly compare
sub <- gps %>% 
  filter(annual_seasonal_data=='annual',
         aggregation_mean_min_max_etc=='mean')

# get world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

col_pal <- viridis(13, option = "D")

ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_point(data = sub, shape = 21, stroke = 0.5, color = 'black',
             aes(x = long, y = lat, fill = spatial.extent, size = rate_C_year))  +
  #scale_fill_manual(values = col_pal) +
  labs(fill = 'Citation',
       size = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 14))

ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_point(data = sub, size = 3, shape = 21, stroke = 0.5, color = 'black',
             aes(x = long, y = lat, fill = rate_C_year))  +
  scale_fill_gradient2(
    low = "blue",       # for cooling
    mid = "white",      # neutral
    high = "red",       # for warming
    midpoint = 0) +
  labs(fill = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 14))

## summer only
ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_point(data = gps[gps$annual_seasonal_data=='summer',], size = 3, shape = 21, stroke = 0.5, color = 'black',
             aes(x = long, y = lat, fill = rate_C_year))  +
  scale_fill_gradient2(
    low = "blue",       # for cooling
    mid = "white",      # neutral
    high = "red",       # for warming
    midpoint = 0) +
  labs(fill = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 14)) +
  ggtitle('Summer rates of change')

ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_point(data = gps, shape = 21, stroke = 1.2, color = 'black',
             aes(x = long, y = lat, fill = annual_seasonal_data, size = rate_C_year))  +
  #scale_fill_manual(values = col_pal) +
  labs(fill = 'Citation',
       size = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 14))

ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_point(data = gps, shape = 21, stroke = 1.2, color = 'black',
             aes(x = long, y = lat, fill = rate_C_year, size = rate_C_year))  +
  scale_fill_gradient(low = 'pink', high = 'darkred') 



