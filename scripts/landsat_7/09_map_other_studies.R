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

gps <- read.csv('./data/LSWT_rates_of_change_literature_06June2025.csv',
                fileEncoding = 'latin1')
mean(gps$rate_C_year, na.rm = TRUE)
sum(gps$n_lakes, na.rm = TRUE)

gps$spatial_extent <- factor(gps$spatial_extent, levels = c('point', 'country', 'region', 'global'))
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
  geom_jitter(data = sub, shape = 21, stroke = 0.5, color = 'black',
             aes(x = long, y = lat, fill = spatial_extent, size = rate_C_year))  +
  #scale_fill_manual(values = col_pal) +
  labs(fill = 'Spatial extent',
       size = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 14))

ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_jitter(data = sub, size = 3, shape = 21, stroke = 0.5, color = 'black',
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
  geom_jitter(data = gps[gps$annual_seasonal_data=='summer',], size = 3, shape = 21, stroke = 0.5, color = 'black',
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
  geom_jitter(data = gps, shape = 21, stroke = 1.2, color = 'black',
             aes(x = long, y = lat, fill = rate_C_year, size = rate_C_year))  +
  scale_fill_gradient(low = 'pink', high = 'darkred') +
  ggtitle('All temporal aggregations (annual, summer, etc.)')


table(gps$spatial_extent)
table(gps$location)
table(gps$method_LSWT)
table(gps$method_LSWT_standard)
table(gps$method_trend)
table(gps$aggregation_mean_min_max_etc)
table(gps$temporal_aggregation)

hist(gps$n_years)
hist(gps$n_lakes)

gps %>% 
  ggplot(aes(x = n_lakes)) +
  geom_histogram()

gps %>% 
  filter(n_lakes < 25000) %>% 
  ggplot(aes(x = n_lakes)) +
  geom_histogram()

# make plot with the actual years on the x-axis geom_segment
gps %>% 
  filter(n_lakes < 25000) %>% 
  ggplot() + 
  geom_segment(aes(x = min_year, xend = max_year, y = n_lakes, yend = n_lakes))

gps %>% 
  filter(n_lakes < 25000) %>% 
  ggplot() + 
  geom_segment(aes(x = min_year, xend = max_year, y = n_lakes, yend = n_lakes, 
                   color = rate_C_year)) +
  scale_color_gradient(low = 'pink', high = 'darkred') 

# jitter the number of lakes so they don't overlap
set.seed(123)  # for reproducibility

gps$jittered_y <- gps$rate_C_year + runif(nrow(gps), -0.2, 0.2)

gps %>% 
  filter(n_lakes < 25000) %>% 
  ggplot() + 
  geom_segment(aes(x = min_year, xend = max_year, y = jittered_y, yend = jittered_y, 
                   color = n_lakes), size = 1) +
  facet_wrap(~temporal_aggregation) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  theme_bw() +
  xlab('Duration of study') +
  ylab('Log of rate of change in LSWT (°C/year)') +
  labs(color = 'Number of lakes')

