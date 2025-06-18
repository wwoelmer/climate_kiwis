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
library(ggpubr)

gps <- read.csv('./data/LSWT_rates_of_change_literature_16June2025.csv',
                fileEncoding = 'latin1')

# get rid of this study on the map
gps <- gps %>% 
  filter(citation_short!='This study')

mean(gps$rate_C_year, na.rm = TRUE)
sum(gps$n_lakes, na.rm = TRUE)

# change the spatial extent so it's either point, region, or global
gps <- gps %>% 
  mutate(spatial_extent = recode(spatial_extent,
                                 'country' = 'region'))
  
gps$spatial_extent <- factor(gps$spatial_extent, levels = c('point', 'region', 'global'))
gps <- gps %>% 
  filter(!is.na(rate_C_year))

# filter to lakes that we can directly compare
sub <- gps %>% 
  filter(annual_seasonal_data=='annual',
         aggregation_mean_min_max_etc=='mean')



# get world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

col_pal <- viridis(13, option = "D")

mapa <- ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_jitter(data = sub, shape = 21, stroke = 0.5, color = 'black',
             aes(x = long, y = lat, fill = spatial_extent, size = rate_C_year))  +
  #scale_fill_manual(values = col_pal) +
  labs(fill = 'Spatial extent',
       size = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 12)) +
  ggtitle(paste0('n = ', length(unique(sub$citation)), ' studies with annual trends'))

mapb <- ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_jitter(data = sub, shape = 21, stroke = 0.5, color = 'black',
              aes(x = long, y = lat, size = spatial_extent, fill = rate_C_year))  +
  scale_fill_gradient2(
    low = "steelblue",       # for cooling
    mid = "#D3D3D3",      # neutral
    high = "firebrick",       # for warming
    midpoint = 0) +
  labs(size = 'Spatial extent',
       fill = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 12)) +
  ggtitle(paste0('n = ', length(unique(sub$citation)), ' studies with annual trends'))

ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_jitter(data = sub, stroke = 0.5, color = 'black',
              aes(x = long, y = lat, shape = spatial_extent, fill = rate_C_year))  +
  scale_shape_manual(values = c(16, 15, 17)) +
  scale_fill_gradient2(
    low = "steelblue",       # for cooling
    mid = "#D3D3D3",      # neutral
    high = "firebrick",       # for warming
    midpoint = 0) +
  labs(shape = 'Spatial extent',
       fill = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 12)) +
  ggtitle(paste0('n = ', length(unique(sub$citation)), ' studies with annual trends'))


## summer only
ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_jitter(data = gps[gps$annual_seasonal_data=='summer',], size = 3, shape = 21, stroke = 0.5, color = 'black',
             aes(x = long, y = lat, fill = rate_C_year))  +
  scale_fill_gradient2(
    low = "steelblue",       # for cooling
    mid = "#D3D3D3",      # neutral
    high = "firebrick",       # for warming
    midpoint = 0) +
  labs(fill = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 14)) +
  ggtitle('Summer rates of change')

gps <- gps %>% 
  mutate(size_cat = case_when(spatial_extent == "point" ~ 'point',
                              spatial_extent == "region" ~ 'regional or global',
                              spatial_extent == "global" ~ 'regional or global'),
         alpha_cat = case_when(spatial_extent == "point" ~ 'point',
                               spatial_extent == "region" ~ 'regional or global',
                               spatial_extent == "global" ~ 'regional or global'))

ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_point(data = gps,  stroke = 1.2,
             aes(x = long, y = lat, shape = size_cat, alpha = alpha_cat,
                 color = temporal_aggregation, size = rate_C_year))  +
  #scale_fill_manual(values = col_pal) +
  labs(fill = 'Temporal aggregation',
       size = 'Rate') +
  scale_alpha_manual(values = c(1, 0.5)) +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 14))+
  ggtitle(paste0('n = ', length(unique(gps$citation)), ' studies'))


ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_point(data = gps,  stroke = 1.2,
             aes(x = long, y = lat, shape = spatial_extent,
                 color = temporal_aggregation, size = rate_C_year))  +
  #scale_fill_manual(values = col_pal) +
  labs(fill = 'Temporal aggregation',
       size = 'Rate') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size = 14))+
  ggtitle(paste0('n = ', length(unique(gps$citation)), ' studies'))

mapc <- ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_jitter(data = gps, shape = 21, stroke = 1.2, color = 'black',
             aes(x = long, y = lat, fill = rate_C_year, size = spatial_extent))  +
  scale_fill_gradient2(
    low = "steelblue",       # for cooling
    mid = "#D3D3D3",      # neutral
    high = "firebrick",       # for warming
    midpoint = 0) +
  ggtitle(paste0('All temporal aggregations (annual, summer, etc.) n = ', length(unique(gps$citation))))
mapc

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
  filter(n_lakes < 2000) %>% 
  ggplot(aes(x = n_lakes)) +
  geom_histogram()

summaries <- gps %>% 
  distinct(citation, .keep_all = TRUE)
summaries

lakes <- summaries %>% 
  filter(n_lakes < 25000) %>% 
  ggplot(aes(x = n_lakes)) +
  geom_histogram(fill = 'black') +
  theme_bw() +
  xlab('Number of lakes')
lakes
years <- summaries %>% 
  filter(n_lakes < 25000) %>% 
  ggplot(aes(x = n_years)) +
  geom_histogram(fill = 'black') +
  theme_bw() +
  xlab('Number of years')
years
method <- summaries %>% 
  filter(n_lakes < 25000,
         method_LSWT_standard %in% c('in situ', 'modelled', 'satellite')) %>% 
  ggplot(aes(x = method_LSWT_standard, fill = spatial_extent)) +
  geom_bar() +
  theme_bw() +
  scale_fill_viridis_d(option = "plasma") +  #theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(legend.position = 'top',
        legend.key.size = unit(0.2, "cm")) +
  labs(fill = 'Spatial Extent') +
  xlab('Method of measurement')+
  ylab('Number of lakes') 

method

spatial <- summaries %>% 
  filter(n_lakes < 25000) %>% 
  ggplot(aes(x = spatial_extent)) +
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab('Study spatial extent')

max(gps$n_lakes)
median(gps$n_lakes)
median(gps$n_years)
min(gps$n_years)
max(gps$n_years)


histos <- ggarrange(lakes, years, method, nrow = 1)
ggarrange(mapc, histos, nrow = 2)


# geom_bar()# make plot with the actual years on the x-axis geom_segment

gps <- gps %>% 
  group_by(temporal_aggregation) %>% 
  mutate(n_time = n(),
         facet_label = paste0(temporal_aggregation, ' (n = ', n_time, ')'))

gps$temporal_aggregation <- factor(gps$temporal_aggregation,
                                   levels = c('annual', 'spring',
                                              'summer', 'autumn',
                                              'winter', 'dry season',
                                              'pre-rainy ', 'rainy ',
                                              'post-rainy'))

gps %>% 
  filter(n_lakes < 25000) %>% 
  ggplot() + 
  geom_segment(aes(x = min_year, xend = max_year, y = rate_C_year, yend = rate_C_year, 
                   color = n_lakes), size = 1) +
  facet_wrap(~facet_label) +
  scale_color_viridis_c(option = "viridis", direction = -1) +
  theme_bw() +
  xlab('Duration of study') +
  ylab('Trend in LSWT (°C/year)') +
  labs(color = 'Number of lakes')

gps %>% 
  filter(n_lakes < 25000) %>% 
  ggplot() + 
  geom_segment(aes(x = min_year, xend = max_year, y = rate_C_year, yend = rate_C_year, 
                   color = method_LSWT_standard), size = 1) +
  facet_wrap(~facet_label) +
  #scale_color_viridis_c(option = "viridis", direction = -1) +
  theme_bw() +
  xlab('Duration of study') +
  ylab('Trend in LSWT (°C/year)') +
  labs(color = 'Number of lakes')

# number of studies that have more than one entry
table(gps$citation_short)
gps %>% 
  ungroup() %>% 
  count(citation_short) %>% 
  filter(n >1) %>% 
  summarise(n_repeated = n())
