# old map figure code
# get world shapefile
# filter to lakes that we can directly compare
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

gps <- read.csv('./data/LSWT_rates_of_change_literature.csv',
                fileEncoding = 'latin1')

gps$rate_C_year <- as.numeric(gps$rate_C_year)

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

sub <- gps %>% 
  filter(annual_seasonal_data=='annual',
         aggregation_mean_min_max_etc=='mean')

world <- ne_countries(scale = "medium", returnclass = "sf")

gps <- gps %>% 
  mutate(size_cat = case_when(spatial_extent == "point" ~ 'point',
                              spatial_extent == "region" ~ 'regional or global',
                              spatial_extent == "global" ~ 'regional or global'),
         alpha_cat = case_when(spatial_extent == "point" ~ 'point',
                               spatial_extent == "region" ~ 'regional or global',
                               spatial_extent == "global" ~ 'regional or global'))

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
  labs(fill = 'Rate (°C/year)',
       size = 'Saptial Extent') +
  guides(
    fill = guide_colorbar(direction = "vertical"),
    size = guide_legend(direction = "vertical")
  ) +
  theme(
    legend.position = "right",       # Keep legends on the right
    legend.box = "horizontal"        # Arrange separate legends side by side
  ) 
mapc