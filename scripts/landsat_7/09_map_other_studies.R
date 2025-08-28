# make map of other studies
library(tidyverse)
library(aemetools)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#library(RColorBrewer)
library(scales)
#library(MetBrewer)
#library(viridis)
library(ggpubr)
library(ggExtra)

gps <- read.csv('./data/LSWT_rates_of_change_literature.csv',
                fileEncoding = 'latin1')

gps$rate_C_year <- as.numeric(gps$rate_C_year)
gps$n_lakes <- as.numeric(gps$n_lakes)

# change the spatial extent so it's either point, region, or global
gps <- gps %>% 
  mutate(spatial_extent = recode(spatial_extent,
                                 'country' = 'region'))
  
gps$spatial_extent <- factor(gps$spatial_extent, levels = c('point', 'region', 'global'))

# make a simpler spatial extent which is either global or point or regional
gps <- gps %>% 
  mutate(spatial_extent_simple = ifelse(spatial_extent=='global', 'global', 'point or region'))

gps <- gps %>% 
  filter(!is.na(rate_C_year),
         !is.na(spatial_extent),
         !is.na(method_LSWT_standard),
         !is.na(n_lakes),
         temporal_aggregation!='?')

# get world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# set the projection
world_moll <- st_transform(world, crs = "+proj=moll") # mollweide

# set projection of points and extract from geometry into new x/y
gps_transf <- st_as_sf(gps,
                             coords = c("long", "lat"),
                             crs = 4326) %>% 
                    st_transform(crs = "+proj=moll")
  
p1 <- ggplot() +
  geom_sf(data = world_moll, fill = 'gray', color = 'gray') +
  geom_sf(data = gps_transf[gps_transf$n_lakes < 5000,],
          aes(fill = method_LSWT_standard, size = n_lakes, shape = spatial_extent_simple),
              color = "black", alpha = 0.5) +
  scale_shape_manual(values = c(23, 21)) +
  scale_fill_manual(values = c("#E69F00",  
                               "#009E73",  
                               "#0072B2",  
                               "#D55E00",  
                               "#CC79A7")) +
  scale_size_continuous(breaks = c(10, 100, 500, 1000),
                        range  = c(1, 10)) +
  labs(shape = 'Spatial extent',
       size = 'Number of lakes') +
  guides(fill = 'none') +
  theme_bw() +
  theme(legend.position = "left") 

p1  

p2 <- ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  geom_point(data = gps[gps$n_lakes < 5000,],
          aes(x = long, y = lat,
              fill = method_LSWT_standard, size = n_lakes, shape = spatial_extent_simple),
          color = "black", alpha = 0.5) +
  scale_shape_manual(values = c(23, 21)) +
  scale_fill_manual(values = c("#E69F00",  
                               "#009E73",  
                               "#0072B2",  
                               "#D55E00",  
                               "#CC79A7")) +
  scale_size_continuous(breaks = c(10, 100, 500, 1000),
                        range  = c(1, 10)) +
  labs(shape = 'Spatial extent',
       size = 'Number of lakes') +
  guides(fill = 'none') +
  theme_bw() +
  theme(legend.position = "left") 
p2

map_bars <- ggMarginal(p2, 
                      # type = "histogram", 
                       margins = "both", 
                       size = 12, 
                       fill = "gray", 
                       color = "black") 
map_bars
ggsave('./figures/landsat_7/map_with_side_bars.png', map_bars, 
       dpi = 300, units = 'mm', height = 400, width = 650, scale = 0.3)


nodups <- gps %>% 
  distinct(citation, location, .keep_all = TRUE)

lakes <- nodups %>% 
  filter(n_lakes < 25000) %>% 
  ggplot(aes(x = n_lakes, fill = method_LSWT_standard)) +
  geom_histogram() +
  scale_fill_manual(values =  c(
    "#E69F00",  # orange
    "#009E73", # bluish green
    "#0072B2",  # blue
    "#D55E00", # reddish orange
    "#CC79A7" # sky blue
  )) +
  theme_bw() +
  xlab('Study size (n lakes)') +
  ylab('# of studies')+
  labs(fill = 'Method of measurement')
lakes

years <- nodups %>% 
  ggplot(aes(x = n_years, fill = method_LSWT_standard)) +
  geom_histogram() +
  scale_fill_manual(values =  c(
    "#E69F00",  # orange
    "#009E73", # bluish green
    "#0072B2",  # blue
    "#D55E00", # reddish orange
    "#CC79A7" # sky blue
  )) +
  theme_bw() +
  xlab('Study duration (n years)') +
  ylab('# of studies')+
  labs(fill = 'Method of measurement')
years

spatial <- nodups %>% 
  ggplot(aes(x = spatial_extent, fill = method_LSWT_standard)) +
  geom_bar() +
  scale_fill_manual(values =  c(
    "#E69F00",  # orange
    "#009E73", # bluish green
    "#0072B2",  # blue
    "#D55E00", # reddish orange
    "#CC79A7" # sky blue
  )) +
  theme_bw() +
  xlab('Study spatial extent') +
  ylab('# of studies') +
  labs(fill = 'Method of measurement')
spatial

histos <- ggarrange(lakes, years, spatial, 
                    nrow = 1,
                    common.legend = TRUE, legend= 'bottom',
                    labels = c('b', 'c', 'd'))
histos
description_plot <- ggarrange(p1, histos, nrow = 2,
                              labels = c('a', '', '', ''))
description_plot
ggsave('./figures/landsat_7/map_histograms_lit_review.png', description_plot, 
       dpi = 300, units = 'mm', height = 400, width = 650, scale = 0.3)

################################################################################

# split temporal aggregation into season and day/night
gps <- gps %>% 
  separate(temporal_aggregation, into = c("temporal_aggregation", "day_night"), 
           sep = " \\(|\\)", extra = "drop", fill = "right") %>% 
  mutate(day_night = ifelse(is.na(day_night), 'day', day_night))
  

gps <- gps %>% 
  group_by(temporal_aggregation) %>% 
  mutate(n_time = n(),
         facet_label = paste0(temporal_aggregation, ' (n = ', n_time, ')'))

gps$facet_label <- factor(gps$facet_label,
                          levels = 
                            c('spring (n = 10)','summer (n = 39)', 
                              'autumn (n = 11)', 'winter (n = 13)', 
                              'dry season (n = 1)', 'pre-rainy (n = 1)', 
                              'rainy (n = 1)','post-rainy (n = 1)',
                              'annual (n = 112)'))

time <- gps %>% 
  #filter(n_lakes < 25000) %>% 
  ggplot() + 
  geom_segment(aes(x = min_year, xend = max_year, y = rate_C_year, yend = rate_C_year,
                   color = day_night), size = 0.6) +
  facet_wrap(~facet_label, ncol = 4) +
  scale_color_manual(values = c('darkorange', 'black')) +
  theme_bw() +
  xlab('Duration of study') +
  ylab('Trend in LSWT (°C/year)') +
  labs(color = 'Day or night time?') +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))
time

ggsave('./figures/landsat_7/timeframe_duration_lit_review.png', time, 
       dpi = 300, units = 'mm', height = 400, width = 800, scale = 0.27)

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
