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

table(gps$spatial_extent)
table(gps$location)
table(gps$method_LSWT)
table(gps$method_LSWT_standard)
table(gps$method_trend)
table(gps$aggregation_mean_min_max_etc)
table(gps$temporal_aggregation)
max(gps$n_lakes)
median(gps$n_lakes)
median(gps$n_years)
min(gps$n_years)
max(gps$n_years)
min(gps$rate_C_year)
max(gps$rate_C_year)

## night time estimates
night <- gps %>% 
  filter(str_detect(annual_seasonal_data, "night")) %>% 
  distinct(citation)
nrow(night)

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

gps_notglobal <- gps %>% 
  filter(spatial_extent!='global')
global <- gps %>% 
  filter(spatial_extent=='global',
         method_LSWT!='MOD11L1', # this study has two entries, remove the one using linear regression but keep sen's slope
         !(citation_short == "Wang et al. 2024" & temporal_aggregation == "summer")) # remove the summer value for the same study

p_notglobal <- ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  theme_bw() +
  geom_jitter(data = gps_notglobal, shape = 21, color = 'black',
              aes(x = long, y = lat, fill = rate_C_year, size = n_lakes))  +
  scale_fill_gradient2(
    low = "steelblue",       # for cooling
    mid = "#D3D3D3",      # neutral
    high = "firebrick",       # for warming
    midpoint = 0) +
  scale_size_continuous(
    breaks = c(10, 100, 500, 1000),
    range  = c(1, 10)) + # optional: controls min/max point size in the plot
  labs(fill = 'Rate (°C/year)',
       size = 'Number of lakes') +
  guides(fill = guide_colorbar(direction = "vertical"),
         size = guide_legend(direction = "vertical")) +
  theme(legend.position = "right",       # Keep legends on the right
        legend.box = "horizontal",
        legend.box.just = 'center')        # Arrange separate legends side by side
p_notglobal  

p_notglobal <- p_notglobal + 
  geom_point(data = global[global$n_lakes < 2000,], 
             aes(x = dummy_long, y = dummy_lat,
             color = rate_C_year, size = n_lakes, alpha = 0.3)) +
  guides(alpha = 'none',
         color = 'none') +
  scale_color_gradient2(
    low = "steelblue",       # for cooling
    mid = "#D3D3D3",      # neutral
    high = "firebrick",       # for warming
    midpoint = 0) 
p_notglobal  

p_global <- ggplot() +
  geom_point(data = global[global$n_lakes < 2000,], 
             aes(x = dummy_long, y = dummy_lat,
                 fill = rate_C_year, size = n_lakes)) +
  scale_fill_gradient2(
    low = "steelblue",       # for cooling
    mid = "#D3D3D3",      # neutral
    high = "firebrick",       # for warming
    midpoint = 0) +
  theme_bw() +
  ylab( 'Rate (°C/year)') +
  xlab('Study citation') +
  ggtitle('Global studies') +
#  guides(size = 'none') +
  theme(legend.position = "right",       # Keep legends on the right
        legend.box = "horizontal",
        axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 12))        
p_global

library(ggbreak)
p_global <- ggplot() +
  geom_point(data = global, shape = 21, color = 'black',
             aes(x = citation_short, y = n_lakes, 
                 fill = rate_C_year, size = 1))  +
  scale_y_break(c(1500, 92000),
                scales = c(1, 7)) + # break between 1260 and 90000
  scale_fill_gradient2(
    low = "steelblue",       # for cooling
    mid = "#D3D3D3",      # neutral
    high = "firebrick",       # for warming
    midpoint = 0) +
  theme_bw() +
  coord_cartesian(clip = "off", ylim = c(min(global$n_lakes) * 0.95, max(global$n_lakes) * 1.05)) +
  ylab( 'Number of lakes') +
  xlab('') +
  ggtitle('Global studies') +
  guides(size = 'none',
         fill = 'none') +
  labs(fill = 'Rate (°C/year)') +
  theme(legend.position = "right",       # Keep legends on the right
        legend.box = "horizontal",
        #axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size = 12),
        plot.margin = unit(c(2, 1, 2, 1), "cm"),
        axis.text.x = element_blank()) 

p_global
ggsave('./figures/landsat_7/global_studies_rates.png', p_global, 
        dpi = 300, units = 'mm', height = 200, width = 250, scale = 0.5,
       bg = 'transparent', type = 'cairo')

summaries <- gps %>% 
  distinct(citation, .keep_all = TRUE)
summaries

lakes <- summaries %>% 
  filter(n_lakes < 25000) %>% 
  ggplot(aes(x = n_lakes, fill = method_LSWT_standard)) +
  geom_histogram() +
  scale_fill_manual(values =  c(
    "#E69F00",  # orange
    "#009E73", # bluish green
    "#0072B2",  # blue
    "#D55E00", # reddish orange
    "#56B4E9" # sky blue
  )) +
  theme_bw() +
  xlab('Study size (n lakes)') +
  ylab('Frequency')+
  labs(fill = 'Method of measurement')
lakes

years <- summaries %>% 
  ggplot(aes(x = n_years, fill = method_LSWT_standard)) +
  geom_histogram() +
  scale_fill_manual(values =  c(
    "#E69F00",  # orange
    "#009E73", # bluish green
    "#0072B2",  # blue
    "#D55E00", # reddish orange
    "#56B4E9" # sky blue
  )) +
  theme_bw() +
  xlab('Study duration (n years)') +
  ylab('Frequency')+
  labs(fill = 'Method of measurement')
years

method <- summaries %>% 
  ggplot(aes(x = method_LSWT_standard, fill = spatial_extent)) +
  geom_bar() +
  theme_bw() +
  scale_fill_manual(values =  c(
    "#E69F00",  # orange
    "#009E73", # bluish green
    "#0072B2",  # blue
    "#D55E00", # reddish orange
    "#56B4E9" # sky blue
  )) +
  theme(legend.position = 'top',
        legend.key.size = unit(0.2, "cm")) +
  labs(fill = 'Spatial Extent') +
  xlab('Study method (type)')+
  ylab('Frequency') 
method

spatial <- summaries %>% 
  ggplot(aes(x = spatial_extent, fill = method_LSWT_standard)) +
  geom_bar() +
  scale_fill_manual(values =  c(
    "#E69F00",  # orange
    "#009E73", # bluish green
    "#0072B2",  # blue
    "#D55E00", # reddish orange
    "#56B4E9" # sky blue
  )) +
  theme_bw() +
  xlab('Study spatial extent') +
  ylab('Frequency') +
  labs(fill = 'Method of measurement')
spatial

histos <- ggarrange(lakes, years, spatial, 
                    nrow = 1,
                    common.legend = TRUE, legend= 'bottom',
                    labels = c('b', 'c', 'd'))
histos
description_plot <- ggarrange(p_notglobal, histos, nrow = 2,
                              labels = c('a', '', '', ''))
description_plot
ggsave('./figures/landsat_7/map_histograms_lit_review.png', description_plot, 
       dpi = 300, units = 'mm', height = 400, width = 650, scale = 0.3)

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

time <- gps %>% 
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
time
ggsave('./figures/landsat_7/timeframe_duration_lit_review.png', time, 
       dpi = 300, units = 'mm', height = 400, width = 650, scale = 0.5)

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
