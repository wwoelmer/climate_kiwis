# make map of studies from lit review
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggpubr)
library(ggExtra)


# download lit review from Zenodo
#download.file('https://zenodo.org/records/19601000/files/LSWT_rates_of_change_literature_pub_April2026.csv?download=1',
#              './data/LSWT_rates_of_change_literature.csv')

gps <- read.csv('./data/LSWT_rates_of_change_literature_pub_april2026.csv',
                fileEncoding = 'latin1')
gps <- gps %>% 
  rename(rate_C_year = ï..rate_C_year)

gps$rate_C_year <- as.numeric(gps$rate_C_year)
gps$n_lakes <- as.numeric(gps$n_lakes)

# number of unique studies and rates per study
gps %>%
  summarise(n_studies = n_distinct(citation),
            n_rates   = n())   # total rows = total rates
  

# change the spatial extent so it's either point, region, or global
gps <- gps %>% 
  mutate(spatial_extent = dplyr::recode(spatial_extent,
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
         annual_seasonal_category!='?')

# get world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# set projection of points and extract from geometry into new x/y
gps_transf <- st_as_sf(gps,
                             coords = c("long", "lat"),
                             crs = 4326) %>% 
                    st_transform(crs = "+proj=moll")
  

# filter to only unique citations
gps_nodups <- gps %>% 
  distinct(citation, .keep_all = TRUE)

p2 <- ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  geom_point(data = gps_nodups[gps_nodups$n_lakes < 5000,],
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
       size = 'Number of lakes',
       y = 'Latitude',
       x = 'Longitude') +
  guides(fill = 'none') +
  theme_bw() +
  theme(legend.position = "left") 
p2

map_bars <- ggMarginal(p2, 
                       type = "histogram", 
                       margins = "both", 
                       size = 12, 
                       fill = "gray", 
                       color = "black") 
map_bars
ggsave('./figures/landsat_7/map_with_side_bars.png', map_bars, 
       dpi = 300, units = 'mm', height = 400, width = 650, scale = 0.3)


# calculate the number of studies in the northern hemisphere
n_hemi <- gps_nodups %>% 
  mutate(hemisphere = ifelse(lat > 0, 'north', 'south')) %>% 
  summarise(pct_north = sum(hemisphere=='north')/n())

# and the number of studies in n america, europe, china
region_counts <- gps_nodups %>% 
  mutate(location = case_when(
    lat >= 5   & lat <= 85  & long >= -170 & long <= -50  ~ "North America",
    lat >= 35  & lat <= 70  & long >=  -10 & long <=  40  ~ "Europe",
    lat >= 18  & lat <= 54  & long >=   73 & long <= 135  ~ "China",
    TRUE ~ "Other"
  )) %>%
  count(location)



lakes <- gps_nodups %>% 
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
  xlab('Study size (# lakes)') +
  ylab('# of studies')+
  labs(fill = 'Method of measurement')
lakes

years <- gps_nodups %>% 
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
  xlab('Study duration (# years)') +
  ylab('# of studies')+
  labs(fill = 'Method of measurement')
years

spatial <- gps_nodups %>% 
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
description_plot <- ggarrange(p2, histos, nrow = 2,
                              labels = c('a', '', '', ''))
description_plot
ggsave('./figures/landsat_7/map_histograms_lit_review.png', description_plot, 
       dpi = 300, units = 'mm', height = 400, width = 650, scale = 0.3)

# map SI Figure map of rates over space, where rate is size
p_SI <- ggplot() +
  geom_sf(data = world, fill = 'gray', color = 'gray') +
  geom_point(data = gps_nodups[gps_nodups$n_lakes < 5000,],
             aes(x = long, y = lat,
                 fill = rate_C_year, size = rate_C_year, shape = spatial_extent_simple),
             color = "black", alpha = 0.8) +
  scale_fill_gradient2(high = '#ca0020',
                       mid = "white",         
                       low = '#00316E', 
                       midpoint = 0) + 
  scale_shape_manual(values = c(23, 21)) +
  theme_bw() +
  labs(fill = 'Rate (°C/year)',
       size = 'Rate (°C/year',
       shape = 'Spatial extent') +
  theme(legend.position = "left") 
p_SI

ggsave('./figures/landsat_7/SI_fig_lit_review_rates_map.png', p_SI, 
       dpi = 300, units = 'mm', height = 400, width = 650, scale = 0.3)


## calculate summary stats
gps_nodups %>% 
  summarise(median_lakes = median(n_lakes),
            median_years = median(n_years))

table(gps_nodups$method_LSWT_standard)

# number of lakes trends
sum(gps_nodups$n_lakes)

