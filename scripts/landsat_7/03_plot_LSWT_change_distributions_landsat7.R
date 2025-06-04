# plot LSWT trends distributions
library(tidyverse)
library(RColorBrewer)
library(ggridges)
library(ggpubr)

# read LSWT output
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv') %>% 
  filter(!is.na(district))

##################
# ADD REGION AS COLUMN, SPLIT DISTRICT
sen <- sen %>% 
  mutate(region = sub(".*\\((.*)\\)", "\\1", district),
         city = sub(" \\(.*\\)", "", district)) %>% 
  mutate(island = ifelse(region %in% c('Northland',
                                       'Auckland', 'Waikato',
                                       'Bay of Plenty',
                                       'Gisborne', "Hawke's Bay", 
                                       'Manawatu-Whanganui', 'Taranaki', 
                                       'Wellington'), 
                         'North Island', 'South Island')) %>% 
  arrange(region, city)


sen$region <- factor(sen$region, levels = c('Northland',
                                            'Auckland', 'Waikato',
                                            'Bay of Plenty',
                                            'Gisborne', "Hawke's Bay", 
                                            'Manawatu-Whanganui', 'Taranaki', 
                                            'Wellington',
                                            'Nelson', "Marlborough",
                                            'West Coast','Canterbury', 
                                            'Otago',
                                            'Southland'))

sen$city <- factor(sen$city, levels = c('Kerikeri', 'Whangarei',
                                        'Whangaparaoa', 'Auckland',
                                        'Hamilton', 'Taupo',
                                        'Rotorua', 'Tauranga',
                                        'Gisborne', 'Napier', 
                                        'Dannevirke', 'Taumarunui',
                                        'Waiouru', 'Whanganui',
                                        'New Plymouth', 'Masterton',
                                        'Wellington', 'Nelson',
                                        'Blenheim', 'Hokitika',
                                        'Reefton', 'Christchurch',
                                        'Lake Tekapo', 'Tara Hills',
                                        'Timaru', 'Dunedin',
                                        'Queenstown', 'Gore',
                                        'Invercargill', 'Milford Sound'))


a <- ggplot(sen, aes(x = sen_slope)) +
  geom_density(size = 2) +
  xlab('Rate of change in LSWT (°C/year)') +
  ylab('Density') +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(text = element_text(size = 12))

# set up color palette
n_colors <- length(levels(sen$region))
colors <- colorRampPalette(brewer.pal(11, "Spectral"))(n_colors)

sen <- sen %>% 
  arrange(region, city) 

b <- sen %>% 
  filter(n > 3) %>% 
  ggplot(aes(x = sen_slope, y = fct_rev(region), fill = region)) +
  geom_density_ridges(scale = 2) +
  xlab('Rate of change in LSWT (°C/year)') +
  coord_cartesian(clip = 'off') +      # Allow plot to extend beyond the default area
  ylab('Region') +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.12))) +  # Add space between ridges
  scale_fill_manual(values = colors) +
  #facet_wrap(~island, scales = 'free_y') +
  geom_vline(xintercept = 0) +
  labs(fill = 'Region') +
  theme_bw() +
  theme(text = element_text(size = 12),
        legend.position = 'none')
b

lswt_rate <- ggarrange(a, b, widths = c(1, 2),
                       labels = 'auto')
lswt_rate

ggsave('./figures/landsat_7/rate_of_change_LWST.png', lswt_rate, 
       dpi = 300, units = 'mm', height = 400, width = 800, scale = 0.3)

mean_island <- sen %>% 
  group_by(island) %>% 
  summarise(mean_sen = mean(sen_slope, na.rm = TRUE),
            median_sen = median(sen_slope, na.rm = TRUE),
            max_sen = max(sen_slope, na.rm = TRUE),
            min_sen = min(sen_slope, na.rm = TRUE),
            sd_sen = sd(sen_slope, na.rm = TRUE),
            n_lakes = n())
mean_island

island_cool_warm <- sen %>% 
  group_by(island) %>% 
  summarise(pct_cool = sum(sen_slope < 0)/n(),
            pct_warm = sum(sen_slope > 0)/n())
island_cool_warm

figSI_islands <- ggplot(sen, aes(x = sen_slope, fill = island)) +
  geom_density() +
  geom_text(
    data = mean_island,
    aes(x = -0.2, y = 5, label = paste("n =", n_lakes)),
    inherit.aes = FALSE,
    vjust = -0.5,
    size = 5) +
  xlab('Rate of change in LSWT (°C/decade)') +
  coord_cartesian(clip = 'off') +      # Allow plot to extend beyond the default area
  scale_fill_manual(values = c(colors[2], colors[14])) +
  facet_wrap(~island) +
  scale_fill_manual(values = c('#2E8B57', '#5F9EA0')) +
  geom_vline(xintercept = 0) +
  labs(fill = 'Island') +
  theme_bw() +
  ylab('Density') +
  theme(text = element_text(size = 14),
        legend.position = 'none')

figSI_islands
ggsave('./figures/SI_figs/landsat_7/distribution_LSWT_change_islands.png', figSI_islands, 
       dpi = 300, units = 'mm', height = 400, width = 600, scale = 0.3)
# mean trends by region
reg_trend <- sen %>% 
  group_by(region) %>% 
  summarise(mean_sen = mean(sen_slope, na.rm = TRUE),
            median_sen = median(sen_slope, na.rm = TRUE),
            sd = sd(sen_slope, na.rm = TRUE),
            n_lakes = n())

reg_trend <- reg_trend %>% 
  mutate(mean_sen = round(mean_sen, 2),
         median_sen = round(median_sen, 2),
         sd = round(sd, 2))
reg_trend

summary_trends <- sen %>% 
  summarise(mean_sen = mean(sen_slope, na.rm = TRUE),
            median_sen = median(sen_slope, na.rm = TRUE),
            sd = sd(sen_slope, na.rm = TRUE),
            n_lakes = n(),
            pct_cool = sum(sen_slope < 0)/n(),
            pct_warm = sum(sen_slope > 0)/n(),
            range = max(sen_slope) - min(sen_slope))

summary_trends
write.csv(reg_trend, './data/output/LSWT_trend_stats_region.csv', row.names = FALSE)

#########################################
## plot for GLEON poster

# do plot b with just regions, no city
b <- sen %>% 
  filter(n > 3) %>% 
  ggplot(aes(x = sen_slope, y = fct_rev(region), fill = region)) +
  geom_density_ridges(scale = 2) +
  xlab('Rate of change in LSWT (°C/decade)') +
  coord_cartesian(clip = 'off') +      # Allow plot to extend beyond the default area
  ylab('City') +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.1))) +  # Add space between ridges
  scale_fill_manual(values = colors) +
  #facet_wrap(~island, scales = 'free_y') +
  geom_vline(xintercept = 0) +
  labs(fill = 'Region') +
  theme_bw() +
  theme(text = element_text(size = 14),
        legend.position = 'none')
b

gleon <- ggarrange(a, figSI_islands, b, widths = c(0.7, 0.9, 0.8),
                   labels = 'auto', ncol = 3)
gleon
ggsave('./figures/landsat_7/change_over_space_gleon.png', gleon, 
       dpi = 300, units = 'mm', height = 400, width = 1350, scale = 0.3)
