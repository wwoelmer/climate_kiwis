# estimate warming trends by season
library(broom)
library(tidyverse)
#install.packages('trend')
library(trend)
library(ggridges)
library(ggpubr)
library(scales)
library(RColorBrewer)

# lake surface water temp data
rstemp <- readRDS('./data/lernzmp_lakes_obs_rs.rds')

# match with StatsNZ sites
district <- read.csv('./data/lake_districts.csv')
data <- left_join(rstemp, district, by = 'lake_id')

# clean up the dataframe a bit
data <- data %>% 
  rename(surface_temp = value) %>% 
  select(-c(depth_from, depth_to, var_aeme))

# calculate the rate of change by region
data <- data %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) 

### do some filtering of lakes with very few obs
data <- data %>% 
  group_by(LID) %>% 
  mutate(temp_ID = row_number(),
         n = n(),
         time_since_previous_obs = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
         lake_max_gap = ifelse(n > 1, max(time_since_previous_obs, na.rm = TRUE), NA)) 

data <- data %>% 
  group_by(LID) %>% 
  mutate(n_lakes = n())

ggplot(data, aes(x = n_lakes, y = district, fill = district)) +
  geom_col() +
  ggtitle('All lakes') +
  theme(legend.position = 'none')

data_sub <- data %>% 
  filter(n > 10) %>% 
  filter(lake_max_gap <=365) 


# categorize dates into seasons
seasons <- data.frame(season = c('spring', 'summer', 'autumn', 'winter'),
                      month_start = c('September', 'December', 'March', 'June'))

data_sub <- data_sub %>% 
  mutate(season = ifelse(month(Date) %in% c(12, 1, 2), 'summer', NA),
         season = ifelse(month(Date) %in% c(3, 4, 5), 'autumn', season),
         season = ifelse(month(Date) %in% c(6, 7, 8), 'winter', season),
         season = ifelse(month(Date) %in% c(9, 10, 11), 'spring', season))

# set season as ordered factor
data_sub$season <- factor(data_sub$season, levels = c('spring', 'summer',
                                                      'autumn', 'winter'))

# take annual mean by season
data_sub <- data_sub %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, district, season) %>% 
  summarise(mean_temp = mean(surface_temp, na.rm = TRUE))


ggplot(data_sub, aes(x = as.Date(year), y = mean_temp, color = season)) +
  geom_point() +
  facet_wrap(~season) +
  geom_smooth(method = 'lm')


## filter the data further to get rid of lakes where there is only one observation in a given season
data_sub <- data_sub %>% 
  group_by(season, district, LID) %>% 
  mutate(n_obs_season_district_LID = n()) %>% 
  filter(n_obs_season_district_LID > 5)

# subset to before 2022 since most lakes don't have 2022 data
data_sub <- data_sub %>% 
  filter(year < 2022)

# calculate the rate of change in temperature
sen <- data_sub %>% 
  group_by(district, LID, season) %>% 
  summarise(sen_slope = sens.slope(mean_temp)$estimates,
            sen_signif = sens.slope(mean_temp)$p.value)
sen

sen <- sen %>% 
  group_by(district) %>% 
  mutate(n_lakes= length(unique(LID)))

ggplot(sen, aes(x = sen_slope, y = district, fill = n_lakes)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  facet_wrap(~season) +
  geom_vline(xintercept = 0) 

length(unique(sen$LID))

lswt_season <- ggplot(sen, aes(x = season, y = sen_slope, fill = season)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  theme_bw() +
  scale_fill_manual(values = c("#A8D08D", "#EE6C4D","#FFB84D", "#96C0B7")) +
  geom_hline(yintercept = 0, size = 1) +
  stat_compare_means(method = 'anova') +
#  stat_compare_means(comparisons = list(c("spring", "summer"), c("spring", "autumn"), c("spring", "winter"), 
#                                        c("summer", "autumn"), c("summer", "winter"), c("autumn", "winter")), 
#                     method = "t.test") +
  ylab('Rate of change in LSWT (°C/decade)') +
  xlab('Season') +
  theme(legend.position = 'none')
lswt_season

ggsave('./figures/rate_of_change_season.png', lswt_season, 
       dpi = 300, units = 'mm', height = 400, width = 450, scale = 0.3)

summ <- sen %>% 
  group_by(season) %>% 
  summarise(mean_temp_change = mean(sen_slope),
            sd_temp_change = sd(sen_slope))
summ

##################################################################################
##################################################################################
## add in geomorphic characteristics
d <- readRDS('./data/lernzmp_lakes_master.rds')
geo <- d$updated %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  select(-char)

sen_geo <- left_join(sen, geo, by = 'LID')

ggplot(sen_geo, aes(x = easting_NZTM, y = northing_NZTM, color = sen_slope)) + 
  geom_point() +
  theme_bw() +
  scale_color_gradientn(colors = c('red', 'darkgoldenrod1', 'darkblue')) +
  facet_wrap(~season)


summ_district <- sen %>% 
  group_by(season, district) %>% 
  summarise(mean_temp_change = mean(sen_slope))
summ_district

ggplot(sen_geo, aes(x = season, y = sen_slope, fill = district)) +
  geom_boxplot() +
  theme(legend.position = 'none')

ggplot(sen_geo, aes(x = season, y = sen_slope, fill = season)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  #stat_compare_means(aes(group = as.factor(season)), 
  #                   label = "p.format", color = 'red',
                    # label.y.npc = 0.95, label.x.npc = 0.5,
  #                   label.y = c(1.5, 1.4, 1.3, 1.2),
  #                   method = 'wilcox.test') +
  #stat_compare_means(method = 't.test') +
  theme(legend.position = 'none') +
  theme_bw()

pairwise.wilcox.test(sen_geo$sen_slope, sen_geo$season, method = 'BH')
