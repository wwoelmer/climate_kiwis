# estimate warming trends by season
library(broom)
library(tidyverse)
#install.packages('trend')
library(trend)
library(ggridges)
library(ggpubr)
library(scales)
library(RColorBrewer)

#d <- readRDS('./data/lernzmp_lakes_master.rds')
d2 <- d$updated 
d2 <- d2 %>% 
  dplyr::select(id_final:northing_NZTM, name_fenz, area_best, max_depth, mean_depth, GeomorphicType) %>% 
  rename(lake_id = id_final)

d2 <- d2 %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  dplyr::select(-char) 

# select geomorphic characteristics
geo <- d$updated %>% 
  dplyr::select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  dplyr::select(-char)

rstemp <- read.csv('./data/landsat7_QAQC.csv')
rstemp$LID <- as.character(rstemp$LID)

data <- left_join(rstemp, d2)


# categorize dates into seasons
seasons <- data.frame(season = c('spring', 'summer', 'autumn', 'winter'),
                      month_start = c('September', 'December', 'March', 'June'))

data <- data %>% 
  mutate(season = ifelse(month(Date) %in% c(12, 1, 2), 'summer', NA),
         season = ifelse(month(Date) %in% c(3, 4, 5), 'autumn', season),
         season = ifelse(month(Date) %in% c(6, 7, 8), 'winter', season),
         season = ifelse(month(Date) %in% c(9, 10, 11), 'spring', season))

# set season as ordered factor
data$season <- factor(data$season, levels = c('spring', 'summer',
                                                      'autumn', 'winter'))

# take annual mean by season
data_sub <- data %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, season) %>% 
  summarise(mean_temp = mean(interp, na.rm = TRUE))


ggplot(data_sub, aes(x = as.Date(year), y = mean_temp, color = season)) +
  geom_point() +
  facet_wrap(~season) +
  geom_smooth(method = 'lm')


# calculate the rate of change in temperature
sen <- data_sub %>% 
  group_by(LID, season) %>% 
  summarise(sen_slope = sens.slope(mean_temp)$estimates,
            sen_signif = sens.slope(mean_temp)$p.value)
sen


ggplot(sen, aes(x = sen_slope, y = season)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  geom_vline(xintercept = 0) +
  theme_bw()

length(unique(sen$LID))

lswt_season <- ggplot(sen, aes(y = season, x = sen_slope, fill = season)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.1) +
  theme_bw() +
  scale_fill_manual(values = c("#A8D08D", "#EE6C4D","#FFB84D", "#96C0B7")) +
  geom_vline(xintercept = 0, size = 1) +
  stat_compare_means(method = 'anova') +
#  stat_compare_means(comparisons = list(c("spring", "summer"), c("spring", "autumn"), c("spring", "winter"), 
#                                        c("summer", "autumn"), c("summer", "winter"), c("autumn", "winter")), 
#                     method = "t.test") +
  xlab('Rate of change in LSWT (°C/decade)') +
  ylab('Season') +
  theme(legend.position = 'none',
        text = element_text(size = 14)) 
lswt_season

ggsave('./figures/landsat_7/rate_of_change_season.png', lswt_season, 
       dpi = 300, units = 'mm', height = 400, width = 450, scale = 0.3)

ggplot(sen, aes(y = season, x = sen_slope, fill = season)) +
  geom_density_ridges() +
  geom_jitter(alpha = 0.1) +
  theme_bw() +
  scale_fill_manual(values = c("#A8D08D", "#EE6C4D","#FFB84D", "#96C0B7")) +
  geom_vline(xintercept = 0, size = 1) +
  #stat_compare_means(method = 'anova') +
  #  stat_compare_means(comparisons = list(c("spring", "summer"), c("spring", "autumn"), c("spring", "winter"), 
  #                                        c("summer", "autumn"), c("summer", "winter"), c("autumn", "winter")), 
  #                     method = "t.test") +
  ylab('Rate of change in LSWT (°C/decade)') +
  xlab('Season') +
  theme(legend.position = 'none')

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
