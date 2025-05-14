library(tidyverse)

# read in LSWT trends
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv')
sen$LID <- as.character(sen$LID)

sen <- sen %>% 
  select(LID, district, sen_slope)

# read in LERNZ master file for the lake names
lernz <- readRDS('./data/lernzmp_lakes_master.rds')
lernz <- lernz$updated %>% 
  select(-geometry)

# modify the LID col
lernz <- lernz %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  select(-char)

names <- lernz %>% 
  select(LID, name_fenz)

landsat_temp <- left_join(sen, names) %>% 
  select(LID, name_fenz, district, sen_slope) %>% 
  rename(sen_landsat = sen_slope)

# read in Ollie's insitu trends
insitu <- read.csv('./data/output/insitu_slope_estimates.csv') %>% 
  select(lake_name, name_fenz, time_range_start, time_range_end, mean_value, sen_slope, sen_signif, lm_slope) %>% 
  rename(sen_insitu = sen_slope,
         lm_insitu = lm_slope)

both_temps <- left_join(insitu, landsat_temp)
both_temps <- both_temps %>% 
  filter(!is.na(sen_landsat))

# get rid of the rotoiti and rotorua from the south island
both_temps <- both_temps %>% 
  filter(!(name_fenz=='Lake Rotorua' & district=='Blenheim (Marlborough)'),
         !(name_fenz=='Lake Rotoiti' & district=='Nelson (Nelson)'))

ggplot(both_temps, aes(x = name_fenz, y = sen_insitu, color = 'insitu')) +
  geom_point() +
  geom_point(aes(x = name_fenz, y = sen_landsat, color = 'landsat')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(both_temps, aes(x = sen_insitu, y = sen_landsat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text(aes(label = name_fenz), 
            position = position_jitter())+
            #vjust = -0.8) +
  geom_smooth(method = 'lm')  

ggplot(both_temps, aes(x = lm_insitu, y = sen_landsat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_text(aes(label = name_fenz), 
            position = position_jitter())+
  #vjust = -0.8) +
  geom_smooth(method = 'lm')  

ggplot(both_temps, aes(x = sen_insitu, y = lm_insitu)) +
  geom_point()
       