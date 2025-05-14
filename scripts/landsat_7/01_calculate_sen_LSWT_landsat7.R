# add lat long to the remote sensing lakes
library(broom)
library(tidyverse)
library(trend)
library(ggridges)
library(ggpubr)
library(scales)
library(RColorBrewer)

d <- readRDS('./data/lernzmp_lakes_master.rds')
d2 <- d$updated 
d2 <- d2 %>% 
  dplyr::select(id_final:northing_NZTM, name_fenz, area_best, max_depth, mean_depth, GeomorphicType) %>% 
  rename(LID = id_final)

d2 <- d2 %>% 
  separate(LID, into = c("char", "LID"), sep = " ") %>% 
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

data <- left_join(rstemp, d2, by = 'LID')

# match with StatsNZ sites
district <- read.csv('./data/lake_districts.csv') %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  dplyr::select(-char) 

data <- left_join(data, district, by = 'LID')

data <- data %>% 
  mutate(region = str_remove(region, " Region"))

# calculate the rate of change by region
### do some filtering of lakes with very few obs

data <- data %>% 
  group_by(LID) %>% 
  mutate(n_lakes = n())

# get rid of the lake which does not belong to a region?
data <- data %>% 
  filter(!is.na(district))

ggplot(data, aes(x = n_lakes, y = district, fill = district)) +
  geom_col() +
  ggtitle('All lakes') +
  theme(legend.position = 'none')


# take annual mean
data_sub <- data %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, district) %>% 
  summarise(mean_temp = mean(hants, na.rm = TRUE))

ggplot(data_sub, aes(x = mean_temp, y = district, fill = district)) +
  geom_density_ridges() +
  theme(legend.position = 'none')

sen <- data_sub %>% 
  group_by(district, LID) %>% 
  summarise(sen_slope = sens.slope(mean_temp)$estimates,
            sen_signif = sens.slope(mean_temp)$p.value,
            median_temp = median(mean_temp),
            intercept = median_temp - (sen_slope*median_temp))
sen
mean(sen$sen_slope)


ggplot(sen, aes(x = sen_slope, y = district, fill = district)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = 'none') 

# add the number of lakes in each district
sen <- sen %>% 
  group_by(district) %>% 
  mutate(n = n_distinct(LID))

################################################################################

write.csv(sen, './data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv', row.names = FALSE)

