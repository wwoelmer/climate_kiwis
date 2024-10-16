# look at wind extremes

library(trend)
library(tidyverse)
library(ggridges)
library(ggpubr)
############################################################
# read in lake data
d <- readRDS('./data/lernzmp_lakes_master.rds')
d2 <- d$updated 
d2 <- d2 %>% 
  select(id_final:northing_NZTM, name_fenz, area_best, max_depth, mean_depth, GeomorphicType) %>% 
  rename(lake_id = id_final)

rstemp <- readRDS('./data/lernzmp_lakes_obs_rs.rds')

data <- left_join(rstemp, d2, by = 'lake_id')

# match with StatsNZ sites
district <- read.csv('./data/lake_districts.csv')
data <- left_join(data, district, by = 'lake_id')

data <- data %>% 
  mutate(region = str_remove(region, " Region"))

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
  mutate(n_lakes = n()) %>% 
  mutate(first_date = min(Date),
         date_diff = difftime(Date, first_date, units = 'days'))

ggplot(data, aes(x = n_lakes, y = district, fill = district)) +
  geom_col() +
  ggtitle('All lakes') +
  theme(legend.position = 'none')

data_sub <- data %>% 
  filter(n > 10) %>% 
  filter(lake_max_gap <=365) 

# split the district column into two
data_sub <- data_sub %>% 
  separate(district, into = c("site", "siteb"),  sep = " \\(", remove = FALSE) %>% 
  mutate(siteb = gsub("\\)", "", siteb)) %>% 
  select(LID, Date, surface_temp:northing_NZTM, max_depth:site, date_diff)

# take annual mean
data_sub <- data_sub %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, site) %>% 
  summarise(mean_temp = mean(surface_temp, na.rm = TRUE))

sen <- data_sub %>% 
  group_by(site, LID) %>% 
  summarise(sen_slope = sens.slope(mean_temp)$estimates,
            sen_signif = sens.slope(mean_temp)$p.value)
sen

ggplot(sen, aes(x = sen_slope, y = site, fill = site)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  geom_vline(xintercept = 0) +
  theme(legend.position = 'none')

sen <- sen %>% 
  group_by(site) %>% 
  mutate(n_lakes= n())

# filter to only include sites that are present in wind data
wnd <- read.csv('./data/extreme-wind-data-to-2022/extreme-wind-trends-1980-2022.csv') %>% 
  filter(method=="Sen's slope") %>% 
  select(site, statistic, slope)

sites <- unique(wnd$site)

sen <- sen %>% 
  filter(site %in% sites)

write.csv(sen, './data/output/sen_slope_LWST_annual_mean_17_sites.csv', row.names = FALSE)


