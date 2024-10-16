library(broom)
library(tidyverse)
#install.packages('trend')
library(trend)
library(ggridges)
library(ggpubr)
library(scales)

d <- readRDS('./data/lernzmp_lakes_master.rds')
d2 <- d$updated 
d2 <- d2 %>% 
  select(id_final:northing_NZTM, name_fenz, area_best, max_depth, mean_depth, GeomorphicType) %>% 
  rename(lake_id = id_final)

# select geomorphic characteristics
geo <- d$updated %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  select(-char)

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
  mutate(n_lakes = n())

ggplot(data, aes(x = n_lakes, y = district, fill = district)) +
  geom_col() +
  ggtitle('All lakes') +
  theme(legend.position = 'none')

data_sub <- data %>% 
  filter(n > 10) %>% 
  filter(lake_max_gap <=365) 

# standardize the dates so they are in difference from first date (in months)
data_sub <- data_sub %>% 
  group_by(LID) %>% 
  mutate(first_date = min(Date),
         date_diff = difftime(Date, first_date, units = 'days'))


# take annual mean
data_sub <- data_sub %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, district) %>% 
  summarise(mean_temp = mean(surface_temp, na.rm = TRUE))

ggplot(data_sub, aes(x = mean_temp, y = district, fill = district)) +
  geom_density_ridges() +
  theme(legend.position = 'none')

out <- plyr::ddply(data_sub, c("district", "LID"), \(x){
  model <- lm(mean_temp ~ year, data = x)
  print(model)
  tidy(model) %>% filter(term == "year") %>% pull(estimate) 
})
out


sen <- data_sub %>% 
  group_by(district, LID) %>% 
  summarise(sen_slope = sens.slope(mean_temp)$estimates,
            sen_signif = sens.slope(mean_temp)$p.value)
sen

compare <- left_join(out, sen, by = c('LID', 'district'))

ggplot(compare, aes(x = V1, y = sen_slope, color = district)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme(legend.position = 'none')

ggplot(out, aes(x = V1, y = district, fill = district)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  geom_vline(xintercept = 0) +
  theme(legend.position = 'none')

ggplot(sen, aes(x = sen_slope, y = district, fill = district)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  geom_vline(xintercept = 0) +
  theme(legend.position = 'none')

out <- out %>% 
  group_by(district) %>% 
  mutate(n_lakes= n())

sen <- sen %>% 
  group_by(district) %>% 
  mutate(n_lakes= n())

ggplot(sen, aes(x = n_lakes, y = district, fill = district)) +
  geom_col() +
  ggtitle('Only lakes with >10 obs, max gap < 365') +
  theme(legend.position = 'none')

length(unique(out$LID))
length(unique(sen$LID))

write.csv(sen, './data/output/sen_slope_LSWT_annual_mean_30_districts.csv', row.names = FALSE)

