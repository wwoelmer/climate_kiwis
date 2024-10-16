# add lat long to the remote sensing lakes
library(broom)
library(tidyverse)

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

ggplot(data, aes(x = n_lakes, y = region, fill = region)) +
  geom_col() +
  ggtitle('All lakes')

data_sub <- data %>% 
  filter(n > 10) %>% 
  filter(lake_max_gap <=365)

# standardize the dates so they are in difference from first date (in months)
data_sub <- data_sub %>% 
  group_by(LID) %>% 
  mutate(first_date = min(Date),
         date_diff = difftime(Date, first_date, units = 'days'))



library(ggridges)
ggplot(data_sub, aes(x = surface_temp, y = region, fill = region)) +
  geom_density_ridges()

out <- plyr::ddply(data_sub, c("region", "LID"), \(x){
  model <- lm(surface_temp ~ date_diff, data = x)
  print(model)
  tidy(model) %>% filter(term == "date_diff") %>% pull(estimate) 
})
out

ggplot(out, aes(x = V1, y = region, fill = region)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  geom_vline(xintercept = 0)

out <- out %>% 
  group_by(region) %>% 
  mutate(n_lakes= n())


ggplot(out, aes(x = n_lakes, y = region, fill = region)) +
  geom_col() +
  ggtitle('Only lakes with >10 obs, max gap < 365')

length(unique(out$LID))

##################################################################################
 
clim <- read.csv('./data/average-annual-temperature-trends-1972-2022.csv')
clim <- clim %>% 
  separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(slope_temp = slope_decade) %>% 
  group_by(region) %>% 
  mutate(slope_temp = mean(slope_temp)) %>% 
  distinct(region, .keep_all = TRUE)

rain <- read.csv('./data/annual-maximum-one-day-rainfall-trends-1960-2022.csv') %>% 
  separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(slope_rain = slope_decade) %>%
  group_by(region) %>% 
  mutate(slope_rain = mean(slope_rain)) %>% 
  distinct(region, .keep_all = TRUE)

drought_freq <- read.csv('./data/meteorological-drought-frequency-trends-1972-2022.csv') %>% 
  separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) 


df <- left_join(out, clim, by = 'region')
df <- left_join(df, rain, by = 'region')
df <- left_join(df, geo, by = 'LID')

# get rid of gisborn because it has so few obs
df <- df %>% 
  filter(region!='Gisborne')

df %>% 
  group_by(region) %>% 
  mutate(n_lakes = n()) %>% 
  ggplot(aes(x = n_lakes, y = region)) +
  geom_col()


##################################
## air temp v surface water temp
ggplot(df, aes(x = slope_temp, y = V1, fill = region, color = region)) +
  geom_boxplot(position = 'identity', width = 0.01, alpha = 0.7) +
  geom_point() +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  theme_bw() +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Rate of change in air temp per decade (C)')

# run linear model
lm_temp <- df %>% 
  do(mod = lm(V1 ~ slope_temp, data = df)) %>% 
  summarize(p_value = summary(mod)$coefficients[2, 4],
            adj_r_squared = summary(mod)$adj.r.squared) %>% 
  distinct(p_value, .keep_all = TRUE)
lm_temp  

##################################
## rain v surface water temp
ggplot(df, aes(x = slope_rain, y = V1, fill = region, color = region)) +
  geom_boxplot(position = 'identity', width = 0.4, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_point() +
  theme_bw() +
  ylab('Rate of change in surface temp') +
  xlab('Rate of change in precip per decade (mm)')

lm_rain <- df %>% 
  do(mod = lm(V1 ~ slope_rain, data = df)) %>% 
  summarize(p_value = summary(mod)$coefficients[2, 4],
            adj_r_squared = summary(mod)$adj.r.squared) %>% 
  distinct(p_value, .keep_all = TRUE)
lm_rain

################################################################
# other descriptive variables to explain change in surface temp
maxd <- ggplot(df, aes(x = max_depth, y = V1)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ylab('Rate of change in surface temp')

meand <- ggplot(df, aes(x = mean_depth, y = V1)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()+
  ylab('Rate of change in surface temp')

sa <- ggplot(df, aes(x = area, y = V1)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()+
  ylab('Rate of change in surface temp')

ggarrange(maxd, meand, sa, common.legend = TRUE)


################################################################################
## look across geomorphic type

lm_temp_geo <- df %>%
  group_by(GeomorphicType) %>%
  do(tidy(lm(V1 ~ slope_temp, data = .))) %>%
  filter(term == "slope_temp") %>%
  select(GeomorphicType, p.value) %>%
  rename(p_value = p.value)
lm_temp_geo

ggplot(df, aes(x = slope_temp, y = V1, fill = region, color = region)) +
  geom_boxplot(position = 'identity', width = 0.01, alpha = 0.7) +
  geom_point() +
  theme_bw() +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_hline(yintercept = 0) +
  facet_wrap(~GeomorphicType) +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Rate of change in air temp per decade (C)')



lm_rain_geo <- df %>%
  group_by(GeomorphicType) %>%
  do(tidy(lm(V1 ~ slope_rain, data = .))) %>%
  filter(term == "slope_rain") %>%
  select(GeomorphicType, p.value) %>%
  rename(p_value = p.value)
lm_rain_geo 

ggplot(df, aes(x = slope_rain, y = V1, fill = region, color = region)) +
  geom_boxplot(position = 'identity', width = 0.01, alpha = 0.7) +
  geom_point() +
  theme_bw() +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_hline(yintercept = 0) +
  facet_wrap(~GeomorphicType) +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Rate of change in air temp per decade (C)')

################################################################################
## change in surf temp by east-west and north-south gradients
e_wtemp <- ggplot(df, aes(x = easting_NZTM, y = V1, color = region)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  theme_bw() +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Easting')

n_wtemp <- ggplot(df, aes(x = northing_NZTM, y = V1, color = region)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Northing')

ggarrange(e_wtemp, n_wtemp, common.legend = TRUE)

################################################################################
# change in climate across e/w n/s gradients
nt <- ggplot(df, aes(x = northing_NZTM, y = slope_temp)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Northing')

et <- ggplot(df, aes(x = easting_NZTM, y = slope_temp)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Rate of change in surface temp (C/day)') +
  xlab('Easting')

er <- ggplot(df, aes(x = easting_NZTM, y = slope_rain)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Rate of change in rain per decade') +
  xlab('Easting')

nr <- ggplot(df, aes(x = northing_NZTM, y = slope_rain)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  ylab('Rate of change in rain per decade') +
  xlab('Northing')
library(ggpubr)
ggarrange(nt, et, nr, er)

ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = slope_rain)) +
  geom_point() +
  scale_color_gradient2(low = 'orange', mid = 'grey', high = 'darkgreen') +
  theme_bw()

ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = slope_temp)) +
  geom_point() + 
  scale_color_viridis_c(option = 'plasma') +
  theme_bw()

ggplot(df, aes(x = easting_NZTM, y = northing_NZTM, color = V1)) +
  geom_point(size = 2) +
  scale_color_viridis_b() +
  theme_bw()
