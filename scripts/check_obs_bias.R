library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(ggpubr)
library(ggridges)

d <- readRDS('./data/lernzmp_lakes_master.rds')
d2 <- d$updated 
d2 <- d2 %>% 
  select(id_final:northing_NZTM, name_fenz, area_best, max_depth, mean_depth, GeomorphicType) %>% 
  rename(lake_id = id_final)

# select geomorphic characteristics
geo <- d$updated %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  select(-char)

rstemp <- readRDS('./data/lernzmp_lakes_obs_rs.rds')

data <- left_join(rstemp, d2, by = 'lake_id')

# match with StatsNZ sites
district <- read.csv('./data/lake_districts.csv')
data <- left_join(data, district, by = 'lake_id')

data <- data %>% 
  mutate(region = sub(".*\\((.*)\\)", "\\1", district),
         city = sub(" \\(.*\\)", "", district)) %>% 
  mutate(island = ifelse(region %in% c('Northland',
                                       'Auckland', 'Waikato',
                                       'Bay of Plenty',
                                       'Gisborne', "Hawke's Bay", 
                                       'Manawatu-Whanganui', 'Taranaki', 
                                       'Wellington'), 
                         'North', 'South')) %>% 
  arrange(region, city)


# clean up the dataframe a bit
data <- data %>% 
  rename(surface_temp = value) %>% 
  select(-c(depth_from, depth_to, var_aeme))

# calculate the rate of change by region
data <- data %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') 

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

data <- data %>% 
  filter(n > 10) %>% 
  filter(lake_max_gap <=365) 

################################################################################
# set order of region column
data$region <- factor(data$region, levels = c('Northland',
                                            'Auckland', 'Waikato',
                                            'Bay of Plenty',
                                            'Gisborne', "Hawke's Bay", 
                                            'Manawatu-Whanganui', 'Taranaki', 
                                            'Wellington',
                                            'Nelson', "Marlborough",
                                            'West Coast','Canterbury', 
                                            'Otago',
                                            'Southland'))

data$city <- factor(data$city, levels = c('Kerikeri', 'Whangarei',
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


################################################################################
# take annual mean

# standardize the dates so they are in difference from first date (in months)
data_sub <- data %>% 
  group_by(LID) %>% 
  mutate(first_date = min(Date),
         date_diff = difftime(Date, first_date, units = 'days'))

# take annual mean
data_sub <- data_sub %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, district) %>% 
  mutate(mean_temp = mean(surface_temp, na.rm = TRUE)) %>% 
  distinct(year, LID, .keep_all = TRUE)

################################################################################
# some plotting
# set up color palette
n_colors <- length(levels(data$city))
colors <- colorRampPalette(brewer.pal(11, "Spectral"))(n_colors)

data %>% 
  filter(year(Date) < 2022) %>% 
  ggplot(aes(x = n, y = fct_rev(city), color = city)) +
  geom_point() +
  scale_color_manual(values = colors) +
  xlab('number of obs per lake') +
  ylab('City')

data %>% 
  filter(year(Date) < 2022) %>% 
  ggplot(aes(x = n, y = fct_rev(city), fill = city)) +
  geom_density_ridges() +
  scale_fill_manual(values = colors) +
  xlab('number of obs per lake') +
  ylab('City')

data %>% 
  filter(year(Date) < 2022) %>% 
  ggplot(aes(x = as.Date(Date), y = surface_temp, color = as.numeric(LID))) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm')

data_sub %>% 
  filter(year < 2022) %>% 
  ggplot(aes(x = year, y = mean_temp, color = as.numeric(LID))) +
  geom_point() +
  geom_line(aes(group = LID)) +
  geom_smooth(method = 'lm')

################################################################################
# calculate sen slope
sen <- data %>% 
  group_by(LID) %>% 
  mutate(sen_slope = sens.slope(surface_temp)$estimates,
            sen_signif = sens.slope(surface_temp)$p.value)


a <- sen %>% 
  filter(year(Date) < 2022) %>% 
  ggplot(aes(x = n, y = sen_slope, color = city)) +
  geom_point() +
  geom_smooth(aes(group = 1)) +
  scale_color_manual(values = colors) +
  xlab('number of obs per lake') +
  ylab('City') 
a

sen_sub <- data_sub %>% 
  group_by(LID) %>% 
  mutate(sen_slope = sens.slope(mean_temp)$estimates,
         sen_signif = sens.slope(mean_temp)$p.value)

b <- sen_sub %>% 
  ggplot(aes(x = n, y = sen_slope, color = city)) +
  geom_point() +
  geom_smooth(aes(group = 1)) +
  scale_color_manual(values = colors) +
  xlab('number of obs per lake') +
  ylab('City')
b
ggarrange(a, b, common.legend = TRUE)
################################################################################
### randomly pick a few lakes to look at
lakes <- sample(data$LID, 5)

data %>% 
  filter(year(Date) < 2022,
         LID %in% lakes) %>% 
  ggplot(aes(x = as.Date(Date), y = surface_temp, color = as.numeric(LID))) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm')

data_sub %>% 
  filter(year < 2022,
         LID %in% lakes) %>% 
  ggplot(aes(x = year, y = mean_temp, color = as.numeric(LID))) +
  geom_point() +
  geom_line(aes(group = LID)) +
  geom_smooth(method = 'lm')


ggplot(data, aes(x = time_since_previous_obs)) +
  geom_histogram()

# how many observations were there in each month (are certain months more likely to be measured)

data <- data %>% 
  select(LID, Date, surface_temp, n:n_lakes) %>% 
  mutate(month = month(Date),
         year = year(Date)) %>% 
  group_by(month) %>% 
  mutate(n_per_month = n()) %>% 
  group_by(month, LID) %>% 
  mutate(n_per_month_lake = n()) %>% 
  group_by(month, year, LID) %>% 
  mutate(n_per_month_year_lake = n()) %>% 
  ungroup() %>% 
  mutate(season = ifelse(month %in% c(12, 1, 2), 'summer', NA),
         season = ifelse(month %in% c(3, 4, 5), 'fall', season),
         season = ifelse(month %in% c(6, 7, 8), 'winter', season),
         season = ifelse(month %in% c(9, 10, 11), 'spring', season)) %>% 
  group_by(year, LID) %>% 
  mutate(n_per_year = n())

ggplot(data, aes(x = season)) +
  geom_histogram(stat = 'count')

ggplot(data, aes(x = surface_temp)) +
  geom_histogram() +
  facet_wrap(~month)

ggplot(data, aes(x = as.factor(month), y = n_per_month)) +
  geom_point(size = 2) +
  theme_bw()

ggplot(data, aes(x = as.factor(month), y = n_per_month_lake, color = as.numeric(LID))) +
  geom_jitter(size = 2) +
  theme_bw()

ggplot(data, aes(x = as.Date(Date, format = "%Y-%m"), y = n_per_month_year_lake)) +
  geom_point()

#################################################################################
# take seasonal mean

data$season <- factor(data$season, levels = c('winter', 'spring', 'summer', 'fall'))

data_season <- data %>% 
  group_by(LID, year, season) %>% 
  summarise(mean_temp = mean(surface_temp, na.rm = TRUE))

# filter out lakes that do not have obs in all four seasons in a given year
data_season <- data_season %>% 
  group_by(LID, year) %>% 
  filter(n_distinct(season)==4)

data_season_mean <- data_season %>% 
  group_by(year, LID) %>% 
  summarise(mean_ann_temp = mean(mean_temp))

ggplot(data_season_mean, aes(x = year, y = mean_ann_temp, color = as.numeric(LID))) +
  geom_point() +
  geom_smooth(method = 'lm')

# filter out lakes with only one obs
data_season_mean <- data_season_mean %>% 
  group_by(LID) %>% 
  filter(n() > 8)

sen <- data_season_mean %>% 
  group_by(LID) %>% 
  summarise(sen_slope = sens.slope(mean_ann_temp)$estimates,
            sen_signif = sens.slope(mean_ann_temp)$p.value)
sen


lid1 <- data %>% 
  filter(LID==1)

lid1 <- ts(lid1, start)

sen_sea <- data %>% 
  group_by(LID) %>%
  do({
    ts_data <- ts(.$surface_temp, start = c(2000, 1), frequency = 1)  # Assume yearly data
    sen_slope_result <- sea.sens.slope(ts_data)
    data.frame(LID = unique(.$LID), slope = sen_slope_result$slope)  # Store result
  })
