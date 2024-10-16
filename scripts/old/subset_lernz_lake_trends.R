library(tidyverse)
#install.packages('Kendall')
library(Kendall)
data <- readRDS('./data/lernzmp_lakes_obs.rds')

data <- data %>% 
  filter(var_aeme=='PHY_tchla') %>% 
  select(datetime, lake, lake_id, var_aeme, units, depth_from, value) %>% 
  mutate(decade = floor(year(as.Date(datetime)) / 10) * 10) %>% 
  filter(decade < 2020) %>% 
  mutate(year = year(datetime)) %>% 
  filter(depth_from < 2) %>% 
  filter(year >= 2000) %>%  # Filter data from the year 2000 onwards
  group_by(lake, year) %>%  # Group by lake and year
  mutate(count = n()) %>%  # Count the number of observations per group
  filter(count >= 12) %>% 
  filter(value < 5000)
 
# some lakes have multiple entries per day, let's take a daily average
data <- data %>% 
  mutate(date = as.Date(datetime)) %>% 
  group_by(lake, date) %>% 
  mutate(daily_chl = mean(value)) %>% 
  distinct(lake, date, .keep_all = TRUE)

data <- data %>% 
  select(date, lake, daily_chl, depth_from, decade, year) %>% 
  arrange(lake, date) %>% 
  group_by(lake) %>% 
  mutate(time_since_previous_obs = as.numeric(difftime(as.Date(date), lag(as.Date(date)), units = "days")))   # Calculate days between observations

# filter out any lakes that have a gap longer than...a year?
datat <- data %>% 
  group_by(lake) %>% 
  mutate(flag = ifelse(any(time_since_previous_obs > 150, na.rm = TRUE), 1, 0)) %>% 
  filter(flag==0)

# what is the minimum year that all lakes have obs
summ <- datat %>% 
  group_by(lake) %>% 
  mutate(min_year = min(year)) %>% 
  group_by(min_year) %>% 
  summarise(n_lakes_in_year = length(unique(lake)))

datat <- datat %>% 
  filter(year > 2003) %>% 
  mutate(min_year = min(year)) 

ggplot(datat, aes(x = as.Date(date), y = daily_chl, color = as.factor(lake))) +
  geom_line() +
  theme(legend.position = 'none')

ggplot(datat, aes(x = as.Date(date), y = time_since_previous_obs, color = lake)) +
  geom_point() +
  geom_line() +
  theme(legend.position = 'none')

length(unique(datat$lake))

datat %>% 
  filter(lake=='Wairewa (Lake Forsyth)') %>% 
  ggplot(aes(x = as.Date(date), y = daily_chl, color = as.factor(lake))) +
  geom_line() +
  theme(legend.position = 'none')

datat %>% 
  filter(lake=='Wairewa (Lake Forsyth)') %>% 
  ggplot(aes(x = as.Date(date), y = time_since_previous_obs, color = lake)) +
  geom_point() +
  geom_line() +
  theme(legend.position = 'none')

## remove Lake Forsyth because it's so eutrophic? major outliers in the mid 2000's

datat <- datat %>% 
  filter(lake!='Wairewa (Lake Forsyth)')

datat <- datat %>% 
  group_by(lake) %>% 
  mutate(first_year = min(year, na.rm = TRUE)) %>% 
  filter(first_year==2004)

ggplot(datat, aes(x = as.Date(date), y = daily_chl, color = lake)) +
  geom_point() +
  geom_line() +
  theme(legend.position = 'none')

lakes <- datat %>% 
  distinct(lake, year) 

####################################################################################
## look at patterns in the distribution
data_yearly <- datat %>% 
  filter(daily_chl < 1000) %>% 
  group_by(year) %>% 
  summarise(across(daily_chl, list(mean =  ~ mean(.x, na.rm = TRUE),
                               median =  ~ median(.x, na.rm = TRUE),
                               sd =  ~ sd(.x, na.rm = TRUE), 
                               IQR =  ~ IQR(.x, na.rm = TRUE),
                               max =  ~ max(.x, na.rm = TRUE),
                               min =  ~ min(.x, na.rm = TRUE),
                               n_lakes = ~length(unique(lake)))))

yearly_long <- data_yearly %>% 
  pivot_longer(daily_chl_mean:daily_chl_n_lakes, names_to = 'summary_stat', values_to = 'value') %>% 
  mutate(summary_stat = sub("^value_", "", summary_stat))

yearly_long %>% 
  ggplot(aes(x = year, y = value, color = summary_stat)) +
  geom_point() +
  geom_line() +
  geom_smooth(aes(x = year, y = value), method = 'lm') +
  facet_wrap(~summary_stat, scales = 'free') +
  ylab('Chl-a concentration') +
  theme(legend.position = 'none')


####################################################################################
## look at patterns in the distribution broken down by month and then calculate a trends?
data_monthly <- datat %>% 
  filter(daily_chl < 1000) %>% 
  mutate(month_year = floor_date(date, unit = 'month')) %>% 
  group_by(month_year) %>% 
  summarise(across(daily_chl, list(mean =  ~ mean(.x, na.rm = TRUE),
                                   median =  ~ median(.x, na.rm = TRUE),
                                   sd =  ~ sd(.x, na.rm = TRUE), 
                                   IQR =  ~ IQR(.x, na.rm = TRUE),
                                   max =  ~ max(.x, na.rm = TRUE),
                                   min =  ~ min(.x, na.rm = TRUE),
                                   n_lakes = ~length(unique(lake)))))

monthly_long <- data_monthly %>% 
  pivot_longer(daily_chl_mean:daily_chl_n_lakes, names_to = 'summary_stat', values_to = 'value') %>% 
  mutate(summary_stat = sub("^value_", "", summary_stat)) 

trend <- monthly_long %>% 
  group_by(summary_stat) %>% 
  summarise(k_trend = Kendall(month_year, value)$tau,
            k_pvalue = Kendall(month_year, value)$sl)
trend$k_pvalue
  
p <- Kendall(monthly_long$month_year, monthly_long$value)

monthly_long %>% 
  ggplot(aes(x = as.Date(month_year), y = value, color = summary_stat)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm') +
  facet_wrap(~summary_stat, scales = 'free') +
  ylab('Chl-a concentration') +
  theme(legend.position = 'none')


####################################################################################
## look at patterns in the distribution broken down by lake

data_yearly_lake <- datat %>% 
  group_by(year, lake) %>% 
  summarise(across(daily_chl, list(mean =  ~ mean(.x, na.rm = TRUE),
                                   median =  ~ median(.x, na.rm = TRUE),
                                   sd =  ~ sd(.x, na.rm = TRUE), 
                                   IQR =  ~ IQR(.x, na.rm = TRUE),
                                   max =  ~ max(.x, na.rm = TRUE),
                                   min =  ~ min(.x, na.rm = TRUE))))

yearly_lake_long <- data_yearly_lake %>% 
  pivot_longer(daily_chl_mean:daily_chl_min, names_to = 'summary_stat', values_to = 'value') %>% 
  mutate(summary_stat = sub("^value_", "", summary_stat))

yearly_lake_long %>% 
  ggplot(aes(x = year, y = value, color = lake)) +
  geom_point() +
  geom_line() +
  geom_smooth(aes(x = year, y = value)) +
  facet_wrap(~summary_stat, scales = 'free') +
  ylab('Chl-a concentration') +
  theme(legend.position = 'none')


data_yearly_lake <- datat %>% 
  group_by(lake, year) %>% 
  mutate(standard_chl = daily_chl/mean(daily_chl, na.rm = TRUE)) %>% 
  group_by(year, lake) %>% 
  summarise(across(standard_chl, list(mean =  ~ mean(.x, na.rm = TRUE),
                                   median =  ~ median(.x, na.rm = TRUE),
                                   sd =  ~ sd(.x, na.rm = TRUE), 
                                   IQR =  ~ IQR(.x, na.rm = TRUE),
                                   max =  ~ max(.x, na.rm = TRUE),
                                   min =  ~ min(.x, na.rm = TRUE))))

yearly_lake_long <- data_yearly_lake %>% 
  pivot_longer(standard_chl_mean:standard_chl_min, names_to = 'summary_stat', values_to = 'value') %>% 
  mutate(summary_stat = sub("^value_", "", summary_stat))

yearly_lake_long %>% 
  ggplot(aes(x = year, y = value, color = lake)) +
  geom_point() +
  geom_line() +
  geom_smooth(aes(x = year, y = value)) +
  facet_wrap(~summary_stat, scales = 'free') +
  ylab('Chl-a concentration') +
  theme(legend.position = 'none')

