# look at remotely sense temp data
library(tidyverse)

rstemp <- readRDS('./data/lernzmp_lakes_obs_rs2.rds')
length(unique(rstemp$lake_id))
# 799 lakes

rstemp <- rstemp %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  mutate(LID = LID,
         year = year(Date)) 

ggplot(rstemp, aes(x = as.Date(Date), y = value, color = as.numeric(LID))) +
  geom_point()

rstemp <- rstemp %>% 
  group_by(LID) %>% 
  mutate(temp_ID = row_number(),
         n = n(),
         temp_climatology = ifelse(n >= 10, mean(value, na.rm = TRUE), NA)) %>% 
  mutate(time_since_previous_obs = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days"))) %>%   # Calculate days between observations
  mutate(lake_max_gap = ifelse(n > 1, max(time_since_previous_obs, na.rm = TRUE), NA)) 

rstemp <- na.omit(rstemp)
length(unique(rstemp$LID))

ggplot(rstemp, aes(x = temp_climatology, color = LID)) +
  geom_histogram() +
  theme(legend.position = 'none')

ggplot(rstemp, aes(x = year, y = value, fill = year, group = year)) +
  geom_violin()

ggplot(rstemp, aes(x = lake_max_gap)) +
  geom_histogram()

ggplot(rstemp, aes(x = time_since_previous_obs)) +
  geom_histogram()

################################################################################
# calculate yearly summary statistics
data_yearly <- rstemp %>% 
  filter(year < 2022 & year > 2013) %>% 
  group_by(year) %>% 
  summarise(across(value, list(mean =  ~ mean(.x, na.rm = TRUE),
                                   median =  ~ median(.x, na.rm = TRUE),
                                   sd =  ~ sd(.x, na.rm = TRUE), 
                                   IQR =  ~ IQR(.x, na.rm = TRUE),
                                   max =  ~ max(.x, na.rm = TRUE),
                                   min =  ~ min(.x, na.rm = TRUE),
                                   n_lakes = ~length(unique(LID)))))
yearly_long <- data_yearly %>% 
  pivot_longer(value_mean:value_n_lakes, names_to = 'summary_stat', values_to = 'value') %>% 
  mutate(summary_stat = sub("^value_", "", summary_stat))

yearly_long %>% 
  ggplot(aes(x = year, y = value, color = summary_stat)) +
  geom_point() +
  geom_line() +
  geom_smooth(aes(x = year, y = value), method = 'lm') +
  facet_wrap(~summary_stat, scales = 'free') +
  ylab('Chl-a concentration') +
  theme(legend.position = 'none')


