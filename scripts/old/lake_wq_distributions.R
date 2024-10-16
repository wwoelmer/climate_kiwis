data <- read.csv('./data/BoP_WQ_formatted.csv')

data_long <- data %>% 
  pivot_longer(chla_mgm3_top:chla_mgm3_bottom, names_to = 'variable', values_to = 'value') %>% 
  mutate(decade = floor(year(as.Date(date)) / 10) * 10) %>% 
  filter(decade < 2020) %>% 
  mutate(year = year(date))

ggplot(data_long, aes(x = value, color = paste0(lake, site), fill = paste0(lake, site))) +
  geom_histogram() +
  geom_density() +
  facet_wrap(variable ~ decade, scales = 'free')

ggplot(data_long, aes(x = value, fill = as.factor(decade))) +
  geom_density(alpha = 0.2) +
  facet_wrap(~variable, scales = 'free')

data_summary <- data_long %>% 
  group_by(decade, variable) %>% 
  summarise(across(value, list(mean =  ~ mean(.x, na.rm = TRUE),
                               median =  ~ median(.x, na.rm = TRUE),
                               sd =  ~ sd(.x, na.rm = TRUE), 
                               IQR =  ~ IQR(.x, na.rm = TRUE),
                               max =  ~ max(.x, na.rm = TRUE),
                               min =  ~ min(.x, na.rm = TRUE))))

data_summary_long <- data_summary %>% 
  pivot_longer(value_mean:value_min, names_to = 'summary_stat', values_to = 'value') %>% 
  mutate(summary_stat = sub("^value_", "", summary_stat),)


ggplot(data_summary_long, aes(x = summary_stat, y = value, fill = as.factor(decade))) +
  geom_col(position = 'dodge') +
  facet_wrap(~variable, scales = 'free') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######
# time series by year
data_yearly <- data_long %>% 
  group_by(year, variable) %>% 
  summarise(across(value, list(mean =  ~ mean(.x, na.rm = TRUE),
                               median =  ~ median(.x, na.rm = TRUE),
                               sd =  ~ sd(.x, na.rm = TRUE), 
                               IQR =  ~ IQR(.x, na.rm = TRUE),
                               max =  ~ max(.x, na.rm = TRUE),
                               min =  ~ min(.x, na.rm = TRUE),
                               n = ~n())))

yearly_long <- data_yearly %>% 
  pivot_longer(value_mean:value_min, names_to = 'summary_stat', values_to = 'value') %>% 
  mutate(summary_stat = sub("^value_", "", summary_stat))

yearly_long %>% 
  filter(variable=='secchi_m') %>% 
ggplot(aes(x = year, y = value, color = summary_stat)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  facet_wrap(~summary_stat) 

yearly_long %>% 
  filter(variable=='chla_mgm3_top') %>% 
  ggplot(aes(x = year, y = value, color = summary_stat)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  facet_wrap(~summary_stat, scales = 'free') 

data_long %>% 
  filter(variable=='chla_mgm3_top') %>% 
  ggplot(aes(x = year, y = value, color = lake)) +
  geom_point() +
  geom_line() +
  geom_smooth() +
  facet_wrap(~lake, scales = 'free')

