# script to relate LSWT rate of change to rate of change in climate variables
##################################################################################
library(tidyverse)

# read LSWT output
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts.csv')


temp <- read.csv('./data/average-annual-temperature-trends-1972-2022.csv')
temp <- temp %>% 
  #separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(district = site) %>% 
  rename(slope_temp = slope_decade) %>% 
  group_by(district) %>% 
  mutate(slope_temp = mean(slope_temp)) %>% 
  distinct(district, .keep_all = TRUE)

rain <- read.csv('./data/annual-maximum-one-day-rainfall-trends-1960-2022.csv') %>% 
  #separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(district = site) %>% 
  rename(slope_rain = slope_decade) %>% 
  group_by(district) %>% 
  mutate(slope_rain = mean(slope_rain)) %>% 
  distinct(district, .keep_all = TRUE)

drought_freq <- read.csv('./data/meteorological-drought-frequency-trends-1972-2022.csv') %>% 
  separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) 


df <- left_join(sen, temp, by = 'district')
df <- left_join(df, rain, by = 'district')

df <- df %>% 
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

df$region <- factor(df$region, levels = c('Northland',
                                          'Auckland', 'Waikato',
                                          'Bay of Plenty',
                                          'Gisborne', "Hawke's Bay", 
                                          'Manawatu-Whanganui', 'Taranaki', 
                                          'Wellington',
                                          'Nelson', "Marlborough",
                                          'West Coast','Canterbury', 
                                          'Otago',
                                          'Southland'))

df$city <- factor(df$city, levels = c('Kerikeri', 'Whangarei',
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


##################################
## air temp v surface water temp
# set up color palette
n_colors <- length(levels(df$city))
colors <- colorRampPalette(brewer.pal(11, "Spectral"))(n_colors)

wtemp_atemp <- ggplot(df, aes(x = slope_temp, y = sen_slope, fill = city)) +
  geom_point() +
  geom_boxplot(position = 'identity', width = 0.01, alpha = 0.9) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  theme_bw() +
  scale_fill_manual(values = colors) +
  ylab('Rate of change in surface temp (C/year)') +
  xlab('Rate of change in air temp per decade (C)')
wtemp_atemp

# run linear model
lm_temp <- df %>% 
  nest() %>% # Nest data into a list-column
  mutate(
    mod = map(data, ~ lm(sen_slope ~ slope_temp, data = .x)), # Fit models
    p_value = map_dbl(mod, ~ summary(.x)$coefficients[2, 4]), # Extract p-values
    adj_r_squared = map_dbl(mod, ~ summary(.x)$adj.r.squared) # Extract adjusted R-squared
  ) %>%
  select(p_value, adj_r_squared) %>% # Keep only desired columns
  distinct(p_value, .keep_all = TRUE) # Remove duplicates based on `p_value`
lm_temp  

##################################
## rain v surface water temp
wtemp_rain <- ggplot(df, aes(x = (slope_rain), y = sen_slope, fill = city)) +
  geom_point() +
  geom_boxplot(position = 'identity', width = 0.3, alpha = 0.9) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  theme_bw() +
  scale_fill_manual(values = colors) +
  ylab('Rate of change in surface temp') +
  xlab('Rate of change in precip per decade (mm)') +
  theme(legend.position = 'none')
wtemp_rain 

lm_rain <- df %>% 
  nest() %>% # Nest data into a list-column
  mutate(
    mod = map(data, ~ lm(sen_slope ~ slope_rain, data = .x)), # Fit models
    p_value = map_dbl(mod, ~ summary(.x)$coefficients[2, 4]), # Extract p-values
    adj_r_squared = map_dbl(mod, ~ summary(.x)$adj.r.squared) # Extract adjusted R-squared
  ) %>%
  select(p_value, adj_r_squared) %>% # Keep only desired columns
  distinct(p_value, .keep_all = TRUE) # Remove duplicates based on `p_value`
lm_rain

ggarrange(wtemp_atemp, wtemp_rain, common.legend = TRUE)

################################################
## add in wind
wnd <- read.csv('./data/extreme-wind-data-to-2022/extreme-wind-trends-1980-2022.csv') %>% 
  filter(method=="Sen's slope") %>% 
  select(site, statistic, slope) %>% 
  rename(wind_slope = slope,
         city = site)

sen <- read.csv('./data/output/sen_slope_LWST_annual_mean_17_sites.csv')

b <- left_join(wnd, df) 

b %>% 
  filter(statistic=='Highest max gust') %>% 
  ggplot(aes(x = (wind_slope), y = sen_slope, fill = city)) +
  geom_point() +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_boxplot(aes(group = city), alpha = 0.7) +
  scale_fill_manual(values = colors_wnd$color) +
  theme_bw() +
  ylab('Rate of change in surface temp') +
  xlab('Rate of change in wind per decade (mm)') +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))


wind_wtemp <- b %>% 
  filter(statistic=='Highest max gust') %>% 
  ggplot(aes(x = (wind_slope), y = sen_slope, fill = city)) +
  geom_point() +
  geom_boxplot(aes(group = factor(city))) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  scale_fill_manual(values = colors_wnd$color) +
  theme_bw() +
  ylab('Rate of change in surface temp') +
  xlab('Rate of change in wind per decade (mm)') +
  theme(
        axis.text.x = element_text(angle = 45, hjust = 1))

wind_wtemp
ggplotly(wind_wtemp)
climvars <- ggarrange(wtemp_atemp, wtemp_rain, wind_wtemp, common.legend = TRUE,
          labels = 'auto', nrow = 1)
climvars
ggsave('./figures/wtemp_vs_clim_vars.png', climvars, 
       dpi = 300, units = 'mm', height = 400, width = 800, scale = 0.45)

# run linear model for both statistics
avg <- b %>% 
  filter(statistic=='Average max gust')
mod1 <- lm(avg$sen_slope ~ avg$wind_slope, data = avg)
summary(mod1)

maxg <- b %>% 
  filter(statistic=='Highest max gust')
mod2 <- lm(maxg$sen_slope ~ maxg$wind_slope, data = maxg)
summary(mod2)
