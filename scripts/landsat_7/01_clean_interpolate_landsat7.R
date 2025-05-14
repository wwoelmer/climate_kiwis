library(tidyverse)
library(zoo)
library(geoTS)

ls <- read.csv('./data/landsat7_data.csv')
ls <- ls %>% 
  dplyr::select(system.index, LID, Name, LST)
ls$LID <- as.character(ls$LID)

# extract the date
ls <- ls %>% 
  mutate(date = sub("^[^_]*_[^_]*_([^_]*)_.*", "\\1", system.index))

# clean up columns
ls <- ls %>% 
  dplyr::select(date, LID, Name, LST)

ls$date <-  as.Date(ls$date, format = "%Y%m%d")

# subset data to begin in 2000 and end in 2023 because data are sparse in 1999 and 2024
ls <- ls %>% 
  mutate(year = year(date)) %>% 
  filter(year > 1999 & year < 2024)

ggplot(ls, aes(x = as.Date(date), y = LST, color = as.numeric(LID))) +
  geom_point() +
  theme_bw() +
  ylab('Lake skin temperature (LST, °C)') +
  xlab('Date') +
  labs(color= 'Lake ID') +
  theme(text = element_text(size = 14))

length(unique(ls$LID))

write.csv(ls, './data/landsat7_data_formatted_original_dates.csv', row.names = FALSE)


ls %>% 
  filter(LID=='1') %>% 
  ggplot(aes(x = as.Date(date), y = LST, color = LID)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm') +
  ylab('Lake skin temperature (LST, °C)') +
  xlab('Date') +
  theme_bw() +
  theme(legend.position = 'none',
        text = element_text(size = 14))

length(unique(ls$LID))


#some summary stats, number of years per lake, number of obs per year
n_year_per_lake <- ls %>% 
  mutate(year = year(date)) %>% 
  group_by(LID) %>% 
  summarise(n = n_distinct(year))
n_year_per_lake

ggplot(n_year_per_lake, aes(x = n)) +
  geom_histogram() +
  ggtitle('Number of years with observations per lake')

n_obs_year <- ls %>% 
  mutate(year = year(date)) %>% 
  group_by(LID, year) %>% 
  summarise(n = n_distinct(LST))
n_obs_year  

ggplot(n_obs_year, aes(x = year, y = n, group = year, fill = 'blue')) +
  geom_boxplot() +
  scale_fill_manual(values = c('#0BA2CC')) +
  ylab('Number of observations per year per lake') +
  theme_bw() +
  xlab('Date') +
  theme(legend.position = 'none',
        text = element_text(size = 14))


################################################################################
# first take means by month

# create dummy date for year-month and average by month
ls <- ls %>% 
  mutate(month = month(date),
         year = year(date),
         date_dum = as.Date(paste0(year, '-', month, '-01')))

# calculate monthly mean if more than one obs exists
ls_mean <- ls %>% 
  group_by(LID, date_dum, Name) %>% 
  summarise(mean_LST = mean(LST, na.rm = TRUE))

# assign a season and filter to lakes with at least one obs per season
ls_mean <- ls_mean %>% 
  mutate(season = ifelse(month(date_dum) %in% c(12, 1, 2), 'summer', NA),
         season = ifelse(month(date_dum) %in% c(3, 4, 5), 'autumn', season),
         season = ifelse(month(date_dum) %in% c(6, 7, 8), 'winter', season),
         season = ifelse(month(date_dum) %in% c(9, 10, 11), 'spring', season)) %>% 
  mutate(year = year(date_dum))

# filter lakes with at least one observation in each season each year
ls_mean2 <- ls_mean %>% 
  group_by(LID, year) %>%
  filter(n_distinct(season) >= 4) %>%
  ungroup() %>% 
  group_by(LID) %>% 
  filter(n_distinct(year) >=10)

length(unique(ls_mean2$LID))


n_obs_year <- ls_mean2 %>% 
  mutate(year = year(date_dum)) %>% 
  group_by(LID, year) %>% 
  summarise(n = n_distinct(mean_LST))
n_obs_year  

ggplot(n_obs_year, aes(x = year, y = n, group = year, fill = 'blue')) +
  geom_boxplot() +
  scale_fill_manual(values = c('#0BA2CC')) +
  ylab('Number of observations per year per lake') +
  theme_bw() +
  xlab('Date') +
  theme(legend.position = 'none',
        text = element_text(size = 14))

ls_mean2 %>% 
  mutate(month = month(date_dum)) %>% 
  ggplot(aes(x = as.factor(month))) +
  geom_histogram(stat = 'count') +
  xlab('Month') +
  ylab('Number of observations') +
  theme_bw()

ls_mean2 %>% 
  ggplot(aes(x = season)) +
  geom_histogram(stat = 'count') +
  xlab('Month') +
  ylab('Number of observations') +
  theme_bw()

################################################################################
# then linear interpolation to get monthly obs
# Create complete time series (monthly intervals)
fill_dates <- data.frame(date_dum = seq(from = min(as.Date(ls_mean2$date_dum)), 
                                        to = max(as.Date(ls_mean2$date_dum)), 
                                        by = 'month'))
expand_dates <- expand_grid(LID = unique(ls_mean2$LID), date_dum = fill_dates) %>% 
  unnest()

ls_fill <- full_join(expand_dates, ls_mean2)

# first interpolate missing data
ls_fill <- ls_fill %>% 
  dplyr::select(-season, -year) %>% 
  arrange(date_dum) %>% 
  group_by(LID) %>% 
  filter(cumsum(!is.na(mean_LST)) > 0 & rev(cumsum(rev(!is.na(mean_LST))) > 0)) %>% 
  #  ^^ Keep rows where temperature is not NA or is within the range of non-NA rows
  mutate(interp = na.approx(mean_LST, maxgap = 4))

length(unique(ls_fill$LID))

# calculate number of unique lake years
lyear <- ls_fill %>% 
  mutate(year = year(date_dum)) %>% 
  distinct(year, LID) %>% 
  mutate(lyear = paste0(year, '_', LID))
length(unique(lyear$lyear))

#### one example lake #######
ls_fill %>% 
  filter(LID=='5264') %>% 
  ggplot() +
  geom_line(aes(x = as.Date(date_dum), y = interp, color = 'interp')) +
  geom_point(aes(x = as.Date(date_dum), y = interp, color = 'interp')) +
  geom_point(aes(x = as.Date(date_dum), y = mean_LST, color = 'obs'), size = 2) +
  theme_bw() +
  xlab('Date') +
  ylab('LSWT (°C)') +
  labs(color = '') +
  theme(text = element_text(size = 14))

ls_fill %>% 
  filter(LID=='5264') %>% 
  mutate(year = year(date_dum)) %>% 
  group_by(year) %>% 
  summarise(annual_mean = mean(mean_LST,  na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = annual_mean)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  ylim(0, 20) +
  theme_bw() +
  xlab('Date') +
  ylab('Annual Mean LSWT (°C)') +
  labs(color = '') +
  theme(text = element_text(size = 18))

ggplot(ls_fill, aes(x = as.Date(date_dum), y = interp)) +
  geom_point()

################################################################################
# HANTS
# Fit the harmonic model
ls_hants <- ls_fill %>% 
  filter(!is.na(interp)) %>% 
  group_by(LID) %>% 
#  filter(n_distinct(year(date_dum)) > 10) %>% 
  mutate(hants = haRmonics(y = interp,
                           #method = 'wls_harmR',
                           numFreq = 40,  # Number of harmonics
                           delta = 0.17)$fitted)

ggplot(ls_hants, aes(x = date_dum)) +
  geom_point(aes(y = interp), color = "red", na.rm = TRUE) +
  geom_line(aes(y = hants), color = "blue") +
  # geom_point(aes(y = month_mean), color = "green") +
  labs(title = "Harmonic Model Fit and Gap Filling", x = "Date", y = "Value") 

ls_hants %>% 
  filter(LID=='5264') %>% 
  ggplot(aes(x = date_dum)) +
  geom_point(aes(y = interp, color = "Obs"), size = 2) +
  geom_line(aes(y = hants, color = "HANTS")) +
#  geom_point(aes(y = hants, color = "HANTS")) +
  theme_bw() +
  xlab('Date') +
  ylab('LSWT (°C)') +
  labs(color = '') +
  theme(text = element_text(size = 14))

# calculate the percent of obs that were interpolated
sum(is.na(ls_hants$mean_LST))/nrow(ls_hants)



length(unique(ls_hants$LID))
length(unique(ls_fill$LID))

ls_hants %>% 
  mutate(diff = mean_LST - hants) %>% 
  ggplot(aes(x = diff)) +
  geom_histogram()

ggplotly(ls_hants %>% 
  mutate(year = year(date_dum)) %>% 
  group_by(LID, year) %>% 
  summarise(obs_mean = mean(mean_LST, na.rm = TRUE),
            interp_mean = mean(interp, na.rm = TRUE),
            hants_mean = mean(hants, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_point(aes(x = year, y = obs_mean, color = 'obs')) +
  geom_point(aes(x = year, y = interp_mean, color = 'interp')) +
  geom_point(aes(x = year, y = hants_mean, color = 'hants')) )

ls_hants %>% 
  mutate(year = year(date_dum)) %>% 
  group_by(LID, year) %>% 
  summarise(obs_mean = mean(mean_LST, na.rm = TRUE),
            interp_mean = mean(interp, na.rm = TRUE),
            hants_mean = mean(hants, na.rm = TRUE)) %>% 
  filter(LID=='40120') %>% # this is the lake in the first three years with weird values
  ggplot() + 
  geom_point(aes(x = year, y = obs_mean, color = 'obs')) +
  geom_point(aes(x = year, y = interp_mean, color = 'interp')) +
  geom_point(aes(x = year, y = hants_mean, color = 'hants')) 

means <- ls_hants %>% 
  mutate(year = year(date_dum)) %>% 
  group_by(LID, year) %>% 
  summarise(obs_mean = mean(mean_LST, na.rm = TRUE),
            interp_mean = mean(interp, na.rm = TRUE),
            hants_mean = mean(hants, na.rm = TRUE))
means

# some untrustworthy data from Lake Rotoroa at the beginning of the time series, remove the lake
ls_hants <- ls_hants %>% 
  filter(LID!='40120')

ls_hants <- ls_hants %>% 
  rename(#lake_id = LID,
         Date = date_dum,
         obs = mean_LST)

ggplot(ls_hants, aes(x = as.Date(Date), y = obs, color = LID)) +
  geom_point() 

ggplot(ls_hants, aes(x = as.Date(Date), y = interp, color = LID)) +
  geom_point() 

ggplot(ls_hants, aes(x = as.Date(Date), y = hants, color = LID)) +
  geom_point() 

write.csv(ls_hants, './data/landsat7_QAQC.csv', row.names = FALSE)
length(unique(ls_hants$LID))
