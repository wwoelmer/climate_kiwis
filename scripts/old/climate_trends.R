# look at lake extremes
library(tidyverse)
library(broom)

clim <- read.csv('./data/processed_data/Rotoehu_met_1980_2022.csv')

clim <- clim %>% 
  mutate(year = year(date),
         decade = (year %/% 10) * 10)

clim <- clim %>% 
  pivot_longer(air_temp:windspeed, names_to = 'variable', values_to = 'value')

ggplot(clim, aes(x = as.Date(date), y = value)) +
  geom_line() +
  geom_smooth(method = 'lm') +
  facet_wrap(~variable, scales = 'free')

clim$date <- as.Date(clim$date)
clim_yrly <- clim %>% 
  mutate(year = year(date)) %>% 
  group_by(year, variable) %>% 
  summarise(avg_value = mean(value, na.rm = TRUE))

out <- plyr::ddply(clim_yrly, c("variable"), \(x){
  model <- lm(avg_value ~ year, data = x)
  print(model)
  tidy(model) %>% filter(term == "year") %>% pull(estimate)
})

# out$V1 is the change per day, need to convert this to decade
out

out <- out %>% 
  rename(change_per_year = V1) %>% 
  group_by(variable) %>% 
  mutate(change_per_decade = change_per_year*10)
out
# Nest data by 'variable'

nested_data <- clim %>%
  group_by(variable) %>%
  nest()

# Define a function to fit a linear model and extract the slope
fit_linear_model <- function(df) {
  model <- lm(value ~ date, data = df)
  tidy(model) %>% filter(term == "time") %>% pull(estimate)
}

# Apply the function to each nested data frame and extract slopes
results <- nested_data %>%
  mutate(slope = map_dbl(data, fit_linear_model))

results