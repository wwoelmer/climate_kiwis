library(tidyverse)

# read in in situ observations
insitu <- read_rds('./data/lernzmp_lakes_obs.rds')
insitu <- insitu %>% 
  select(lake_id, lake, datetime, variable, var_aeme, depth_from, value) %>% 
  filter(var_aeme=='HYD_temp')

# subset to surface
insitu <- insitu %>% 
  filter(depth_from < 1,
         value < 50 & value > 1)  # subset a few erroneous obs

# subset to 2000 to 2023, following time frame of landsat 
insitu <- insitu %>% 
  filter(datetime > '2000-01-01' & datetime < '2023-12-31')


ggplot(insitu, aes(x = as.Date(datetime), y = value)) +
  geom_point()

length(unique(insitu$lake_id))

# change name of LID
insitu <- insitu %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  select(-char)

# take only the mininum depth if multiple exist for a given date
insitu <- insitu %>% 
  mutate(Date = as.Date(datetime)) %>% 
  group_by(Date, lake) %>% 
  filter(depth_from == min(depth_from))

# rename and remove extra cols before merging with landsat
insitu <- insitu %>% 
  select(LID, lake, Date, depth_from, value) %>% 
  rename(obs_insitu = value)
str(insitu)

################################################################################
# read in satellite observations
rstemp <- read.csv('./data/landsat7_data_formatted_original_dates.csv')
rstemp$LID <- as.character(rstemp$LID)
rstemp <- rstemp %>% 
  rename(obs_landsat = LST,
         Date = date) %>% 
  select(LID, Name, Date, obs_landsat)
rstemp$Date <- as.Date(rstemp$Date)

################################################################################
# combine the temps
wtemp <- left_join(insitu, rstemp)
wtemp <- wtemp %>% 
  filter(!is.na(obs_landsat))

length(unique(wtemp$lake))
unique(wtemp$lake)

lm_temps <- lm(wtemp$obs_insitu ~ wtemp$obs_landsat)
summary(lm_temps)

lm_eqn <- function(model) {
  a <- format(coef(model)[1], digits = 2)
  b <- format(coef(model)[2], digits = 2)
  r2 <- format(summary(model)$r.squared, digits = 2)
  
  eq <- substitute(italic(y) == a + b %.% italic(x) * "," ~~ R^2 ~ "=" ~ r2, 
                   list(a = a, b = b, r2 = r2))
  
  as.character(as.expression(eq))
}

r2 <- round(summary(lm_temps)$r.squared, 2)
n <- nrow(wtemp)

ggplot(wtemp, aes(x = obs_insitu, y = obs_landsat, color = lake)) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw() +
  annotate("text", x = 10, y = 28, label = paste("n = ", n)) +  # Add n
  annotate("text", x = 12, y = 30, label = lm_eqn(lm_temps), parse = TRUE)
  
length(unique(wtemp$lake))
