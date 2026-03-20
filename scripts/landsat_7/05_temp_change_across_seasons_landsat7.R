# estimate warming trends by season
library(broom)
library(tidyverse)
library(trend)
library(ggridges)
library(ggpubr)
library(scales)
library(RColorBrewer)
library(lme4)
library(lmerTest)  # for p-values
library(emmeans)
library(circlize)
library(ggpmisc)

d <- readRDS('./data/lernzmp_lakes_master.rds')
d2 <- d$updated 
d2 <- d2 %>% 
  dplyr::select(id_final:northing_NZTM, name_fenz, area_best, max_depth, mean_depth, GeomorphicType) %>% 
  rename(lake_id = id_final)

d2 <- d2 %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  dplyr::select(-char) 

# select geomorphic characteristics
geo <- d$updated %>% 
  dplyr::select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  dplyr::select(-char)

rstemp <- read.csv('./data/landsat7_QAQC.csv')
rstemp$LID <- as.character(rstemp$LID)

data <- left_join(rstemp, d2)


# categorize dates into seasons
seasons <- data.frame(season = c('spring', 'summer', 'autumn', 'winter'),
                      month_start = c('September', 'December', 'March', 'June'))

data <- data %>% 
  mutate(season = ifelse(month(Date) %in% c(12, 1, 2), 'summer', NA),
         season = ifelse(month(Date) %in% c(3, 4, 5), 'autumn', season),
         season = ifelse(month(Date) %in% c(6, 7, 8), 'winter', season),
         season = ifelse(month(Date) %in% c(9, 10, 11), 'spring', season))

# then make a season that is 'all'
data_long <- data %>% 
  mutate(season = as.character(season)) %>%
  bind_rows(mutate(., season = "annual"))

# take annual mean by season
data_sub <- data_long %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, season) %>% 
  summarise(mean_temp = mean(interp, na.rm = TRUE))

# calculate the rate of change in temperature
sen <- data_sub %>% 
  group_by(LID, season) %>% 
  summarise(sen_slope = sens.slope(mean_temp)$estimates,
            sen_signif = sens.slope(mean_temp)$p.value)
sen

length(unique(sen$LID))

sen$season <- factor(sen$season, levels = c('annual', 'winter',
                                            'autumn', 'summer',
                                            'spring'))

# write as csv all trends across seasons and annual
write.csv(sen, './data/output/LSWT_trends_by_season_annual.csv', row.names = FALSE)

### figure with lines for LID
lswt_season_lines <- ggplot(sen, aes(x = fct_rev(season), y = sen_slope, fill = season)) +
  geom_line(aes(x = fct_rev(season), y = sen_slope, color = as.numeric(LID),
                group = LID), alpha = 0.7) +
  geom_boxplot(alpha = 0.9) +
  theme_bw() +
  scale_color_gradient(low = "black", high = "lightgray") +
  scale_fill_manual(values = c("#454545", "#96C0B7", "#FFB84D", "#EE6C4D", "#A8D08D")) +
  geom_hline(yintercept = 0, size = 1) +
  ylab('LSWT Trend (°C/year)') +
  xlab('Season') +
  theme(legend.position = 'none',
        text = element_text(size = 14))

lswt_season_lines
  
ggsave('./figures/landsat_7/rate_of_change_season_lines.png', lswt_season_lines, 
       dpi = 300, units = 'mm', height = 400, width = 450, scale = 0.4)

# run mixed-effects model to test for statistical differences
model <- lmer(sen_slope ~ season + (1 | LID), data = sen)
model
summary(model)
anova(model)

# test pairwise differences across seasons
emmeans(model, pairwise ~ season)

summ <- sen %>% 
  group_by(season) %>% 
  summarise(mean_temp_change = round(mean(sen_slope), 3),
            median_temp_change = round(median(sen_slope), 3),
            sd_temp_change = sd(sen_slope),
            range = max(sen_slope) - min(sen_slope))
summ
write.csv(summ, './data/output/LSWT_trend_summaries_by_season.csv', row.names = FALSE)
