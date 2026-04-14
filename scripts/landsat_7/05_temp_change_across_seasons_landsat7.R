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

# match with StatsNZ sites
district <- read.csv('./data/lake_districts.csv') %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  dplyr::select(-char) 

data <- left_join(data, district, by = 'LID')

# get rid of the lake which does not belong to a region?
data <- data %>% 
  filter(!is.na(district))

# categorize dates into seasons
seasons <- data.frame(season = c('Spring', 'Summer', 'Autumn', 'Winter'),
                      month_start = c('September', 'December', 'March', 'June'))

data <- data %>% 
  mutate(season = ifelse(month(Date) %in% c(12, 1, 2), 'Summer', NA),
         season = ifelse(month(Date) %in% c(3, 4, 5), 'Autumn', season),
         season = ifelse(month(Date) %in% c(6, 7, 8), 'Winter', season),
         season = ifelse(month(Date) %in% c(9, 10, 11), 'Spring', season))

# take annual mean by season
data_sub <- data %>% 
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

sen %>% 
  group_by(season) %>% 
  summarise(sen_slope = mean(sen_slope))

sen$season <- factor(sen$season, levels = c('Winter',
                                            'Autumn', 
                                            'Summer',
                                            'Spring'))
sen$LID <- as.numeric(sen$LID)

# bring in annual rates and add to df
sen_ann <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv')
sen_ann$season <- 'Annual'
sen_ann <- sen_ann %>% 
  select(LID, season, sen_slope, sen_signif)

sen <- full_join(sen, sen_ann)


# write as csv all trends across seasons and annual
write.csv(sen, './data/output/LSWT_trends_by_season_annual.csv', row.names = FALSE)

### figure with lines for LID
sen$season <- factor(sen$season, levels = c('Spring', 'Summer', 'Autumn', 'Winter', 'Annual'))

lswt_season_lines <- ggplot(sen, aes(x = fct_rev(season), y = sen_slope, fill = season)) +
  geom_line(aes(x = season, y = sen_slope, color = as.numeric(LID),
                group = LID), alpha = 0.7) +
  geom_boxplot(alpha = 0.9) +
  theme_bw() +
  scale_color_gradient(low = "black", high = "lightgray") +
  scale_fill_manual(values = c("#A8D08D", "#EE6C4D", "#FFB84D", "#96C0B7", "#454545")) +
  geom_hline(yintercept = 0, size = 1) +
  ylab('LSWT trend (°C/year)') +
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


##############################################################################
# now compare annual and seasonal trends

# split seasonal and annual
sen_seasonal <- sen %>%
  filter(season != "Annual") %>%
  select(LID, season, seasonal = sen_slope)

sen_annual2 <- sen %>%
  filter(season == "Annual") %>%
  select(LID, annual = sen_slope)

# join
sen_both <- left_join(sen_seasonal, sen_annual2, by = "LID")

p_a <- ggplot(sen_both, aes(x = seasonal, y = annual, color = season)) +
  geom_point(size = 2.5, alpha = 0.9) +
  geom_smooth(method = "lm", color = "black", fill = "grey70") +
  geom_hline(yintercept = 0, size = 0.8) +
  geom_vline(xintercept = 0, size = 0.8) +
  stat_cor(aes(label = paste(..r.label..)),
           method = "pearson",
           cor.coef.name = 'r',
           label.x = -0.55,
           label.y = 0.22,
           size = 4,
           color = 'black') +
  facet_wrap(~season, nrow = 1) +
  scale_color_manual(values = c(
    Spring = "#A8D08D",
    Summer = "#EE6C4D",
    Autumn = "#FFB84D",
    Winter = "#96C0B7"
  )) +
  theme_bw() +
  labs(x = "Seasonal trend (°C/year)",
       y = "Annual trend (°C/year)",
       color = "Season") +
  theme(legend.position = "top",
        text = element_text(size = 12))
p_a

sen_quad <- sen_both %>%
  group_by(season) %>% 
  mutate(category = case_when(
    seasonal > 0 & annual > 0 ~ "Both warm",
    seasonal < 0 & annual < 0 ~ "Both cool",
    seasonal < 0 & annual > 0 ~ "Cool season, warm year",
    seasonal > 0 & annual < 0 ~ "Warm season, cool year"))

quad_sum <- sen_quad %>%
  group_by(season, category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(season) %>%
  mutate(percent = 100 * n / sum(n))

p_b <- ggplot(quad_sum, aes(x = season, y = percent, fill = season)) +
  geom_col() +
  facet_wrap(~factor(category,
                     levels = c('Cool season, warm year',
                                'Both warm',
                                'Warm season, cool year',
                                'Both cool')), nrow = 1) +
  scale_fill_manual(values = c(
    Spring = "#A8D08D",
    Summer = "#EE6C4D",
    Autumn = "#FFB84D",
    Winter = "#96C0B7"
  )) +
  theme_bw() +
  labs(
    x = NULL,
    y = "Percent of lakes"
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())
p_b

ggplot(quad_sum, aes(x = category, y = percent, fill = category)) +
  geom_col() +
  facet_wrap(~season, nrow = 1) +
  theme_bw() +
  labs(
    x = "Season",
    y = "Percent of lakes"
  ) +
  theme(
    legend.position = "none",
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

a <- lswt_season_lines
b <- ggarrange(p_a, p_b, common.legend = TRUE, nrow = 2,
               labels = c('b', 'c'), legend = 'bottom')
b
season_plot <- ggarrange(a, b, nrow = 2, labels = c('a', '', ''))
season_plot

ggsave('./figures/landsat_7/season_annual_trends_comparisons.png', season_plot, 
       dpi = 300, units = 'mm', height = 600, width = 400, scale = 0.45)
