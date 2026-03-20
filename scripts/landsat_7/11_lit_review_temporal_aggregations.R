# timeframe figures
library(tidyverse)
library(ggridges)
library(RColorBrewer)

gps <- read.csv('./data/LSWT_rates_of_change_literature.csv',
                fileEncoding = 'latin1')
gps <- gps %>% 
  rename(rate_C_year = ï..rate_C_year)

gps$rate_C_year <- as.numeric(gps$rate_C_year)
gps$n_lakes <- as.numeric(gps$n_lakes)

length(unique(gps$citation))

# split temporal aggregation into season and day/night
gps <- gps %>% 
  separate(temporal_aggregation, into = c("temporal_aggregation", "day_night"), 
           sep = " \\(|\\)", extra = "drop", fill = "right") %>% 
  mutate(day_night = ifelse(is.na(day_night), 'day', day_night))


total_seasons <- sum(table(gps$temporal_aggregation))

table(gps$temporal_aggregation)
table(gps$aggregation_cleaned)
table(gps$day_night)

# and remove the night measurements
gps_day <- gps %>% 
  filter(day_night!='night')


gps_day <- gps_day %>% 
  group_by(temporal_aggregation) %>% 
  mutate(n_time = n(),
         facet_label = paste0(temporal_aggregation, ' (n = ', n_time, ')'))

gps_day$facet_label <- factor(gps_day$facet_label,
                          levels = 
                            c('spring (n = 15)','summer (n = 37)', 
                              'autumn (n = 14)', 'winter (n = 16)', 
                              'dry season (n = 1)', 'pre-rainy (n = 1)', 
                              'rainy (n = 1)','post-rainy (n = 1)',
                              'annual (n = 71)'))



a <- gps_day %>% 
  filter(temporal_aggregation %in% c('annual', 'spring', 'summer', 'autumn', 'winter')) %>% 
  mutate(violin_x = max(max_year, na.rm = TRUE) + 2) %>% 
  ggplot() + 
  geom_point(aes(x = min_year, y = rate_C_year, alpha = 0.01, color = temporal_aggregation)) +
  geom_point(aes(x = max_year, y = rate_C_year, alpha = 0.01, color = temporal_aggregation)) +
  geom_segment(aes(x = min_year, xend = max_year, y = rate_C_year, yend = rate_C_year,
                   color = temporal_aggregation), size = 0.7) +
  facet_wrap(~facet_label, ncol = 5) +
  scale_color_manual(values = c("#A8D08D", "#EE6C4D", "#FFB84D", "#96C0B7", "#454545")) +
  scale_fill_manual(values = c("#A8D08D", "#EE6C4D", "#FFB84D", "#96C0B7", "#454545")) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlab('Duration of study') +
  ylab('LSWT trend (°C/year)') +
  labs(color = 'Temporal aggregation') +
  theme(axis.text.x = element_text(angle = 55, hjust = 1),
        legend.position = 'none')
a

ggsave('./figures/landsat_7/timeframe_duration_lit_review.png', a, 
       dpi = 300, units = 'mm', height = 300, width = 700, scale = 0.3)

# mean rates by season
mean_season <- gps_day %>% 
  group_by(temporal_aggregation) %>% 
  summarise(mean = mean(rate_C_year),
            sd = sd(rate_C_year),
            min = min(rate_C_year),
            max = max(rate_C_year))
mean_season
write.csv(mean_season, './data/output/lit_review_summary_rates.csv', row.names = FALSE)

table(gps$day_night)
27/184

# number of studies that have more than one season
multiple_seasons <- gps %>% 
  filter(temporal_aggregation!='?') %>% 
  group_by(citation) %>% 
  summarise(n_seasons = n_distinct(temporal_aggregation),
            season_list = str_c(
              sort(factor(unique(temporal_aggregation))),
              collapse = ", "))

multiple_seasons$season_list <- factor(multiple_seasons$season_list, 
                                       levels = c('annual',
                                                  'annual, summer',
                                                  "annual, summer, winter",
                                                  'annual, spring, summer',
                                                  "annual, autumn, spring, summer, winter",
                                                  "annual, dry season, post rainy, pre rainy, rainy",
                                                  "autumn, spring, summer, winter",
                                                  "autumn, spring, summer",
                                                  "autumn, summer",
                                                  "summer",
                                                  "winter"))


my_colors <- colorRampPalette(brewer.pal(9, "Set1"))(10)

ggplot(multiple_seasons, aes(x = n_seasons, fill = season_list)) +
  geom_bar(position = 'dodge') +
  theme_bw() +
  scale_fill_manual(values = my_colors) +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 1),  # aligns with bars
    vjust = -0.3) +
  xlab('# of seasons') +
  ylab('# of studies') +
  labs(fill = 'Seasons included') 

# studies reporting just one citation
42/(42+12+1)
12/(42+12+1)

table(multiple_seasons$n_seasons)
gps %>% 
  group_by(temporal_aggregation) %>% 
  summarise(CV = sd(rate_C_year)/mean(rate_C_year))
