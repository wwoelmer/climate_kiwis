# timeframe figures

gps <- read.csv('./data/LSWT_rates_of_change_literature.csv',
                fileEncoding = 'latin1')

gps$rate_C_year <- as.numeric(gps$rate_C_year)
gps$n_lakes <- as.numeric(gps$n_lakes)

length(unique(gps$citation))

# split temporal aggregation into season and day/night
gps <- gps %>% 
  separate(temporal_aggregation, into = c("temporal_aggregation", "day_night"), 
           sep = " \\(|\\)", extra = "drop", fill = "right") %>% 
  mutate(day_night = ifelse(is.na(day_night), 'day', day_night))


gps <- gps %>% 
  group_by(temporal_aggregation) %>% 
  mutate(n_time = n(),
         facet_label = paste0(temporal_aggregation, ' (n = ', n_time, ')'))

gps$facet_label <- factor(gps$facet_label,
                          levels = 
                            c('spring (n = 10)','summer (n = 39)', 
                              'autumn (n = 11)', 'winter (n = 13)', 
                              'dry season (n = 1)', 'pre-rainy (n = 1)', 
                              'rainy (n = 1)','post-rainy (n = 1)',
                              'annual (n = 112)'))

a <- gps %>% 
  filter(temporal_aggregation %in% c('annual', 'spring', 'summer', 'autumn', 'winter')) %>% 
  ggplot() + 
  geom_segment(aes(x = min_year, xend = max_year, y = rate_C_year, yend = rate_C_year,
                   color = temporal_aggregation), size = 0.7) +
  facet_wrap(~facet_label, nrow = 1) +
  scale_color_manual(values = c("#454545", "#96C0B7", "#FFB84D", "#EE6C4D", "#A8D08D")) +
  theme_bw() +
  xlab('Duration of study') +
  ylab('Trend in LSWT (°C/year)') +
  labs(color = 'Temporal aggregation') +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))
a

ggsave('./figures/landsat_7/timeframe_duration_lit_review.png', a, 
       dpi = 300, units = 'mm', height = 250, width = 800, scale = 0.27)


b <- gps %>% 
  filter(temporal_aggregation %in% c('annual', 'spring', 'summer', 'autumn', 'winter')) %>% 
  ggplot() + 
  geom_density(aes(y = rate_C_year,
                   fill = temporal_aggregation), size = 0.7) +
  facet_wrap(~facet_label, nrow = 1) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("#454545", "#96C0B7", "#FFB84D", "#EE6C4D", "#A8D08D")) +
  theme_bw() +
  xlab('Density') +
  ylab('Trend in LSWT (°C/year)') +
  labs(color = 'Temporal aggregation') +
  theme(axis.text.x = element_text(angle = 55, hjust = 1))

time_density <- ggarrange(a, b, common.legend = TRUE, labels = 'auto', nrow = 2)
time_density
ggsave('./figures/landsat_7/lit_review_season_density.png', time_density, 
       dpi = 300, units = 'mm', height = 490, width = 730, scale = 0.27)


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
                                                  'annual, spring, summer',
                                                  "annual, autumn, spring, summer, winter",
                                                  "annual, dry season, post-rainy, pre-rainy, rainy",
                                                  "autumn, spring, summer, winter",
                                                  "autumn, summer",
                                                  "summer",
                                                  "winter"))


my_colors <- brewer.pal(10, "Set1")

ggplot(multiple_seasons, aes(x = n_seasons, fill = season_list)) +
  geom_bar(position = 'dodge') +
  theme_bw() +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 1),  # aligns with bars
    vjust = -0.3) +
  xlab('# of seasons') +
  ylab('# of studies') +
  labs(fill = 'Seasons included') +
  scale_fill_manual(values = my_colors)

table(multiple_seasons$n_seasons)

