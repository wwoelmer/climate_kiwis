
season_groups <- sen %>% 
  select(-sen_slope, -sen_signif) %>% 
  pivot_wider(names_from = season,
              values_from = season_trend_qual) %>% 
  unite("pattern", spring, summer, autumn, winter, sep = "_", remove = FALSE)

group_summary <- season_groups %>% 
  group_by(pattern) %>% 
  summarise(count = n())

season_df <- season_groups %>% 
  pivot_longer(autumn:winter, names_to = 'season', values_to = 'trend_qual') %>% 
  ungroup() %>% 
  distinct(pattern, season, .keep_all = TRUE) %>% 
  ungroup() %>% 
  select(-LID)

group_df <- left_join(group_summary, season_df)

# add some plotting details
group_df <- group_df %>% 
  mutate(angle_id = case_when(season=='spring' ~ 1,
                              season=='summer' ~ 2,
                              season=='autumn' ~ 3,
                              season=='winter' ~ 4,
                              TRUE ~ NA_real_),
         x = angle_id,
         y = 1)

# set order of patterns based on count
group_df <- group_df %>% 
  mutate(pattern = fct_reorder(pattern, count))
  
# and scale the y based on count
group_df <- group_df %>% 
  mutate(scaled_y = y*count/max(count) + 0.5)

# add n as a label
group_df <- group_df %>% 
  mutate(label = paste0('n = ', count))

labels_named <- setNames(group_df$label, group_df$pattern)
labels_named <- labels_named[!duplicated(names(labels_named))]  # keep unique

group_df$pattern <- factor(group_df$pattern, 
                           levels = c('warm_warm_warm_warm',
                                      'cool_warm_warm_warm',
                                      'warm_cool_warm_warm',
                                      'warm_warm_cool_warm',
                                      'warm_warm_warm_cool',
                                      'cool_cool_warm_warm',
                                      'warm_warm_cool_cool',
                                      'warm_cool_warm_cool',
                                      'cool_warm_cool_warm',
                                      'warm_cool_cool_warm',
                                      'cool_warm_warm_cool',
                                      'cool_cool_cool_cool',
                                      'cool_cool_cool_warm',
                                      'cool_cool_warm_cool',
                                      'cool_warm_cool_cool',
                                      'warm_cool_cool_cool'))

group_df <- group_df %>% 
  mutate(group = case_when(pattern %in% c('warm_warm_warm_warm',
                                          'warm_warm_warm_cool',
                                          'warm_warm_cool_warm',
                                          'warm_cool_warm_warm',
                                          'cool_warm_warm_warm') ~ 'Majority warming',
                           pattern %in% c('cool_cool_cool_cool',
                                          'cool_cool_cool_warm',
                                          'cool_cool_warm_cool',
                                          'cool_warm_cool_cool',
                                          'warm_cool_cool_cool') ~ 'Majority cooling',
                           pattern %in% c('cool_cool_warm_warm',
                                          'warm_warm_cool_cool',
                                          'warm_cool_warm_cool',
                                          'cool_warm_cool_warm',
                                          'warm_cool_cool_warm',
                                          'cool_warm_warm_cool') ~ 'Equal warming and cooling'))

ggplot(group_df, aes(x = x, y = scaled_y, fill = trend_qual, group = pattern)) +
  geom_col(width = 1, color = "white") +
  scale_fill_manual(values = c("warm" = "firebrick", "cool" = "steelblue")) +
#  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_polar() +
  facet_wrap(~pattern, labeller = labeller(pattern = labels_named)) +
  theme_void() +
  theme(strip.text = element_text(size = 10)) +
  labs(fill = "Trend")


# separate into majority warming, cooling, and split
m_warm <- group_df %>% 
  filter(pattern %in% c('warm_warm_warm_warm',
                        'warm_warm_warm_cool',
                        'warm_warm_cool_warm',
                        'warm_cool_warm_warm',
                        'cool_warm_warm_warm')) %>% 
  ggplot(aes(x = x, y = scaled_y, fill = trend_qual, group = pattern)) +
  geom_col(width = 1, color = "white") +
  scale_fill_manual(values = c("warm" = "firebrick", "cool" = "steelblue")) +
  #  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_polar() +
  facet_wrap(~pattern, labeller = labeller(pattern = labels_named), nrow = 1) +
  theme_void() +
  theme(strip.text = element_text(size = 10)) +
  labs(fill = "Trend") +
  ggtitle('Majority warming')
m_warm

m_cool <- group_df %>% 
  filter(pattern %in% c('cool_cool_cool_cool',
                        'cool_cool_cool_warm',
                        'cool_cool_warm_cool',
                        'cool_warm_cool_cool',
                        'warm_cool_cool_cool')) %>% 
  ggplot(aes(x = x, y = scaled_y, fill = trend_qual, group = pattern)) +
  geom_col(width = 1, color = "white") +
  scale_fill_manual(values = c("warm" = "firebrick", "cool" = "steelblue")) +
  #  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_polar() +
  facet_wrap(~pattern, labeller = labeller(pattern = labels_named), nrow = 1) +
  theme_void() +
  theme(strip.text = element_text(size = 10)) +
  labs(fill = "Trend") +
  ggtitle('Majority cooling')
m_cool

split <- group_df %>% 
  filter(pattern %in% c('cool_cool_warm_warm',
                        'warm_warm_cool_cool',
                        'warm_cool_warm_cool',
                        'cool_warm_cool_warm',
                        'warm_cool_cool_warm',
                        'cool_warm_warm_cool')) %>% 
  ggplot(aes(x = x, y = scaled_y, fill = trend_qual, group = pattern)) +
  geom_col(width = 1, color = "white") +
  scale_fill_manual(values = c("warm" = "firebrick", "cool" = "steelblue")) +
  #  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  coord_polar() +
  facet_wrap(~pattern, labeller = labeller(pattern = labels_named), nrow = 1) +
  theme_void() +
  theme(strip.text = element_text(size = 10)) +
  labs(fill = "Trend") +
  ggtitle('Equal warming and cooling')
split

ggarrange(m_warm, split, m_cool, common.legend = TRUE, labels = 'auto', ncol = 1)
