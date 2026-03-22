# get trens for big lake
library(tidyverse)
library(ggridges)

sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv') %>% 
  filter(!is.na(district))

# read lake x-y locations to match with output
geo <- readRDS('./data/lernzmp_lakes_master.rds')
geo <- geo$updated 
geo <- geo %>% 
  dplyr::select(id_final, name_fenz, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  dplyr::select(-char)
geo$LID <- as.numeric(geo$LID)

lakes <- c('Lake Wakatipu', 'Lake Te Anau', 'Lake Ellesmere (Te Waihora)',
           'Lake Wanaka', 'Lake Pukaki', 'Lake Manapouri', 'Lake Hawea',
           'Lake Tekapo', 'Lake Benmore', 'Lake Ohau', 'Lake Coleridge',
           'Lake Hauroko', 'Lake Brunner (Moana)')

df <- left_join(sen, geo, by = 'LID')

l_lakes <- df %>% 
  filter(name_fenz %in% lakes)

ggplot(df, aes(x = sen_slope, fill = 'all lakes', alpha = 0.6)) +
  geom_density() +
  theme_bw() +
  geom_density(data = l_lakes, aes(x = sen_slope, fill = 'SI large lakes'))

ggplot(df, aes(x = sen_slope, fill = 'all lakes', alpha = 0.6)) +
  geom_histogram() +
  theme_bw() +
  geom_histogram(data = l_lakes, aes(x = sen_slope, fill = 'SI large lakes'))

df_summary <- df %>% 
  summarise(mean_slope = mean(sen_slope, na.rm = TRUE),
            se_slope = sd(sen_slope, na.rm = TRUE)/sqrt(n()))

ggplot() +
  geom_boxplot(data = df, aes(x = sen_slope, y = 'All lakes')) +
  geom_point(data = l_lakes, aes(y = name_fenz, x = sen_slope)) +
  geom_vline(xintercept = 0) +
  theme_bw() +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab('Lake Name') +
  xlab('LSWT Trend (°C/year)')

