# script to relate LSWT rate of change with lake geomorphic characteristics
library(tidyverse)
library(RColorBrewer)

# read LSWT output
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv') %>% 
  filter(!is.na(district))


# read lake x-y locations to match with output
geo <- readRDS('./data/lernzmp_lakes_master.rds')
geo <- geo$updated 
geo <- geo %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  select(-char)
geo$LID <- as.numeric(geo$LID)

df <- left_join(sen, geo)

################################################################################
# figures 
maxd <- ggplot(df, aes(x = max_depth, y = sen_slope)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw() +
  ylab('LSWT slope (C/decade)') +
  xlab('Max depth (m)')

meand <- ggplot(df, aes(x = mean_depth, y = sen_slope)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()+
  ylab('LSWT slope (C/decade)') +
  xlab('Mean depth (m)')

sa <- ggplot(df, aes(x = area, y = sen_slope)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()+
  ylab('LSWT slope (C/decade)') +
  xlab('Surface area (UNITS)')

fig <- ggarrange(maxd, meand, sa, common.legend = TRUE)
fig
ggsave('./figures/wtemp_vs_size.png', fig, 
       dpi = 300, units = 'mm', height = 400, width = 600, scale = 0.3)
