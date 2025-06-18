library(tidyverse)
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)

# read in FENZ drivers and keann# read in FENZ drivers and keep the relevant ones
fenz <- read.csv('./data/drivers/FENZ_Lake_Update_2024_25.09.2024.csv')
fenz$LID <- as.character(fenz$LID)
fenz <- fenz %>% 
  select(LID, Name, NewAreaHa, Region, MaxDepth:SumWind, # Geomorphic
         MeanWind, LakeAreaHa, LakeVolume, LakeElev, Lat, Long,  Abell_Secc, catAnnTemp)

# and get dist_to_shore
lernz <- readRDS('./data/lernzmp_lakes_master.rds')
lernz <- as.data.frame(lernz$updated)
lernz <- lernz %>% 
  select(-geometry)

# modify the LID col
lernz <- lernz %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  select(-char)

# select relevant variables
lernz <- lernz %>% 
  select(LID, dist_to_shore, GeomorphicType) 

# read in LSWT data
lswt <- read.csv('./data/output/LSWT_trends_by_season_annual.csv')
lswt$LID <- as.character(lswt$LID)

data <- left_join(lswt, lernz)
data <- left_join(data, fenz)

# categorize LSWT trends into cooling or warming
data <- data %>% 
  mutate(season_trend_qual = ifelse(sen_slope > 0, 'warm', 'cool'))

season_groups <- data %>% 
  select(-sen_slope, -sen_signif) %>% 
  pivot_wider(names_from = season,
              values_from = season_trend_qual) %>% 
  unite("pattern", spring, summer, autumn, winter, sep = "_", remove = FALSE)

# categorize patterns 
season_patterns <- season_groups %>% 
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

#############################################################################
#  driver analysis using regression tree to predict each lake's pattern

# organize the dataframe
season_patterns <- season_patterns %>% 
  select(LID, Name, pattern, group, everything())

drivers <- season_patterns %>% 
  select(LID, pattern, group, 
         Region, dist_to_shore, GeomorphicType, 
         NewAreaHa, MaxDepth, Fetch, LakeVolume, LakeElev,
         Lat, Long, Abell_Secc, SumWind, catAnnTemp)

drivers <- na.omit(drivers)

# run regression tree
tree_pattern <- rpart(group ~ dist_to_shore +
                      NewAreaHa + MaxDepth + Fetch + LakeVolume + LakeElev +
                      Lat + Long + Abell_Secc + SumWind + catAnnTemp,
                      data = drivers,
                      method = 'class')

rpart.plot(tree_pattern, type = 3, extra = 102, cex = 0.6, 
           box.palette = list('#D3D3D3', 'steelblue', 'firebrick'))

png("./figures/landsat_7/tree_plot.png", width = 1000, height = 800)
rpart.plot(tree_pattern, type = 0, extra = 102, cex = 1, 
           box.palette = list('#D3D3D3', 'steelblue', 'firebrick'))
dev.off()

preds <- predict(tree_pattern, type = 'class')
confusion_matrix <- table(predicted = preds, actual = drivers$group)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)

diag(confusion_matrix) / rowSums(confusion_matrix)

ggplot(drivers, aes(x = preds, fill = group)) +
  geom_bar(position = 'dodge') +
  theme_bw() +
  xlab('Observed group') +
  ylab('Number of lakes in each group') +
  labs(fill = 'Predicted group')

accuracy_by_group <- drivers %>% 
  group_by(group) %>% 
  

# for majority cooling lakes, what is the elevation range
summaries_groups <- drivers %>% 
  select(group, LakeElev, SumWind, dist_to_shore, MaxDepth, NewAreaHa,
         Abell_Secc, Lat, LakeVolume) %>% 
  group_by(group) %>% 
  summarise(across(everything(), list(min = min, max = max,
                                      mean = mean, sd = sd), na.rm = TRUE))

drivers %>% 
  select(group, LakeElev, SumWind, dist_to_shore, MaxDepth, NewAreaHa,
         Abell_Secc, Lat, LakeVolume) %>% 
  pivot_longer(LakeElev:LakeVolume, names_to = 'driver', values_to = 'value') %>% 
  ggplot(aes(x = group, y = value, fill = group)) +
  geom_boxplot() +
  facet_wrap(~driver, scales = 'free')
