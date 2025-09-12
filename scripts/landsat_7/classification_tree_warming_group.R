#install.packages('rpart.plot')
#install.packages('rattle')
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)
library(ggpubr)
library(car)

# read in FENZ drivers and keep the relevant ones
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

# read in met stats
met <- read.csv('./data/output/era5_met_summary_stats_trends_WIDE.csv')
met$LID <- as.character(met$LID)

data <- left_join(lswt, lernz)
data <- left_join(data, fenz)
data <- left_join(data, met)

# categorize LSWT trends into cooling or warming
data <- data %>% 
  mutate(season_trend_qual = ifelse(sen_slope > 0, 'warm', 'cool')) %>% 
  select(LID, sen_slope, season, season_trend_qual, everything())

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
         Lat, Long, Abell_Secc, SumWind, catAnnTemp, 
         mean_rain_annual:sen_atemp_winter)

drivers <- na.omit(drivers)
drivers$group <- as.factor(drivers$group)

# Fit a linear model with all predictors (response can be numeric placeholder)
lm_model <- lm(as.numeric(group) ~ dist_to_shore +
                 NewAreaHa + MaxDepth + Fetch + LakeVolume + LakeElev +
                 Lat + Long + Abell_Secc + SumWind + catAnnTemp,
               data = drivers)

vif_values <- vif(lm_model)
print(vif_values)

# remove variables with VIF > 10 and re run
lm_model <- lm(as.numeric(group) ~ dist_to_shore +
                 NewAreaHa + MaxDepth + LakeElev +
                 Lat + Abell_Secc + SumWind,
               data = drivers)

vif_values <- vif(lm_model)
print(vif_values)

# fit classification tree with reduced group of drivers
set.seed(123)
tree_pattern <- rpart(group ~ dist_to_shore +
                        NewAreaHa + MaxDepth + LakeElev +
                        Lat + Abell_Secc + mean_rain_annual + mean_rain_autumn +
                        mean_rain_spring +  mean_rain_summer +  mean_rain_winter+ mean_atemp_annual +mean_atemp_autumn+ mean_atemp_spring +
                        mean_atemp_summer + mean_atemp_winter  + min_atemp_annual  + min_atemp_autumn + min_atemp_spring + min_atemp_summer + 
                        min_atemp_winter + 
                        max_rain_annual   + max_rain_autumn    + max_rain_spring + max_rain_summer + max_rain_winter + max_atemp_annual + 
                        max_atemp_autumn + max_atemp_spring + max_atemp_summer + max_atemp_winter + sen_rain_annual + sen_rain_autumn +  
                        sen_rain_spring   + sen_rain_summer    + sen_rain_winter + sen_atemp_annual + sen_atemp_autumn + sen_atemp_spring +
                        sen_atemp_summer + sen_atemp_winter,
                      data = drivers,
                      method = 'class',
                      control = rpart.control(cp = 0.0001, # higher value means each split must reduce more error, smaller value is more lenient
                                              minsplit = 20, #minimum number of obs in a node before a split
                                              maxdepth = 3))


rpart.plot(tree_pattern, type = 0, extra = 102, cex = 0.6, 
           box.palette = list('#D3D3D3', 'steelblue', 'firebrick'))

preds <- predict(tree_pattern, type = 'class')
confusion_matrix <- table(predicted = preds, actual = drivers$group)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)


### now prune the tree based on cp
plotcp(tree_pattern)
printcp(tree_pattern)
tree_pattern$cptable[which.min(tree_pattern$cptable[,"xerror"]),"CP"]
bestcp <- tree_pattern$cptable[which.min(tree_pattern$cptable[,"xerror"]),"CP"]
tree.pruned <- rpart::prune(tree_pattern, cp = bestcp)

#this time we add a few arguments to add some mojo to our graphed tree.
#Actually this will give us a very similar graphed tree as rattle (and we like that graph!)
rpart.plot(tree.pruned, extra=102, cex = 0.6, type = 0,
           box.palette = list('#D3D3D3', 'steelblue', 'firebrick'),
           branch.lty=3, nn=TRUE)

png("./figures/landsat_7/tree_plot.png", width = 1800, height = 1000)
rpart.plot(tree.pruned, extra=102, cex = 2, type = 0,
           box.palette = list('#D3D3D3', 'steelblue', 'firebrick'),
           branch.lty=3, nn=TRUE)
dev.off()

preds <- predict(tree.pruned, type = 'class')
confusion_matrix <- table(predicted = preds, actual = drivers$group)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)


a <- ggplot(drivers, aes(x = group, y = log(NewAreaHa), fill = group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  theme_bw() +
  geom_hline(yintercept = log(37)) +
  stat_compare_means(method = "kruskal.test") +
  theme(axis.text.x = element_blank(),
        legend.position = 'none')
a

ggsave('./figures/landsat_7/driver_boxplots/boxplot_area.png', a,
       dpi = 300, units = 'mm', height = 250, width = 250, scale = 0.25)

b <- ggplot(drivers, aes(x = group, y = Lat, fill = group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  theme_bw() +
  stat_compare_means(method = "kruskal.test") +
  geom_hline(yintercept = -45) +
  theme(axis.text.x = element_blank(),
        legend.position = 'none')
ggsave('./figures/landsat_7/driver_boxplots/boxplot_lat.png', b,
       dpi = 300, units = 'mm', height = 250, width = 275, scale = 0.25)

c <- ggplot(drivers, aes(x = group, y = log(MaxDepth), fill = group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  theme_bw() +
  geom_hline(yintercept = log(20)) +
  stat_compare_means(method = "kruskal.test") +
  theme(axis.text.x = element_blank(),
        legend.position = 'none')
ggsave('./figures/landsat_7/driver_boxplots/boxplot_depth.png', c,
       dpi = 300, units = 'mm', height = 250, width = 275, scale = 0.25)

d <- ggplot(drivers, aes(x = group, y = min_atemp_spring, fill = group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  theme_bw() +
  geom_hline(yintercept = -0.053) +
  stat_compare_means(method = "kruskal.test") +
  theme(axis.text.x = element_blank(),
        legend.position = 'none')
ggsave('./figures/landsat_7/driver_boxplots/boxplot_spring_atemp.png', d,
       dpi = 300, units = 'mm', height = 250, width = 275, scale = 0.25)

e <- ggplot(drivers, aes(x = group, y = max_atemp_winter, fill = group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  theme_bw() +
  geom_hline(yintercept = 17) +
  stat_compare_means(method = "kruskal.test") +
  theme(axis.text.x = element_blank(),
        legend.position = 'none')
ggsave('./figures/landsat_7/driver_boxplots/boxplot_winter_atemp.png', e,
       dpi = 300, units = 'mm', height = 250, width = 275, scale = 0.25)

f <- ggplot(drivers, aes(x = group, y = sen_rain_annual, fill = group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  theme_bw() +
  geom_hline(yintercept = -0.011) +
  stat_compare_means(method = "kruskal.test") +
  theme(axis.text.x = element_blank(),
        legend.position = 'none')
ggsave('./figures/landsat_7/driver_boxplots/boxplot_rain_trend.png', f,
       dpi = 300, units = 'mm', height = 250, width = 275, scale = 0.25)

p1 <- ggarrange(a, b, c, d, e, f, common.legend = TRUE)
p1 <- ggarrange(a, b, c, e,common.legend = TRUE)
p1
ggsave('./figures/landsat_7/boxplots_cart_drivers.png', p1,
       dpi = 300, units = 'mm', height = 325, width = 500, scale = 0.5)

ggplot(drivers, aes(x = group, y = dist_to_shore, fill = group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  theme_bw() +
  stat_compare_means(method = "kruskal.test") 

ggplot(drivers, aes(x = group, y = Abell_Secc, fill = group)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2) +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  theme_bw() +
  stat_compare_means(method = "kruskal.test") 

###########################################################################################
# try out conditional inference trees
#install.packages('party')
library(party)       # or partykit
# Fit conditional inference tree
fit_ctree <- ctree(group ~ dist_to_shore +
                     NewAreaHa + MaxDepth + LakeElev +
                     Lat + Abell_Secc + mean_rain_annual + mean_rain_autumn +
                     mean_rain_spring +  mean_rain_summer +  mean_rain_winter+ mean_atemp_annual +mean_atemp_autumn+ mean_atemp_spring +
                     mean_atemp_summer + mean_atemp_winter  + min_atemp_annual  + min_atemp_autumn + min_atemp_spring + min_atemp_summer + 
                     min_atemp_winter + 
                     max_rain_annual   + max_rain_autumn    + max_rain_spring + max_rain_summer + max_rain_winter + max_atemp_annual + 
                     max_atemp_autumn + max_atemp_spring + max_atemp_summer + max_atemp_winter + sen_rain_annual + sen_rain_autumn +  
                     sen_rain_spring   + sen_rain_summer    + sen_rain_winter + sen_atemp_annual + sen_atemp_autumn + sen_atemp_spring +
                     sen_atemp_summer + sen_atemp_winter,
                   data = drivers)

plot(fit_ctree)

fit_cforest <- cforest(group ~ dist_to_shore +
                         NewAreaHa + MaxDepth + LakeElev +
                         Lat + Abell_Secc + mean_rain_annual + mean_rain_autumn +
                         mean_rain_spring +  mean_rain_summer +  mean_rain_winter+ mean_atemp_annual +mean_atemp_autumn+ mean_atemp_spring +
                         mean_atemp_summer + mean_atemp_winter  + min_atemp_annual  + min_atemp_autumn + min_atemp_spring + min_atemp_summer + 
                         min_atemp_winter + 
                         max_rain_annual   + max_rain_autumn    + max_rain_spring + max_rain_summer + max_rain_winter + max_atemp_annual + 
                         max_atemp_autumn + max_atemp_spring + max_atemp_summer + max_atemp_winter + sen_rain_annual + sen_rain_autumn +  
                         sen_rain_spring   + sen_rain_summer    + sen_rain_winter + sen_atemp_annual + sen_atemp_autumn + sen_atemp_spring +
                         sen_atemp_summer + sen_atemp_winter,
                       data = drivers,
                       controls = cforest_unbiased(ntree = 500, mtry = 3))
varimp(fit_cforest)

dotchart(sort(varimp(fit_cforest)), main = "Conditional Inference Forest Variable Importance")


library(randomForest)

rf_model <- randomForest(group ~ dist_to_shore +
                           NewAreaHa + MaxDepth + LakeElev +
                           Lat + Abell_Secc + mean_rain_annual + mean_rain_autumn +
                           mean_rain_spring +  mean_rain_summer +  mean_rain_winter+ mean_atemp_annual +mean_atemp_autumn+ mean_atemp_spring +
                           mean_atemp_summer + mean_atemp_winter  + min_atemp_annual  + min_atemp_autumn + min_atemp_spring + min_atemp_summer + 
                           min_atemp_winter + 
                           max_rain_annual   + max_rain_autumn    + max_rain_spring + max_rain_summer + max_rain_winter + max_atemp_annual + 
                           max_atemp_autumn + max_atemp_spring + max_atemp_summer + max_atemp_winter + sen_rain_annual + sen_rain_autumn +  
                           sen_rain_spring   + sen_rain_summer    + sen_rain_winter + sen_atemp_annual + sen_atemp_autumn + sen_atemp_spring +
                           sen_atemp_summer + sen_atemp_winter,
                         data = drivers,
                         ntry = 500,
                         mtry = 3, 
                         importance = TRUE)
rf_model

importance(rf_model)
varImpPlot(rf_model)

imp <- importance(rf_model, type = 1)  # type = 1 = MeanDecreaseAccuracy
imp_df <- data.frame(
  Variable = rownames(imp),
  MeanDecreaseAccuracy = imp[, 1]
)

# Filter for > 8%
imp_df_filtered <- subset(imp_df, MeanDecreaseAccuracy > 8)

# Plot
ggplot(imp_df_filtered, aes(x = reorder(Variable, MeanDecreaseAccuracy), 
                            y = MeanDecreaseAccuracy)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance (Mean Decrease Accuracy > 8%)",
       x = "Variable", 
       y = "Mean Decrease Accuracy (%)") +
  theme_minimal()



################################################################################
## OLD STUFF, PROBABLY DELETE
# run regression tree
tree_pattern <- rpart(group ~ dist_to_shore +
                      NewAreaHa + MaxDepth + Fetch + LakeVolume + LakeElev +
                      Lat + Long + Abell_Secc + SumWind + catAnnTemp,
                      data = drivers,
                      method = 'class')


rpart.plot(tree_pattern, type = 0, extra = 102, cex = 0.6, 
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

accuracy_by_group <- diag(confusion_matrix)/colSums(confusion_matrix)
accuracy_by_group
  

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

plotcp(tree_pattern)
printcp(tree_pattern)
tree_pattern$cptable[which.min(tree_pattern$cptable[,"xerror"]),"CP"]
bestcp <- tree_pattern$cptable[which.min(tree_pattern$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree_pattern, cp = bestcp)

#this time we add a few arguments to add some mojo to our graphed tree.
#Actually this will give us a very similar graphed tree as rattle (and we like that graph!)
rpart.plot(tree.pruned, extra=102, cex = 0.6, type = 0,
           box.palette = list('#D3D3D3', 'steelblue', 'firebrick'),
           branch.lty=3, nn=TRUE)

conf.matrix <- round(prop.table(table(tree_pattern$group, predict(tree.pruned, type="class"))), 2)
rownames(conf.matrix) <- c("Actually died", "Actually Survived")
colnames(conf.matrix) <- c("Predicted dead", "Predicted Survived")
conf.matrix

fancyRpartPlot(tree.pruned)
