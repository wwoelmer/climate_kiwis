# look at correlations between met summaries and LSWT trends, as well as warming 'groups'
library(tidyverse)
library(Hmisc)
library(corrplot)
library(psych)
library(reshape2)

met <- read.csv('./data/output/era5_met_summary_stats_trends.csv')
met$LID <- as.character(met$LID)

ggplot(met, aes(y = mean_season, x = season)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = 'free')

# read in LSWT data
lswt <- read.csv('./data/output/LSWT_trends_by_season_annual.csv') %>% 
  select(-sen_signif)
lswt$LID <- as.character(lswt$LID)

data <- left_join(lswt, met)
data <- na.omit(data)

# convert to wide for correlation analysis
data_wide <- data %>% 
  mutate(variable = dplyr::recode(variable,
                           "MET_pprain" = "rain",
                           "MET_tmpair" = 'atemp')) %>% 
  rename(sen_lswt = sen_slope,
         mean = mean_season,
         min = min_season,
         max= max_season,
         sen = sen_season) %>%
  pivot_wider(names_from = variable, values_from = c(mean, min, max, sen)) %>% 
  pivot_wider(names_from = season, values_from = sen_lswt:sen_atemp)

write.csv(data_wide, './data/output/era5_met_summary_stats_trends_WIDE.csv', row.names = FALSE)

# remove the min rain variables because they are zero
data_wide <- data_wide %>% 
  select(-min_rain_annual, 
         -min_rain_autumn, 
         -min_rain_spring,  
         -min_rain_summer,
         -min_rain_winter)

# bring in other drivers
fenz <- read.csv('./data/drivers/FENZ_Lake_Update_2024_25.09.2024.csv')
fenz$LID <- as.character(fenz$LID)
fenz <- fenz %>% 
  select(LID, Name, NewAreaHa, MaxDepth, Fetch, 
         LakeVolume, LakeElev, Abell_Secc)


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
  select(LID, dist_to_shore) 

data_wide <- left_join(data_wide, fenz)
data_wide <- left_join(data_wide, lernz)

data_cor <- data_wide %>% 
  select(-LID, -Name)

response <- data_cor %>% 
  select(sen_lswt_annual:sen_lswt_winter)
predictor <- data_cor %>% 
  select(mean_rain_annual:dist_to_shore) %>% 
  select(-NewAreaHa, -LakeVolume) %>% # remove these because no significant correlations
  select(-c(mean_rain_autumn, # keep only annual rain since all seasons show similar pattern
            mean_rain_spring, 
            mean_rain_summer, 
            mean_rain_winter)) %>% 
  select(-c(mean_atemp_autumn, # keep only annual rain since all seasons show similar pattern
            mean_atemp_spring, 
            mean_atemp_summer, 
            mean_atemp_winter)) # keep only annual air temp since all seasons show similar pattern

ct <- corr.test(response, predictor, use = 'pairwise', method = 'spearman')
cor_matrix <- ct$r 
p_mat <- ct$p

# Keep only significant (p < 0.05)
cor_matrix_sig <- cor_matrix
cor_matrix_sig[p_mat > 0.05] <- NA

corrplot(cor_mat_masked, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", is.corr = FALSE, na.label = " ",
         addCoef.col = 'black', number.cex = 0.5,
         tl.srt = 45,
         tl.pos = 'lt')

cor_mat_masked <- ifelse(abs(cor_matrix_sig) > 0.15, cor_matrix_sig, NA)
cor_mat_masked

cor_long <- melt(cor_mat_masked, varnames = c("Var1", "Var2"), value.name = "Correlation")
cor_long <- na.omit(cor_long)

# do some cleaning up of variable names
cor_long$Var1 <- factor(cor_long$Var1, 
                        levels = c('sen_lswt_annual',
                                   'sen_lswt_autumn',
                                   'sen_lswt_winter',
                                   'sen_lswt_spring',
                                   'sen_lswt_summer'),
                        labels = c("Trend Annual",
                                   "Trend Autumn",
                                   "Trend Winter",
                                   "Trend Spring",
                                   "Trend Summer"))

cor_long$Var2 <- factor(cor_long$Var2, 
                        levels = c("mean_rain_annual",
                                   "mean_atemp_annual",
                                   "min_atemp_annual",
                                   "min_atemp_autumn",
                                   "min_atemp_spring",
                                   "min_atemp_summer",
                                   "min_atemp_winter",
                                   "max_rain_annual",
                                   "max_rain_autumn",
                                   "max_rain_spring",
                                   "max_rain_summer",
                                   "max_rain_winter",
                                   "max_atemp_annual",
                                   "max_atemp_autumn",
                                   "max_atemp_spring",
                                   "max_atemp_summer",
                                   "max_atemp_winter",
                                   "sen_rain_annual",
                                   "sen_rain_autumn",
                                   "sen_rain_spring",
                                   "sen_rain_summer",
                                   "sen_rain_winter",
                                   "sen_atemp_annual",
                                   "sen_atemp_autumn",
                                   "sen_atemp_spring",
                                   "sen_atemp_summer",
                                   "sen_atemp_winter",
                                   "MaxDepth",
                                   "Fetch",
                                   "LakeElev",
                                   "Abell_Secc",
                                   "dist_to_shore"),
                        labels = c("Mean annual rain",
                                   "Mean annual atemp",
                                   "Min annual atemp",
                                   "Min autumn atemp",
                                   "Min spring atemp",
                                   "Min summer atemp",
                                   "Min winter atemp",
                                   "Max annual rain",
                                   "Max autumn rain",
                                   "Max spring rain",
                                   "Max summer rain",
                                   "Max winter rain",
                                   "Max annual atemp",
                                   "Max autumn atemp",
                                   "Max spring atemp",
                                   "Max summer atemp",
                                   "Max winter atemp",
                                   "Trend annual rain",
                                   "Trend autumn rain",
                                   "Trend spring rain",
                                   "Trend summer rain",
                                   "Trend winter rain",
                                   "Trend annual atemp",
                                   "Trend autumn atemp",
                                   "Trend spring atemp",
                                   "Trend summer atemp",
                                   "Trend winter atemp",
                                   "Max Depth",
                                   "Fetch",
                                   "Elevation",
                                   "Secchi depth",
                                   "Distance to shore"))

p1 <- ggplot(cor_long, aes(Var1, Var2, fill = Correlation)) +
  geom_tile(color = "grey90") +
  geom_text(aes(label = round(Correlation, 2)), size = 3, na.rm = TRUE) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, na.value = "white") +
  coord_fixed() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        axis.title = element_blank())
p1
ggsave('./figures/landsat_7/correlation_annual_seasonal_trends.png', p1,
       dpi = 300, units = 'mm', height = 500, width = 300, scale = 0.35)


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
