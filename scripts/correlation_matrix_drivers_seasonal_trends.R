# look at correlations between met summaries and LSWT trends, as well as warming 'groups'
library(tidyverse)
library(Hmisc)
library(corrplot)
library(psych)

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
         LakeVolume, LakeElev, Lat, Long,  Abell_Secc)


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
  select(mean_rain_annual:dist_to_shore) 


ct <- corr.test(response, predictor, use = 'pairwise')
cor_matrix <- ct$r 
p_mat <- ct$p

# Keep only significant (p < 0.05)
cor_matrix_sig <- cor_matrix
cor_matrix_sig[p_mat > 0.05] <- NA

corrplot(cor_matrix_sig, method = "color", 
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.col = "black", is.corr = FALSE)

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
