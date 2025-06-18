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
#install.packages('circlize')
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


# categorize dates into seasons
seasons <- data.frame(season = c('spring', 'summer', 'autumn', 'winter'),
                      month_start = c('September', 'December', 'March', 'June'))

data <- data %>% 
  mutate(season = ifelse(month(Date) %in% c(12, 1, 2), 'summer', NA),
         season = ifelse(month(Date) %in% c(3, 4, 5), 'autumn', season),
         season = ifelse(month(Date) %in% c(6, 7, 8), 'winter', season),
         season = ifelse(month(Date) %in% c(9, 10, 11), 'spring', season))

# then make a season that is 'all'
data_long <- data %>% 
  mutate(season = as.character(season)) %>%
  bind_rows(mutate(., season = "annual"))

# take annual mean by season
data_sub <- data_long %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, season) %>% 
  summarise(mean_temp = mean(interp, na.rm = TRUE))


ggplot(data_sub, aes(x = as.Date(year), y = mean_temp, color = season)) +
  geom_point() +
  facet_wrap(~season) +
  geom_smooth(method = 'lm')


# calculate the rate of change in temperature
sen <- data_sub %>% 
  group_by(LID, season) %>% 
  summarise(sen_slope = sens.slope(mean_temp)$estimates,
            sen_signif = sens.slope(mean_temp)$p.value)
sen

sen$season <- factor(sen$season, levels = c('annual', 'winter',
                                            'autumn', 'summer',
                                            'spring'))

# write as csv all trends across seasons and annual
write.csv(sen, './data/output/LSWT_trends_by_season_annual.csv', row.names = FALSE)

lswt_season <- ggplot(sen, aes(y = fct_rev(season), x = sen_slope, fill = season)) +
  geom_boxplot(alpha = 0.9) +
  geom_jitter(alpha = 0.1) +
  theme_bw() +
  scale_fill_manual(values = c("#454545", "#96C0B7", "#FFB84D", "#EE6C4D", "#A8D08D")) +
  geom_vline(xintercept = 0, size = 1) +
#  stat_compare_means(method = 'anova') +
#  stat_compare_means(comparisons = list(c("spring", "summer"), c("spring", "autumn"), c("spring", "winter"), 
#                                        c("summer", "autumn"), c("summer", "winter"), c("autumn", "winter")), 
#                     method = "t.test") +
  xlab('Rate of change in LSWT (°C/year)') +
  ylab('Season') +
  theme(legend.position = 'none',
        text = element_text(size = 14)) 
lswt_season

ggsave('./figures/landsat_7/rate_of_change_season.png', lswt_season, 
       dpi = 300, units = 'mm', height = 400, width = 450, scale = 0.3)

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
##################################################################################
##################################################################################
## plot map by season, bring back xy coords
d <- readRDS('./data/lernzmp_lakes_master.rds')
geo <- d$updated %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  select(-char)

sen_geo <- left_join(sen, geo, by = 'LID')

ggplot(sen_geo, aes(x = easting_NZTM, y = northing_NZTM, color = sen_slope)) + 
  geom_point() +
  theme_bw() +
  scale_color_gradient2(
    high = 'red',#brewer.pal(9, "RdBu")[2],   # Cold color
    mid = "#D3D3D3",                    # Midpoint (neutral color)
    low = 'blue', #brewer.pal(9, "RdBu")[8],  # Hot color
    midpoint = 0                      # Set midpoint to 0 for diverging effect
  ) +  #scale_color_viridis_b() +
  facet_wrap(~season)

#################################################################################
# show how lakes change via season spaghetti plot
#install.packages('ggalluvial')
library(ggalluvial)
# break up LSWT trends into cooling or warming
sen <- sen %>% 
  mutate(season_trend_qual = ifelse(sen_slope > 0, 'warm', 'cool'))

ggplot(sen, aes(x = season, stratum = season_trend_qual, alluvium = LID, fill = season_trend_qual, label = season_trend_qual)) +
  geom_flow() +
  geom_stratum(width = 0.6)  +
  scale_fill_manual(values= c('#568EA3', '#B3192B')) +
  geom_text(stat = "flow",
    aes(label = after_stat(ifelse(flow == "to",
        scales::percent(ave(count, x, flow, group, FUN = sum) /ave(count, x, flow, FUN = sum),
                        accuracy = 1),NA)),
        hjust = after_stat(flow) == "to")) +
  theme_bw() +
  labs(fill = 'Trend direction',
       y = 'Number of lakes',
       x = 'Season') 


ggplot(sen, aes(x = season, stratum = season_trend_qual, alluvium = LID, fill = season_trend_qual)) +
  geom_flow(stat = 'alluvium', lode.guidance = 'forward') +
  geom_stratum(width = 0.6)  +
  scale_fill_manual(values= c('#568EA3', '#B3192B')) +
  theme_bw() +
  labs(fill = 'Trend direction',
       y = 'Number of lakes',
       x = 'Season') 

ggplot(sen, aes(x = season, fill = season_trend_qual)) +
  geom_bar(position = 'dodge') +
  scale_fill_manual(values= c('#568EA3', '#B3192B')) +
  theme_bw() +
  ylab('Number of lakes') +
  xlab("") +
  labs(fill = 'Trend')

################################################################################
# calculate which lakes are consistently cooling, warming, or changing
sen_summ <- sen %>% 
  group_by(LID) %>% 
  summarise(n_cool = sum(season_trend_qual=='cool'),
            n_warm = sum(season_trend_qual=='warm'),
            cv = sd(sen_slope)/abs(mean(sen_slope))) %>% 
  mutate(lake_group = case_when(n_cool > 0 & n_warm > 0 ~ 'variable',
                                n_cool > 0 ~ 'always cooling',
                                n_warm > 0 ~ 'always warming'))

cool_warm <- sen_summ %>% 
  count(lake_group) %>% 
  mutate(percent = 100*n/sum(n))

# combine with annual rates and other explanatory variables
# read LSWT output
ann <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv')

# bit of cleaning up
ann <- ann %>% 
  select(LID, sen_slope) %>% 
  mutate(LID = as.character(LID)) %>% 
  rename(annual_trend = sen_slope)

# combine seasonal and annual trends
season_ann <- left_join(sen, ann)

# remove the season = annual
season_ann <- season_ann %>% 
  filter(season!='annual')

a <- ggplot(season_ann, aes(x = sen_slope, y = annual_trend, color = fct_rev(season))) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~season) +
  theme_bw() +
  stat_poly_eq(
    aes(label = ..rr.label.. ),
    formula = y ~ x,
    parse = TRUE,
    color = 'black',
    label.x.npc = "left",  # position inside plot area
    label.y.npc = 0.95) +
  scale_color_manual(values = c("#A8D08D", "#EE6C4D","#FFB84D", "#96C0B7")) +
  geom_abline(slope = 1, intercept = 0) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(color = 'Season') +
  ylab('Annual trend (°C/year)') +
  xlab('Seasonal trend (°C/year)')
a

# make qualitative column for annual trend
season_ann <- season_ann %>% 
  mutate(annual_trend_qual = ifelse(annual_trend > 0, 'warm', 'cool')) %>% 
  mutate(compare = case_when(
    season_trend_qual == "warm" & annual_trend_qual == "warm" ~ "both warm",
    season_trend_qual == "cool" & annual_trend_qual == "cool" ~ "both cool",
    season_trend_qual == "warm" & annual_trend_qual == "cool" ~ "warm season, cool year",
    season_trend_qual == "cool" & annual_trend_qual == "warm" ~ "cool season, warm year",
    TRUE ~ NA_character_  # catches any missing or unmatched cases
  ))

compare_summary <- season_ann %>% 
  filter(!is.na(compare)) %>% 
  group_by(season, compare) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(season) %>%
  mutate(percent = 100 * count / sum(count)) %>%
  ungroup()

compare_summary$compare <- factor(compare_summary$compare, levels = c('cool season, warm year',
                                                                      'both warm', 
                                                                      'both cool',
                                                                      'warm season, cool year'))

b <- ggplot(compare_summary, aes(x = compare, y = percent, fill = season)) +
  geom_col(position = 'dodge') +
  facet_wrap(~compare, scales = 'free') +
  scale_fill_manual(values = c("#A8D08D", "#EE6C4D","#FFB84D", "#96C0B7")) +
  theme_bw() +
  ylim(0, 50) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  xlab('Season')
  
ggarrange(a, b, common.legend = TRUE)


# read in FENZ drivers and keann# read in FENZ drivers and keep the relevant ones
fenz <- read.csv('./data/drivers/FENZ_Lake_Update_2024_25.09.2024.csv')
fenz$LID <- as.character(fenz$LID)
fenz <- fenz %>% 
  select(LID, Name, Geomorphic, NewAreaHa, Region, MaxDepth:SumWind, 
         MeanWind, LakeElev, catArea, catSlope, catAnnTemp, catDecSolR, catJuneSol,
         Lat, Long,  Abell_Secc,Per_Snow, River, Sand)

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

# join together
df <- left_join(ann, fenz)
df <- left_join(df, lernz)
df <- df %>% 
  select(LID, annual_trend, Region, Name, everything())

df <- left_join(df, sen_summ) 
df <- df %>% 
  select(LID, Name, annual_trend, lake_group, cv, everything()) %>% 
  rename(cv_trend = cv)

write.csv(df, './data/output/LSWT_annual_seasonal_trends_predictors.csv', row.names = FALSE)
df %>% 
  filter(cv_trend < 2500) %>% 
ggplot(aes(y = cv_trend, x = lake_group, color = lake_group)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(df, aes(y = annual_trend, x = lake_group, color = lake_group)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(df, aes(y = annual_trend, x = dist_to_shore, color = lake_group)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(df, aes(y = MaxDepth, x = lake_group, color = lake_group)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

df %>% 
  filter(cv_trend < 2500) %>% 
ggplot(aes(y = MaxDepth, x = cv_trend, color = lake_group)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

ggplot(df, aes(y = LakeElev, x = lake_group, color = lake_group)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  theme_bw()

df %>% 
  filter(DecTemp > 1) %>% 
ggplot(aes(y = catAnnTemp, x = lake_group, color = lake_group)) +
  geom_point() +
  theme_bw()

df <- na.omit(df)

# get rid of super high cv value
df <- df %>% 
  filter(cv < 2500)

predictors <- df %>% 
  select(-n_cool, -n_warm) %>% 
  select(where(is.numeric))

pca_result <- prcomp(predictors, center = TRUE, scale. = TRUE)

pca_df <- bind_cols(
  df %>% select(LID, lake_group, cv),
  as.data.frame(pca_result$x)  # adds PC1, PC2, ...
)


ggplot(pca_df, aes(x = PC1, y = PC2, color = lake_group)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(
    title = "PCA of Lake Characteristics",
    x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)")
  )

ggplot(pca_df, aes(x = PC1, y = PC2, color = cv)) +
  geom_point(alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(
    title = "PCA of Lake Characteristics",
    x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)")
  ) +
  scale_color_viridis_b()

loadings <- pca_result$rotation
loadings_df <- as.data.frame(loadings)
loadings_df$variable <- rownames(loadings_df)

ggplot(loadings_df, aes(x = PC1, y = PC2, label = variable)) +
  geom_point() +
  geom_point(data = pca_df, aes(x = PC1, y = PC2, color = lake_group)) +
  geom_text(nudge_y = 0.02) +
  coord_equal() +
  labs(title = "PCA Loadings Plot", x = "PC1", y = "PC2")

library(vegan)

adonis_result <- adonis2(predictors ~ lake_group, data = df, method = "euclidean")
print(adonis_result)

ggplot(df, aes(x = Long, y = Lat, color = lake_group)) +
  geom_point()
