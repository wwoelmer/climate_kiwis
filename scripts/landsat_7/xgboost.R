library(pdp)
library(xgboost)
library(caret)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(car)
library(ggpubr)
library(sf)
library(aemetools)

set.seed(142)

# organize driver data
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

# convert categorical response to numeric
label <- as.numeric(factor(drivers$group)) - 1  # xgboost labels start at 0

# all potential predictors
pot_predictors <- c("dist_to_shore", "NewAreaHa", "MaxDepth", "LakeVolume", 
                    "LakeElev", "Fetch", "Abell_Secc", 
                    "mean_rain_annual",
                    "mean_atemp_annual", "min_atemp_annual", 
                    "min_atemp_autumn", "min_atemp_spring",
                    "min_atemp_summer", "min_atemp_winter",
                    "max_rain_annual", "max_rain_autumn",
                    "max_rain_spring", "max_rain_summer",
                    "max_rain_winter", "max_atemp_annual", # collinear with "max_atemp_summer",
                    "max_atemp_autumn", "max_atemp_spring",
                     "max_atemp_winter",
                    "sen_rain_annual",  "sen_rain_autumn",
                    "sen_rain_spring",   "sen_rain_summer",
                    "sen_rain_winter",   "sen_atemp_annual",
                    "sen_atemp_autumn",  "sen_atemp_spring",
                    "sen_atemp_summer", "sen_atemp_winter" )

# variables removed after VIF inspection to reduce VIF < 10:  "min_atemp_annual",
# mean_atemp_annual, min_atemp_spring, min_atemp_summer, max_rain_winter, 
# mean_rain_annual, max_rain_annual, max_atemp_autumn, max_atemp_winter,
# sen_atemp_annual, max_atemp_annual, NewAreaHa, max_rain_spring

pot_predictors <- c("dist_to_shore", "MaxDepth", "LakeVolume", 
                    "LakeElev", "Fetch", "Abell_Secc", 
                   # "min_atemp_autumn", 
                    "max_rain_autumn",
                    #"max_rain_summer",
                    #"max_atemp_spring",
                    "sen_rain_annual", # "sen_rain_autumn",
                    "sen_rain_spring",   "sen_rain_summer",
                    "sen_rain_winter",
                    "sen_atemp_autumn",  "sen_atemp_spring",
                    "sen_atemp_summer", "sen_atemp_winter")

# calculate variance inflation factor for remove covariates
fmla <- as.formula(paste("rnorm(nrow(drivers)) ~", paste(pot_predictors, collapse = " + ")))
lm_predictors <- lm(fmla, data = drivers)
vif_vals <- vif(lm_predictors)
vif_sorted <- sort(vif_vals, decreasing = TRUE)
print(vif_sorted)

############## set up xgboost #######################
# predictors matrix
X <- as.matrix(drivers[, pot_predictors])

# set multiclass parameters
num_class <- length(unique(label))

# convert to DMatrix
dtrain <- xgb.DMatrix(data = X, label = label)

# parameters for xgboost
params <- list(
  objective = "multi:softprob",   # multiclass classification
  eval_metric = "mlogloss",
  num_class = num_class,
  eta = 0.08,                      # learning rate
  max_depth = 6,                  # max tree depth
  subsample = 0.8,                 # 80% of training rows per boosting round
  colsample_bytree = 0.8           # 80% of features per tree
)

# run 10-fold cross validation to find optimal number of rounds
cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,       # upper bound, let early stopping decide
  nfold = 10,
  stratified = TRUE,    # stratify folds by class labels
  showsd = TRUE,
  prediction = TRUE,
  early_stopping_rounds = 20,  # stop if no improvement
  verbose = 1
)

# best number of rounds
best_iter <- cv_results$evaluation_log$iter[
  which.min(cv_results$evaluation_log$test_mlogloss_mean)
]
best_iter


cat("Optimal number of boosting rounds:", best_iter, "\n")

# train final model using the optimal nrounds
bst <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_iter
)


pred_probs <- predict(bst, X)  # probabilities for each class
pred_matrix <- matrix(pred_probs, ncol = num_class, byrow = TRUE)
pred_labels <- max.col(pred_matrix) - 1  # predicted class indices

# convert back to original class names
pred_class <- factor(pred_labels, labels = levels(factor(drivers$group)))
head(pred_class)

# Get feature names
feature_names <- colnames(X)

# Compute variable importance
importance_matrix <- xgb.importance(feature_names = feature_names, model = bst)

# make plot
importance_matrix <- importance_matrix %>% 
  mutate(Feature = fct_reorder(Feature, Gain)) %>% 
  slice_max(Gain, n = 10) %>% 
  mutate(category = case_when(Feature %in% c('LakeVolume', 'Fetch', 'MaxDepth') 
                              ~ 'Lake Morphometry',
                              Feature %in% c('Abell_Secc')
                              ~ 'Lake WQ',
                              Feature %in% c('LakeElev', 'dist_to_shore') 
                              ~ 'Lake Location',
                              Feature %in% c('sen_rain_summer', 'sen_rain_annual', 'sen_rain_winter',
                                             'min_atemp_autumn', 'sen_atemp_spring', 'sen_atemp_autumn',
                                             'max_rain_autumn') 
                              ~ 'Lake Climate'))

importance_matrix$Feature <- factor(importance_matrix$Feature,
                                    levels = c('LakeVolume',
                                               'sen_rain_summer',
                                               'sen_rain_annual',
                                               'LakeElev',
                                               'MaxDepth',
                                               'Fetch',
                                               'max_rain_autumn',
                                               'sen_atemp_autumn',
                                               'sen_rain_winter',
                                               'dist_to_shore',
                                               'Abell_Secc'),
                                    labels = c('Lake volume',
                                               'Summer rain trend',
                                               'Annual rain trend',
                                               'Lake elevation',
                                               'Max depth',
                                               'Lake fetch',
                                               'Max autumn rain',
                                               'Autumn airtemp trend',
                                               'Winter rain trend',
                                               'Distance from ocean',
                                               'Secchi depth'))

a <- ggplot(importance_matrix, aes(x = Gain, y = fct_reorder(Feature, Gain), fill = category)) +
  geom_col() +
  labs(x = "Gain (Importance)",
       y = NULL) +
  scale_fill_grey() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))
a

ggsave('./figures/landsat_7/xgboost_feature_importance.png', a, 
       dpi = 300, units = 'mm', height = 300, width = 300, scale = 0.5)

top_vars <- importance_matrix$Feature[1:3]

shap_values <- predict(bst, newdata = X, predcontrib = TRUE)
shap_df <- as.data.frame(shap_values)
shap_df$lake_row <- 1:nrow(shap_df)
shap_long <- melt(shap_df, id.vars = "lake_row")

# 0: equal
# 1: majority cooling
# 2: majority warming

shap_long <- shap_long %>% 
  mutate(
    group = str_extract(variable, "^\\d+"),            # extract leading digits
    group = ifelse(is.na(group), "0", group),         # fallback if no number
    variable = str_replace(variable, "^\\d+\\.", "")  # remove leading digits + dot
  )

# rename the categories to the actual group names
shap_long <- shap_long %>% 
  mutate(group = dplyr::recode(group,
                           "1" = 'Equal warming and cooling',
                           '2' = 'Majority cooling',
                           '3' = 'Majority warming')) %>% 
  rename(SHAP_value = value)


shap_long %>% 
  #filter(variable %in% top_vars) %>% 
  ggplot(aes(x = variable, y = SHAP_value)) +
  facet_wrap(~group) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


c <- shap_long %>% 
  filter(variable %in% c('LakeVolume', 'sen_rain_summer', 'LakeElev')) %>% 
  mutate(variable = factor(variable, levels = c('LakeVolume', 'sen_rain_summer', 'LakeElev'),
                           labels = c('Lake volume', 'Summer rain trend', 'Lake elevation'))) %>% 
  ggplot(aes(x = variable, y = SHAP_value, fill = group)) +
  facet_wrap(~variable, scales = 'free') +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()
c

# add back the original values of the drivers
drivers_long <- drivers %>% 
  mutate(lake_row = row_number()) %>% 
  select(lake_row, LID, Lat, Long,
         dist_to_shore:sen_atemp_winter,
         -GeomorphicType) %>% 
         #top_vars, Abell_Secc, mean_rain_annual:mean_rain_winter) %>% 
  pivot_longer(dist_to_shore:sen_atemp_winter, names_to = 'variable', values_to = 'driver_value') 

shap_df2 <- left_join(shap_long, drivers_long, by = c('lake_row', 'variable'))

b <- shap_df2 %>% 
  filter(variable %in% c('LakeVolume', 'sen_rain_summer', 'LakeElev')) %>% 
  mutate(variable = factor(variable, levels = c('LakeVolume', 'sen_rain_summer', 'LakeElev'),
                           labels = c('Lake volume', 'Summer rain trend', 'Lake elevation'))) %>% 
  ggplot(aes(x = driver_value, y = SHAP_value, color = group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "gam", se = FALSE) +
  facet_wrap(~variable, 
             scales = "free") +
  theme_minimal() +
  xlab("Feature value") +
  ylab("SHAP value") +
  scale_color_manual(values = c("Equal warming and cooling" = "gray",
                                "Majority cooling" = "steelblue",
                                "Majority warming" = "firebrick")) +
  theme(legend.position = 'bottom')
b

p_drivers <- ggarrange(c, b, labels = 'auto', common.legend = TRUE, nrow = 2)
p_drivers
ggsave('./figures/landsat_7/xgboost_drivers_shap.png', p_drivers,
       dpi = 300, units = 'mm',
       height = 275, width = 475, scale = 0.4)

###############################################################################
# some SI figs
# Transform to WGS84 (latitude/longitude)
df_gps <- shap_df2 %>% 
  filter(!is.na(Lat)) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326)

# get country shapefile
# read from LINZ
url <- "https://data.linz.govt.nz/"
layer_id <- 51560
key <- Sys.getenv("LINZ_API_KEY") # LINZ API key

# Read the shapefile data
nz_shapefile <- read_web_sf(url = url, layer_id = layer_id, key = key)

shap_map <- ggplot() +
  geom_sf(data = nz_shapefile, fill = "darkgrey", color = "black") +
  geom_sf(
    data = df_gps %>%
      filter(variable %in% c("LakeVolume", "sen_rain_summer", "LakeElev")) %>% 
      mutate(variable = factor(variable, levels = c('LakeVolume', 'sen_rain_summer', 'LakeElev'),
                               labels = c('Lake volume', 'Summer rain trend', 'Lake elevation'))),
    aes(color = SHAP_value),
    alpha = 0.6
  ) +
  facet_wrap(variable ~ group) +
  scale_color_gradient2(
    high = "#ca0020",
    mid = "white",
    low = "#00316E",
    midpoint = 0
  ) +
  theme_bw() +
  labs(color = "SHAP value") +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(size = "none") +
  theme(
    text = element_text(size = 12),
    legend.position = "left",
    legend.direction = "vertical",
    legend.box = "vertical"
  )
shap_map

ggsave('./figures/landsat_7/shap_map_top3.png', shap_map,
       dpi = 300, units = 'mm',
       height = 600, width = 500, scale = 0.45)

###################################################################################
p_low_vol <- shap_df2 %>% 
  filter(variable=='LakeVolume',
         driver_value < 10000000) %>% 
  mutate(variable = factor(variable, levels = c('LakeVolume', 'sen_rain_summer'),
                           labels = c('Lake volume', 'Summer rain trend'))) %>% 
  ggplot(aes(x = driver_value, y = SHAP_value, color = group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "gam", se = FALSE) +
  theme_minimal() +
  xlab("Feature value") +
  ylab("SHAP value") +
  ggtitle('Lake volume < 10,000,000') +
  scale_color_manual(values = c("Equal warming and cooling" = "gray",
                                "Majority cooling" = "steelblue",
                                "Majority warming" = "firebrick"))
p_low_vol

ggsave('./figures/landsat_7/shap_vol_lower_end.png', p_low_vol,
       dpi = 300, units = 'mm',
       height = 300, width = 400, scale = 0.45)

ggplotly(shap_df2 %>%
           dplyr::rename(feature = variable) %>%
           dplyr::filter(feature %in% c("LakeElev"),
                         group == "Majority warming") %>%
           ggplot(aes(x = driver_value, y = SHAP_value, color = group)) +
           geom_point(alpha = 0.6, size = 2) +
           geom_hline(yintercept = 0) +
           facet_wrap(~feature, scales = "free") +
           theme_minimal() +
           xlab("Feature value") +
           ylab("SHAP value") +
           scale_color_manual(values = c(
             "Equal warming and cooling" = "gray",
             "Majority cooling" = "steelblue",
             "Majority warming" = "firebrick"
           )) +
           theme(legend.position = "bottom"))
  
#################################################################################

# do a train/test for accuracy calculations
n <- nrow(X)
train_idx <- sample(seq_len(n), size = 0.7 * n)

X_train <- X[train_idx, ]
X_test  <- X[-train_idx, ]
y_train <- label[train_idx]
y_test  <- label[-train_idx]

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test, label = y_test)

watchlist <- list(train = dtrain, eval = dtest)

# Train model
bst_split <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 1000,
  watchlist = watchlist,
  early_stopping_rounds = 20,
  verbose = 1
)

# Predict
pred_probs <- predict(bst_split, dtrain)
pred_matrix <- matrix(pred_probs, ncol = num_class, byrow = TRUE)
pred_labels <- max.col(pred_probs) - 1
accuracy <- mean(pred_labels == y_train)
cat("Train Accuracy:", accuracy, "\n")


# Accuracy on test data
pred_probs <- predict(bst_split, dtest)
pred_matrix <- matrix(pred_probs, ncol = num_class, byrow = TRUE)
pred_labels <- max.col(pred_probs) - 1
accuracy <- mean(pred_labels == y_test)
cat("Test Accuracy:", accuracy, "\n")
