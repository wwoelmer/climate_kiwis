library(pdp)
library(xgboost)
library(caret)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(car)
library(ggpubr)

set.seed(142)

# use drivers df from classification_tree_warming_group script

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
best_nrounds <- cv_results$best_iteration

cat("Optimal number of boosting rounds:", best_nrounds, "\n")

# train final model using the optimal nrounds
bst <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds
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
                              Feature %in% c('LakeElev', 'dist_to_shore') 
                              ~ 'Lake Location',
                              Feature %in% c('sen_rain_summer', 'sen_rain_annual', 'sen_rain_winter',
                                             'min_atemp_autumn', 'sen_atemp_spring', 'sen_atemp_autumn',
                                             'max_rain_autumn') 
                              ~ 'Lake Climate'))

a <- ggplot(importance_matrix, aes(x = Gain, y = Feature)) +
  geom_col() +
  labs(x = "Gain (Importance)",
       y = NULL,
       title = "XGBoost Feature Importance") +
  scale_fill_grey() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))
a

top_vars <- importance_matrix$Feature[1:2]

pred_probs <- matrix(predict(bst, X), ncol = 3, byrow = TRUE)
pred_labels <- max.col(pred_probs) - 1
true_labels <- as.numeric(factor(drivers$group)) - 1
accuracy <- mean(pred_labels == true_labels)
accuracy


xgb.plot.shap(data = X, model = bst, top_n = 7) # this is for teh first class only
shap_values <- predict(bst, newdata = X, predcontrib = TRUE)

shap_df <- as.data.frame(shap_values)
shap_df$lake_row <- 1:nrow(shap_df)
shap_long <- melt(shap_df, id.vars = "lake_row")

# 0: equal
# 1: majority cooling
# 2: majority warming

shap_long <- shap_long %>% 
  mutate(
    group = str_extract(variable, "\\.\\d+$"),   # extract ".1", ".2", etc.
    group = ifelse(is.na(group), "0", str_replace(group, "\\.", "")), # assign "1" if no suffix
    variable = str_replace(variable, "\\.\\d+$", "")  # remove suffix from feature name
  )

# rename the categories to the actual group names
shap_long <- shap_long %>% 
  mutate(group = dplyr::recode(group,
                           "0" = 'Equal warming and cooling',
                           '1' = 'Majority cooling',
                           '2' = 'Majority warming')) %>% 
  rename(SHAP_value = value)


shap_long %>% 
  filter(variable %in% top_vars) %>% 
  ggplot(aes(x = variable, y = SHAP_value)) +
  facet_wrap(~group) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

c <- shap_long %>% 
  filter(variable %in% top_vars) %>% 
  ggplot(aes(x = variable, y = SHAP_value, fill = group)) +
  facet_wrap(~variable, scales = 'free') +
  scale_fill_manual(values = c('#D3D3D3', 'steelblue', 'firebrick')) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

# add back the original values of the drivers
drivers_long <- drivers %>% 
  mutate(lake_row = row_number()) %>% 
  select(lake_row, 
         dist_to_shore:sen_atemp_winter,
         -GeomorphicType) %>% 
         #top_vars, Abell_Secc, mean_rain_annual:mean_rain_winter) %>% 
  pivot_longer(dist_to_shore:sen_atemp_winter, names_to = 'variable', values_to = 'driver_value') 

shap_df2 <- left_join(shap_long, drivers_long, by = c('lake_row', 'variable'))

b <- shap_df2 %>% 
  filter(variable %in% top_vars) %>% 
  ggplot(aes(x = driver_value, y = SHAP_value, color = group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "gam", se = FALSE) +
  facet_wrap(~factor(variable, levels = c('LakeVolume', 'sen_rain_summer', 
                                          'sen_rain_annual', 'Fetch')), 
             scales = "free") +
  theme_minimal() +
  xlab("Feature value") +
  ylab("SHAP value") +
  scale_color_manual(values = c("Equal warming and cooling" = "gray",
                                "Majority cooling" = "steelblue",
                                "Majority warming" = "firebrick")) +
  ggtitle("SHAP values vs Feature values by group") +
  theme(legend.position = 'bottom')
b

shap_df2 %>% 
  filter(variable=='LakeVolume',
         driver_value < 10000000) %>% 
  ggplot(aes(x = driver_value, y = SHAP_value, color = group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "gam", se = FALSE) +
  facet_wrap(~factor(variable, levels = c('LakeVolume', 'sen_rain_summer', 
                                          'sen_rain_annual', 'Fetch')), 
             scales = "free") +
  theme_minimal() +
  xlab("Feature value") +
  ylab("SHAP value") +
  scale_color_manual(values = c("Equal warming and cooling" = "gray",
                                "Majority cooling" = "steelblue",
                                "Majority warming" = "firebrick")) +
  ggtitle("SHAP values vs Feature values by group") 
  

p_drivers <- ggarrange(c, b, labels = 'auto', common.legend = TRUE)
p_drivers
ggarrange(a, p_drivers, ncol = 1)

ggsave('./figures/landsat_7/xgboost_drivers.png', p_drivers,
       dpi = 300, units = 'mm',
       height = 300, width = 500, scale = 0.45)

##############################################################

shap_df2 %>% 
  filter(variable %in% top_vars) %>% 
  mutate(driver_value_scaled = scales::rescale(driver_value, to = c(0,1))) %>% 
  ggplot(aes(x = driver_value_scaled, y = SHAP_value, color = group)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  xlab("Feature value") +
  ylab("SHAP value") +
  scale_color_manual(values = c("Equal warming and cooling" = "gray",
                                "Majority cooling" = "steelblue",
                                "Majority warming" = "firebrick")) +
  ggtitle("SHAP values vs Feature values by group")


shap_df2 %>% 
  filter(variable %in% top_vars) %>% 
  group_by(variable) %>% 
  mutate(driver_value_scaled = scales::rescale(driver_value, to = c(0,1)))  %>% 
  ggplot(aes(y = variable, x = SHAP_value, color = driver_value_scaled)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_vline(xintercept = 0) +
  facet_wrap(~group) +
  theme_bw() +
  scale_color_gradient(low = 'blue', high = 'red') +
  xlab("Feature value") +
  ylab("SHAP value") +
  ggtitle("SHAP values vs Feature values by group")

shap_df2 %>% 
  filter(variable %in% top_vars) %>% 
  group_by(variable) %>% 
  mutate(driver_value_scaled = scales::rescale(driver_value, to = c(0,1)))  %>% 
  ggplot(aes(y = variable, x = SHAP_value, fill = group)) +
  geom_boxplot() +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = c("Equal warming and cooling" = "gray",
                                "Majority cooling" = "steelblue",
                                "Majority warming" = "firebrick")) +
  theme_bw() 


shap_df2 %>% 
  filter(variable %in% top_vars) %>% 
  group_by(variable) %>% 
  mutate(driver_value_scaled = scales::rescale(driver_value, to = c(0,1)))  %>% 
  ggplot(aes(x = driver_value_scaled, y = SHAP_value, color = group)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_point() +
  facet_wrap(~variable, scales = 'free') +
  geom_hline(yintercept = 0) +
  scale_color_manual(values = c("Equal warming and cooling" = "gray",
                               "Majority cooling" = "steelblue",
                               "Majority warming" = "firebrick")) +
  theme_bw() 

shap_df2 %>% 
  filter(variable %in% top_vars) %>% 
  group_by(variable) %>% 
  mutate(driver_value_scaled = scales::rescale(driver_value, to = c(0,1)))  %>% 
  ggplot(aes(x = driver_value_scaled, y = SHAP_value, color = variable)) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_point() +
  facet_wrap(~group, scales = 'free') +
  geom_hline(yintercept = 0) +  
  theme_bw() 


## see if there are interactions with the max atemp winter vaariable
winter_df <- shap_long %>% 
  filter(variable=='max_atemp_winter') %>% 
  dplyr::rename('SHAP_max_winter_atemp' = 'SHAP_value') %>% 
  select(-variable)

winter_df <- left_join(winter_df, drivers_long, by = c('lake_row'))

winter_df <- winter_df %>% 
  filter(!is.na(driver_value)) %>% 
  pivot_wider(names_from = 'variable', values_from = 'driver_value')

winter_df %>% 
  ggplot(aes(x = max_atemp_winter, y = SHAP_max_winter_atemp, 
             color = sen_atemp_winter)) +
  geom_point() +
  scale_color_viridis_c(option = "D") +
  facet_wrap(~group) +
  geom_hline(yintercept = 0) +  
  theme_bw() 

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

bst_split <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 200,
  watchlist = watchlist,
  early_stopping_rounds = 10,
  verbose = 1
)

# Accuracy on train data
pred_probs <- matrix(predict(bst_split, X_train), ncol = num_class, byrow = TRUE)
pred_labels <- max.col(pred_probs) - 1
accuracy <- mean(pred_labels == y_train)
cat("Train Accuracy:", accuracy, "\n")


# Accuracy on test data
pred_probs <- matrix(predict(bst_split, X_test), ncol = num_class, byrow = TRUE)
pred_labels <- max.col(pred_probs) - 1
accuracy <- mean(pred_labels == y_test)
cat("Test Accuracy:", accuracy, "\n")
