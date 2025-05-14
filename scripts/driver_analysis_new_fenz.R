# try driver analysis with new FENZ data
library(tidyverse)
library(Hmisc)
library(corrplot)
library(tidymodels)
library(vip)

# read in LSWT trends
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv')
sen$LID <- as.character(sen$LID)

sen <- sen %>% 
  select(LID, district, sen_slope)

# read in FENZ drivers
fenz <- read.csv('./data/drivers/FENZ_Lake_Update_2024_25.09.2024.csv')
fenz$LID <- as.character(fenz$LID)

# join together
df <- left_join(sen, fenz)
df <- df %>% 
  select(LID, district, Region, Name, everything())

# get rid of a few columns that aren't relevant for drivers
df <- df %>% 
  select(-c(t50_fid, LCDB_UID, ArtInfo, Primary:Current5, WoniUnit,
            WoniProvin))

# get rid of drivers which are overlapping (e.g., MfE TP and Abell TP)

# get rid of upper and low CI
df <- df %>% 
  select(-c(Ab_TLI:Ab_Sec_Upp))

df <- df %>% 
  select(sen_slope, everything())

cor_df <- na.omit(df)
cor_df <- cor_df %>% 
  select(-c(LID, district, Region, Name, Geomorphic, SBTI_Troph, Wetland, Ephemeral,
            RegionalRa, RegionalCu))



cor_df <- cor_df[complete.cases(cor_df), ] # drop rows with any NA
cor_out <- rcorr(as.matrix(cor_df), type = 'spearman') # default is pearson

cor_out$r

# Get row name of interest (first row)
first_var <- rownames(cor_out$r)[1]

# Get correlation and p-value rows
r_row <- cor_out$r[first_var, ]
p_row <- cor_out$P[first_var, ]

# Logical condition
selected_vars <- names(r_row)[r_row > 0.15 & p_row < 0.05 & names(r_row) != first_var]
selected_vars <- na.omit(selected_vars)
selected_vars

cor_selected_vars <- cor_df %>% 
  select(sen_slope, selected_vars)

cor_selected_vars <- cor_df %>% 
  select(sen_slope, selected_vars) %>% 
  select(sen_slope, NewAreaHa, MaxDepth, ClarityPro, catFlow, 
         Indi_For)

selected_cor_out <- rcorr(as.matrix(cor_selected_vars), type = 'spearman') # default is pearson
corrplot(selected_cor_out$r, type = 'upper', method = 'number',
         sig.level = 0.01, insig = 'blank', p.mat = selected_cor_out$P,
         number.cex = 0.7)         


# Set a seed for reproducibility
set.seed(123)

# get rid of vars that aren't predictors
vars_rf <- df %>% 
  select(-c(LID, district, Region, Name, Geomorphic, SBTI_Troph, Wetland, Ephemeral))

# Split the data into training (70%) and testing (30%) sets
split <- initial_split(vars_rf, prop = 0.7)
trainData <- training(split)
testData <- testing(split)

# Define the random forest model
rfModel <- rand_forest(mtry = 4, trees = 500, min_n = 5) %>%
  set_engine("ranger", importance = "impurity", num.threads = 1) %>%
  set_mode("regression")

# Create a recipe for preprocessing
recipe <- recipe(sen_slope ~ ., data = trainData) %>%
  step_normalize(all_predictors()) 


# Create a workflow
workflow <- workflow() %>%
  add_model(rfModel) %>%
  add_recipe(recipe)

# Fit the model to the training data
fit <- fit(workflow, data = trainData)
fit
vip(fit, num_features = 20L)

# Make predictions on the test data
predictions <- predict(fit, testData) %>%
  bind_cols(testData)

# Evaluate the model's performance
metrics <- predictions %>%
  metrics(truth = sen_slope, estimate = .pred)

metrics
