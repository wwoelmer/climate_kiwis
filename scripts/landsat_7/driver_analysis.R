# gather explanatory variables for driver analysis
library(tidyverse)
library(mgcv)
library(Hmisc)
library(corrplot)
library(vip)
library(tidymodels)
library(vip)


# morphometry: max depth, latitude, altitude (all from LERNZmp)
# physical mixing: estimated thermocline depth using Davies-Colley 1988 eq.  T = 9.52*fetch^0.425
# water quality: TLI, Secchi estimated from Abell et al. (reason to include TN, TP, and/or chl-a?)
# climate: mean air temp, precip, wind over time period of LSWT measurements; rate of change in air temp, precip, wind

##################################################################################
# gather morphometry variables
bgo <- read.csv('./data/drivers/lakes_table_for_analysis_prepared_V3.csv')

# select relevant variables
bgo <- bgo %>% 
  select(LID, Name, lat, MaxDepth, LakeElevat, Fetch_sqrt) %>% 
  mutate(fetch = Fetch_sqrt^2) %>% 
  rename(name_bgo = Name,
         maxdepth_bgo = MaxDepth,
         elevation = LakeElevat) %>% 
  select(-Fetch_sqrt)

bgo$LID <- as.character(bgo$LID)

# some data are in the lernz master file
lernz <- readRDS('./data/lernzmp_lakes_master.rds')
lernz <- as.data.frame(lernz$updated)

# modify the LID col
lernz <- lernz %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  select(-char)

# select relevant variables
lernz <- lernz %>% 
  select(LID, name_fenz, area:northing_NZTM, depth_measured, max_depth, mean_depth, dist_to_shore) %>% 
  rename(maxdepth_lernz = max_depth)

# compare max depths between the two
vars <- full_join(lernz, bgo)

ggplot(vars, aes(x = maxdepth_lernz, y = maxdepth_bgo)) +
  geom_point()

## not sure which depth to go with, so pick one for now and will need to update, potentially with predictions from Tadhg's bathy paper
vars <- vars %>% 
  select(LID, name_fenz, northing_NZTM, easting_NZTM, area, maxdepth_bgo, lat, elevation, fetch, dist_to_shore)

ggplot(vars, aes(x = fetch)) +
  geom_density()

################################################################################
# calculate mixing depth based on fetch

vars <- vars %>% 
  mutate(area_km2 = area/1000000,
         fetch_km2 = sqrt(area_km2))

vars <- vars %>% 
  mutate(mix_depth = 9.52*fetch_km2^0.425)

ggplot(vars, aes(y = northing_NZTM, x = easting_NZTM, color = mix_depth)) +
  geom_point()

ggplot(vars, aes(x = mix_depth)) +
  geom_density()

################################################################################
# read in trophic state data
ts <- read.csv('./data/drivers/tnzm_a_1726974_sm7186.csv')

ts <- ts %>% 
  filter(State=='Current') %>% 
  select(LID, Type, PredictedValue)

ts$LID <- as.character(ts$LID)

ts_wide <- ts %>%
  pivot_wider(names_from = Type, values_from = PredictedValue) %>% 
  rename(SecchiDepth = 'Secchi Depth',
         TP = 'Total Phosphorus',
         TN = 'Total Nitrogen',
         chl_a = 'Chlorophyll a')

# combine with morphometry
vars <- full_join(vars, ts_wide)

ggplot(vars, aes(y = northing_NZTM, x = easting_NZTM, color = TLI)) +
  geom_point() 

table(!is.na(vars$TLI))

################################################################################
# read in LSWT trends
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv')
sen$LID <- as.character(sen$LID)

sen <- sen %>% 
  select(LID, district, sen_slope)

################################################################################
# read in climate data
temp <- read.csv('./data/average-annual-temperature-trends-1972-2022.csv')
temp <- temp %>% 
  #separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(district = site) %>% 
  rename(slope_temp = slope_decade) %>% 
  group_by(district) %>% 
  mutate(slope_temp = mean(slope_temp)) %>% 
  distinct(district, .keep_all = TRUE) %>% 
  select(district, slope_temp)

# read in seasonal statistical values for air temp
#temp_stats <- read.csv('./data/drivers/seasonal_temperature_statistics.csv') %>% 
#  select(-lat, -lon)
#temp <- full_join(temp, temp_stats)

rain <- read.csv('./data/annual-maximum-one-day-rainfall-trends-1960-2022.csv') %>% 
  #separate(site, into = c('city', 'region'), sep = " \\(|\\)", remove = TRUE) %>% 
  rename(district = site) %>% 
  rename(slope_rain = slope_decade) %>% 
  group_by(district) %>% 
  mutate(slope_rain = mean(slope_rain)) %>% 
  distinct(district, .keep_all = TRUE)%>% 
  select(district, slope_rain)


df <- left_join(sen, temp, by = 'district')
df <- left_join(df, rain, by = 'district')

################################################################################
# combine with LSWT trends

vars2 <- full_join(vars, df, by = 'LID')

# get rid of lakes without sen slopes
vars2 <- vars2 %>% 
  filter(!is.na(sen_slope)) %>% 
  filter(!is.na(TLI))
# leaves us with 269 lakes

# make correlation matrix and remove correlated vars
cor_df <- vars2 %>% 
  select(-c(district, LID, name_fenz)) %>% 
  select(sen_slope, everything())

cor_out <- rcorr(as.matrix(cor_df), type = 'spearman') # default is pearson
corrplot(cor_out$r, type = 'upper', method = 'number',
          sig.level = 0.01, insig = 'blank', p.mat = cor_out$P)
round(cor_out$r[1,],2)

################################################################################
# do GAM modelling after removing variables that are highly correlated?
# or do GAM modelling on top variables from RF to look at relationships?

vars_filt <- cor_df %>% 
  select(-c(lat, area, fetch,
            TP, TN, chl_a, SecchiDepth, maxdepth_bgo))

cor_out <- rcorr(as.matrix(vars_filt), type = 'spearman') # default is pearson
corrplot(cor_out$r, type = 'upper', method = 'number',
         sig.level = 0.01, insig = 'blank', p.mat = cor_out$P)

# now subset vars to get rid of the selected correlated variables
vars_mod <- vars2 %>% 
  select(-c(lat, mix_depth, fetch,
            TP, TN, chl_a, TLI, maxdepth_bgo, area,
            northing_NZTM, easting_NZTM, area_km2))

#format district as a factor
vars_mod$district <- as.factor(vars_mod$district)

drivers <- setdiff(names(vars_mod), c('sen_slope', 'LID', 'name_fenz', 'district'))
drivers

formula <- as.formula(paste("sen_slope ~", paste(paste0("s(", drivers, ")"), collapse = " + ")))
formula_rf <- as.formula(paste("sen_slope ~", paste0(paste(paste0("s(", drivers, ")"), collapse = " + "), " + s(district, bs = 're')")))

formula
formula_rf

gam_model <- gam(formula, data = vars_mod, method = 'REML', select = TRUE)
gam_model_rf <- gam(formula_rf, 
                    data = vars_mod, method = 'REML', select = TRUE)

AIC(gam_model, gam_model_rf)

summary(gam_model)
summary(gam_model_rf)

plot(gam_model)
plot(gam_model_rf)

gam.check(gam_model)
gam.check(gam_model_rf)

install.packages('visreg')
library(visreg)
# Setting some ggplot style here
ggstyle <- theme_classic()+
  theme(axis.text = element_text(color = "black"))

# Covariates of model m1
p1 <- visreg(gam_model_rf, "fetch_km2", gg = TRUE, plot = TRUE) +
  ggstyle 

p2 <- visreg(gam_model_rf, "SecchiDepth", gg = TRUE, plot = TRUE)+
  ggstyle

# Covariates of model m2
p3 <- visreg(gam_model_rf, "slope_temp", gg = TRUE, plot = TRUE)+
  ggstyle 

p4 <- visreg(gam_model_rf, "dist_to_shore", gg = TRUE, plot = TRUE)+
  ggstyle 


# Aggregate plots
grid.arrange(arrangeGrob(p1, p2, p3, p4, ncol = 2,
                         left = textGrob("Global Y-axis Label", rot = 90, vjust = 1)))

p1 <- visreg(gam_model_rf, "SecchiDepth", gg = TRUE, plot = FALSE)
ggplot()+
  geom_point(data = p1$res, aes(SecchiDepth, visregRes), shape = 21, size = 3, fill = "#2ca25f")+
  geom_line(data = p1$fit, aes(SecchiDepth, visregFit))+
  geom_line(data = p1$fit, aes(SecchiDepth, visregLwr), linetype = 3)+
  geom_line(data = p1$fit, aes(SecchiDepth, visregUpr), linetype = 3)+
  theme_classic()

################################################################################
################################################################################
# do random forest modelling

# Set a seed for reproducibility
set.seed(123)

# get rid of vars that aren't predictors
vars_rf <- vars2 %>% 
  select(-c(district, LID, name_fenz, lat, area, fetch))

# Split the data into training (70%) and testing (30%) sets
split <- initial_split(vars_rf, prop = 0.7)
trainData <- training(split)
testData <- testing(split)

# Define the random forest model
rfModel <- rand_forest(mtry = 2, trees = 500, min_n = 5) %>%
  set_engine("ranger", importance = "permutation", num.threads = 1) %>%
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

