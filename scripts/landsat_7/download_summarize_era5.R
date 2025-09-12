# get ERA5 drivers
#remotes::install_github("limnotrack/AEME")
#remotes::install_github("limnotrack/aemetools")
library(aemetools)
library(tidyverse)
library(sf)
library(trend)
# download ERA5 for each lake from 2000-2023

# read LSWT output
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts_landsat7.csv') %>% 
  filter(!is.na(district)) %>% 
  select(LID, sen_slope, sen_signif)
sen$LID <- as.character(sen$LID)

# read lake x-y locations to match with output
d <- readRDS('./data/lernzmp_lakes_master.rds')
d2 <- d$updated 
d2 <- d2 %>% 
  dplyr::select(id_final, name_fenz, easting_NZTM, northing_NZTM) %>% 
  rename(LID = id_final) %>% 
  separate(LID, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID),
         char=='LID') %>% 
  dplyr::select(-char)

df <- left_join(sen, d2, by = 'LID')

# format easting and northing into lat/long
df <- df %>% 
  sf::st_as_sf(coords = c("easting_NZTM", "northing_NZTM"), crs = 2193)   # NZGD2000 / New Zealand Transverse Mercator 2000

# Transform to WGS84 (latitude/longitude)
sf_data_latlon <- st_transform(df, crs = 4326)

# Add lat/lon columns to the original dataframe
df$lat <- st_coordinates(sf_data_latlon)[, 2]
df$lon <- st_coordinates(sf_data_latlon)[, 1]

years <- 2000:2023 # match years of landsat 7 data
vars <- c("MET_tmpair", "MET_pprain")

out <- NULL
for(i in 1:nrow(df)){
  print(i)
  
  # subset the appropriate row to get lat and long
  lon <- df$lon[i]
  lat <- df$lat[i]
  
  met <- get_era5_land_point_nz(lat = lat, lon = lon, years = 2000:2023,
                                vars = vars, api_key = "lernzmp_lakes")
  
  # put in long format 
  met_long <- met %>% 
    pivot_longer(vars, names_to = 'variable', values_to = 'value')
  
  # calculate mean, min, max annual temperature from 2000-2023
  # calculate trend in annual mean temperature from 2000-2023
  met_annual <- met_long %>% 
    mutate(year = year(Date)) %>% 
    group_by(year, variable) %>%
    summarise(mean_year = mean(value),
              min_year = min(value),
              max_year = max(value)) %>% 
    group_by(variable) %>% 
    summarise(sen_annual = sens.slope(mean_year)$estimates,
              mean_annual = mean(mean_year),
              min_annual = min(min_year),
              max_annual = max(max_year))%>% 
    mutate(LID = df$LID[i]) %>% 
    mutate(season = 'annual') %>% 
    rename(sen_season = sen_annual,  # rename these for merge with seasonal df
           mean_season = mean_annual,
           min_season = min_annual,
           max_season = max_annual)
  
  # do the same but for four seasons
  met_seasons <- met_long %>% 
    mutate(year = year(Date),
           season = case_when(month(Date) %in% c(12, 1, 2) ~ 'summer',
                              month(Date) %in% c(3, 4, 5) ~ 'autumn',
                              month(Date) %in% c(6, 7, 8) ~ 'winter',
                              month(Date) %in% c(9, 10, 11) ~ 'spring')) %>% 
    group_by(year, season, variable) %>%
    summarise(mean_season_year = mean(value),
              min_season_year = min(value),
              max_season_year = max(value)) %>% 
    group_by(variable, season) %>% 
    summarise(sen_season = sens.slope(mean_season_year)$estimates,
              mean_season = mean(mean_season_year),
              min_season = min(min_season_year),
              max_season = max(max_season_year)) %>% 
    mutate(LID = df$LID[i])
  
  # join and reorder df
  summ_stats <- full_join(met_annual, met_seasons) %>% 
    select(LID, variable, season, mean_season, min_season, max_season, sen_season)
  
  out <- rbind(out, summ_stats)
  
}

write.csv(out, './data/output/era5_met_summary_stats_trends.csv', row.names = FALSE)


