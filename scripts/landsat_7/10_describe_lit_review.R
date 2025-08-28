# calculate some summary/descriptive stats from lit review
library(tidyverse)
library(aemetools)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(scales)
library(MetBrewer)
library(viridis)
library(ggpubr)

gps <- read.csv('./data/LSWT_rates_of_change_literature.csv',
                fileEncoding = 'latin1')

gps$rate_C_year <- as.numeric(gps$rate_C_year)

# get rid of this study on the map
gps <- gps %>% 
  filter(citation_short!='This study')

mean(gps$rate_C_year, na.rm = TRUE)
sum(gps$n_lakes, na.rm = TRUE)

# change the spatial extent so it's either point, region, or global
gps <- gps %>% 
  mutate(spatial_extent = recode(spatial_extent,
                                 'country' = 'region'))

gps$spatial_extent <- factor(gps$spatial_extent, levels = c('point', 'region', 'global'))
gps <- gps %>% 
  filter(!is.na(rate_C_year))

unique_study <- gps %>% 
  distinct(citation, .keep_all = TRUE)

table(unique_study$spatial_extent)
table(gps$location)
table(gps$method_LSWT)
table(gps$method_LSWT_standard)
table(gps$method_trend)
table(gps$aggregation_mean_min_max_etc)
table(gps$temporal_aggregation)
max(gps$n_lakes)
median(gps$n_lakes)
median(gps$n_years)
min(gps$n_years)
max(gps$n_years)
min(gps$rate_C_year)
max(gps$rate_C_year)

## night time estimates
night <- gps %>% 
  filter(str_detect(annual_seasonal_data, "night")) %>% 
  distinct(citation)
nrow(night)



# filter to lakes that we can directly compare
sub <- gps %>% 
  filter(annual_seasonal_data=='annual',
         aggregation_mean_min_max_etc=='mean')


########################################
## below is for formatting all studies cites, still a working example, not real data
library(stringi)

lit <- read.csv('./lit_review_example.csv')
lit <- lit %>% 
  mutate(
    authors = str_extract(citation, "^(.*?)\\(\\d{4}\\)") %>% 
      str_remove("\\(\\d{4}\\)") %>% str_trim(),
    year    = str_extract(citation, "\\(\\d{4}\\)") %>% str_remove_all("[()]"),
    title   = str_match(citation, "\\(\\d{4}\\)\\.\\s*(.*?)\\.\\s*[^.]+,")[,2] %>% str_trim(),
    journal = str_match(citation, "\\.\\s*([^.,]+),\\s*\\d")[,2] %>% str_trim()
  )

dups <- lit %>% 
  distinct(title, year, .keep_all = TRUE)

screened <- read.csv('./data/LSWT_rates_of_change_literature.csv')
screened <- screened %>% 
  select(paper_source:journal) %>% 
  filter(citation_short!='This study') %>% 
  mutate(across(where(is.character), ~ stri_encode(., from = "latin1", to = "UTF-8")))

screened_format <- screened %>%  
  mutate(
    authors = str_extract(citation, "^(.*?)\\(\\d{4}\\)") %>% 
      str_remove("\\(\\d{4}\\)") %>% str_trim(),
    year    = str_extract(citation, "\\(\\d{4}\\)") %>% str_remove_all("[()]"),
    title   = str_match(citation, "\\(\\d{4}\\)\\.\\s*(.*?)\\.\\s*[^.]+,")[,2] %>% str_trim(),
    journal2 = str_match(citation, "\\.\\s*([^.,]+),\\s*\\d")[,2] %>% str_trim()
  )

screened_format <- screened_format %>% 
  mutate(journal = na_if(journal, "")) %>% 
  mutate(journal = ifelse(!is.na(journal), journal, journal2)) %>% 
  select(-journal2, -paper_source)

screened_clean <- screened_format %>% 
  distinct(year, authors, title, .keep_all = TRUE)

# the number of studies which remained after screening
sum(dups$citation %in% screened_clean$citation)
# so 17 of the studies from my original list were kept