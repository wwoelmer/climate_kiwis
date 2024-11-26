# add lat long to the remote sensing lakes
library(broom)
library(tidyverse)
#install.packages('trend')
library(trend)
library(ggridges)
library(ggpubr)
library(scales)
library(RColorBrewer)

d <- readRDS('./data/lernzmp_lakes_master.rds')
d2 <- d$updated 
d2 <- d2 %>% 
  select(id_final:northing_NZTM, name_fenz, area_best, max_depth, mean_depth, GeomorphicType) %>% 
  rename(lake_id = id_final)

# select geomorphic characteristics
geo <- d$updated %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  select(-char)

rstemp <- readRDS('./data/lernzmp_lakes_obs_rs.rds')

data <- left_join(rstemp, d2, by = 'lake_id')

# match with StatsNZ sites
district <- read.csv('./data/lake_districts.csv')
data <- left_join(data, district, by = 'lake_id')

data <- data %>% 
  mutate(region = str_remove(region, " Region"))

# clean up the dataframe a bit
data <- data %>% 
  rename(surface_temp = value) %>% 
  select(-c(depth_from, depth_to, var_aeme))

# calculate the rate of change by region
data <- data %>% 
  separate(lake_id, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) 

### do some filtering of lakes with very few obs
data <- data %>% 
  group_by(LID) %>% 
  mutate(temp_ID = row_number(),
       n = n(),
       time_since_previous_obs = as.numeric(difftime(as.Date(Date), lag(as.Date(Date)), units = "days")),
       lake_max_gap = ifelse(n > 1, max(time_since_previous_obs, na.rm = TRUE), NA)) 

data <- data %>% 
  group_by(LID) %>% 
  mutate(n_lakes = n())

ggplot(data, aes(x = n_lakes, y = district, fill = district)) +
  geom_col() +
  ggtitle('All lakes') +
  theme(legend.position = 'none')

data_sub <- data %>% 
  filter(n > 10) %>% 
  filter(lake_max_gap <=365) 

# standardize the dates so they are in difference from first date (in months)
data_sub <- data_sub %>% 
  group_by(LID) %>% 
  mutate(first_date = min(Date),
         date_diff = difftime(Date, first_date, units = 'days'))


# take annual mean
data_sub <- data_sub %>% 
  mutate(year = year(Date)) %>% 
  group_by(year, LID, district) %>% 
  summarise(mean_temp = mean(surface_temp, na.rm = TRUE))

ggplot(data_sub, aes(x = mean_temp, y = district, fill = district)) +
  geom_density_ridges() +
  theme(legend.position = 'none')

out <- plyr::ddply(data_sub, c("district", "LID"), \(x){
  model <- lm(mean_temp ~ year, data = x)
  print(model)
  tidy(model) %>% filter(term == "year") %>% pull(estimate) 
})
out


sen <- data_sub %>% 
  group_by(district, LID) %>% 
  summarise(sen_slope = sens.slope(mean_temp)$estimates,
            sen_signif = sens.slope(mean_temp)$p.value)
sen

compare <- left_join(out, sen, by = c('LID', 'district'))

ggplot(compare, aes(x = V1, y = sen_slope, color = district)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, size = 2, color = 'black', linetype = 'dashed') +
  geom_smooth(method = 'lm') +
  theme(legend.position = 'none')

ggplot(out, aes(x = V1, y = district, fill = district)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  geom_vline(xintercept = 0) +
  theme_bw() +
  theme(legend.position = 'none') 

##################
# ADD REGION AS COLUMN, SPLIT DISTRICT
sen <- sen %>% 
  mutate(region = sub(".*\\((.*)\\)", "\\1", district),
         city = sub(" \\(.*\\)", "", district)) %>% 
  mutate(island = ifelse(region %in% c('Northland',
                                       'Auckland', 'Waikato',
                                       'Bay of Plenty',
                                       'Gisborne', "Hawke's Bay", 
                                       'Manawatu-Whanganui', 'Taranaki', 
                                       'Wellington'), 
                         'North', 'South')) %>% 
  arrange(region, city)


sen$region <- factor(sen$region, levels = c('Northland',
                                            'Auckland', 'Waikato',
                                            'Bay of Plenty',
                                            'Gisborne', "Hawke's Bay", 
                                            'Manawatu-Whanganui', 'Taranaki', 
                                            'Wellington',
                                            'Nelson', "Marlborough",
                                            'West Coast','Canterbury', 
                                            'Otago',
                                            'Southland'))

sen$city <- factor(sen$city, levels = c('Kerikeri', 'Whangarei',
                                        'Whangaparaoa', 'Auckland',
                                        'Hamilton', 'Taupo',
                                        'Rotorua', 'Tauranga',
                                        'Gisborne', 'Napier', 
                                        'Dannevirke', 'Taumarunui',
                                        'Waiouru', 'Whanganui',
                                        'New Plymouth', 'Masterton',
                                        'Wellington', 'Nelson',
                                        'Blenheim', 'Hokitika',
                                        'Reefton', 'Christchurch',
                                        'Lake Tekapo', 'Tara Hills',
                                        'Timaru', 'Dunedin',
                                        'Queenstown', 'Gore',
                                        'Invercargill', 'Milford Sound'))

  
a <- ggplot(sen, aes(x = sen_slope)) +
  geom_density(size = 2) +
  xlab('Rate of change in LSWT (°C/decade)') +
  ylab('Density') +
  geom_vline(xintercept = 0) +
  theme_bw() 

ggplot(sen, aes(x = sen_slope, y = city, fill = region)) +
  geom_density_ridges() +
  xlab('Rate of change') +
  geom_vline(xintercept = 0) +
  theme_bw() 

# set up color palette
n_colors <- length(levels(sen$region))
colors <- colorRampPalette(brewer.pal(11, "Spectral"))(n_colors)

sen <- sen %>% 
  arrange(region, city) 

b <- ggplot(sen, aes(x = sen_slope, y = fct_rev(city), fill = region)) +
  geom_density_ridges(scale = 2) +
  xlab('Rate of change in LSWT (°C/decade)') +
  coord_cartesian(clip = 'off') +      # Allow plot to extend beyond the default area
  ylab('City') +
  scale_y_discrete(expand = expansion(mult = c(0.05, 0.1))) +  # Add space between ridges
  scale_fill_manual(values = colors) +
#  facet_wrap(~island, scales = 'free_y') +
  geom_vline(xintercept = 0) +
  labs(fill = 'Region') +
  theme_bw()
b
lswt_rate <- ggarrange(a, b, widths = c(1, 2), 
                       labels = 'auto')
ggsave('./figures/rate_of_change_LWST.png', lswt_rate, 
       dpi = 300, units = 'mm', height = 400, width = 800, scale = 0.3)

# mean trends by region
reg_trend <- sen %>% 
  group_by(region) %>% 
  summarise(mean_sen = mean(sen_slope, na.rm = TRUE),
            sd = sd(sen_slope, na.rm = TRUE))

reg_trend <- reg_trend %>% 
  mutate(mean_sen = round(mean_sen, 2),
         sd = round(sd, 2))
################################################################################
out <- out %>% 
  group_by(district) %>% 
  mutate(n_lakes= n())

sen <- sen %>% 
  group_by(district) %>% 
  mutate(n_lakes= n())

ggplot(sen, aes(x = n_lakes, y = district, fill = district)) +
  geom_col() +
  ggtitle('Only lakes with >10 obs, max gap < 365') +
  theme(legend.position = 'none')

length(unique(out$LID))
length(unique(sen$LID))

write.csv(sen, './data/output/sen_slope_LSWT_annual_mean_30_districts.csv', row.names = FALSE)

