# script to make figure with LSWT change over e-w and n-s gradients

# read LSWT output
sen <- read.csv('./data/output/sen_slope_LSWT_annual_mean_30_districts.csv')

# read lake x-y locations to match with output
xy <- readRDS('./data/lernzmp_lakes_master.rds')
xy <- xy$updated 
xy <- xy %>% 
  select(id_final, area, easting_NZTM, northing_NZTM, max_depth, mean_depth, GeomorphicType) %>% 
  separate(id_final, into = c("char", "LID"), sep = " ") %>% 
  filter(!is.na(LID)) %>% 
  select(-char)
xy$LID <- as.numeric(xy$LID)

# combine based on LID
df <- left_join(sen, xy)

df <- df %>% 
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

df$region <- factor(df$region, levels = c('Northland',
                                            'Auckland', 'Waikato',
                                            'Bay of Plenty',
                                            'Gisborne', "Hawke's Bay", 
                                            'Manawatu-Whanganui', 'Taranaki', 
                                            'Wellington',
                                            'Nelson', "Marlborough",
                                            'West Coast','Canterbury', 
                                            'Otago',
                                            'Southland'))

df$city <- factor(df$city, levels = c('Kerikeri', 'Whangarei',
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


################################################################################
## change in surf temp by east-west and north-south gradients
# set up color palette
n_colors <- length(levels(df$region))
colors <- colorRampPalette(brewer.pal(11, "Spectral"))(n_colors)

e_wtemp <- ggplot(df, aes(x = easting_NZTM, y = sen_slope, color = region)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  theme_bw() +
  scale_color_manual(values = colors) +
  ylab('Rate of change in LSWT (°C/decade)') +
  xlab('Easting')
e_wtemp

n_wtemp <- ggplot(df, aes(x = northing_NZTM, y = sen_slope, color = region)) +
  geom_point(size = 2) +
  geom_smooth(aes(group = 1), method = 'lm', color = 'black') +
  geom_hline(yintercept = 0) +
  theme_bw() +
  scale_color_manual(values = colors) +
  ylab('Rate of change in LSWT (°C/decade)') +
  xlab('Northing')

NSEW_LSWT <- ggarrange(e_wtemp, n_wtemp, common.legend = TRUE)
ggsave('./figures/rate_of_change_NSEW.png', NSEW_LSWT, 
       dpi = 300, units = 'mm', height = 400, width = 700, scale = 0.3)
