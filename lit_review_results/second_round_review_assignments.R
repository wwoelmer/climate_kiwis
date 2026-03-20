# extract already duplicated papers from lit review
# and create sheet with papers not yet duplicated

library(tidyverse)

gps <- read.csv('./data/LSWT_rates_of_change_literature.csv',
                fileEncoding = 'latin1')

counts <- gps %>% 
  group_by(citation) %>% 
  mutate(n_reviewers = n_distinct(reviewer.1),
         all_reviewers = paste(unique(reviewer.1), collapse = ", "))

#######################################
# look at studies already duplicated
lit_dups <- counts %>% 
  filter(n_reviewers > 1)

write.csv(lit_dups, './duplicate_studies.csv', row.names = FALSE)

# cady
cb <- lit_dups %>% 
  filter(reviewer.1=='CB') %>% 
  select(citation, n_reviewers, all_reviewers)
write.csv(cb, './lit_review_results/cb_duplicates_to_resolve.csv', row.names = FALSE)

# Matt
mp <- lit_dups %>% 
  filter(reviewer.1=='MP') %>% 
  distinct(citation, .keep_all = TRUE) %>% 
  select(citation, n_reviewers, all_reviewers)
write.csv(mp, './lit_review_results/mp_duplicates_to_resolve.csv', row.names = FALSE)

# Martin
ms <- lit_dups %>% 
  filter(reviewer.1=='MS') %>% 
  distinct(citation, .keep_all = TRUE) %>% 
  select(citation, n_reviewers, all_reviewers)
write.csv(ms, './lit_review_results/ms_duplicates_to_resolve.csv', row.names = FALSE)

# Ollie
or <- lit_dups %>% 
  filter(reviewer.1=='OR') %>% 
  distinct(citation, .keep_all = TRUE) %>% 
  select(citation, n_reviewers, all_reviewers)
write.csv(or, './lit_review_results/or_duplicates_to_resolve.csv', row.names = FALSE)

# maggie
ma <- lit_dups %>% 
  filter(reviewer.1=='MA') %>% 
  select(citation, n_reviewers, all_reviewers)
write.csv(ma, './lit_review_results/ma_duplicates_to_resolve.csv', row.names = FALSE)

# Whitney
ww <- lit_dups %>% 
  filter(reviewer.1=='WW') %>% 
  distinct(citation, .keep_all = TRUE) %>% 
  select(citation, n_reviewers, all_reviewers)
write.csv(ww, './lit_review_results/ww_duplicates_to_resolve.csv', row.names = FALSE)

######################################
# look at studies not yet duplicated
lit_unique <- counts %>% 
  filter(n_reviewers==1)

reviewer_list <- c("CB", "MP", "OR", "MS", "MA", "WW", "BM")

reviewer_assignments <- lit_unique %>% 
  distinct(citation, .keep_all = TRUE) %>% 
  select(citation, reviewer.1) %>% 
  filter(reviewer.1!='NA')

table(reviewer_assignments$reviewer.1)
write.csv(reviewer_assignments, './reviewer_assignments_round2.csv', 
          fileEncoding = 'UTF-8-BOM',
          row.names = FALSE)

round_2 <- read.csv('./reviewer_assignments_round2.csv')
table(round_2$reviewer.2)
