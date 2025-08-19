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