# identify popular words in one group that are not in another
# David Gill
# Sept 2023

pacman::p_load(rio,tidytext,tidyverse)
setwd("D:/data/Dropbox/data/analysis/blue_justice")

iati.coast <- import("data/iati_GPS_db_100km_100m (1).rds")
#names(iati.coast)

# unnest words
coastal <- iati.coastal2 %>%
  as.data.frame() %>% 
  select(iati_identifier,comb,coastal) %>% 
  dplyr::filter(coastal==1) %>% 
  mutate(across(c(comb),~replace_na(.,""))) %>%
  tidytext::unnest_tokens(word,comb)

non.coastal <- iati.coastal2 %>%
  as.data.frame() %>% 
  select(iati_identifier,comb,coastal) %>% 
  dplyr::filter(coastal==0) %>% 
  mutate(across(c(comb),~replace_na(.,""))) %>%
  tidytext::unnest_tokens(word,comb)

# remove stop words
tidy.coastal<- coastal %>%
  anti_join(stop_words, by = "word")

tidy.non.coastal <- non.coastal %>%
  anti_join(stop_words, by = "word")

# look at most popular words
tidy.non.coastal %>%
  count(word, sort = TRUE) %>%
  filter(n>1000)

# remove unimportant and non-coastal words using "too popular" non-coastal words, as well as those already used
word.key.marine <- gsub("\\\\b","",key.marine)

top.pop.words <- non.coastal %>%
  count(word, sort = TRUE) %>%
  filter(n>500) %>%
  pull(word)
coastal.unique <- tidy.coastal %>%
   filter(!word%in%top.pop.words & !word%in%word.key.marine) %>% 
  count(word, sort = TRUE) %>% 
  filter(n>5)
export(coastal_tf_idf_words,"data/data_check/potential_marine_terms_1.csv")



# Identify unique words?
coastal_tf_idf <- coastal.unique %>%
  count(iati_identifier, word, sort = TRUE) %>%
  bind_tf_idf(word, iati_identifier, n) %>%
  arrange(-tf_idf) %>%
  group_by(iati_identifier) %>%
  top_n(15) %>%
  # filter(tf_idf>0.05) %>%
  ungroup
head(coastal_tf_idf)

coastal_tf_idf_words <- coastal_tf_idf %>% 
  group_by(word) %>% 
  count() %>% 
  arrange(-n) %>% 
  filter(n>1 & !word%in%word.key.marine  & !word%in%coastal.unique$word) 
  
export(coastal_tf_idf_words,"data/data_check/potential_marine_terms_2.csv")

