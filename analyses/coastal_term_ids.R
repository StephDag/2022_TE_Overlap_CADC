# identify popular words in one group that are not in another
# David Gill
# Sept 2023

pacman::p_load(rio,tidytext,tidyverse)
setwd("D:/data/Dropbox/data/analysis/blue_justice")

iati.coast <- import("data/iati_GPS_db_100km_100m (1).rds")
#names(iati.coast)

# unnest words
coastal <- iati.coast %>%
  as.data.frame() %>% 
  select(iati_identifier_bis,title_narrative,description_narrative,coastal) %>% 
  dplyr::filter(coastal==1) %>% 
  mutate(across(c(title_narrative,description_narrative),~replace_na(.,"")),
       comb.var=str_c(title_narrative,description_narrative)) %>%
  select(-c(title_narrative,description_narrative)) %>%
  tidytext::unnest_tokens(word,comb.var)

non.coastal <- iati.coast %>%
  as.data.frame() %>% 
  select(iati_identifier_bis,title_narrative,description_narrative,coastal) %>% 
  filter(coastal==0)%>% 
  mutate(across(c(title_narrative,description_narrative),~replace_na(.,"")),
         comb.var=str_c(title_narrative,description_narrative), .keep = "used") %>%
  select(-c(title_narrative,description_narrative)) %>%
  tidytext::unnest_tokens(word,comb.var)

# remove stop words
tidy.coastal<- coastal %>%
  anti_join(stop_words, by = "word")

tidy.non.coastal <- non.coastal %>%
  anti_join(stop_words, by = "word")

# look at most popular words
tidy.non.coastal %>%
  count(word, sort = TRUE) %>%
  filter(n>1000)

# remove unimportant and non-coastal words using "too popular" non-coastal words
top.pop.words <- non.coastal %>%
  count(word, sort = TRUE) %>%
  filter(n>1500) %>%
  pull(word)
coastal.unique <- tidy.coastal %>%
   filter(!word%in%top.pop.words)


coastal.unique %>%
  count(word, sort = TRUE) %>%
  filter(n>500)




# Identify unique words?
coastal_tf_idf <- coastal.unique %>%
  count(iati_identifier_bis, word, sort = TRUE) %>%
  bind_tf_idf(word, iati_identifier_bis, n) %>%
  arrange(-tf_idf) %>%
  group_by(iati_identifier_bis) %>%
  top_n(15) %>%
  # filter(tf_idf>0.05) %>%
  ungroup
head(coastal_tf_idf)
