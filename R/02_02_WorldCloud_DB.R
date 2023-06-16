
# word cloud of hte title and descriptive narrative
CADC.db <- readRDS(here("data","raw-data","CADC.rds"))

#Create a vector containing only the text
text <- CADC.db$description_narrative
# Create a corpus  
docs <- Corpus(VectorSource(text))

# rm unrelevant words
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("french"))
docs <- tm_map(docs, removeWords, stopwords("swedish"))

# transform to matrix with count
dtm <- TermDocumentMatrix(docs) 

ap_td <- tidy(dtm) %>%
  arrange(desc(count)) 
ap_td <- ap_td[,-2]
ap_td <- ap_td %>%
  distinct()
dim(ap_td)

write.csv(ap_td,here("outputs","count_word_db.csv"),row.names=F)
ap_td %>% filter(count >100 & count <166)
