# code to retrieve and correct GPS coordinates from the full database - in the descriptive narrative
source(here::here("analyses","001_Coastal_countries.R"))

# load raw data
all <- readRDS(here("data","derived-data","CADC.db.final.rds"))

# unique country vector
ctry <- unique(coastal.ctr$iso2)
length(ctry)


length(unique(all.coastal$iso_a2))

test <- all$description_narrative[1]

# all in lowcase
# download list of locations
library(stringr)
list = c("Wien", "London", "New York")
text = "Er sah den Stadtplan von Wien in New York."
words=as.character()

for (i in 1:length(list)){
  
  if (is.na(str_extract(text,regex(list[i], ignore_case = T)))) next
  
  x<-str_extract(text,regex(list[i], ignore_case = T))
  words<-c(words,x)
}

words











***********
library(gptchatteR)
chatter.auth("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")
chatter.create()

# test
install.packages("openNLPmodels.de", repos = "http://datacube.wu.ac.at")
# require("openNLPmodels.de")

text <- data.frame(Example_Text = c("the project is focused on restoration of selected monuments and quarters of wh property gjirokastra ( parte of serial wh property berat and gjirokastra) Paris"))
test <- as.String(test)

sent_annot = Maxent_Sent_Token_Annotator()
word_annot = Maxent_Word_Token_Annotator()
loc_annot = Maxent_Entity_Annotator(kind = "location",model="en-ner-location.bin")
annot.l1 = NLP::annotate(test, list(sent_annot,word_annot,loc_annot))
k <- sapply(annot.l1$features, `[[`, "kind")
GE_locations = text[annot.l1[k == "location"]]
#removing underscores from the entire text




test <- gsub("_", "", test$Example_Text)
test <- gsub("(", "", test$Example_Text)
test <- gsub(")", "", test$Example_Text)


test.token <- unnest_tokens(test, input = "Example_Text", output = "Sentence", token = "words")
library(dplyr)



#create sentence tokens
library(tidytext)

 test %>%
  unnest_tokens(word, text)
library(spacyr)
full_txt_sentence <- spacyr::spacy_tokenize(test, 'sentence') 

if (!require("pacman")) install.packages("pacman")
pacman::p_load_gh("trinker/entity")
library(entity)
library(openNLP)
chat<-chatter.chat("retrieve the geographical locations in this text?: the project is focused on restoration of selected monuments and quarters of wh property gjirokastra ( parte of serial wh property berat and gjirokastra)",return_response=TRUE)
chat<-chatter.chat("what is the meaning of life",return_response=TRUE)


location_entity(c("Gjirokastra","and","Paris","Sedona"))
Maxent_Entity_Annotator(language = "en", kind = "person", probs = FALSE,
                        model = NULL)


#### chatgpt
install.packages("chatgpt")
remotes::install_github("jcrodriguez1989/chatgpt")
