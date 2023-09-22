### linked to projects documents

all <- readRDS(here("data","derived-data","CADC.db.final.rds"))

#  uri
url1 <- all %>% 
  mutate(linked_data_uri = ifelse(linked_data_uri == "NA",NA,linked_data_uri)) %>%
  filter(!is.na(linked_data_uri))
dim(url1)
as.factor(url1$linked_data_uri)

# link to document
url2 <- all %>% 
  mutate(result_document_link_url = ifelse(result_document_link_url == "NA",NA,result_document_link_url)) %>%
  filter(!is.na(result_document_link_url))
dim(url2)

# link to document
url3 <- all %>% 
  mutate(document_link_url = ifelse(document_link_url == "NA",NA,document_link_url)) %>%
  filter(!is.na(document_link_url)) %>%
  filter(str_detect(sector_narrative,"Fishing"))
dim(url3)

head(url3 %>% select(iati_identifier,document_link_url))
head(url3 %>% select(iati_identifier,result_document_link_url))
head(url3 %>% select(iati_identifier,linked_data_uri))
result_document_link_url

url3$document_link_url[6602]

paste0("https://d-portal.org/ctrack.html#view=act&aid=",url3$iati_identifier[6602],"")


### test
# pull html from the page of interest
library(tidyverse)
library(here)
library(rvest)
library(purrr)
library(tabulizer)
library(rJava)

page <- read_html(paste0("https://d-portal.org/ctrack.html#view=act&aid=",url3$iati_identifier[6602],""))
#page <- read_html("https://wttc.org/Research/Economic-Impact")
# name and create our download destination folder
pdf_dir <- here(paste0("outputs/wttc_pdfs/"))
dir.create(pdf_dir)

raw_list <- page %>% # takes the page above for which we've read the html
  html_elements("a") %>%  # find all links in the page
  html_attr("href") %>% # get the url for these links
  str_subset("/QuickDownload") %>% 
  unique() %>% 
  walk2(., paste0(pdf_dir, "wttc_", # extract unique identifier from URL for file name, and download file
                  (str_remove(., "https://wttc.org/Research/Economic-Impact/moduleId/704/itemId/") %>% 
                     str_remove("/controller/DownloadRequest/action/QuickDownload")), ".pdf"),
        download.file, mode = "wb")
