


library(here)
source(here("analyses","00_setup.R"))
### linked to projects documents

all.gps.sp.100m.100km.CADC <- readRDS(here("data","derived-data","iati_GPS_db_100km_100m_keywords_sectors.rds"))

#  uri
url1 <- all.gps.sp.100m.100km.CADC %>% 
  mutate(linked_data_uri = ifelse(linked_data_uri == "NA",NA,linked_data_uri)) %>%
  filter(!is.na(linked_data_uri))
dim(url1)
as.factor(url1$linked_data_uri)

# link to document
url2 <- all.gps.sp.100m.100km.CADC %>% 
  mutate(result_document_link_url = ifelse(result_document_link_url == "NA",NA,result_document_link_url)) %>%
  filter(!is.na(result_document_link_url))
dim(url2)

# link to document
url3 <- all.gps.sp.100m.100km.CADC %>% 
  mutate(document_link_url = ifelse(document_link_url == "NA",NA,document_link_url)) %>%
  filter(!is.na(document_link_url)) %>%
  filter(str_detect(sector_narrative,"Fishing"))
dim(url3)

######## rbind all project with url documnents
all.gps.sp.100m.100km.CADC.url <- rbind(url1,url2)
all.gps.sp.100m.100km.CADC.url <- rbind(all.gps.sp.100m.100km.CADC.url,url3)

######## focus only on columns with url information
all.gps.sp.100m.100km.CADC.url.simp <- all.gps.sp.100m.100km.CADC.url %>%
  select(iati_identifier,iati_identifier_bis,linked_data_uri,result_document_link_url,document_link_url) %>%
  distinct() %>%
  st_drop_geometry()

######## separate the links
all.gps.sp.100m.100km.CADC.url.simp <- all.gps.sp.100m.100km.CADC.url.simp
  strsplit(html_string, "\\|")[[1]]

######## separate the pdf link with |
html_links_1 <- str_split(all.gps.sp.100m.100km.CADC.url.simp$linked_data_uri, "\\|"); names(html_links_1) <- all.gps.sp.100m.100km.CADC.url.simp$iati_identifier #; html_links_1 <- trimws(html_links_1)
html_links_2 <- str_split(all.gps.sp.100m.100km.CADC.url.simp$result_document_link_url, "\\|") ; names(html_links_2) <- all.gps.sp.100m.100km.CADC.url.simp$iati_identifier #; html_links_2 <- trimws(html_links_2)
html_links_3 <- str_split(all.gps.sp.100m.100km.CADC.url.simp$document_link_url, "\\|") ; names(html_links_3) <- all.gps.sp.100m.100km.CADC.url.simp$iati_identifier #; html_links_3 <- trimws(html_links_3)

  # list to df
html_links_1.df <- setNames(stack(html_links_1)[2:1], c('iati_identifier','url')) %>%
  filter(!is.na(url)) %>% 
  distinct()
html_links_2.df <- setNames(stack(html_links_2)[2:1], c('iati_identifier','url')) %>%
  filter(!is.na(url)) %>% 
  distinct()
html_links_3.df <- setNames(stack(html_links_3)[2:1], c('iati_identifier','url')) %>%
  filter(!is.na(url)) %>% 
  distinct()

  # html link full
rm(html_links_full)
html_links_full <- left_join(html_links_1.df,html_links_2.df, by="iati_identifier",relationship = "many-to-many")
html_links_full <- left_join(html_links_full,html_links_3.df, by="iati_identifier",relationship = "many-to-many")

# pivot the dataframe into a long format: dataframe of pdf link to projects
html_links_full_long <- pivot_longer(html_links_full, cols = c("url.x", "url.y", "url"), names_to = "url_type", values_to = "url_value") %>%
  filter(!is.na(url_value)) %>%
  select(-url_type) %>%
  distinct() %>%
  as.data.frame()

# create a column to specify whether it is a pdf lr not
html_links_full_long <- html_links_full_long %>%
  mutate(pdf = ifelse(str_detect(url_value,"pdf"),1,0)) %>%
  mutate(pdf=as.factor(pdf)) %>%
  filter(!is.na(url_value)) %>%
  distinct()
html_links_full_long$ID <- seq(1,dim(html_links_full_long)[1],1)

library(rvest)
library(purrr)
library(tabulizer)
library(rJava)

# loop for download all pdf
html_links_full_long.pdf <- html_links_full_long %>%
  filter(pdf == 1)

for (i in 1:dim(html_links_full_long.pdf)){
 
#  for (i in 474:476){
  print(paste("i=",i)) 
  # pdf link
  temp.page <- gsub(" ","",html_links_full_long.pdf[i,"url_value"]) # rm white space
  
  # change backlash to _
  #filename <- gsub("/", "", temp.page)

  # name and create our download destination folder
  temp.proj.name <- gsub("-","_",html_links_full_long.pdf[i,"iati_identifier"])
  temp.proj.name <- gsub(" ","",temp.proj.name)
  temp.proj.name.2 <- paste0(temp.proj.name,"_",i,sep="")
  
  temp.pdf_dir <- here(paste0("outputs/",temp.proj.name,sep=""))
  dir.create(temp.pdf_dir) # create directory
  
  # download pdf
  #temp.destfile <- "/Users/stephanie/Documents/GitHub/2022_TE_Overlap_CADC/outputs/XM-DAC-41302-106878-1277-2019-ARG155_pdfs/ISR-Disclosable-P118979-12-22-2016-1482451254909.pdf"
  #temp.destfile <- paste0(temp.pdf_dir,filename,sep="/")
  
  # Looks at page in firefox
  #browseURL(temp.page) #, encodeIfNeeded = T, browser = "firefox.exe")
  
  # if error
  skip_to_next <- FALSE
  
  # Saves pdf to "downloaded" folder if it exists
  tryCatch(download.file(temp.page, path.expand(paste0(temp.pdf_dir,"/",temp.proj.name.2,".pdf",sep=""))), error = function(e) { skip_to_next <<- TRUE})
  
  if(skip_to_next) { next }
  #download.file(url=temp.page,temp.destfile, mode = "wb")
}

# extract pdf from a html link (no specific pdf provided)
# loop for download all pdf
html_links_full_long.noPDF <- html_links_full_long %>%
  filter(pdf == 0)

i=1
# extract all links from the page
temp.page <- gsub(" ","",html_links_full_long.noPDF[i,"url_value"]) # rm white space

links <- temp.page %>% 
  html_nodes("a") %>% 
  html_attr("href")

library(downloader)
download.file(page, pdf_dir)

# filter the links to keep only the PDF documents
pdf_links <- grep("\\.pdf$", links, value = TRUE)

library(tidyverse)
library(rvest)
library(xml2)
# scraping hyperlinks

# Looks at page in firefox
browseURL(links[1], encodeIfNeeded = T, browser = "firefox.exe")
browseURL(links[2])

# Saves pdf to "downloaded" folder if it exists
download.file(full_links[1], path.expand("~/downloaded/teste.pdf"))


