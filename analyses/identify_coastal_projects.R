# Idenitfies potential coastal projects
# outputs: datasets with full and select variable lists
# David Gill
# Oct 2023
# Updates: Oct 2023
library(here)
source(here("analyses","00_setup.R"))

############################
#   COASTAL COUNTRIES      #
############################

# load entire IATI dataset & gps dataset to identify all coastal countries
all.gps.sp.100m.100km <- readRDS(here("data","derived-data","iati_GPS_db_100km_100m.rds"))
all.iati <- readRDS(here("data","raw-data","CADC.rds"))
#agrep("country", names(all.iati),value=T,fixed = TRUE)

# Unnest iso2 country codes
iso2.unnest <- all.iati %>%
  as.data.frame() %>% 
  mutate(temp.id=seq(1:nrow(.))) %>%  # temp unique ID as there are duplicate iati_identifier_bis
  select(temp.id,iati_identifier,recipient_country_code,recipient_country_narrative) %>% 
  mutate(recipient_country_code = gsub(" ", "", as.character(recipient_country_code)), # remove white spaces
         iso2 = strsplit(as.character(recipient_country_code), "\\|")) %>%  # split values into rows by "|"
  unnest(iso2)%>%
  filter(iso2!="|") %>%
  mutate(iso2=toupper(iso2)) %>% 
  group_by(temp.id) %>% 
  ungroup
sort(table(iso2.unnest$iso2,useNA = c("always")))

# identify coastal countries
ctr.region <- read.csv(here("data","raw-data","ISO-3166-Countries-with-Regional-Codes-master","all","all.csv"))
ctr <- read.csv(here("data","raw-data","country-coastline-distance-master","coastlines.csv"))
head(ctr)
ctr[which(ctr$iso2 == ""),]
ctr[which(ctr$iso2 == ""),"iso2"] <- c("NA","AN","PS")
coastal.ctr <- ctr %>% filter(coastline_wf >0)

# some incorrect non-iso codes in IATI dataset, many are multi-country and almost have coastal countries in them so they will be included (as keywords should eliminate all non-coastal projects)
iso2.code.error <- iso2.unnest %>%
  filter(!iso2%in%ctr.region$alpha.2) %>% 
  distinct(iso2,recipient_country_narrative)  
unique(iso2.code.error$iso2)

# filter for potential coastal countries
iati.coastal <- all.iati %>% 
  filter(recipient_country_code%in%coastal.ctr$iso2 | iati_identifier%in%all.gps.sp.100m.100km$iati_identifier) %>% 
  filter(!is.na(recipient_country_code)) 
saveRDS(iati.coastal,here("data","derived-data","all.potential.coastal.rds"))

# save file with unique projects and select variables
iati.coastal.unique <- iati.coastal %>% 
  select(iati_identifier,recipient_country_narrative,recipient_country_code,activity_date_iso_date,title_narrative,description_narrative) %>% 
  distinct(iati_identifier,.keep_all = T)
saveRDS(iati.coastal.unique,here("data","derived-data","all.potential.coastal.unique.rds"))

