# code for identifying sector descriptions 
# David Gill
# Oct 2023

# load rds of initiatives to see if some are indeed coastal.
library(here)
pacman::p_load(rio,tidyverse,here)
# setwd("D:/data/Dropbox/data/analysis/blue_justice")
# output.dir <- "R:/Gill/research/2022_TE_Overlap_CADC/data/derived-data"
# all.gps.sp.100m.100km <- import("data/derived-data/iati_GPS_db_100km_100m (1).rds")
all.gps.sp.100m.100km <- readRDS(here("data","derived-data","iati_GPS_db_100km_100m.rds"))

# ---- split sector information into separate rows ----
# do we have duplicate iati_identifier_bis?
n_distinct(all.gps.sp.100m.100km$iati_identifier_bis)/nrow(all.gps.sp.100m.100km)

# Unnest sector codes
sector.unnest <- all.gps.sp.100m.100km %>%
  as.data.frame() %>% 
  mutate(temp.id=seq(1:nrow(.))) %>%   # temp unique ID as there are duplicate iati_identifier_bis
  select(temp.id,iso_a3,title_narrative,description_narrative,iati_identifier_bis,iati_identifier,sector_code) %>% 
  mutate(sector_code = gsub(" ", "", as.character(sector_code)),  # remove white spaces
         sector = strsplit(as.character(sector_code), "\\|")) %>%   # split values into rows by "|"
  unnest(sector) %>% 
  filter(sector!="|") %>%
  group_by(temp.id) %>% 
  mutate(sect.num = row_number())%>% 
  ungroup

# Unnest sector vocabulary
vocab.unnest <- all.gps.sp.100m.100km %>%
  as.data.frame() %>% 
  mutate(temp.id=seq(1:nrow(.))) %>%  # temp unique ID as there are duplicate iati_identifier_bis
  select(temp.id,iati_identifier_bis,iati_identifier,sector_vocabulary) %>% 
  mutate(sector_vocabulary = gsub(" ", "", as.character(sector_vocabulary)), # remove white spaces
         vocab = strsplit(as.character(sector_vocabulary), "\\|")) %>%  # split values into rows by "|"
  unnest(vocab)%>%
  filter(vocab!="|") %>%
  group_by(temp.id) %>% 
  mutate(sect.num = row_number())%>%  # ID for sectors for each project
  ungroup
sort(table(vocab.unnest$vocab,useNA = c("always")))

# Unnest sector percentage
perc.unnest <- all.gps.sp.100m.100km %>%
  as.data.frame() %>% 
  mutate(temp.id=seq(1:nrow(.))) %>%    # temp unique ID as there are duplicate iati_identifier_bis
  select(temp.id,iati_identifier_bis,iati_identifier,sector_percentage) %>%
  mutate(sector_percentage = gsub(" ", "", as.character(sector_percentage)),  # remove white spaces
         percent = strsplit(as.character(sector_percentage), "\\|")) %>%   # split values into rows by "|"
  unnest(percent)%>%
  filter(percent!="|") %>%
  mutate(percent=as.numeric(percent)) %>% 
  group_by(temp.id) %>% 
  mutate(sect.num = row_number()) %>%   # ID for sectors for each project
  ungroup

sector.list.org <- sector.unnest %>% 
  left_join(vocab.unnest,by=c("temp.id","sect.num")) %>% 
  left_join(select(perc.unnest,temp.id,sect.num,sector_percentage,percent),by=c("temp.id","sect.num")) %>% 
  select(temp.id,iati_identifier_bis=iati_identifier_bis.x,
         iso_a3,title_narrative,description_narrative,
         sect.num,sector_vocabulary,sector_percentage,vocab,sector_code,sector,percent)
sector.list <- sector.list.org
# ---- checks ----
# 1. some were missing vocab entry but seem to be 5 digit codes
# sector.list %>%
#   filter(vocab=="") %>%
#   view
sector.list <- sector.list %>% 
  mutate(vocab=ifelse(vocab=="","1",vocab)) # weird ones that were missing codes but had 5 digit code in front so using DAC codes

# 2. see if unnesting by "|" worked (should be 0)
 sector.list %>%
  filter(grepl("\\|", sector)) 

#3. ID incorrect DAC codes
 # read in DAC code databases
 dac.5.code <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="dac5digit_sector") %>% 
   janitor::clean_names() %>% 
   select(dac5.code=code,dac3.code=category,name, description,status) %>% 
   mutate(across(dac3.code:dac5.code,~ as.character(.x))) 
 
 dac.3.code <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="dac3digit_sector_category") %>% 
  janitor::clean_names() %>% 
   select(dac3.code=code,name,description,status) %>% 
   mutate(across(dac3.code,~ as.character(.x))) 
 # add higher level dac3
 dac.3.code1 <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="DAC Purpose codes") %>%
   janitor::clean_names() %>% 
   select(dac3.code=dac_5, name=description,description=clarifications_additional_notes_on_coverage) %>%
   filter(!is.na(dac3.code)) %>% 
   mutate(across(dac3.code,~ as.character(.x))) 
 dac.3.code <- dac.3.code %>% 
   bind_rows(filter(dac.3.code1,!dac3.code%in%c(dac.3.code$dac3.code)))
 
# dac.code.chechi <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="5-3 sector-CADC") %>%
#   select(dac5.code="code", dac3.code="category",cadc="CADC", everything())%>%
#   mutate(across(dac5.code:dac3.code,~ as.character(.x)),
#          vocab=1)
# dac.code.chechi %>% 
#   anti_join(dac.5.code,by="dac5.code") 
# dac.code.chechi %>% 
#   anti_join(dac.3.code,by="dac3.code") 
# dac.5.code %>% 
#   anti_join(dac.code.chechi,by="dac5.code") 
# dac.3.code %>% 
#   anti_join(dac.code.chechi,by="dac3.code") 

# 3a. any DAC5s in sector list not in conversion table?
missing.dac5 <- sector.list %>% 
  filter(vocab==1) %>% 
  anti_join(dac.5.code,by=c("sector"="dac5.code"))
unique(missing.dac5$sector)
#- any DAC3s in sector list not in conversion table?
missing.dac3 <- sector.list %>% 
  filter(vocab==2) %>% 
  anti_join(dac.3.code,by=c("sector"="dac3.code"))
unique(missing.dac3$sector)

# 4. any 5 digit likely to be 3 digit codes instead?
missing.dac5 %>% 
  filter(vocab=="1" & nchar(sector)==3) %>% 
  distinct(sector)
# change vocab code from 1 to 2 when there are 3 digit codes used
sector.list <- sector.list %>% 
  mutate(vocab=ifelse(vocab=="1" & nchar(sector)==3,"2",vocab))

# 5a. there are no DAC 5 with 000s. Will convert them to 3 digit
sector.list %>%
  filter(grepl("000",sector) & vocab%in%c("1")) %>% 
  distinct(sector)
sector.list <- sector.list %>%
  mutate(vocab=ifelse(grepl("000",sector) & vocab%in%c("1"),"2",vocab), # change vocab to 3 digit category
         sector=ifelse(vocab%in%c("2")& nchar(sector) == 5,
                       gsub("000","0",sector),sector)) # replace 000 with 0

# 5b. there are no DAC 5 with 00s. Will convert them to 3 digit
sector.list %>%
  filter(grepl("00$",sector) & vocab%in%c("1")) %>% 
  distinct(sector)
sector.list <- sector.list %>%
  mutate(vocab=ifelse(grepl("00$",sector) & vocab%in%c("1"),"2",vocab), # change vocab to 3 digit category
         sector=ifelse(vocab%in%c("2") & nchar(sector) == 5,
                       gsub("00$","",sector),sector)) # replace 000 with 0


# 6. non-digit characters incorrectly entered? 
non.digit.code.error <- sector.list %>%
  filter(grepl("[^0-9]",sector) & vocab%in%c("1","2","6","10","11")) %>% 
  distinct(sector)
non.digit.code.error
gsub("[^0-9]","",non.digit.code.error$sector)

sector.list <- sector.list %>%
  mutate(sector=ifelse(grepl("[^0-9]",sector) & vocab%in%c("1","2","6","10","11"),
                       gsub("[^0-9]","",sector),sector)) 


# 7. create fields to identify those with 3 and 5 digit DAC codes that are potential DACs (got Chechi to check )
sector.list <- sector.list %>% 
  group_by(iati_identifier_bis) %>%
  mutate(has.dac5=ifelse(sum(vocab == "1") > 0,1,0),
         has.dac3=ifelse(sum(vocab %in% c("1","2")) > 0,1,0)) %>% 
  ungroup()

# ID those in the 5 digit table (note: Chechi spot-checked)
no.dac <- sector.list %>% 
  filter(has.dac3==0) %>% 
  distinct(iati_identifier_bis,.keep_all = T)  
potential.dac <- no.dac %>% 
  filter(nchar(sector) == 5)
table(potential.dac$vocab)
#export(potential.dac,"data/potential.dac.csv")


# 7a. ID those not in 5 digit table that might be errors
potential.dac.to.convert <- potential.dac %>% 
  anti_join(dac.5.code,by=c("sector"="dac5.code"))
potential.dac.to.convert

#convert the ones that are coding errors
sector.list <- sector.list %>%
  mutate(sector=ifelse(temp.id==6809,gsub("00","",sector),sector))  # uses AidData coding which is the same as DAC

filter(sector.list,temp.id%in%c(6809,86562)) 

# 7b. those with 5 digit codes that match DAC code table, but say they aren't DAC codes
dac.to.convert <- sector.list %>% 
  filter(has.dac3==0 & sector%in%dac.5.code$dac5.code & !vocab%in%c("6","11"))  # 6 (AidData) and 11 (NAICS) also use 5 digit codes
table(dac.to.convert$vocab)

sector.list <- sector.list %>% 
  # those without DAC, has a 5 digit code that is in the DAC5 table that isn't AidData and NAICS
  mutate(vocab=ifelse(has.dac3==0 & sector%in%dac.5.code$dac5.code & !vocab%in%c("6","11"),1,vocab))


# 8. re-run check after above steps
# any DAC5s in sector list not in conversion table?
missing.dac5 <- sector.list %>% 
  filter(vocab==1) %>% 
  anti_join(dac.5.code,by=c("sector"="dac5.code"))
unique(missing.dac5$sector) 
n_distinct(missing.dac5$sector)# 17 values

#- any DAC3s in sector list not in conversion table?
missing.dac3 <- sector.list %>% 
  filter(vocab==2) %>% 
  anti_join(dac.3.code,by=c("sector"="dac3.code"))
unique(missing.dac3$sector) # 3 values
#export to check (727 projects)
missing.dac <- sector.list %>% 
  filter(sector%in%c(missing.dac5$sector,missing.dac3$sector)) %>% 
  distinct(sector,iati_identifier_bis,.keep_all = T)
n_distinct(missing.dac$iati_identifier_bis) 
#export(missing.dac,"data/missing.dac.csv")

#9. re-create fields to identify those with 3 and 5 digit DAC codes
sector.list <- sector.list %>% 
  group_by(iati_identifier_bis) %>%
  mutate(has.dac5=ifelse(sum(vocab == "1") > 0,1,0),
         has.dac3=ifelse(sum(vocab %in% c("1","2")) > 0,1,0)) %>% 
  ungroup()

#10. create fields to identify those with SDG goals
sector.list <- sector.list %>% 
  group_by(iati_identifier_bis) %>%
  mutate(has.sdg=ifelse(sum(vocab %in% c("7","8")) > 0,1,0)) %>% 
  ungroup()

# how many projects don't have 3 or 5 digit codes? 
n_distinct(sector.list$iati_identifier_bis[sector.list$has.dac3==0]) #n=2674
n_distinct(sector.list$iati_identifier_bis[sector.list$has.dac5==0]) #n=15055
n_distinct(sector.list$iati_identifier_bis[sector.list$has.sdg==0]) #n=77154

# what vocab are used in projects don't have 3 or 5 digit codes?
table(sector.list$vocab[sector.list$has.dac3==0])


# how many sector codes and vocab were changed?
length(sector.list$iati_identifier_bis[(!sector.list$vocab==sector.list.org$vocab)|(!sector.list$sector==sector.list.org$sector)]) #5939
n_distinct(sector.list$iati_identifier_bis[(!sector.list$vocab==sector.list.org$vocab)|(!sector.list$sector==sector.list.org$sector)]) #376

# ---- Add sector descriptions ----

# 1. read in Chechi coding 
dac.cadc <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="5-3 sector-CADC") %>%
  select(dac5.code="code", dac3.code="category",cadc="CADC", climate,development,conservation)
head(dac.cadc)

#NTEE codes
ntee.code <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="NTEE-IRS-CADC-DAC codes") %>% 
  select(sector_code="NTEE Code", dac5.code="5 code sector", dac3.code="3 code sector",orig.name="Description",orig.description="Definition",
         cadc="CADC?",climate,development,conservation,comment=Comment,certainty) %>% 
  distinct() %>% 
  mutate(across(sector_code:dac3.code,~ as.character(.x)),
         vocab=5,
         certainty=ifelse(sector_code%in%c("A99","E99"),"low",certainty)) %>% 
  filter(!is.na(dac3.code)|!is.na(dac5.code))

# Global cluster codes (not going to use these codes as all have projects corresponding DAC codes)
glob.clust.code <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="Global Groups-CADC-5-3 codes") %>% 
  slice(-1) %>% # unused first row
  select(sector_code="HRinfo ID", dac5.code="5 code sector",dac3.code="3 code sector",orig.name="Preferred Term",orig.description="Notes",
         cadc="CADC?",climate,development,conservation,comment="comment...17",certainty) %>% 
  distinct() %>% 
  mutate(across(sector_code:dac3.code,~ as.character(.x)),
         vocab=10,
         certainty="low")%>% 
  filter(!is.na(dac3.code)|!is.na(dac5.code))
names(glob.clust.code)
# all the Global cluster codes have associated dac codes
# sector.list %>% 
#   filter(vocab==10 & has.dac3==1)


# NAICS codes
naics.code <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="NAICS_DAC") %>% 
  select(sector_code=sector_code, dac5.code="dac 5 code", dac3.code="dac 3 code",
         comment="Comment",certainty) %>% 
 # distinct() %>% 
  mutate(across(sector_code:dac3.code,~ as.character(.x)),
         vocab=11)%>% 
  filter(!is.na(dac3.code)|!is.na(dac5.code))
names(naics.code)

#SDG codes (not going to use these codes due to inability to translate)
sdg.code <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="SDG_DAC") %>% 
  select(sector_code="sdg.target", dac5.code="5 code sector", dac3.code="3 code sector",comment="Comment",certainty) %>% 
  distinct() %>% 
  mutate(across(sector_code:dac3.code,~ as.character(.x)),
         vocab=999,
         certainty="low") %>% #  data inputters used 7 and 8 interchangably
  filter(!is.na(dac3.code)|!is.na(dac3.code)) 

# combine code lists
other.vocab <- data.frame(vocab=NA,sector_code=NA,dac3.code=NA,dac5.code=NA,cadc=NA,climate=NA,development=NA,conservation=NA,
                          orig.name=NA,orig.description=NA,comment=NA,certainty=NA) %>% 
  bind_rows(ntee.code,glob.clust.code,naics.code,sdg.code) %>% 
  filter(!is.na(dac3.code)|!is.na(dac3.code)) %>% 
  mutate(across(dac3.code:dac5.code,~ as.character(.x))) %>% 
  distinct(sector_code,vocab,.keep_all = T)
summary(other.vocab)
table(other.vocab$vocab)


# 2. merge to sector code
sort(table(sector.list$vocab))
sort(table(other.vocab$vocab))
sector.list.tmp <- sector.list %>%
  mutate(vocab=as.integer(vocab),
         vocab=ifelse(vocab%in%c(7,8),999,vocab)) %>% # data inputters used 7 and 8 interchangably
  left_join(other.vocab,by=c("sector_code","vocab")) %>% 
  # Use DAC3 or 5 digit code when available, alternative when not
  mutate(dac5=case_when(
      vocab==1 & !is.na(sector) & nchar(sector)==5 ~ sector, # 5 DAC digit codes 
      vocab==6 & !is.na(sector) & nchar(sector)==5 & has.dac5==0 ~ sector, # AIDdata, same as DAC 5 codes
      vocab%in%c(5,10,11,999) & !is.na(dac5.code) & has.dac5==0 & certainty=="high" ~ dac5.code, # 5 DAC digit codes
      TRUE ~ NA_character_),
      dac3=case_when(
        vocab==2 & !is.na(sector) & nchar(sector)==3 ~ sector, # 3 DAC digit codes
        vocab==1 & !is.na(sector) & nchar(sector)==5 ~ substr(sector, start = 1, stop = 3), # 5 DAC digit codes
        vocab==6 & !is.na(sector) & nchar(sector)==5 & has.dac3==0 ~ substr(sector, start = 1, stop = 3), # AIDdata, same as DAC 5 codes
        vocab%in%c(5,10,11,999) & !is.na(dac3.code) & has.dac3==0 & certainty=="high" ~ dac3.code, 
        TRUE ~ NA_character_),
      certainty=ifelse((has.dac3==0 & !is.na(dac3))|(has.dac5==0 & !is.na(dac5)),"med",certainty))

table(sector.list.tmp$certainty)

# 3. add Chechi's coding of project CADC type 
dac.cadc  <- dac.cadc %>% 
  filter(cadc=="Yes") %>% 
  distinct(dac5.code,.keep_all = T) 
dac.climate <- dac.cadc %>% 
  filter(climate==1) %>% 
  distinct(dac5.code,.keep_all = T) 
dac.dev <- dac.cadc %>% 
  filter(development==1) %>% 
  distinct(dac5.code,.keep_all = T) 
dac.cons <- dac.cadc %>% 
  filter(conservation==1) %>% 
  distinct(dac5.code,.keep_all = T) 

# 3b. add DAC descriptive info 
sector.list.coded <- sector.list.tmp %>% 
  mutate(across(dac3.code,~ as.integer(.x))) %>% 
  left_join(dac.3.code,by=c("dac3"="dac3.code")) %>% 
  left_join(select(dac.5.code,-dac3.code),by=c("dac5"="dac5.code")) %>% 
  mutate(name=ifelse(!is.na(name.y),name.y,name.x), # use DAC5 description where available 
         description=ifelse(!is.na(description.y),description.y,description.x),
         status=ifelse(!is.na(status.y),status.y,status.x),
         cadc=ifelse(dac3%in%dac.cadc$dac3.code | dac5%in%dac.cadc$dac5.code,1,0),
         climate=ifelse(dac3%in%dac.climate$dac3.code | dac5%in%dac.climate$dac5.code,1,0),
         development=ifelse(dac3%in%dac.dev$dac3.code | dac5%in%dac.dev$dac5.code,1,0),
         conservation=ifelse(dac3%in%dac.cons$dac3.code | dac5%in%dac.cons$dac5.code,1,0)) %>% 
  select(temp.id:has.sdg,dac3,dac5,name:status,cadc:certainty)
names(sector.list.coded)
table(sector.list.coded$conservation)

# Spot check: DAC 3 and 5 digits used (chechi checked)
# dac3.codes.used <- sector.list.coded %>% 
#   group_by(dac3) %>% 
#   count() %>% 
#   left_join(dac.3.code,by=c("dac3"="dac3.code")) %>% 
#   rename(dac=dac3) 
# 
# dac5.codes.used <- sector.list.coded %>% 
#   group_by(dac5,name,description,status) %>% 
#   rename(dac=dac5) %>% 
#   count()
# dac.codes.used <- dac3.codes.used %>% 
#   bind_rows(dac5.codes.used) %>% 
#   filter(!is.na(dac))
# 
# dac.codes.used %>% 
#   filter(is.na(name))
# 
# dac.codes.used[duplicated(dac.codes.used$dac),]
# n_distinct(dac.codes.used$dac)/ nrow(dac.codes.used)
# export(dac.codes.used,"data/dac_used.coded.csv")


# Spot check: see which codes were translated to ensure that they match up with project description
# to.check <- sector.list.coded %>% 
#   filter(vocab%in%c(5,10,11,999) & !is.na(dac3) & has.dac3==0)
# table(to.check$vocab)
# export(to.check,"data/sector.to.check.csv")
# 
# 
# ---- Export data -----
export(sector.list.coded,here("data","derived-data","sector.list.coded.csv"))

