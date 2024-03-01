# Categorized geographic projects based based on coastal, sustainability (CADC) and sustainable land-based projects
# outputs: dataset identifying potential CADC projects 
# Stephanie D'Agata & David Gill
# Sept 2023
# Updates: Oct 2023
library(here)
source(here("analyses","00_setup.R"))

############################
#   COASTAL COUNTRIES      #
############################

# load dataset with potential projects in coastal countries
oecd.dat <- import(here("data","raw-data","OECD","ocean_ODA_2010_2021.csv"),sep=";",dec=",")
names(oecd.dat)
dim(oecd.dat) # 61102 26
################################################
#           identify key words                #
################################################

# ----- import keywords ------
# Create function to read in keyword lists from Excel sheet Chechi prepared (keywords list.xlsx)
# Level of confidence: 1: drop/doesn't work; 2: needs additional term; 3: works well; 4: new term to validate; 5: preexisting term not validated yet
my_get_keyword <- function(x) {
  # import dataframe
  keyword.list.all <- import("data/data_check/keywords list.xlsx", sheet=x) %>% 
    mutate(keyword=str_trim(tolower(keyword)), # remove leading and trailing white space, convert to lowercase
           keyword=gsub("\\//","\\\\",keyword)) %>%  # have no idea why we have to do this, using "\\b" in Excel creating issues
    distinct(keyword,.keep_all = T) 
  
  # keyword list
  keyword.list <- keyword.list.all %>%
    filter(level_of_confidence!=1) %>% # remove low confidence keywords (see definitions tab in excel sheet for details)
    mutate(keyword=str_c("\\b",keyword)) %>% # paste break term in front of each term
    pull(keyword)
  assign(paste0(tolower(x), ".list"), keyword.list.all, envir = parent.frame()) #assign variable name
  
  # low confidence list (those that require 2 marine terms)
  lowconfid.list <- keyword.list.all$keyword[keyword.list.all$level_of_confidence%in%c(2)]
  assign(paste0(tolower(x), ".low.confid"), lowconfid.list, envir = parent.frame())
  
  # to check list (new or older terms that we havent' checked yet)
  check.confid.list <- keyword.list.all$keyword[keyword.list.all$level_of_confidence%in%c(4,5)]
  assign(paste0(tolower(x), ".check.confid"), check.confid.list, envir = parent.frame())  
  return(keyword.list)
}

key.marine <- my_get_keyword("Marine")
key.sustainability <- my_get_keyword("Sustainability")
key.land <- my_get_keyword("Land")
key.equity <- my_get_keyword("Equity")
recogn.list <- equity.list$keyword[!is.na(equity.list$Recognition)]
proc.list <- equity.list$keyword[!is.na(equity.list$Procedural)]
distrib.list <- equity.list$keyword[!is.na(equity.list$Distributional)]
# str_c("\\b",marine.list$keyword)
# str_c("\\b",gsub("\\\\\\b","",marine.list$keyword, perl=TRUE))

#----- Identify terms in project title and description ----
# alternative way to find matching project along with terms
oecd.dat.to.search <- oecd.dat %>% 
  mutate(id=row_number(),
         comb = str_to_lower(paste(project,long_description,purpose,sector_name))) %>% # combine two fields to search
  select(-starts_with("V")) %>% 
  select(id,everything())
names(oecd.dat.to.search)
# take a subsample for keyword validation
oecd.sample <- slice_sample(oecd.dat.to.search,prop=0.25) 

# identify matching terms
start.time <- Sys.time()
oecd.dat2 <-  oecd.dat.to.search %>% 
    rowwise() %>% 
    mutate(marine.term= paste(unique(str_extract_all(comb,paste0(key.marine, collapse="|"))[[1]]),collapse="; "), # extract all matching terms
           n.marine.term=str_count(marine.term, '\\w+'), # count # different terms identified
           marine=ifelse(n.marine.term>1,1,0), # indicator to determine if the project meets criteria (to adjust)
           # sustainability projects
           sustain.term= paste(unique(str_extract_all(comb,paste0(key.sustainability, collapse="|"))[[1]]),collapse="; "),
           n.sustain.term=str_count(sustain.term, '\\w+'),
           sustainability=ifelse(n.sustain.term>1,1,0),
           # land projects
           land.term= paste(unique(str_extract_all(comb,paste0(key.land, collapse="|"))[[1]]),collapse="; "),
           n.land.term=str_count(land.term, '\\w+'),
           land=ifelse(n.land.term>1,1,0),
           # mentionning equity 
           equity.term= paste(unique(str_extract_all(comb,paste0(key.equity, collapse="|"))[[1]]),collapse="; "),
           n.equity.term=str_count(equity.term, '\\w+'),
           equity=ifelse(n.equity.term>1,1,0))

# identify high/low confidence projects
# if only 1 term --> futther check, if >1 , ok
oecd.dat2 <- oecd.dat2 %>%
  mutate(
    marine.confid = case_when(
      n.marine.term == 0 ~ "NA",
      n.marine.term == 1 & grepl(paste0(marine.check.confid, collapse="|"),marine.term) ~ "check", # terms that were not validated yet
      n.marine.term == 1 & grepl(paste0(marine.low.confid, collapse="|"),marine.term) ~ "low", # terms that require 2 terms
      TRUE ~ "high"), # high confidence = matches 1 high confidence term or >1 term
    sustain.confid = case_when(
      n.sustain.term == 0 ~ "NA",
      n.sustain.term == 1 & grepl(paste0(sustainability.check.confid, collapse="|"),marine.term) ~ "check", # Steph on the 26/02/2024 - sustain term instead of marine term?
      n.sustain.term == 1 & grepl(paste0(sustainability.low.confid, collapse="|"),sustain.term) ~ "low",
/      TRUE ~ "high"),
    land.confid = case_when(
      n.land.term  == 0 ~ "NA",
      n.land.term == 1 & grepl(paste0(land.check.confid, collapse="|"),land.term) ~ "check",
      n.land.term == 1 & grepl(paste0(land.low.confid, collapse="|"),land.term) ~ "low",
      TRUE ~ "high"),
    equity.confid = case_when(
      n.equity.term  == 0 ~ "NA",
      n.equity.term == 1 & grepl(paste0(equity.check.confid, collapse="|"),equity.term) ~ "check",
      n.equity.term == 1 & grepl(paste0(equity.low.confid, collapse="|"),equity.term) ~ "low",
      TRUE ~ "high"),
    recognition = ifelse(grepl(paste0(recogn.list, collapse="|"),equity.term),1,0),
    procedural = ifelse(grepl(paste0(proc.list, collapse="|"),equity.term),1,0),
    distributional = ifelse(grepl(paste0(distrib.list, collapse="|"),equity.term),1,0)
    
  ) 
# total time
Sys.time()-start.time

oecd.dat3<-  oecd.dat2 %>% 
  mutate(  
    marine = case_when(
      n.marine.term == 0 ~ 0,
      n.marine.term > 1 ~ 1,
      n.marine.term == 1 & marine.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      n.marine.term == 1 & marine.confid%in%c("low","check") ~ 2,
      TRUE ~ NA_integer_),
    sustainability = case_when(
      n.sustain.term == 0 ~ 0,
      n.sustain.term > 1 ~ 1,
      n.sustain.term == 1 & sustain.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      n.sustain.term == 1 & sustain.confid%in%c("low","check") ~ 2,
      TRUE ~ NA_integer_),
    land = case_when(
      n.land.term == 0 ~ 0,
      n.land.term > 1 ~ 1,
      n.land.term == 1 & land.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      n.land.term == 1 & land.confid%in%c("low","check") ~ 2,
      TRUE ~ NA_integer_),
    equity = case_when(
      gender_equality == 1 ~ 1, # trusting oecd coding
      n.equity.term == 0 ~ 0,
      n.equity.term > 1 ~ 1,
      n.equity.term == 1 & equity.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      n.equity.term == 1 & equity.confid%in%c("low","check") ~ 2,
      TRUE ~ NA_integer_),
    cadc=case_when( # 1 = CADC fir sure, 2 = lower certainty, 0 (not sure at all, include 2 and 2?) # check combination of confidence scores
      marine == 1 & sustainability == 1 | marine == 1 & land == 1~ 1,
      marine == 1 & sustainability == 2 | marine == 2 & sustainability == 1 ~ 2,
      marine == 1 & land == 2 | marine == 2 & land == 1 ~ 2,
      TRUE ~ 0))

# #check results
# term.check <- oecd.dat2 %>% 
#   filter(n.marine.term == 1 | n.sustain.term == 1 | n.land.term == 1 | n.equity.term == 1)
#     
# View(term.check)
# export(term.check,"data/data_check/keyword_check.csv")
export(oecd.dat3,here("data","data_check","ocean_ODA_2010_2021_equity.csv"))

############################
#   COASTAL COUNTRIES      #
############################

# load dataset with potential projects in coastal countries
oecd.dat <- import(here("data","raw-data","OECD","ocean_ODA_2010_2021.csv"))
#names(oecd)
summary(oecd.dat)

## map cumulative number of initiatives
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
projected.world = st_transform(world, crs.val.robinson)

# rename countries 
sort(unique(oecd.dat$recipient[!oecd.dat$recipient%in%projected.world$name_en]))
sort(unique(projected.world$name_en))

oecd <- oecd.dat %>% 
  mutate(
    cadc=ifelse(sustainable_ocean_ODA==TRUE|(landbased_ODA==TRUE&ocean_ODA==FALSE),"cadc","non-cadc"),
    equity.cadc=ifelse(equity==1 & cadc=="cadc","equity","not equity"), #non-adc take also not equity
    cadc.usd=ifelse(cadc=="cadc",disbursement_usd_million_defl,0),
    equity.usd=ifelse(equity.cadc=="equity",disbursement_usd_million_defl,0),
    cadc.type=case_when(
      equity.cadc=="equity" ~ "equity CADC",
      cadc=="cadc" ~ "not equity CADC",
      ocean_ODA==TRUE & cadc=="non-cadc" ~ "other ocean economy",
      TRUE ~ NA_character_),
    ctry=case_when(
      recipient=="Türkiye" ~ "Turkey",
      recipient=="Congo" ~ "Democratic Republic of the Congo",
      recipient=="Côte d'Ivoire" ~ "Ivory Coast",
      recipient=="Gambia" ~ "The Gambia",
      # recipient=="Mayotte" ~ "Turkey",
      # recipient=="Timor-Leste" ~ "Turkey",
      # recipient=="Tokelau" ~ "Turkey",
      recipient=="Viet Nam" ~ "Vietnam",
      # recipient=="West Bank and Gaza Strip" ~ "Turkey",
      recipient=="Cabo Verde" ~ "Cape Verde",
      recipient=="Sao Tome and Principe" ~ "São Tomé and Príncipe",
      recipient=="Syrian Arab Republic" ~ "Syria",
      TRUE ~  recipient)
  )

# check for remaining missing countries
sort(unique(oecd$ctry[!oecd$ctry%in%projected.world$name_en]))



saveRDS(CADC.OECD.ocean.wide.proj.ctr.sf,here("data","derived-data","ocean_ODA_CADC_2010_2021_equity_sf.RDS"))
