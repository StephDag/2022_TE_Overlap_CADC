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
iati.coastal <- readRDS(here("data","derived-data","all.potential.coastal.unique.rds"))

#names(all.gps.sp.100m.100km)[grepl("languag",names(all.gps.sp.100m.100km))] # code to search column names
#dim(all.gps.sp.100m.100km)
# 
# %>% 
# ########################################################################
# # list of keywords


# # number of projects by key words
# # updates on the 29/09/2023
# key.marine <- c("\\bcoast","\\bmarin","\\bsea\\b","\\bseas\\b","\\bsea-\\b","\\bocean","\\bshore","\\bblue\\b","\\bbeach","\\bbmaritime","\\blittoral\\b","mangrove","seagrass","saltmarsh","\\breef","kelp",
#                 "\\bestuar","coral","\\bfish","\\finfish","\\bwave","\\btidal","\\balgae","marsh","\\baquac","\\bmariculture", "\\bsubmarine","\\bdesalination","\\boffshore","\\bport\\b","\\bports\\b",
#                 "\\bseaweed","\\bcrustacean","\\bmollus","\\bwetland","\\btransitional water","\\blagoon","\\bsalt pond","bshell","\\bseamount","\\bmid-ocean ridge","\\bcentral gyre","\\bdeep sea","\\bship","\\bwater transport",
#                 "\\bfinfish\\b", "\\bcontinental shelf","\\bsand","\\bnautical","\\batoll","\\bunderwater","\\bdemersal","\\baphotic", "pelagic","neritic","surf\\b","surf-\\b","diver\\b","dive\\b","SCUBA\\b","CPUE","\\bbivalve","\\bboat",
#                 "\\bcaribbean biodiversity fund", "\\bclam", "\\bcockle", "\\bcrab", "\\bcruise","\\bdeep water", "\\bdesalini", "\\bdolphin", "\\bdredg", "\\bgrouper",
#                 "\\bgulf", "\\bharbor", "\\bharbour",  "\\blionfish", "\\bmpa", "\\bmussel", "\\bnaval", "\\boyster","\\bprawn", 
#                 "\\bproblue", "\\bsnapper", "\\bsaltwater", "\\bsargassum", "\\bshark", "\\bshrimp", "\\btuna", "\\bturtle", "\\bunclos", "\\bvessel", "\\bwhale", "\\bwharf",
#                 "\\bcôte\\b","\\bcôtière","\\bcôtier","\\bpêche","\\bpêcheur","\\bpêcherie","\\bocéan","\\bleue","\\bplage",  # french translation
#                 "\\bmaritime","\\blittoral","\\bherbier","\\bmarais salant\\b","\\bmarée","\\bvague","\\balgues","\\bdessalement","\\btransport maritime\\b","\\beaux usées\\b","\\bruisselement","\\bsous-marin",
#                 "\\bcrustacé","\\bmollusque","\\bzones humides","\\bzone humide","\\beaux de transition","\\blagon","\\bétang salé","\\bétangs salés","\\bcoquillage",
#                 "dorsale médio-océanique","dorsales médio-océaniques","\\bgyre","eaux profondes","eau profonde","gyre océanique","\\bbateau","\\bpoisson","\\bnautique","\\bdémersa","pélagique","\\bplongée\\b","\\bplongeur\\b",
#                 "\\bcapture","coquillage","crabe","\\bcoque","croisière","mérou","\\bdauphin","\\bcrevette","\\bsargasse","requin","thon","tortue","baleine","\\bmoule")
# 
# key.sustainability <- c("\\bacidifica", "\\badapt", "\\badaptation", "\\bbio sphere","\\bbio-logical diversity","\\bbio-sphere","\\bbiodiversity", "\\bbio-diversity","\\bbiological diversity","\\bbiosphere",
#                         "\\bcarbon sink","\\bclimate change","\\bconserv","\\bdisaster risk reduction", "\\bDRR", "\\beco-system", "\\beco-touris", "\\becosystem", "\\becotouris", "\\benvironment","\\becological",
#                         "\\bspecies","\\beutriphication", "\\bhabitat", "\\biuu", "\\bmarine debris ", "\\bmarine energy", "\\bmarine protection", "\\bmitigate", "\\bnature-based solutions",
#                         "capacity-build","capacity build","capacity develop", "\\badapt","\\bocean health","\\bocean protection", "\\bvulnerab", "tenure\\b", "TURF", "territorial user rights",
#                         "\\boceanograph","\\boff-shore wind", "\\boffshore wind", "\\bover-fishing", "\\boverfishing","\\bclimat",
#                         "\\boxygen", "\\bpreserv", "\\bprotected area", "\\brenewable", "\\bresilien","\\bresponse preparedness", "\\brestoration", "\\brestore", "\\brestoring", "\\bSDG 14", "\\bsea level rise", "\\bsea-level rise", "\\bsequestration", "\\bslr","\\bsustainab", "\\btidal energy", "\\bwildlife", "\\bwind energy","\\bbblue",
#                         "\\bbmpa","\\bblmma", "\\bboecm", "reserve", "park","no-take","natural monument","enclosure","closure","wilderness area", "payment for ecosystem services", "PES", "\\bbMSC\\b","certification","eco-label", "ecotourism", ("communit((\\w+)(\\W+)){1,3}management\\b"), 
#                         "conserv((\\w+)(\\W+)){1,3}management\\b", ("communit((\\w+)(\\W+)){1,3}conserv"), ("communit((\\w+)(\\W+)){1,3}fish"), ("communit((\\w+)(\\W+)){1,3}natural resource"), 
#                         "comanagement\\b", "co-management\\b",
#                         "\\bbiodiversité", "\\bbio-diversité","\\diversité biologique","\\bpuit de carbone","\\bchangement climatique","\\bconserv","\\bdisaster risk reduction", "\\bDRR", "\\béco-system", "\\béco-touris", "\\bécosystem", "\\bécotouris", "\\benvironment","\\bécologique",
#                         "\\bespèces","\\beutrophication", "\\bhabitat", "\\bdébris marin", "\\benergie marine", "\\bprotection marine", "\\bmitigate", "\\bsolutions fondées sur la nature", "\\bsolutions basées sur la nature",
#                         "renforcement de capacité", "développement de capacités", "\\badapt","\\bprotection des océans", "\\bvulnerab","droits territoriaux",
#                         "\\boceanograph","\\boff-shore wind", "\\béolien offshore","\\béolien en mer","\\bsurpeche",
#                         "\\boxygène", "\\bpreserv", "\\baire protégée", "\\brenouvelable", "\\brecherche", "\\brésilien","\\bpréparation à la réponse", "\\brestoration", "\\brestor","\\bscience",
#                         "\\bscientifi", "\\bODD 14", "\\bmontée des eaux", "\\bsequestration","\\bdurab", "\\bénergie marémotrice", "\\bfaune", "\\bénergie éolienne","\\bbbleu",
#                         "\\bamp", "reserve", "parc","monument naturel","fermeture","zone sauvage", "paiement pour services écosystémiques", "PES", "\\bbMSC\\b","certification","eco-label", "ecotourism",
#                         ("communit((\\w+)(\\W+)){1,3}gestion\\b"), 
#                         "conserv((\\w+)(\\W+)){1,3}gestion\\b", ("communit((\\w+)(\\W+)){1,3}conserv"), ("communit((\\w+)(\\W+)){1,3}fish"), ("communit((\\w+)(\\W+)){1,3}natural resource"), 
#                         "comanagement\\b", "co-gestion\\b")
# 
# key.land <- c("\\bwastewater","\\bwaste water","\\brunoff","\\bmarine debris",
#               "\\bmarine litter","\\bmarine plastic","\\bocean debris", "\\bocean litter", "\\bocean plastic",
#               "\\bocean pollut","\\bwaste","\\bmarine pollut","\\bwater treatment","\\bseawage","\\bnutrient pollution","water resources conservation","river basins development",
#               "\\beaux usees","\\beaux usees","\\brunoff","\\bdebris marins", # french
#               "\\bdechets marins","\\bplastique marin","\\bpollution marine","\\dechets marins","plastique oceanique",
#               "pollution des oceans","\\bdechets","pollution marine","traitement des eaux","eaux marines","pollution par les nutriments",
#               "conservation des ressources en eau"," développement des bassins fluviaux")
# 
# key.equity <- c("\\baccountab","\\baffordable housing","\\bageism","\\banti-discriminatory","\\banti-oppressive",
#                 "\\banti-racist","\\bbasic living standards","\\bbasic services","\\bbenefit-sharing ","\\bbipoc","\\bburden ","\\bclassism ",
#                 "\\bco-benefit ","\\bco-production","\\bcompensate ","\\bcompensation ","\\bconflict resolution","\\bconflict","\\bconsultation",
#                 "\\bcorrectability","\\bcost-sharing","\\bcultural values","\\bdecision control", "\\bvoice","\\bdecolonization","\\bdignity","\\bdisabilit","\\bdisadvantaged",
#                 "\\bdiscriminat","\\bdisparity","\\bdiverse groups","\\bempower","\\bend poverty","\\benvironmental and social safeguards framework","ESSF","\\bequal access",
#                 "\\bequal opportunit","\\bequal pay","\\bequality","\\bequit","\\bethicality","\\bethnicity","\\bexploitation","\\bextreme poverty","\\bfair",
#                 "\\bfemale","\\bfemale genital mutilation","\\bfeminis","\\bfinancial inclusion","\\bforced marriage","\\bfpic","\\bgender","\\bgini","\\bgirl","\\bgrievance",
#                 "\\bhomeless","\\bhomophobia","\\bhuman rights","\\bhuman trafficking","\\bidentities","\\bimpoverished","\\binclusion","\\binclusiv",
#                 "\\bincome distribution","\\bincome equality","\\bincome inequality","\\bindigenous","\\binequalit","\\binjustice","\\bknowledge systems","\\blgbtq",
#                 "\\bmarginalise","\\bmarginalize","\\bmicrofinance","\\bminorit","\\bno poverty","\\bnon binary","\\bnon-binary","\\bnon-discriminat","\\bof color","\\bof colour","\\bparity",
#                 "\\bparticipat","\\bpatriarchy","\\bpoor","\\bpoverty eradication","\\bpoverty line","\\bprivileged","\\bpro-poor","\\bqueer","\\brace","\\bracism","\\breconciliat",
#                 "\\brefugee","\\breligion","\\breproductive health","\\breproductive rights","\\brights","\\bsafeguard","\\bsex and inequality","\\bsexes","\\bsexism","\\bsexual and reproductive health",
#                 "\\bsexual exploitation","\\bsexual health","\\bsexual violence","\\bsocial inclusion","\\bsocial monitoring","\\bsocial protection","\\bsocial protection systems","\\bsocial safety","\\bsocial security",
#                 "\\btrade-off","\\btradeoff","\\btraditional ecological knowledge","\\btraditional knowledge","\\btrafficking","\\btransgender","\\btrust","\\btrustworthiness",
#                 "\\btwo-spirit","\\bunderserved","\\buniversal health coverage","\\bviolence against girls","\\bviolence against women","\\bviolence and girls","\\bviolence and women",
#                 "\\bvulnerability","\\bvulnerable","\\bwealth distribution","\\bwelfare","\\bwomen","\\byouth")
# 



# ############################
# #        COASTAL           #
# ############################
# 
# # new variable - 1/0 coastal
# all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
#   mutate(Year = substring(activity_date_iso_date,1,4),      # start year # 392946    257
#          comb = str_to_lower(paste(title_narrative,description_narrative, sep = ". "))) %>% 
#   filter(Year %in% seq(2000,2022,1)) %>%
#   mutate(coastal= ifelse(str_detect(comb,paste0(key.marine, collapse="|")),1,0),
#          coastal= as.factor(ifelse(is.na(coastal),0,coastal))) # keep the NAs in title or narrative, since it might be filtered through the OECD codes
# summary(all.gps.sp.100m.100km$coastal) #         0:98451     1: 25260  
# 
# ###########################################
# #             SUSTAINABILITY              #
# ###########################################
# 
# # new variable - 1/0 sustainability
# all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
#   mutate(Year = substring(activity_date_iso_date,1,4),      # start year # 392946    257
#          comb = str_to_lower(paste(title_narrative,description_narrative))) %>% 
#   filter(Year %in% seq(2000,2022,1)) %>%
#   mutate(sustainable= ifelse(str_detect(comb,paste0(key.sustainability, collapse="|")),1,0),
#          sustainable= as.factor(ifelse(is.na(sustainable),0,sustainable))) # keep the NAs in title or narrative, since it might be filtered through the OECD codes
# summary(all.gps.sp.100m.100km$sustainable) #      0: 62812      1: 60899 
# 
# ###########################################
# #        LAND-BASED           #
# ###########################################
# 
# # new variable - 1/0 land based
# all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
#   mutate(Year = substring(activity_date_iso_date,1,4),      # start year # 392946    257
#          comb = str_to_lower(paste(title_narrative,description_narrative))) %>% 
#   filter(Year %in% seq(2000,2022,1)) %>%
#   mutate(land= ifelse(str_detect(comb,paste0(key.land, collapse="|")),1,0),
#          land= as.factor(ifelse(is.na(land),0,land))) # keep the NAs in title or narrative, since it might be filtered through the OECD codes
# summary(all.gps.sp.100m.100km$land) #      0: 121261      1: 2450
# 
# #############################################
# #            equity keywords                #
# #############################################
# 
# all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
#   mutate(Year = substring(activity_date_iso_date,1,4),      # start year # 392946    257
#          comb = str_to_lower(paste(title_narrative,description_narrative))) %>% 
#   filter(Year %in% seq(2000,2022,1)) %>%
#   mutate(equity= ifelse(str_detect(comb,paste0(key.equity, collapse="|")),1,0),
#          equity= as.factor(ifelse(is.na(equity),0,equity))) # keep the NAs in title or narrative, since it might be filtered through the OECD codes
# summary(all.gps.sp.100m.100km$equity) #      0: 121261      1: 2450
# 
# #############################################
# #           add sector codes                #
# #############################################
# 
# # load sector code list
# sector.list <- import(here("data","derived-data","sector.list.coded.csv"))
# #load dac code database
# dac.cadc <- import(here("data","derived-data","DAC.code.conversion.xlsx"),sheet="5-3 sector-CADC") %>%
#   select(dac5.code="code", dac3.code="category",cadc="CADC", climate,development,conservation)
# head(dac.cadc)
# table(dac.cadc$cadc)
# 
# cadc.project.ids <- unique(sector.list$iati_identifier_bis[sector.list$cadc==1])
# climate.project.ids <- unique(sector.list$iati_identifier_bis[sector.list$climate==1])
# dev.project.ids <- unique(sector.list$iati_identifier_bis[sector.list$development==1])
# cons.project.ids <- unique(sector.list$iati_identifier_bis[sector.list$conservation==1])
# unique(sector.list$name[sector.list$cadc==1])
# 



# # 3. add Chechi's coding of project CADC type 
# dac.climate <- dac.cadc %>% 
#   filter(climate==1) %>% 
#   distinct(dac5.code,.keep_all = T) 
# dac.dev <- dac.cadc %>% 
#   filter(development==1) %>% 
#   distinct(dac5.code,.keep_all = T) 
# dac.cons <- dac.cadc %>% 
#   filter(conservation==1) %>% 
#   distinct(dac5.code,.keep_all = T) 
# 
# # 3b. add DAC descriptive info 
# sector.list.coded <- sector.list.tmp %>% 
#   mutate(across(dac3.code,~ as.integer(.x))) %>% 
#   left_join(dac.3.code,by=c("dac3"="dac3.code")) %>% 
#   left_join(select(dac.5.code,-dac3.code),by=c("dac5"="dac5.code")) %>% 
#   mutate(name=ifelse(!is.na(name.y),name.y,name.x), # use DAC5 description where available 
#          description=ifelse(!is.na(description.y),description.y,description.x),
#          status=ifelse(!is.na(status.y),status.y,status.x),
#          climate=ifelse(dac3%in%dac.climate$dac3.code | dac5%in%dac.climate$dac5.code,1,climate),
#          development=ifelse(dac3%in%dac.dev$dac3.code | dac5%in%dac.dev$dac5.code,1,development),
#          conservation=ifelse(dac3%in%dac.cons$dac3.code | dac5%in%dac.cons$dac5.code,1,conservation)) %>% 
#   select(temp.id:has.sdg,dac3,dac5,name:status,cadc:certainty)
# names(sector.list.coded)


# test <- import("data/data_check/keywords list.xlsx", sheet="Marine")
# test$keyword
# gsub("\\//","\\\\",test$keyword, perl=F)
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
iati.coastal.to.search <- iati.coastal %>% 
  mutate(Year = substring(activity_date_iso_date,1,4),      # start year # 392946    257
         comb = str_to_lower(paste(title_narrative,description_narrative))) %>% # combine two fields to search
  filter(Year %in% seq(2000,2022,1)) %>% 
  select(-c(recipient_country_narrative,activity_date_iso_date,title_narrative,description_narrative)) %>% 
  distinct(iati_identifier,.keep_all = T)

# take a subsample for keyword validation
iati.sample <- slice_sample(iati.coastal.to.search,prop=0.25) 

# identify matching terms
start.time <- Sys.time()
iati.coastal2 <-  iati.coastal.to.search %>% 
    rowwise() %>% 
    mutate(marine.term= paste(unique(str_extract_all(comb,paste0(key.marine, collapse="|"))[[1]]),collapse="; "), # extract all matching terms
           n.marine.term=str_count(marine.term, '\\w+'), # count # different terms identified
           marine=ifelse(n.marine.term>1,1,0), # indicator to determine if the project meets criteria (to adjust)
           sustain.term= paste(unique(str_extract_all(comb,paste0(key.sustainability, collapse="|"))[[1]]),collapse="; "),
           n.sustain.term=str_count(sustain.term, '\\w+'),
           sustainability=ifelse(n.sustain.term>1,1,0),
           land.term= paste(unique(str_extract_all(comb,paste0(key.land, collapse="|"))[[1]]),collapse="; "),
           n.land.term=str_count(land.term, '\\w+'),
           land=ifelse(n.land.term>1,1,0),
           equity.term= paste(unique(str_extract_all(comb,paste0(key.equity, collapse="|"))[[1]]),collapse="; "),
           n.equity.term=str_count(equity.term, '\\w+'),
           equity=ifelse(n.equity.term>1,1,0))

# identify high/low confidence projects
iati.coastal2 <- iati.coastal2 %>%
  mutate(
    marine.confid = case_when(
      n.marine.term == 0 ~ "NA",
      n.marine.term == 1 & grepl(paste0(marine.check.confid, collapse="|"),marine.term) ~ "check", # terms that were not validated yet
      n.marine.term == 1 & grepl(paste0(marine.low.confid, collapse="|"),marine.term) ~ "low", # terms that require 2 terms
      TRUE ~ "high"), # high confidence = matches 1 high confidence term or >1 term
    sustain.confid = case_when(
      n.sustain.term == 0 ~ "NA",
      n.sustain.term == 1 & grepl(paste0(sustainability.check.confid, collapse="|"),marine.term) ~ "check",
      n.sustain.term == 1 & grepl(paste0(sustainability.low.confid, collapse="|"),sustain.term) ~ "low",
      TRUE ~ "high"),
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
    distributional = ifelse(grepl(paste0(distrib.list, collapse="|"),equity.term),1,0)) %>% 
    select(iati_identifier:marine,marine.confid,
           sustain.term:sustainability,sustain.confid,
           land.term:land,land.confid,
           equity.term:equity.confid,recognition:distributional)

# total time
Sys.time()-start.time

# recode CADC and equity dummy variables based on current review (temporary)
iati.coastal3 <-  iati.coastal2 %>% 
  mutate(  
    marine = case_when(
      n.marine.term == 0 ~ 0,
      n.marine.term > 1 ~ 1,
      n.marine.term == 1 & marine.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      n.marine.term == 1 & marine.confid%in%c("low","check") ~ 2),
    sustainability = case_when(
      sustain.term == 0 ~ 0,
      sustain.term > 1 ~ 1,
      sustain.term == 1 & sustain.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      sustain.term == 1 & sustain.confid%in%c("low","check") ~ 2),
    land = case_when(
      land.term == 0 ~ 0,
      land.term > 1 ~ 1,
      land.term == 1 & land.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      land.term == 1 & land.confid%in%c("low","check") ~ 2),
    equity = case_when(
      equity.term == 0 ~ 0,
      equity.term > 1 ~ 1,
      equity.term == 1 & equity.confid == "high" ~ 1, # high confidence = matches 1 high confidence term or >1 term
      equity.term == 1 & equity.confid%in%c("low","check") ~ 2),
    cadc=case_when(
      marine == 1 & sustainability == 1 | marine == 1 & land == 1~ 1,
      marine == 1 & sustainability == 2 | marine == 2 & sustainability == 1 ~ 2,
      TRUE ~ 0))
         
         
export(iati.coastal2,here("data","data_check","iati_coastal_check.csv"))

#check results
term.check <- iati.coastal2 %>% 
  filter(n.marine.term == 1 | n.sustain.term == 1 | n.land.term == 1 | n.equity.term == 1)
    
View(term.check)
export(term.check,"data/data_check/keyword_check.csv")


##### select sustainable ocean economy (CADC) projects
iati.coastal3 <- iati.coastal2 %>%
  filter((marine == 1 & sustainable == 1) | land == 1)
length(unique(iati.coastal3$iati_identifier)) # 7612 projects, both coastal and sustainable and potentially land-based


#----- previous code----- #
# ##### estimate the number of CADC projects
# all.gps.sp.100m.100km.CADC <- all.gps.sp.100m.100km %>%
#   filter((coastal == 1 & sustainable == 1) | land == 1)
# length(unique(all.gps.sp.100m.100km.CADC$iati_identifier)) # 7612 projects, both marine and sustainable and potentially land-based
# 
# # spot check to see if these are indeed CADC
# keyword.check <- all.gps.sp.100m.100km.CADC %>% 
#   as.data.frame() %>% 
#   filter((!is.na(document_link_url)|!is.na(result_document_link_url)) & equity==1) %>% 
#   select(iati_identifier_bis,marine,sustainable,land,equity,iso_a3,title_narrative,description_narrative,linked_data_uri,result_document_link_url,document_link_url)
# #export(slice(keyword.check,sample(nrow(keyword.check),100)),"data/CADC_results_docs_check.csv")
# 
# # which words are being picked up
# samp.rows <-sample(length(unique(keyword.check$iati_identifier_bis)),500)
# word.check <- keyword.check %>% 
#   distinct(iati_identifier_bis,.keep_all = T) %>% 
#   mutate(comb=tolower(paste(title_narrative,description_narrative)))%>% 
#   slice(samp.rows) %>% 
#   select(iati_identifier_bis,marine,sustainable,land,equity,title_narrative,description_narrative,comb)
# 
# word.check$marine.word <- NA
# word.check$sust.word <- NA
# word.check$land.word <- NA
# word.check$equity <- NA
# for (i in 1:nrow(word.check)){
#   for (j in 1:length(key.marine)){
#     mar.term <- key.marine[j]
#     if(str_detect(word.check$comb[i],mar.term)==TRUE){
#       print(paste("Marine term: ", mar.term))
#       word.check$marine.word[i] <- mar.term
#     }
#   }
# }
# for (i in 1:nrow(word.check)){
#   for (j in 1:length(key.sustainability)){
#     sus.term <- key.sustainability[j]
#     if(str_detect(word.check$comb[i],sus.term)==TRUE){
#       print(paste("Marine term: ", sus.term))
#       word.check$sust.word[i] <- sus.term
#     }
#   }
# }
# for (i in 1:nrow(word.check)){
#   for (j in 1:length(key.land)){
#     land.term <- key.land[j]
#     if(str_detect(word.check$comb[i],land.term)==TRUE){
#       print(paste(i,"Marine term: ", land.term))
#       word.check$land.word[i] <- land.term
#     }
#   }
# }
# 
# for (i in 1:nrow(word.check)){
#   for (j in 1:length(key.equity)){
#     equity.term <- key.equity[j]
#     if(str_detect(word.check$comb[i],equity.term)==TRUE){
#       print(paste(i,"Marine term: ", equity.term))
#       word.check$equity.word[i] <- equity.term
#     }
#   }
# }
# 
# word.check %>% 
#   group_by(marine.word) %>% 
#   count() %>% 
#   arrange(-n) %>% 
#   view
# 
# word.check %>% 
#   group_by(sust.word) %>% 
#   count() %>% 
#   arrange(-n) 
# 
# word.check %>% 
#   group_by(land.word) %>% 
#   count() %>% 
#   arrange(-n)
# 
# word.check %>% 
#   group_by(equity.word) %>% 
#   count() %>% 
#   arrange(-n)
# 
# export(word.check,"data/keyword_check.csv")
# 
# ### test potential terms
# 
# test.key.equity <- c("\\badaptive management","\\bambassadors","\\bchampions","\\bcorrectability","\\bfemale","\\bgirl","\\bjustice","\\bleadership",
#                      "\\bmicrofinance","\\bpoverty line","\\breligio*","\\bsexes","\\bsocial monitoring","\\btrade-off","\\btradeoff",
#                      "\\bunderserved", "\\bcompensation", "\\bsexes", "\\bempower", "\\bsocial security")
# test.key.marine <- c("\\bdivers\\b", "\\blittoral", "\\bwave", "\\binternational water", "\\bsand\\b", "\\bdredg",
#                      "\\boffshore", "\\bunderwater ", "\\beep water", "\\bharbor", "\\bharbour","\\bship")
# test.key.sustain <- c("\\bmitigate","\\bparc","\\bpark", "\\bscience","\\bscientific", "\\bresearch", "\\bclosure", 
#                       "\\badapt", "\\bPES", "\\bpreserve")
# 
# test.equity <- all.gps.sp.100m.100km %>%
#   as.data.frame() %>% 
#   mutate(Year = substring(activity_date_iso_date,1,4),      # start year # 392946    257
#          comb = str_to_lower(paste(title_narrative,description_narrative))) %>% 
#   filter(Year %in% seq(2000,2022,1) & marine==1) %>%
#   mutate(equity= ifelse(str_detect(comb,paste0(test.key.equity, collapse="|")),1,0),
#          equity= as.factor(ifelse(is.na(equity),0,equity))) %>%  # keep the NAs in title or narrative, since it might be filtered through the OECD codes
#   filter(equity==1) %>% 
#   select(iati_identifier_bis,comb) %>% 
#   distinct(iati_identifier_bis,.keep_all = T)
# 
# test.equity$equity.word <- NA
# for (i in 1:nrow(test.equity)){
#   for (j in 1:length(test.key.equity)){
#     equity.term <- test.key.equity[j]
#     if(str_detect(test.equity$comb[i],equity.term)==TRUE){
#       test.equity$equity.word[i] <- equity.term
#     }
#   }
# }
# 
# export(test.equity,"data/equity_keyword_check.csv")
# 
# test.marine <- all.gps.sp.100m.100km %>%
#   as.data.frame() %>% 
#   mutate(Year = substring(activity_date_iso_date,1,4),      # start year # 392946    257
#          comb = str_to_lower(paste(title_narrative,description_narrative))) %>% 
#   filter(Year %in% seq(2000,2022,1)) %>%
#   mutate(test.marine= ifelse(str_detect(comb,paste0(test.key.marine, collapse="|")),1,0),
#          test.marine= as.factor(ifelse(is.na(test.marine),0,test.marine)),
#          test.sustain= ifelse(str_detect(comb,paste0(test.key.sustain, collapse="|")),1,0),
#          test.sustain= as.factor(ifelse(is.na(test.sustain),0,test.sustain))) %>%  # keep the NAs in title or narrative, since it might be filtered through the OECD codes
#   filter(test.marine==1 |test.sustain==1) %>% 
#   select(iati_identifier,comb,marine) %>% 
#   distinct(iati_identifier,.keep_all = T)
# 
# test.marine$marine.word <- NA
# for (i in 1:nrow(test.marine)){
#   for (j in 1:length(test.key.marine)){
#     marine.term <- test.key.marine[j]
#     if(str_detect(test.marine$comb[i],marine.term)==TRUE){
#       print(paste(i,"Marine term: ", marine.term))
#       test.marine$marine.word[i] <- marine.term
#     }
#   }
# }
# test.marine$sustain.word <- NA
# for (i in 1:nrow(test.marine)){
#   for (j in 1:length(test.key.sustain)){
#     sustain.term <- test.key.sustain[j]
#     if(str_detect(test.marine$comb[i],sustain.term)==TRUE){
#       print(paste(i,"sustain term: ", sustain.term))
#       test.marine$sustain.word[i] <- sustain.term
#     }
#   }
# }
# export(test.marine,"data/marine_keyword_check.csv")
# 
# # convert to RIS
# test.marine.ris <- test.marine %>% 
#   select(TI=iati_identifier_bis,
#          ABST=comb)
# 
# detect_parser(test.marine.ris) # = "parse_ris"
# test.marine.ris <- parse_ris(test.marine.ris)
# synthesisr::write_refs(test.marine.ris, tag_naming = "synthesisr", format = "ris", "data/marine_keyword_check.ris")
# export(test.marine.ris,"data/marine_keyword_check.ris.csv")
# convert("data/marine_keyword_check.ris.csv", "data/marine_keyword_check.ris")
# ##### save the coded/sectors, keywords full db
# saveRDS(all.gps.sp.100m.100km.CADC,here("data","derived-data","iati_GPS_db_100km_100m_keywords_sectors.rds"))
# dim(all.gps.sp.100m.100km.CADC)


# - Potential words
#key.equity.test <- cc("\\badaptive management","\\bambassadors","\\bchampions","\\bcorrectability","\\bfemale","\\bgirl*","\\bjustice","\\bleadership","\\bmicrofinance","\\bpoverty line","\\breligio*","\\bsexes","\\bsocial monitoring","\\btrade-off*","\\btradeoff*")


