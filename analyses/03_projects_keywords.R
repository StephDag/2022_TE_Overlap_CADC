# Categorized geographic projects based based on coastal, sustainability (CADC) and sustainable land-based projects
# outputs: 3 terrestrial raster of 5km resolution each
# Stephanie D'Agata
# Sept 2023
# Updates: Sept 2023
library(here)
source(here("analyses","00_setup.R"))

# load data
all.gps.sp.100m.100km <- readRDS(here("data","derived-data","iati_GPS_db_100km_100m.rds"))
# load sector code list
sector.list <- import(here("data","derived-data","sector.list.coded.csv"))

#names(all.gps.sp.100m.100km)[grepl("languag",names(all.gps.sp.100m.100km))] # code to search column names
#dim(all.gps.sp.100m.100km)

# list of keywords
# number of projects by key words
# updates on the 29/09/2023
key.marine <- c("\\bcoast","\\bmarin","\\bsea\\b","\\bseas\\b","\\bsea-\\b","\\bocean","\\bshore","\\bblue\\b","\\bbeach","\\bbmaritime","\\blittoral\\b","mangrove","seagrass","saltmarsh","\\breef","kelp",
                "\\bestuar","coral","\\bbfish","\\finfish","\\bwave","\\btidal","\\balgae","marsh","\\baquac","\\bmariculture", "\\bsubmarine","\\bdesalination","\\boffshore","\\bport\\b","\\bports\\b",
                "\\bseaweed","\\bcrustacean","\\bmollus","\\bwetland","\\btransitional water","\\blagoon","\\bsalt pond","bshell","\\bseamount","\\bmid-ocean ridge","\\bcentral gyre","\\bdeep sea","\\bship","\\bwater transport",
                "\\bfinfish\\b", "\\bcontinental shelf","\\bsand","\\bnautical","\\batoll","\\bunderwater","\\bdemersal","\\baphotic", "pelagic","neritic","surf\\b","surf-\\b","diver\\b","dive\\b","SCUBA\\b","CPUE","\\bbivalve","\\bboat",
                "\\bcaribbean biodiversity fund", "\\bclam", "\\bcockle", "\\bcrab", "\\bcruise","\\bdeep water", "\\bdesalini", "\\bdolphin", "\\bdredg", "\\bgrouper",
                "\\bgulf", "\\bharbor", "\\bharbour", "\\binternational water", "\\blionfish", "\\bmpa", "\\bmussel", "\\bnaval", "\\boyster","\\bprawn", 
                "\\bproblue", "\\bsnapper", "\\bsaltwater", "\\bsargassum", "\\bshark", "\\bshrimp", "\\btuna", "\\bturtle", "\\bunclos", "\\bvessel", "\\bwhale", "\\bwharf",
                "\\bcôte\\b","\\bcôtière","\\bcôtier","\\bpêche","\\bpêcheur","\\bpêcherie","\\bocéan","\\bleue","\\bplage",  # french translation
                "\\bmaritime","\\blittoral","\\bherbier","\\bmarais salant\\b","\\bmarée","\\bvague","\\balgues","\\bdessalement","\\btransport maritime\\b","\\beaux usées\\b","\\bruisselement","\\bsous-marin",
                "\\bcrustacé","\\bmollusque","\\bzones humides","\\bzone humide","\\beaux de transition","\\blagon","\\bétang salé","\\bétangs salés","\\bcoquillage",
                "dorsale médio-océanique","dorsales médio-océaniques","\\bgyre","eaux profondes","eau profonde","gyre océanique","\\bbateau","\\bpoisson","\\bnautique","\\bdémersa","pélagique","\\bplongée\\b","\\bplongeur\\b",
                "\\bcapture","coquillage","crabe","\\bcoque","croisière","mérou","\\bdauphin","\\bcrevette","\\bsargasse","requin","thon","tortue","baleine","\\bmoule")

key.sustainability <- c("\\bacidifica", "\\badapt", "\\badaptation", "\\bbio sphere","\\bbio-logical diversity","\\bbio-sphere","\\bbiodiversity", "\\bbio-diversity","\\bbiological diversity","\\bbiosphere",
                        "\\bcarbon sink","\\bclimate change","\\bconserv","\\bdisaster risk reduction", "\\bDRR", "\\beco-system", "\\beco-touris", "\\becosystem", "\\becotouris", "\\benvironment","\\becological",
                        "\\bspecies","\\beutriphication", "\\bhabitat", "\\biuu", "\\bmarine debris ", "\\bmarine energy", "\\bmarine protection", "\\bmitigate", "\\bnature-based solutions",
                        "capacity-build","capacity build","capacity develop", "\\bbadapt","\\bocean health","\\bocean protection", "\\bvulnerab", "tenure\\b", "TURF", "territorial user rights",
                        "\\boceanograph","\\boff-shore wind", "\\boffshore wind", "\\bover-fishing", "\\boverfishing","\\bclimat",
                        "\\boxygen", "\\bpreserv", "\\bprotected area", "\\brenewable", "\\bresearch", "\\bresilien","\\bresponse preparedness", "\\brestoration", "\\brestore", "\\brestoring", "\\bscience",
                        "\\bscientifi", "\\bSDG 14", "\\bsea level rise", "\\bsea-level rise", "\\bsequestration", "\\bslr","\\bsustainab", "\\btidal energy", "\\bwildlife", "\\bwind energy","\\bbblue",
                        "\\bbmpa","\\bblmma", "\\bboecm", "reserve", "park","no-take","natural monument","enclosure","closure","wilderness area", "payment for ecosystem services", "PES", "\\bbMSC\\b","certification","eco-label", "ecotourism", ("communit((\\w+)(\\W+)){1,3}management\\b"), 
                        "conserv((\\w+)(\\W+)){1,3}management\\b", ("communit((\\w+)(\\W+)){1,3}conserv"), ("communit((\\w+)(\\W+)){1,3}fish"), ("communit((\\w+)(\\W+)){1,3}natural resource"), 
                        "comanagement\\b", "co-management\\b",
                        "\\bbiodiversité", "\\bbio-diversité","\\diversité biologique","\\bpuit de carbone","\\bchangement climatique","\\bconserv","\\bdisaster risk reduction", "\\bDRR", "\\béco-system", "\\béco-touris", "\\bécosystem", "\\bécotouris", "\\benvironment","\\bécologique",
                        "\\bespèces","\\beutrophication", "\\bhabitat", "\\bdébris marin", "\\benergie marine", "\\bprotection marine", "\\bmitigate", "\\bsolutions fondées sur la nature", "\\bsolutions basées sur la nature",
                        "renforcement de capacité", "développement de capacités", "\\bbadapt","\\bprotection des océans", "\\bvulnerab","droits territoriaux",
                        "\\boceanograph","\\boff-shore wind", "\\béolien offshore","\\béolien en mer","\\bsurpeche",
                        "\\boxygène", "\\bpreserv", "\\baire protégée", "\\brenouvelable", "\\brecherche", "\\brésilien","\\bpréparation à la réponse", "\\brestoration", "\\brestor","\\bscience",
                        "\\bscientifi", "\\bODD 14", "\\bmontée des eaux", "\\bsequestration","\\bdurab", "\\bénergie marémotrice", "\\bfaune", "\\bénergie éolienne","\\bbbleu",
                        "\\bbamp", "reserve", "parc","monument naturel","fermeture","zone sauvage", "paiement pour services écosystémiques", "PES", "\\bbMSC\\b","certification","eco-label", "ecotourism",
                        ("communit((\\w+)(\\W+)){1,3}gestion\\b"), 
                        "conserv((\\w+)(\\W+)){1,3}gestion\\b", ("communit((\\w+)(\\W+)){1,3}conserv"), ("communit((\\w+)(\\W+)){1,3}fish"), ("communit((\\w+)(\\W+)){1,3}natural resource"), 
                        "comanagement\\b", "co-gestion\\b")

key.land <- c("\\bwastewater","\\bwaste water","\\brunoff","\\bmarine debris",
              "\\bmarine litter","\\bmarine plastic","\\bocean debris", "\\bocean litter", "\\bocean plastic",
              "\\bocean pollut","\\bwaste","\\bmarine pollut","\\bwater treatment","\\bseawage","\\bnutrient pollution","water resources conservation","river basins development",
              "\\beaux usees","\\beaux usees","\\brunoff","\\bdebris marins", # french
              "\\bdechets marins","\\bplastique marin","\\bpollution marine","\\dechets marins","plastique oceanique",
              "pollution des oceans","\\bdechets","pollution marine","traitement des eaux","eaux marines","pollution par les nutriments",
              "conservation des ressources en eau"," développement des bassins fluviaux")


############################
#        COASTAL           #
############################

# new variable - 1/0 coastal
all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
  mutate(Year = substring(activity_date_iso_date,1,4)) %>% # start year # 392946    257
  filter(Year %in% seq(2000,2022,1)) %>%
  mutate(coastal.nar = ifelse(str_detect(description_narrative,paste0(key.marine, collapse="|")),1,0)) %>%
  mutate(coastal.title = ifelse(str_detect(title_narrative,paste0(key.marine, collapse="|")),1,0))

all.gps.sp.100m.100km$coastal <- ifelse(all.gps.sp.100m.100km$coastal.nar == 1 | all.gps.sp.100m.100km$coastal.title == 1, 1, 0)
all.gps.sp.100m.100km$coastal[is.na(all.gps.sp.100m.100km$coastal)] = 0 # keep the NAs in title or narrative, since it might be filtered through the OECD codes
all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
  mutate(coastal = as.factor(coastal))

summary(all.gps.sp.100m.100km$coastal) #         0:98451     1: 25260  

###########################################
#             SUSTAINABILITY              #
###########################################

# new variable - 1/0 sustainability
all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
  mutate(Year = substring(activity_date_iso_date,1,4)) %>% # start year # 392946    257
  filter(Year %in% seq(2000,2022,1)) %>%
  mutate(sustainable.nar = ifelse(str_detect(description_narrative,paste0(key.sustainability, collapse="|")),1,0)) %>%
  mutate(sustainable.title = ifelse(str_detect(title_narrative,paste0(key.sustainability, collapse="|")),1,0))

all.gps.sp.100m.100km$sustainable <- ifelse(all.gps.sp.100m.100km$sustainable.nar == 1 | all.gps.sp.100m.100km$sustainable.title == 1, 1, 0)
all.gps.sp.100m.100km$sustainable[is.na(all.gps.sp.100m.100km$sustainable)] = 0 # keep the NAs in title or narrative, since it might be filtered through the OECD codes
all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
  mutate(sustainable = as.factor(sustainable))

summary(all.gps.sp.100m.100km$sustainable) #      0: 62812      1: 60899 

###########################################
#        LAND-BASED           #
###########################################

# new variable - 1/0 land based
all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
  mutate(Year = substring(activity_date_iso_date,1,4)) %>% # start year # 392946    257
  filter(Year %in% seq(2000,2022,1)) %>%
  mutate(land.nar = ifelse(str_detect(description_narrative,paste0(key.land, collapse="|")),1,0)) %>%
  mutate(land.title = ifelse(str_detect(title_narrative,paste0(key.land, collapse="|")),1,0))

all.gps.sp.100m.100km$land <- ifelse(all.gps.sp.100m.100km$land.nar == 1 | all.gps.sp.100m.100km$land.title == 1, 1, 0)
all.gps.sp.100m.100km$land[is.na(all.gps.sp.100m.100km$land)] = 0 # keep the NAs in title or narrative, since it might be filtered through the OECD codes
all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
  mutate(land = as.factor(land))

summary(all.gps.sp.100m.100km$land) #      0: 121261      1: 2450

#############################################
#           add sector codes                #
#############################################






#############################################
#            equity keywords                #
#############################################
key.equity <- c("\\baccountability","\\baccountable ","\\baffordable housing","\\bageism","\\banti-discriminatory","\\banti-oppressive",
                "\\banti-racist","\\bbasic living standards","\\bbasic services","\\bbenefit-sharing ","\\bbipoc","\\bburden ","\\bclassism ",
                "\\bco-benefit ","\\bco-production","\\bcompensate ","\\bcompensation ","\\bconflict- resolution ","\\bconflict ","\\bconsultation ",
                "\\bcorrectability","\\bcost-sharing","\\bculture","\\bdecision control voice","\\bdecolonization","\\bdignity","\\bdisabilit","\\bdisadvantaged",
                "\\bdiscriminat","\\bdisparity","\\bdiverse groups ","\\bempower ","\\bend poverty","\\benvironmental and social safeguards framework","\\bequal access",
                "\\bequal opportunit","\\bequal pay ","\\bequality ","\\bequit ","\\bessf ","\\bethicality ","\\bethnicity","\\bexploitation","\\bextreme poverty","\\bfair",
                "\\bfemale","\\bfemale genital mutilation","\\bfeminis","\\bfinancial inclusion","\\bforced marriage","\\bfpic","\\bgender","\\bgini","\\bgirl","\\bgrievance",
                "\\bhomeless","\\bhomophobia","\\bhuman rights","\\bhuman trafficking","\\bidentities","\\bimpoverished","\\binclusion","\\binclusiv","\\binclusive decision-making",
                "\\bincome distribution","\\bincome equality","\\bincome inequality","\\bindigenous","\\binequalit","\\binjustice","\\bknowledge systems","\\blgbtq",
   "\\bmarginalise","\\bmarginalize","\\bmicrofinance","\\bminorit","\\bno poverty","\\bnon binary","\\bnon-binary","\\bnon-discrimination","\\bof color","\\bof colour","\\bparity",
   "\\bparticipat","\\bpatriarchy","\\bpoor","\\bpoor and vulnerable","\\bpoverty eradication","\\bpoverty line","\\bprivileged","\\bpro-poor ","\\bqueer","\\brace","\\bracism","\\breconciliation",
   "\\brefugee","\\breligion","\\breproductive health","\\breproductive rights","\\brights","\\bsafeguard","\\bsex and inequality","\\bsexes","\\bsexism","\\bsexual and reproductive health",
   "\\bsexual exploitation","\\bsexual health","\\bsexual violence","\\bsocial inclusion","\\bsocial monitoring","\\bsocial protection","\\bsocial protection systems","\\bsocial safety","\\bsocial security",
   "\\btrade-off","\\btradeoff","\\btraditional ecological knowledge","\\btraditional knowledge","\\btrafficking","\\btransgender","\\btrus","\\btrustworthiness","\\btruth and reconciliation",
   "\\btwo-spirit","\\bunderserved","\\buniversal health coverage","\\bviolence against girls","\\bviolence against women","\\bviolence and girls","\\bviolence and women",
   "\\bvulnerability","\\bvulnerable","\\bwealth distribution","\\bwelfare","\\bwomen","\\byouth")


all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
  mutate(equity.nar = ifelse(str_detect(description_narrative,paste0(key.equity, collapse="|")),1,0)) %>%
  mutate(equity.title = ifelse(str_detect(title_narrative,paste0(key.equity, collapse="|")),1,0))

all.gps.sp.100m.100km$equity <- ifelse(all.gps.sp.100m.100km$equity.nar == 1 | all.gps.sp.100m.100km$equity.title == 1, 1, 0)
all.gps.sp.100m.100km$equity[is.na(all.gps.sp.100m.100km$equity)] = 0 # keep the NAs in title or narrative, since it might be filtered through the OECD codes
all.gps.sp.100m.100km <- all.gps.sp.100m.100km %>%
  mutate(equity = as.factor(equity))

################################################
#           quick look into db                 #
################################################
##### estimate the 
all.gps.sp.100m.100km.CADC <- all.gps.sp.100m.100km %>%
  filter((coastal == 1 & sustainable == 1) | land == 1)
length(unique(all.gps.sp.100m.100km.CADC$iati_identifier)) # 7612 projects, both coastal and sustainable and potentially land-based

# spot check to see if these are indeed
keyword.check <- all.gps.sp.100m.100km.CADC %>% 
  as.data.frame() %>% 
  filter((!is.na(document_link_url)|!is.na(result_document_link_url)) & equity==1) %>% 
  select(iati_identifier_bis,coastal,sustainable,land,equity,iso_a3,title_narrative,description_narrative,linked_data_uri,result_document_link_url,document_link_url)
#export(slice(keyword.check,sample(nrow(keyword.check),100)),"data/CADC_results_docs_check.csv")

# which words are being picked up
samp.rows <-sample(length(unique(keyword.check$iati_identifier_bis)),500)
word.check <- keyword.check %>% 
  distinct(iati_identifier_bis,.keep_all = T) %>% 
  mutate(comb=paste(title_narrative,description_narrative)) %>% 
  slice(samp.rows) %>% 
  select(iati_identifier_bis,coastal,sustainable,land,equity,title_narrative,description_narrative,comb)

word.check$marine.word <- NA
word.check$sust.word <- NA
word.check$land.word <- NA
word.check$equity <- NA
for (i in 1:nrow(word.check)){
  for (j in 1:length(key.marine)){
    mar.term <- key.marine[j]
    if(str_detect(word.check$comb[i],mar.term)==TRUE){
    print(paste("Marine term: ", mar.term))
    word.check$marine.word[i] <- mar.term
    }
  }
}
for (i in 1:nrow(word.check)){
  for (j in 1:length(key.sustainability)){
    sus.term <- key.sustainability[j]
    if(str_detect(word.check$comb[i],sus.term)==TRUE){
      print(paste("Marine term: ", sus.term))
      word.check$sust.word[i] <- sus.term
    }
  }
}
for (i in 1:nrow(word.check)){
  for (j in 1:length(key.land)){
    land.term <- key.land[j]
    if(str_detect(word.check$comb[i],land.term)==TRUE){
      print(paste(i,"Marine term: ", land.term))
      word.check$land.word[i] <- land.term
    }
  }
}

for (i in 1:nrow(word.check)){
  for (j in 1:length(key.equity)){
    equity.term <- key.equity[j]
    if(str_detect(word.check$comb[i],equity.term)==TRUE){
      print(paste(i,"Marine term: ", equity.term))
      word.check$equity.word[i] <- equity.term
    }
  }
}

word.check %>% 
  group_by(marine.word) %>% 
  count() %>% 
  arrange(-n) %>% 
  view

word.check %>% 
  group_by(sust.word) %>% 
  count() %>% 
  arrange(-n) 

word.check %>% 
  group_by(land.word) %>% 
  count() %>% 
  arrange(-n)

word.check %>% 
  group_by(equity.word) %>% 
  count() %>% 
  arrange(-n)

export(word.check,"data/keyword_check.csv")

##### save the coded/sectors, keywords full db
saveRDS(all.gps.sp.100m.100km.CADC,here("data","derived-data","iati_GPS_db_100km_100m_keywords_sectors.rds"))
dim(all.gps.sp.100m.100km.CADC)

