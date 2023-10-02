# Categorized geographic projects based based on coastal, sustainability (CADC) and sustainable land-based projects
# outputs: 3 terrestrial raster of 5km resolution each
# Stephanie D'Agata
# Sept 2023
# Updates: Sept 2023
library(here)
source(here("analyses","00_setup.R"))

# load data
all.gps.sp.100m.100km <- readRDS(here("data","derived-data","iati_GPS_db_100km_100m.rds"))
dim(all.gps.sp.100m.100km)

# list of keywords
# number of projects by key words
# updates on the 29/09/2023
key.marine <- c("\\bcoast","\\bmarin*","\\bsea","\\bocean*","\\bshore","\\bblue\\b","\\bbeach","\\bbmaritime","\\blittoral\\b","mangrove*","seagrass*","saltmarsh*","\\breef*\\b","kelp*",
                "\\bestuar*","coral*","\\bbfish*","\\finfish*","\\bwave*","\\btidal","\\balgae","marsh*","\\baquac*","\\bmariculture", "\\bsubmarine","\\bdesalination","\\boffshore","\\bport",
                "\\bseaweed*","\\bcrustacean*","\\bmollus*","\\bwetland*","\\btransitional water*","\\blagoon","\\bsalt pond*","bshell","\\bseamount*","\\bmid-ocean ridge*","\\bcentral gyre*","\\bdeep sea","\\bship*","\\bwater transport",
                "\\bfinfish\\b", "\\bcontinental shelf*","\\bsand*","\\bnautical","\\batoll","\\bunderwater","\\bdemersal","\\baphotic", "pelagic*","neritic","surf","diver\\b", "divers\\b","dive\\b","SCUBA\\b","CPUE","\\bbivalve*","\\bboat*",
                "\\bcaribbean biodiversity fund", "\\bclam", "\\bcockle", "\\bcrab", "\\bcruise","\\bdeep water", "\\bdesalini", "\\bdolphin", "\\bdredg", "\\bgrouper",
                "\\bgulf", "\\bharbor", "\\bharbour", "\\binternational water", "\\blionfish", "\\bmpa", "\\bmussel", "\\bnaval", "\\boyster","\\bprawn", 
                "\\bproblue", "\\bsnapper", "\\bsaltwater", "\\bsargassum", "\\bshark", "\\bshrimp", "\\btuna", "\\bturtle", "\\bunclos", "\\bvessel", "\\bwhale", "\\bwharf",
                "\\bcôte\\b","\\bcôtière","\\bcôtier","\\bpêche","\\bpêcheur","\\bpêcherie","\\bocéan","\\bleue","\\bplage",  # french translation
                "\\bmaritime","\\blittoral","\\bherbier","\\bmarais salant\\b","\\bmarée","\\bvague","\\balgues","\\bdessalement","\\btransport maritime\\b","\\beaux usées\\b","\\bruisselement","\\bsous-marin",
                "\\bcrustacé*","\\bmollusque*","\\bzones humides","\\bzone humide","\\beaux de transition","\\blagon*","\\bétang salé","\\bétangs salés","\\bcoquillage*",
                "dorsale médio-océanique","dorsales médio-océaniques","\\bgyre*","eaux profondes","eau profonde","gyre océanique","\\bbateau*","\\bpoisson*","\\bnautique*","\\bdémersa*","pélagique*","\\bplongée*\\b","\\bplongeur*\\b",
                "\\bcapture*","coquillage*","crabe*","\\bcoque*","croisière*","mérou*","\\bdauphin*","\\bcrevette*","\\bsargasse*","requin*","thon*","tortue*","baleine*","\\bmoule*")

key.sustainability <- c("\\bacidifica", "\\badapt", "\\badaptation", "\\bbio sphere","\\bbio-logical diversity","\\bbio-sphere","\\bbiodiversity", "\\bbio-diversity","\\bbiological diversity","\\bbiosphere",
                        "\\bcarbon sink","\\bclimate change","\\bconserv","\\bdisaster risk reduction", "\\bDRR", "\\beco-system", "\\beco-touris", "\\becosystem", "\\becotouris", "\\benvironment*","\\becological",
                        "\\bspecies","\\beutriphication", "\\bhabitat", "\\biuu", "\\bmarine debris ", "\\bmarine energy", "\\bmarine protection", "\\bmitigate", "\\bnature-based solutions",
                        "capacity-build", "capacity build", "capacity develop", "\\bbadapt*","\\bocean health","\\bocean protection", "\\bvulnerab*", "tenure\\b", "TURF*", "territorial user rights",
                        "\\boceanograph","\\boff-shore wind", "\\boffshore wind", "\\bover-fishing", "\\boverfishing","\\bclimat*",
                        "\\boxygen", "\\bpreserv", "\\bprotected area", "\\brenewable", "\\bresearch", "\\bresilien","\\bresponse preparedness", "\\brestoration", "\\brestore", "\\brestoring", "\\bscience",
                        "\\bscientifi", "\\bSDG 14", "\\bsea level rise", "\\bsea-level rise", "\\bsequestration", "\\bslr","\\bsustainab", "\\btidal energy", "\\bwildlife", "\\bwind energy","\\bbblue",
                        "\\bbmpa","\\bblmma", "\\bboecm", "reserve*", "park*","no-take","natural monument","enclosure","closure","wilderness area*", "payment for ecosystem services", "PES", "\\bbMSC\\b","certification","eco-label", "ecotourism","protect*", ("communit((\\w+)(\\W+)){1,3}management\\b"), 
                        "conserv((\\w+)(\\W+)){1,3}management\\b", ("communit((\\w+)(\\W+)){1,3}conserv"), ("communit((\\w+)(\\W+)){1,3}fish"), ("communit((\\w+)(\\W+)){1,3}natural resource"), 
                        "comanagement\\b", "co-management\\b",
                        "\\bbiodiversité", "\\bbio-diversité","\\diversité biologique","\\bpuit de carbone","\\bchangement climatique","\\bconserv","\\bdisaster risk reduction", "\\bDRR", "\\béco-system", "\\béco-touris", "\\bécosystem", "\\bécotouris", "\\benvironment*","\\bécologique",
                        "\\bespèces","\\beutrophication", "\\bhabitat", "\\bdébris marin", "\\benergie marine", "\\bprotection marine", "\\bmitigate", "\\bsolutions fondées sur la nature", "\\bsolutions basées sur la nature",
                        "renforcement de capacité", "développement de capacités", "\\bbadapt*","\\bprotection des océans", "\\bvulnerab*","droits territoriaux",
                        "\\boceanograph","\\boff-shore wind", "\\béolien offshore","\\béolien en mer","\\bsurpeche",
                        "\\boxygène", "\\bpreserv", "\\baire protégée", "\\brenouvelable", "\\brecherche", "\\brésilien","\\bpréparation à la réponse", "\\brestoration", "\\brestor*","\\bscience",
                        "\\bscientifi", "\\bODD 14", "\\bmontée des eaux", "\\bsequestration","\\bdurab", "\\bénergie marémotrice", "\\bfaune", "\\bénergie éolienne","\\bbbleu",
                        "\\bbamp", "reserve*", "parc*","monument naturel","fermeture","zone sauvage*", "paiement pour services écosystémiques", "PES", "\\bbMSC\\b","certification","eco-label", "ecotourism","protect*",
                        ("communit((\\w+)(\\W+)){1,3}gestion\\b"), 
                        "conserv((\\w+)(\\W+)){1,3}gestion\\b", ("communit((\\w+)(\\W+)){1,3}conserv"), ("communit((\\w+)(\\W+)){1,3}fish"), ("communit((\\w+)(\\W+)){1,3}natural resource"), 
                        "comanagement\\b", "co-gestion\\b")

key.land <- c("\\bwastewater","\\bwaste water","\\brunoff","\\bmarine debris",
              "\\bmarine litter","\\bmarine plastic", "\\bmarine pollution","\\bocean debris", "\\bocean litter", "\\bocean plastic",
              "\\bocean pollut","\\bwaste","\\bmarine pollut\\b","\\bwater treatment","\\bseawage","\\bnutrient pollution","water resources conservation","river basins development",
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

key.equity <- c() # to add once finalize with team (Steph - 01/10/2023)


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
  filter(coastal == 1 & sustainable == 1 & land == 0)
length(unique(all.gps.sp.100m.100km.CADC$iati_identifier)) # 6722 projects, both coastal and sustainable and potentially land-based

##### save the coded/sectors, keywords full db
saveRDS(all.gps.sp.100m.100km.CADC,here("data","derived-data","iati_GPS_db_100km_100m_keywords_sectors.rds"))
dim(all.gps.sp.100m.100km.CADC)

