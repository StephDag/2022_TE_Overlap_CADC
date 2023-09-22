# extract all world bank projects for students 
# climate change

# code to retrieve and correct GPS coordinates from the full database - in the descriptive narrative
source(here::here("analyses","001_Coastal_countries.R")) # load countries and cities in countries

# load raw data - all countries
all <- readRDS(here("data","raw-data","CADC.rds")) # 633735    284
# all <- readRDS("/media/seagate/sdagata/CADC.rds")

# filter by coastal countries
all.coastal <- all %>%
  filter(recipient_country_code %in% coastal.ctr$iso2  | transaction_recipient_country_code  %in% coastal.ctr$iso2)
dim(all.coastal)

#### filter by keywords
# climate change /restoration projects
# from meeting notes:
# Climate adaptation/mitigation: projects that aim to build eco and social adaptive capacity to climate change: 
# restoration, capacity development
key.marine <- c("\\bCoast","\\bMarin","\\bSea","\\bOcean","\\bshore","Mangrove","Seagrass","Saltmarsh","reef",
                "kelp","beach","\\bestuar","\\bfish","\\bBlue","\\bbeach","\\bshore","coral","mangrove",
                "seagrass","tidal marshes","\\bwave","\\btidal","\\balgae")
key.climate <- c("climate","\\badapt","\\brestoration") # 1061

rm(CADC.db.CLIMATE.wb)
CADC.db.CLIMATE.wb <- all.coastal %>%
#  dplyr::filter(str_detect(participating_org_ref,"44000")) %>%
  #dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|")) & str_detect(description_narrative,paste0(key.climate, collapse="|"))) # %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.climate, collapse="|")))
length(unique(CADC.db.CLIMATE$iati_identifier))
dim(CADC.db.CLIMATE.wb)


key.development <- c("\\bblue","\\bfish","\\baquac","\\btouris","seabed mining","oil and gas","\\bdesal","offshore","\\bport\\b","\\bshipping\\b","\\bbiotech","\\bpharmaceut","\\bbiochem","plastic","tidal","\\bwave\\b","wastewater")
key.conservation <- c("\\bmpa\\b","\\bamp\\b","\\blmma","conservation","marine reserve","marine protected area")
