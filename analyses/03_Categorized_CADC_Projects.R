# Script to categorize development and climate change projects
# load data
CADC.db.filtered <- readRDS(here("data","derived-data","CADC.db.filtered.final.rds"))
CADC.db.filtered %>% dim()


unique(CADC.db.filtered$iati_identifier) %>% length()

length(which(is.na(CADC.db.filtered$transaction_value)))

# climate change /restoration projects
# from meeting notes:
# Climate adaptation/mitigation: projects that aim to build eco and social adaptive capacity to climate change: 
# restoration, capacity development
key.climate <- c("climate","\\badapt","\\brestoration","mangrove","seagrass","tidal marshes") # 1061
key.development <- c("\\bblue","\\bfish","\\baquac","\\btouris","seabed mining","oil and gas","\\bdesal","offshore","\\bport\\b","\\bshipping\\b","\\bbiotech","\\bpharmaceut","\\bbiochem","plastic","tidal","\\bwave\\b","wastewater")
key.conservation <- c("\\bmpa\\b","\\bamp\\b","\\blmma","conservation","marine reserve","marine protected area")

###########################
#  Climate projects       #
###########################

rm(CADC.db.CLIMATE)
CADC.db.CLIMATE <- CADC.db.filtered %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.climate, collapse="|")))

## Allocate climate category
CADC.db.CLIMATE$Type <- rep('Climate',dim(CADC.db.CLIMATE)[1])
length(unique(CADC.db.CLIMATE$iati_identifier))
length(unique(CADC.db.CLIMATE$sov_a3))
100*length(which(is.na(CADC.db.CLIMATE$transaction_value)))/dim(CADC.db.CLIMATE)[1]
CADC.db.CLIMATE[62,]

# % of projects with transaction data
x <- length(unique(CADC.db.CLIMATE[which(!is.na(CADC.db.CLIMATE$transaction_value)),"iati_identifier"]))
perc.climate <- 100*x/length(unique(CADC.db.CLIMATE$iati_identifier)); perc.climate
#write.csv2(CADC.db.CLIMATE, file=here("data","derived-data","CADC.db.CLIMATE.csv"),row.names = F)

###########################
#  Conservation           #
###########################

rm(CADC.db.CONS)
CADC.db.CONS <- CADC.db.filtered %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.conservation, collapse="|")))

## Allocate climate category
CADC.db.CONS$Type <- rep('Conservation',dim(CADC.db.CONS)[1])
length(unique(CADC.db.CONS$iati_identifier))
length(unique(CADC.db.CONS$sov_a3))

# number of project with transaction information
x <- length(unique(CADC.db.CONS[which(!is.na(CADC.db.CONS$transaction_value)),"iati_identifier"]))
perc.conservation <- 100*x/length(unique(CADC.db.CONS$iati_identifier)); perc.conservation

###########################
# Development          #
###########################

rm(CADC.db.DEV)
CADC.db.DEV <- CADC.db.filtered %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.development, collapse="|")))

## Allocate climate category
CADC.db.DEV$Type <- rep('Development',dim(CADC.db.DEV)[1])
length(unique(CADC.db.DEV$iati_identifier))
length(unique(CADC.db.DEV$sov_a3))

# number of project with transaction information
x <- length(unique(CADC.db.DEV[which(!is.na(CADC.db.DEV$transaction_value)),"iati_identifier"]))
perc.dev <- 100*x/length(unique(CADC.db.DEV$iati_identifier)); perc.conservation

  # save CSV file to modify
write.csv2(CADC.db.CLIMATE %>% select(iati_identifier,iati_identifier_bis,title_narrative,description_narrative,Type), file=here("data","derived-data","CADC.db.CLIMATE.2categorized.csv"),row.names = F)

  # save climate database
saveRDS(CADC.db.CLIMATE,here("data","derived-data","CADC.db.CLIMATE.rds"))

###### separate between capacity development vs restoration projects
key.rest <- c("restoration")
key.cd <- c("capacity")

rm(CADC.db.CLIMATE.rest)
CADC.db.CLIMATE.rest <- CADC.db.CLIMATE %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.rest, collapse="|"))) 
unique(CADC.db.CLIMATE.rest$iati_identifier) %>% length() # 54 projects

rm(CADC.db.CLIMATE.cd)
CADC.db.CLIMATE.cd <- CADC.db.CLIMATE %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.cd, collapse="|"))) 
unique(CADC.db.CLIMATE.cd$iati_identifier) %>% length() # 169 projects

# specific climate adaptation funding
CF <- c("Adaptation Fund","Green Climate Fund","PACJA")

CADC.db.CLIMATE %>%
  dplyr::filter(str_detect(transaction_provider_org_narrative,paste0(CF, collapse="|"))) %>%
  select(description_narrative)
unique(CADC.db.CLIMATE$iati_identifier) %>% length() # 419 projects

CADC.db.CLIMATE$

  # create archetype climate change adaptation projects database - 5 by row
arch.climate <- c("US-EIN-300108263-GGProj:39661","AU-5-INI504-AG","US-EIN-300108263-GGProj:27692","CH-4-2011004982","REE-CALL Scale up and Extension project II",
                  "XM-DAC-41114-OUTPUT-00093415","XM-DAC-3-1-218062","AU-5-INI504-BZ","XM-DAC-3-1-270437","KH-INGO-20201231-205210-274",
                  "GB-CHC-274467-205210-274","XM-DAC-41304-39-1358","AU-5-INI504-CV","XM-DAC-41114-OUTPUT-00099212","XM-DAC-41114-PROJECT-00069416")
                  
CADC.db.CLIMATE.archetype <- CADC.db.CLIMATE %>% filter(iati_identifier %in% arch.climate)

  # test find the 15 projects (25 subprojects in total)
key.climate.coastal <- c("climat","adapt","restor","coast","marin","ocean")
CADC.db.CLIMATE.archetype.test <- CADC.db.CLIMATE.archetype %>% filter(str_detect(description_narrative,paste0(key.climate, collapse="|")) & 
                                                                         str_detect(description_narrative,paste0(key.coastal, collapse="|")))
CADC.db.CLIMATE.archetype.test %>% dim()

unique(CADC.db.CLIMATE$iati_identifier) %>% length()

tail(CADC.db.CLIMATE$title_narrative)
tail(CADC.db.CLIMATE$description_narrative)
CADC.db.CLIMATE$description_narrative[5]
str_detect(CADC.db.CLIMATE$description_narrative[6],paste0(key.climate, collapse="|"))
stringr::str_detect(CADC.db.CLIMATE$description_narrative[6], "capacit\\b")

###################################
#  Fisheries / Aquacultures       #
###################################

# Dev: Fisheries & Aquaculture 
key.dev <- c("aquac","fish") 
key.coastal <- c("coast","marine","ocean")

rm(CADC.db.DEV)
CADC.db.DEV <- CADC.db.filtered %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.dev, collapse="|")) & 
                  str_detect(description_narrative,paste0(key.coastal, collapse="|"))) # projects to include 

## Allocate development category
CADC.db.DEV$Type <- rep('Development',dim(CADC.db.DEV)[1])
unique(CADC.db.DEV$iati_identifier) %>% length() # 342 projects

# save CSV file to modify
write.csv2(CADC.db.DEV %>% select(iati_identifier,iati_identifier_bis,title_narrative,description_narrative,Type), file=here("data","derived-data","CADC.db.DEV.2categorized.csv"),row.names = F)

# save development projects
saveRDS(CADC.db.DEV,here("data","derived-data","CADC.db.DEVELOPMENT.rds"))


