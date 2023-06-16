### The projects have been categorized in development and climate projects
### This scrips is to analysis the distribution of drivers for each

## load updated categories
CADC.CLIMATE.cat <- read.csv2(here("data","derived-data","CADC.db.CLIMATE.2categorized_Filled_Steph.csv"),header=T)
CADC.CLIMATE.cat<- CADC.CLIMATE.cat[-2791,-c(7:49)]
CADC.DEV.cat <- read.csv2(here("data","derived-data","CADC.db.DEV.2categorized_Filled_Steph.csv"))
CADC.DEV.cat<- CADC.DEV.cat[-843,-c(8:23)]
## load developement and climate projects database
CADC.db.DEV <- readRDS(here("data","derived-data","CADC.db.DEVELOPMENT.rds"))
CADC.db.CLIMATE <- readRDS(here("data","derived-data","CADC.db.CLIMATE.rds"))

## link the files

  # climate
CADC.db.CLIMATE <- cbind(CADC.db.CLIMATE,CADC.CLIMATE.cat)

  # development
CADC.db.DEV <- cbind(CADC.db.DEV,CADC.DEV.cat)

#CADC.db.DEV <- CADC.db.DEV %>%
#  mutate(iati_identifier_bis = as.factor(iati_identifier_bis))
#CADC.DEV.cat <- CADC.DEV.cat %>%
#  mutate(iati_identifier_bis = as.factor(iati_identifier_bis))
#CADC.db.DEV <- left_join(CADC.db.DEV,CADC.DEV.cat,by=c("iati_identifier"="iati_identifier","iati_identifier_bis"="iati_identifier_bis"))

#  CLIMATE - filter for project relevance == 1
CADC.db.CLIMATE.relevant <- CADC.db.CLIMATE[which(CADC.db.CLIMATE$Relevance == 1),] # 470 locations
unique(CADC.db.CLIMATE.relevant$iati_identifier) %>% length() # 135 unique projects
saveRDS(CADC.db.CLIMATE.relevant,here("data","derived-data","CADC.db.CLIMATE.relevant.rds"))

#  DEVELOPMENT - filter for project relevance == 1
CADC.db.DEV.relevant <- CADC.db.DEV[which(CADC.db.DEV$Relevance == 1),] # 385 locations
unique(CADC.db.DEV.relevant$iati_identifier) %>% length() # 157 unique projects
saveRDS(CADC.db.DEV.relevant,here("data","derived-data","CADC.db.DEV.relevant.rds"))

