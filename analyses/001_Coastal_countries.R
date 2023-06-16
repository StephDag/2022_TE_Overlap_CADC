## list of coastal countries

# load eez
eez <- read.csv(here("data","raw-data","List_territory","eez_territory_region.csv"))
head(eez)
dim(eez) # 221

# coastal countries
ctr <- read.csv(here("data","raw-data","country-coastline-distance-master","coastlines.csv"))

rm(coastal.ctr)
coastal.ctr <- ctr %>% filter(coastline_wf >0)
coastal.ctr[which(coastal.ctr$iso2 == ""),"iso2"] <- c("NA","AN")
# list of countries with regions, etc.
ctr.region <- read.csv(here("data","raw-data","ISO-3166-Countries-with-Regional-Codes-master","all","all.csv"))
head(ctr.region)

# create full country database
coastal.ctr <- left_join(coastal.ctr,ctr.region,by=c("country"="name"))
head(coastal.ctr);tail(coastal.ctr)

# check
#coastal.ctr %>% filter(country == "Namibia")
#ctr.region  %>% filter(name == "Namibia")