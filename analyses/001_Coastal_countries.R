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

## load names of cities for all countries and filter for only coastal countries
# load geonames data for all countries
#rm(geo.names)
#geo.names <- read.delim2(here("data","raw-data","allCountries.txt"), header = F, sep = "\t", dec = ",")
#colnames(geo.names) <- c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature_class","feature_code","country_code","cc2","admin1_code",
#                        "admin2_code","admin3_code","admin4_code","population","elevation","dem","timezone","modification_date")
#dim(geo.names)

# colnames
#geonameid         : integer id of record in geonames database
#name              : name of geographical point (utf8) varchar(200)
#asciiname         : name of geographical point in plain ascii characters, varchar(200)
#alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
#latitude          : latitude in decimal degrees (wgs84)
#longitude         : longitude in decimal degrees (wgs84)
#feature class     : see http://www.geonames.org/export/codes.html, char(1)
#feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
#country code      : ISO-3166 2-letter country code, 2 characters
#cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
#admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
#admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80) 
#admin3 code       : code for third level administrative division, varchar(20)
#admin4 code       : code for fourth level administrative division, varchar(20)
#population        : bigint (8 byte int) 
#elevation         : in meters, integer
#dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
#timezone          : the iana timezone id (see file timeZone.txt) varchar(40)
#modification date

  # geo.names for only coastal countries
#geo.names.coastal <- geo.names %>%
#  filter(country_code %in% coastal.ctr$iso2)
#dim(geo.names.coastal)
  # save
#saveRDS(geo.names.coastal,here("data","derived-data","geo.names.coastal.rds"))
  # load
geo.names.coastal <- readRDS(here("data","derived-data","geo.names.coastal.rds"))
