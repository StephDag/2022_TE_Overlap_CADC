# script to retrieve bathymetry /elevetion information for each gps coordinates

  # 14/08/2023 - test on GPS database - still not adding gps location from retrieving location in the narrative
rm(list=ls())
# load GPS location extracted from narrative
gps.cor.1 <- readRDS(here("data","derived-data","loc.gps.projects.rds"))
gps.cor.2 <- readRDS(here("data","derived-data","loc.gps.projects_b.rds"))
gps.cor <- rbind(gps.cor.1,gps.cor.2)
names(gps.cor) = c("iati_identifier_bis","locations","latitude","longitude")

# clean to remove non relevant locations (country, etc.)
summary(as.factor(gps.cor$locations))

# projection
robin = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# remove countries
source(here::here("analyses","001_Coastal_countries.R")) # countries and cities
rm(gps.cor.V1)
gps.cor.V1 <- gps.cor %>% 
  filter(locations %in% ctr$country == F) %>% # remove countries
  filter(locations %in% ctr$iso2 == F)
gps.cor.V1 <- gps.cor.V1[which(str_detect(gps.cor.V1$locations,"Republic")==F),] # remove country with republic
gps.cor.V1 <- data.frame(gps.cor.V1,n = nchar(gps.cor.V1$locations))
  # remove locations with 1 letters
gps.cor.V1 <- gps.cor.V1 %>%
  filter(n != 1)
  # remove numeric name

  # ctry to remove
ctr2rm <- c("Myanmar","Maroc","America","Indien","Central","Asia","Guinée","Perú","Lagos","UK","Irak","Libanon","To","The","Data","Change","Project",
            "Timor-Leste","Philippine","Country","Indonesien","République de Madagascar","le","Pakistani","by","Timor Leste","Public","Urban","South",
            "Centre","FCT","West","More","No","One","Second","Nigéria","MDG","Non","Total","Sud","PCR","In","North","PDO","Sector","FATA","We","Note",
            "Sud","Mission","Support","Northern","Provincial","Basin","East","North East","AFS","CDO","Estuaire","La","Org","a long",
            "By","They","Many","Fiji Islands","UAE","Émirats arabes unis","Albanie","Les","Liban","At","Die","Man","As","Chain","Bank","Israeli","provincial",
            "HUman","Das","Plan","palm","Sheet","ran","Grand","Down","Academy,College,Cancer Care","Royal","Eye","Ambassador","School","Test","North West","Town",
            "President","Water","How","Middle","Mount","Box","Prince's","OK","THE","Northern Ireland","Beach","Le","Cameroun","Tchad","Over","UNESCO",
            "an","so","It","This","Val","Our","Men","Camp","So","Le","March","River","England","international","Beach","space","From","University","Test","test",
            "Quality","College","Stock","Health Services","ran","Mountain","Tower","quality","gabon","she","On","Charles","Europe","Ile","For","Hotel Royal","Tete",
            "Aeropuerto","Agip Oil Company","Africa","Argentína","Argentine","Argentinien","argentino","Argentino","Atlántico Norte","Atlantico Sur","Atlántico Sur","Green",
            "The Academy","Academy","Albanien","city","9 de Julio","French","Two","First","University of the South Pacific","Ministry of Foreign Affairs",
            "fiji","Fidji","Northern Division","Reef Resort","She","stay","Park","Irlande du Nord","United Kingdom of Great Britain and Northern Ireland",
            "Church","Parliament","Standard","Cancer Care","Service","Trash","Romani","German","Capital","capital","Coast",
            "25","21","Pacific","Art","North 24 Parganas","South 24 Paraganas","SUCH","Rivière","Petite","Me","Home",
            "Western","Red","University of Sydney","Southern","Research","Seminar","15 de Septiembre","32","75",
            "AAP","2009","IOM","NAD","Fellowship","Resource","Stockholm","Paris","Rumah Cemara","Arla","Storm",
            "AND","Bank of Ceylon","Hire","Third","Bureau of Fisheries and Aquatic Resources","Universidad Católica",
            "Third","Hospital","Sir Robert Rex Wharf","Central Bank of Oman","Media","Final","Ministry of Foreign Affairs and Emigrants",
            "Samsung","Alliance","South Industrial Area","Central Division","Pays-Bas","Bénin","Terminal 1",
            "COI","Horse","The Royal","Team","United Nations Office","ten","Ten","University of the Philippines",
            "Small Island","Easter","Bureau of Internal Revenue","National Capital Region","Smart","Congo",
            "Occidental","Negros Occidental","National Capital District","Arab","Band","Delta State","Population Services International",
            "Ministry of Justice","Crane","NDI","USA","NR","SR","América","Barracas","Unión","DAP","DFAT","Gonnet",
            "Bosques","Energía","CABA","Anderson","IFC","AMBA","Retiro","Pilar","BUE","Cockatoo","Success","make",
            "City","Wilderness","part","Light","Coral","WHL","Big","Senior","Anti","Reve","Normandy","Evian","WWII",
            "Franklin","Hotel Royal","Rouge","CNER","Some","Big","Ga","Se","Ta","AP","AN","BP","WH","DI","DV","DD","YP","SP",
            "EN","GC","Da","He","Po","br","Ti","dr","Un","NH","IP","BL","GS","RF","Ye","Ka")

gps.cor.V2 <- gps.cor.V1 %>%
  filter(locations %in% ctr2rm == F) %>%
  filter(!is.na(latitude)) # 106842      5

#all[which(all$iati_identifier_bis == "CH-4-2021-2016003510_HT"),"description_narrative"]
#gps.cor.V2[which(gps.cor.V2$iati_identifier_bis == "CH-4-2021-2016003510_HT"),]

# load raw data
all <- readRDS(here("data","derived-data","CADC.db.final.rds"))

# merge with full DB
rm(all.merge)
all.merge <- left_join(all,gps.cor.V2,by="iati_identifier_bis",relationship = "many-to-many")

# coalesce longitude/latitude
all.merge <- all.merge %>%
  distinct()  %>%
  mutate(latitude = coalesce(latitude.x,as.numeric(latitude.y))) %>%
  mutate(longitude = coalesce(longitude.x,as.numeric(longitude.y))) # 599591    311

# number of projects without GPS coordinates
noGPS <- which(is.na(all.merge$latitude)) %>% length() # 158037

# marine no GPS
noGPS.db <- all.merge %>%
  filter(is.na(latitude)) %>%
  distinct()

# save rds data
saveRDS(noGPS.db, here("data","derived-data","iati_NO_GPS.rds"))
write.csv(noGPS.db, here("data","derived-data","iati_NO_GPS.csv"))

######
noGPS.marine.db <- all.merge %>%
  filter(is.na(latitude)) %>% # non gps
  distinct() %>%
  filter(activity_status_code %in% c(5,6) == F) %>% # stopped projects: https://iatistandard.org/en/iati-standard/203/codelists/activitystatus/ rm 5 and 6
  mutate(coastal.nar = ifelse(str_detect(description_narrative,paste0(key.marine, collapse="|")),1,0)) %>%
  mutate(coastal.title = ifelse(str_detect(title_narrative,paste0(key.marine, collapse="|")),1,0))

noGPS.marine.db$coastal <- ifelse(noGPS.marine.db$coastal.nar == 1 | noGPS.marine.db$coastal.title == 1, 1, 0)
noGPS.marine.db$coastal[is.na(noGPS.marine.db$coastal)] = 0 # keep the NAs in title or narrative, since it might be filtered through the OECD codes

  # check benefits of searching in the title
noGPS.marine.db %>% select(coastal.nar,coastal.title,coastal) %>% filter(coastal.title==1 & coastal.nar==0)

    # check plural or not
#noGPS.marine.db %>% 
#  filter(coastal == 0) %>%
#  str_detect("crustaceans")

  # check
summary(as.factor(noGPS.marine.db$coastal))   # 0: 136493      1:20978 

n.projects <- noGPS.marine.db %>%
  filter(coastal == 1)
length(unique(n.projects$iati_identifier))
  

 # save rds data
  saveRDS(noGPS.marine.db, here("data","derived-data","iati_marine_NO_GPS.rds"))
  write.csv(noGPS.marine.db, here("data","derived-data","iati_marine_NO_GPS.csv"))

# elevation for all project with GPS location
prj_dd <- "EPSG:4326"

# Create an example SpatialPoints - 1st database
all.merge.gps <- all.merge %>% filter(!is.na(longitude))  # 441554/599591 == 74% with GPS
all.gps.sp <- st_as_sf(all.merge.gps, coords = c("longitude","latitude"))
st_crs(all.gps.sp) = 4326
# get elevation points
all.gps.spdf_elev_epqs <- get_elev_point(all.gps.sp, prj = prj_dd, src = "aws")
# bind with db
all.gps.sp <- cbind(all.gps.sp,all.gps.spdf_elev_epqs)
    # filter 100m elevation
all.gps.sp.100m <- all.gps.sp %>%
  filter(elevation <=100) # 163195    313

    # Year
all.gps.sp.100m <- all.gps.sp.100m %>%
  mutate(Year = substring(activity_date_iso_date,1,4)) # start year # 392946    257

##### intersect with XXkm inland buffer
# load
inlandWaters.10km <- readRDS(here::here("data","derived-data","inlandBuffer_10km.rds"))
inlandWaters.20km <- readRDS(here::here("data","derived-data","inlandBuffer_20km.rds"))
inlandWaters.30km <- readRDS(here::here("data","derived-data","inlandBuffer_30km.rds"))
inlandWaters.40km <- readRDS(here::here("data","derived-data","inlandBuffer_40km.rds"))
inlandWaters.50km <- readRDS(here::here("data","derived-data","inlandBuffer_50km.rds"))
inlandWaters.60km <- readRDS(here::here("data","derived-data","inlandBuffer_60km.rds"))
inlandWaters.70km <- readRDS(here::here("data","derived-data","inlandBuffer_70km.rds"))
inlandWaters.80km <- readRDS(here::here("data","derived-data","inlandBuffer_80km.rds"))
inlandWaters.90km <- readRDS(here::here("data","derived-data","inlandBuffer_90km.rds"))
inlandWaters.100km <- readRDS(here::here("data","derived-data","inlandBuffer_100km.rds"))

  # st as sf for 1st database
all.gps.sp.sf <- sf::st_as_sf(all.gps.sp.100m, 
                       coords = c("longitude", "latitude")) %>%
                       st_transform(crs=robin)

  # plot to check
plot.inland <- ggplot() +
  geom_sf(data = inlandWaters.10km) +
  geom_sf(data = inlandWaters.10km, fill = "lightblue", col = "transparent") +
  geom_sf(data = all.gps.sp.sf,
          aes(geometry = geometry),
          size = 1,
          color = "red")

st_crs(inlandWaters.10km) == st_crs(all.gps.sp.sf)

# intersect 100m and 10km to 100km buffer (return T/F)
int.10km <- st_intersects(all.gps.sp.sf,inlandWaters.10km); TF_10km <- apply(int.10km, 1, any)
int.20km <- st_intersects(all.gps.sp.sf,inlandWaters.20km); TF_20km <- apply(int.20km, 1, any)
int.30km <- st_intersects(all.gps.sp.sf,inlandWaters.30km); TF_30km <- apply(int.30km, 1, any)
int.40km <- st_intersects(all.gps.sp.sf,inlandWaters.40km); TF_40km <- apply(int.40km, 1, any)
int.50km <- st_intersects(all.gps.sp.sf,inlandWaters.50km); TF_50km <- apply(int.50km, 1, any)
int.60km <- st_intersects(all.gps.sp.sf,inlandWaters.60km); TF_60km <- apply(int.60km, 1, any)
int.70km <- st_intersects(all.gps.sp.sf,inlandWaters.70km); TF_70km <- apply(int.70km, 1, any)
int.80km <- st_intersects(all.gps.sp.sf,inlandWaters.80km); TF_80km <- apply(int.80km, 1, any)
int.90km <- st_intersects(all.gps.sp.sf,inlandWaters.90km); TF_90km <- apply(int.90km, 1, any)
int.100km <- st_intersects(all.gps.sp.sf,inlandWaters.100km); TF_100km <- apply(int.100km, 1, any)

# bind vectors
all.gps.sp.100m <- cbind(all.gps.sp.100m,TF_10km,TF_20km,TF_30km,TF_40km,TF_50km,TF_60km,TF_70km,TF_80km,TF_90km,TF_100km)

# filter for each buffer
all.gps.sp.100m.10km <- all.gps.sp.100m %>% filter(TF_10km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.10km) # 82655    314
all.gps.sp.100m.20km <- all.gps.sp.100m %>% filter(TF_20km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.20km) # 98465    314
all.gps.sp.100m.30km <- all.gps.sp.100m %>% filter(TF_30km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.30km) # 105679    314
all.gps.sp.100m.40km <- all.gps.sp.100m %>% filter(TF_40km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.40km) # 111951    314
all.gps.sp.100m.50km <- all.gps.sp.100m %>% filter(TF_50km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.50km) # 118389    314
all.gps.sp.100m.60km <- all.gps.sp.100m %>% filter(TF_60km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.60km) # 120956    314
all.gps.sp.100m.70km <- all.gps.sp.100m %>% filter(TF_70km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.70km) # 123087    314
all.gps.sp.100m.80km <- all.gps.sp.100m %>% filter(TF_80km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.80km) # 126392    314
all.gps.sp.100m.90km <- all.gps.sp.100m %>% filter(TF_90km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.90km) # 128112    314
all.gps.sp.100m.100km <- all.gps.sp.100m %>% filter(TF_100km == T) %>%
  filter(Year %in% seq(2000,2022,1)); dim(all.gps.sp.100m.100km) # 129693    314

# save # of projects in matrix
rm(n.projects.buffer)
n.projects.buffer <- matrix(nrow=10,ncol=5)
rownames(n.projects.buffer) <- c("buf_10km","buf_20km","buf_30km","buf_40km","buf_50km","buf_60km","buf_70km","buf_80km",
                                 "buf_90km","buf_100km")
colnames(n.projects.buffer) <- c("100m_elev_proj","100m_elev_loc","100m_elev_keywords_proj","100m_elev_keywords_loc","n.country")

# unique projects
  n.projects.buffer[1,1] <- length(unique(all.gps.sp.100m.10km$iati_identifier))
  n.projects.buffer[2,1] <- length(unique(all.gps.sp.100m.20km$iati_identifier))
  n.projects.buffer[3,1] <- length(unique(all.gps.sp.100m.30km$iati_identifier))
  n.projects.buffer[4,1] <- length(unique(all.gps.sp.100m.40km$iati_identifier))
  n.projects.buffer[5,1] <- length(unique(all.gps.sp.100m.50km$iati_identifier))
  n.projects.buffer[6,1] <- length(unique(all.gps.sp.100m.60km$iati_identifier))
  n.projects.buffer[7,1] <- length(unique(all.gps.sp.100m.70km$iati_identifier))
  n.projects.buffer[8,1] <- length(unique(all.gps.sp.100m.80km$iati_identifier))
  n.projects.buffer[9,1] <- length(unique(all.gps.sp.100m.90km$iati_identifier))
  n.projects.buffer[10,1] <- length(unique(all.gps.sp.100m.100km$iati_identifier))

# locations projects
  n.projects.buffer[1,2] <- length(unique(all.gps.sp.100m.10km$iati_identifier_bis))
  n.projects.buffer[2,2] <- length(unique(all.gps.sp.100m.20km$iati_identifier_bis))
  n.projects.buffer[3,2] <- length(unique(all.gps.sp.100m.30km$iati_identifier_bis))
  n.projects.buffer[4,2] <- length(unique(all.gps.sp.100m.40km$iati_identifier_bis))
  n.projects.buffer[5,2] <- length(unique(all.gps.sp.100m.50km$iati_identifier_bis))
  n.projects.buffer[6,2] <- length(unique(all.gps.sp.100m.60km$iati_identifier_bis))
  n.projects.buffer[7,2] <- length(unique(all.gps.sp.100m.70km$iati_identifier_bis))
  n.projects.buffer[8,2] <- length(unique(all.gps.sp.100m.80km$iati_identifier_bis))
  n.projects.buffer[9,2] <- length(unique(all.gps.sp.100m.90km$iati_identifier_bis))
  n.projects.buffer[10,2] <- length(unique(all.gps.sp.100m.100km$iati_identifier_bis))
  
# db with projects withing 100km
all.gps.sp.100m.100km <- all.gps.sp.100m %>% 
  filter(TF_100km == T)

# remove lat and long y
all.gps.sp.100m.100km$latitude.x <- NULL
all.gps.sp.100m.100km$latitude.y <- NULL
all.gps.sp.100m.100km$longitude.y <- NULL
all.gps.sp.100m.100km$longitude.x <- NULL

# national vs local scale
# https://iatistandard.org/en/iati-standard/203/codelists/activityscope/
summary(as.factor(all.gps.sp.100m.100km$activity_scope_code)) # many NAs     1:33196     2:437     3:2371     4:26617     5:1753     6:1055     7:677     8:1567    NA:55879  NA's:23
# check NAs
dd <- all.gps.sp.100m.100km %>%
  dplyr::filter(activity_scope_code == "NA")

summary(as.factor(all.gps.sp.100m.100km$sector_code)) # many NAs         NA:0  NA's:737
ddd <- all.gps.sp.100m.100km %>%
  dplyr::filter(sector_code == "NA")

# location_feature_designation_cod http://codelists103.archive.iatistandard.org/data/codelist/LocationType/
                     
# save rds data
saveRDS(all.gps.sp.100m.100km, here("data","derived-data","iati_GPS_db_100km_100m.rds"))
write.csv(all.gps.sp.100m.100km, here("data","derived-data","iati_GPS_db_100km_100m.csv"))

###################################################
# supplemental infos  - stop code here for now (01/10/2023)

### select randomly non coastal 400 projects 
no.coast.100KM <- all.gps.sp.100m.100km %>%
  filter(coastal == 0) %>%
  filter(!is.na(title_narrative) & !is.na(description_narrative))

  # 1st random selection
no.coast.100KM.800 <- sample_n(no.coast.100KM, 800)
dim(no.coast.100KM.800)
no.coast.100KM.800.c <- no.coast.100KM.800[1:400,]
saveRDS(no.coast.100KM.800.c, here("data","derived-data","iati_GPS_db_100km_100m_NOCOAST_random_400_c.rds"))
no.coast.100KM.800.d <- no.coast.100KM.800[401:800,]
saveRDS(no.coast.100KM.800.d, here("data","derived-data","iati_GPS_db_100km_100m_NOCOAST_random_400_d.rds"))

head(no.coast.100KM.800.c)
# check number of projects
coast.100KM <- all.gps.sp.100m.100km %>%
  filter(coastal == 1)

summary(coast.100KM)

length(unique(coast.100KM$iati_identifier)) # 2826
length(unique(coast.100KM$iati_identifier_bis)) # 4519

# number of projects, within k buffer, with marine specific keywords
  # 10km
db.gps.marine.10km <- all.gps.sp.100m.10km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))  
n.projects.buffer[1,3] <- length(unique(db.gps.marine.10km$iati_identifier)) # 1052
n.projects.buffer[1,4] <- length(unique(db.gps.marine.10km$iati_identifier_bis)) # 1766
n.projects.buffer[1,5] <- length(unique(db.gps.marine.10km$iso_a2)) # 1766

# 20km
db.gps.marine.20km <- all.gps.sp.100m.20km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))
n.projects.buffer[2,3] <- length(unique(db.gps.marine.20km$iati_identifier)) # 1052
n.projects.buffer[2,4] <- length(unique(db.gps.marine.20km$iati_identifier_bis)) # 1766
n.projects.buffer[2,5] <- length(unique(db.gps.marine.20km$iso_a2)) # 1766

# 30km
db.gps.marine.30km <- all.gps.sp.100m.30km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))
n.projects.buffer[3,3] <- length(unique(db.gps.marine.30km$iati_identifier)) # 1052
n.projects.buffer[3,4] <- length(unique(db.gps.marine.30km$iati_identifier_bis)) # 1766
n.projects.buffer[3,5] <- length(unique(db.gps.marine.30km$iso_a2)) # 1766

# 40km
db.gps.marine.40km <- all.gps.sp.100m.40km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))
n.projects.buffer[4,3] <- length(unique(db.gps.marine.40km$iati_identifier)) # 1052
n.projects.buffer[4,4] <- length(unique(db.gps.marine.40km$iati_identifier_bis)) # 1766
n.projects.buffer[4,5] <- length(unique(db.gps.marine.40km$iso_a2)) # 1766

# 50km
db.gps.marine.50km <- all.gps.sp.100m.50km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))
n.projects.buffer[5,3] <- length(unique(db.gps.marine.50km$iati_identifier)) # 1052
n.projects.buffer[5,4] <- length(unique(db.gps.marine.50km$iati_identifier_bis)) # 1766
n.projects.buffer[5,5] <- length(unique(db.gps.marine.50km$iso_a2)) # 1766

# 60km
db.gps.marine.60km <- all.gps.sp.100m.60km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))
n.projects.buffer[6,3] <- length(unique(db.gps.marine.60km$iati_identifier)) # 1052
n.projects.buffer[6,4] <- length(unique(db.gps.marine.60km$iati_identifier_bis)) # 1766
n.projects.buffer[6,5] <- length(unique(db.gps.marine.60km$iso_a2)) # 1766

# 70km
db.gps.marine.70km <- all.gps.sp.100m.70km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))
n.projects.buffer[7,3] <- length(unique(db.gps.marine.70km$iati_identifier)) # 1052
n.projects.buffer[7,4] <- length(unique(db.gps.marine.70km$iati_identifier_bis)) # 1766
n.projects.buffer[7,5] <- length(unique(db.gps.marine.70km$iso_a2)) # 1766

# 80km
db.gps.marine.80km <- all.gps.sp.100m.80km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))
n.projects.buffer[8,3] <- length(unique(db.gps.marine.80km$iati_identifier)) # 1052
n.projects.buffer[8,4] <- length(unique(db.gps.marine.80km$iati_identifier_bis)) # 1766
n.projects.buffer[8,5] <- length(unique(db.gps.marine.80km$iso_a2)) # 1766

# 90km
db.gps.marine.90km <- all.gps.sp.100m.90km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|")))
n.projects.buffer[9,3] <- length(unique(db.gps.marine.90km$iati_identifier)) # 1052
n.projects.buffer[9,4] <- length(unique(db.gps.marine.90km$iati_identifier_bis)) # 1766
n.projects.buffer[9,5] <- length(unique(db.gps.marine.90km$iso_a2)) # 1766

# 100km
db.gps.marine.100km <- all.gps.sp.100m.100km %>% distinct() %>% 
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>% 
  dplyr::filter(str_detect(title_narrative,paste0(key.marine, collapse="|"))) 
n.projects.buffer[10,3] <- length(unique(db.gps.marine.100km$iati_identifier)) # 1052
n.projects.buffer[10,4] <- length(unique(db.gps.marine.100km$iati_identifier_bis)) # 1766
n.projects.buffer[10,5] <- length(unique(db.gps.marine.100km$iso_a2)) # 1766

      # barplot
          # wide to long
n.projects.buffer.long <- n.projects.buffer %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  gather(key="project_filters",value="n.projects",-rowname)

# Create the barplot
n.proj.buffer <- ggplot(data=n.projects.buffer.long %>% filter(project_filters == "100m_elev_proj") %>% droplevels(), aes(x=fct_inorder(rowname), y=n.projects, fill=project_filters)) +
  geom_bar(stat="identity",fill="blue",color="blue") +
  #geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
  #          color="white", size=3.5)+
  theme(legend.position = "none")  +
  geom_abline(slope=0,intercept=n.projects.buffer[1,1],col="red",linetype = "dashed") +
  ggtitle("Number of GPS projects below 100m elevation between 2000 and 2022") +
  geom_text(aes(x=fct_inorder(rowname), label=n.projects), vjust = 0) +
  xlab("buffer size") +
  theme_minimal()

n.proj.buffer.keywords <- ggplot(data=n.projects.buffer.long %>% filter(project_filters == "100m_elev_keywords_proj") %>% droplevels(), aes(x=fct_inorder(rowname), y=n.projects, fill=project_filters)) +
  geom_bar(stat="identity",fill="blue",color="blue") +
  #geom_text(aes(y=label_ypos, label=len), vjust=1.6, 
  #          color="white", size=3.5)+ 
  ggtitle("Number of GPS projects below 100m elevation selected by coastal/marine keywords between 2000 and 2022") +
  theme_minimal() +
  geom_abline(slope=0,intercept=n.projects.buffer[1,3],col="red",linetype = "dashed") +
  geom_text(aes(x=fct_inorder(rowname), label=n.projects), vjust = 0) +
  xlab("buffer size") +
  theme(legend.position = "none") 

p = ggarrange(n.proj.buffer,n.proj.buffer.keywords,ncol=1,nrow=2,labels=c("A",'B'))
p
# save plot
ggsave(here::here("figures","Supp_N_projects_coastal_buffer.pdf"),p,width=10,height=12)

# by country for 30km and 100km
ctr.proj.marine.30km <- db.gps.marine.30km %>% 
  select(region_un,iso_a3,iso_a2,admin,iati_identifier) %>% 
  distinct() %>%
  mutate(region_un = as.factor(region_un)) %>%
  mutate(admin = as.factor(admin)) %>%
  mutate(iati_identifier = as.factor(iati_identifier)) %>%
  group_by(region_un,admin) %>%
  summarize(n= n_distinct(iati_identifier))
ctr.proj.marine.30km$buf <- rep("30km",times=dim(ctr.proj.marine.30km)[1])
  
ctr.proj.marine.100km <- db.gps.marine.100km %>% 
  select(region_un,iso_a3,iso_a2,admin,iati_identifier) %>% 
  distinct() %>%
  mutate(region_un = as.factor(region_un)) %>%
  mutate(admin = as.factor(admin)) %>%
  mutate(iati_identifier = as.factor(iati_identifier)) %>%
  group_by(region_un,admin) %>%
  summarize(n= n_distinct(iati_identifier))
ctr.proj.marine.100km$buf <- rep("100km",times=dim(ctr.proj.marine.100km)[1])

# bind
ctr.proj.marine <- rbind(ctr.proj.marine.30km,ctr.proj.marine.100km) %>%
  mutate(buf=as.factor(buf))

  # plot by country
ctr.proj.marine.plot.30km <- ggplot(ctr.proj.marine %>% filter(buf=="30km"),aes(x=reorder(admin,-n), y=n, fill=region_un)) +
         geom_bar(position="dodge", stat="identity") +
         ggtitle("Number of GPS projects below 100m elevation, 30km buffer, by countries, selected by coastal/marine keywords between 2000 and 2022") +
         theme_minimal() +
         scale_x_discrete(guide = guide_axis(angle = 45)) +
         xlab("Country") + 
         ylab("Number of projects") 
ctr.proj.marine.plot.30km

ctr.proj.marine.plot.100km <- ggplot(ctr.proj.marine %>% filter(buf=="100km"),aes(x=reorder(admin,-n), y=n, fill=region_un)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Number of GPS projects below 100m elevation, 100km buffer, by countries, selected by coastal/marine keywords between 2000 and 2022") +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  xlab("Country") + 
  ylab("Number of projects")
ctr.proj.marine.plot.100km

#
p = ggarrange(ctr.proj.marine.plot.30km,ctr.proj.marine.plot.100km,ncol=1,nrow=2,labels=c("A",'B'),common.legend = T)
p
# save plot
ggsave(here::here("figures","Supp_N_projects_coastal_buffer_30_100_ctry.pdf"),p,width=15,height=12)

##### FINAL MAP - all coastal 100km
plot.inland.final <- ggplot() +
  geom_sf(data = inlandWaters.100km) +
  geom_sf(data = inlandWaters.100km, fill = "lightblue", col = "transparent") +
  geom_sf(data = all.gps.sp.100m.100km,
          aes(geometry = geometry),
          size = 1,
          color = "red") +
  theme_bw() +
  ggtitle("67817 coastal projects, 100248 uniques locations (100m-100km)")
plot.inland.final
ggsave(here("figures","Total_coastal_projects_100km.png"),plot.inland.final)

##### FINAL MAP - all coastal 100km - keywords
plot.inland.final.key <- ggplot() +
  geom_sf(data = inlandWaters.100km) +
  geom_sf(data = inlandWaters.100km, fill = "lightblue", col = "transparent") +
  geom_sf(data = db.gps.marine.100km,
          aes(geometry = geometry),
          size = 1,
          color = "red") +
  theme_bw() +
  ggtitle("67817 coastal projects, 100248 uniques locations (100m-100km)")
plot.inland.final.key
ggsave(here("figures","Total_coastal_projects_100mkm_key.png"),plot.inland.final)

########## key words filter
key.climate <- c("\\bclimat") # 1061
key.development <- c("\\bblue","\\bfish","\\baquac","\\btouris","seabed mining","oil and gas","\\bdesal","offshore","\\bport\\b","\\bshipping\\b","\\bbiotech","\\bpharmaceut","\\bbiochem","plastic","tidal","\\bwave\\b","wastewater")
key.conservation <- c("\\bmpa\\b","\\bamp\\b","\\blmma","\\bconserv","marine reserve","marine protected area","\\oecm\b","\\oecms\b")

############### 1str filter - still using coastal
length(unique(all.gps.sp.100m.100km$iati_identifier)) # 65657
length(unique(all.gps.sp.100m.100km$iati_identifier_bis)) # 95153

  # save rds data
saveRDS(all.gps.sp.100m.100km, here("data","derived-data","iati_GPS_db_100km_100m.rds"))
write.csv(all.gps.sp.100m.100km, here("data","derived-data","iati_GPS_db_100km_100m.csv"))

all.gps.sp.100m.100km.coastal<-readRDS(here("data","derived-data","iati_GPS_db_100km_100m.rds"))
length(unique(all.gps.sp.100m.100km.coastal$coastal))
all.gps.sp.100m.100km.coastal$coastal
head(all.gps.sp.100m.100km.coastal)

# marine gps + no gps
CADC.marine <- all.merge %>%
  distinct() %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) # 11064  309

length(unique(CADC.marine$iati_identifier)) # 4297
length(unique(CADC.marine$iati_identifier_bis)) # 6468

# marine no GPS
marine.noGPS <- all.merge %>%
  filter(is.na(latitude)) %>%
  distinct() %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) # 2092  311

length(unique(marine.noGPS$iati_identifier)) # 2022
length(unique(marine.noGPS$iati_identifier_bis)) # 2024

# marine all gps
db.gps.marine.all <- all.gps.sp.100m.100km %>%
  distinct() %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|")))
length(unique(db.gps.marine.all$iati_identifier)) # 1052
length(unique(db.gps.marine.all$iati_identifier_bis)) # 1766

summary(as.factor(CADC.marine$TF.100km))

  # 3074 COASTAL PROJECTS 
  # 65% WITHOUT GPS

    # merge db gps + non gps
CADC.marine <- bind_rows(CADC.marine,db.gps.marine.all)

length(unique(CADC.marine$iati_identifier)) # 4297
length(unique(CADC.marine$iati_identifier_bis)) # 6468

  # check gps
test <- CADC.marine %>% 
  distinct() %>%
  filter(!is.na(latitude))

length(unique(test$iati_identifier)) # 2275
length(unique(test$iati_identifier_bis)) # 4444


dim(CADC.marine) # 
head(CADC.marine)
summary(CADC.marine$latitude)
    # save

test <- CADC.db.marine100mkm %>% filter(iati_identifier == "NL-KVK-27378529-CBIXV13")
test$location_point_pos
test$latitude.x


# climate
CADC.db.marine100mkm <- CADC.db.marine100mkm %>%
  mutate(CADC = ifelse(str_detect(description_narrative,paste0(key.marine, collapse="|")) & str_detect(description_narrative,paste0(key.climate, collapse="|")),"Climate",
                       ifelse(str_detect(description_narrative,paste0(key.marine, collapse="|")) & str_detect(description_narrative,paste0(key.conservation, collapse="|")),"Conservation",
                              ifelse(str_detect(description_narrative,paste0(key.marine, collapse="|")) & str_detect(description_narrative,paste0(key.development, collapse="|")),"Development","Others"))))
CADC.db.marine100mkm$CADC <- as.factor(CADC.db.marine100mkm$CADC)
    # number of projects 
summary(CADC.db.marine100mkm$CADC)

  # unique iati identifier
length(unique(CADC.db.marine100mkm$iati_identifier[which(CADC.db.marine100mkm$CADC== "Climate")]))
length(unique(CADC.db.marine100mkm$iati_identifier[which(CADC.db.marine100mkm$CADC== "Conservation")]))
length(unique(CADC.db.marine100mkm$iati_identifier[which(CADC.db.marine100mkm$CADC== "Development")]))

# map
plot.marine.100mkm <- ggplot() +
  geom_sf(data = inlandWaters.100km) +
  geom_sf(data = inlandWaters.100km, fill = "lightblue", col = "transparent") +
  geom_sf(data = CADC.db.marine100mkm,
          aes(geometry = geometry,color=CADC),
          size = 1) +
  theme_bw() +
  ggtitle("70382 100mkm projects - 1134 coastal projects, 1935 uniques locations (100m-100km)",
          subtitle = "Climate: 146/1012(72) - Conservation: 74/432(37) - Development: 463/1400(262)")
plot.marine.100mkm
ggsave(here("figures","Coastal_projects_GPS_CADC.png"),plot.marine.100mkm)

  # pdf 
length(which(is.na(unique(CADC.db.marine100mkm$document_link_url[which(CADC.db.marine100mkm$CADC== "Climate")]))==F))  #72
length(which(is.na(unique(CADC.db.marine100mkm$document_link_url[which(CADC.db.marine100mkm$CADC== "Conservation")]))==F)) # 37
length(which(is.na(unique(CADC.db.marine100mkm$document_link_url[which(CADC.db.marine100mkm$CADC== "Development")]))==F))  # 262

# n unique project
#### map with only coastal keyworks
rm(CADC.db.coastal.V3)
CADC.db.coastal.V3 <- all.gps.sp.1 %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>%
  filter(elevation <= 100)
  
#dplyr::filter(str_detect(description_narrative,paste0(key.climate, collapse="|")))
length(unique(CADC.db.coastal.V3$iati_identifier)) # 1790
dim(CADC.db.coastal.V3) # 6583  313

summary(CADC.db.coastal.V3$elevation)

CADC.db.coastal.V3[which(CADC.db.coastal.V3$elevation > ),"description_narrative"]
CADC.db.coastal.V3[1,"iso_a2"]

str_locate(CADC.db.coastal.V3$description_narrative[4326],key.marine[14])
CADC.db.coastal.V3$iati_identifier[4326]

######## without the coastal keywords
# climate

CADC.db.coastal.V3$description_narrative[100]

rm(CADC.db.climate.100mkm.V2)
CADC.db.climate.100mkm.V2 <- all.gps.sp.1.100m.100km %>%
 # dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.climate, collapse="|")))
length(unique(CADC.db.climate.100mkm.V2$iati_identifier)) # 1658
dim(CADC.db.climate.100mkm.V2) # 4179  313

CADC.db.climate.100mkm.V2$description_narrative[100]

# conservation
rm(CADC.db.conservation.100mkm)
CADC.db.conservation.100mkm <- all.gps.sp.1.100m.100km %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.conservation, collapse="|")))
length(unique(CADC.db.conservation.100mkm$iati_identifier)) # 61
dim(CADC.db.conservation.100mkm) # 311 313
CADC.db.conservation.100mkm$description_narrative[50]

# development
rm(CADC.db.dev.100mkm)
CADC.db.dev.100mkm <- all.gps.sp.1.100m.100km %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.marine, collapse="|"))) %>%
  dplyr::filter(str_detect(description_narrative,paste0(key.development, collapse="|")))
length(unique(CADC.db.dev.100mkm$iati_identifier)) # 381
dim(CADC.db.dev.100mkm) # 1307 313

CADC.db.dev.100mkm$description_narrative[200]


  # retrieve organizations and number of projects for each
all.filter <- all %>%
  filter(iati_identifier %in% iati_id) %>%
  distinct() %>%
  group_by(participating_org_narrative) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
    
## check locations of 2nd db
loc.unique <- unique(all.gps.sp.2.100m.100km$locations)
n <- nchar(as.character(loc.unique))
loc.unique.n <- data.frame(locations= loc.unique,n = n)
loc.unique.n <- loc.unique.n %>% arrange(n)
loc.unique.n %>% filter(n <= 2) %>% select(locations) %>% as.vector()

i=121
loc.unique[i]
all.gps.sp.2.100m.100km[which(all.gps.sp.2.100m.100km$locations == "Se"),"description_narrative"]
all.gps.sp.2.100m.100km <- all.gps.sp.2.100m.100km[-which(all.gps.sp.2.100m.100km$locations %in% ctr2rm),]


# links to document
# link to document
url.docs.gps.1 <- all.gps.sp.1.100m.100km %>% 
  mutate(document_link_url = ifelse(document_link_url == "NA",NA,document_link_url)) %>%
  filter(!is.na(document_link_url))
dim(url.docs.gps.1) # 51903/98221

url.docs.gps.2 <- all.gps.sp.2.100m.100km %>% 
  mutate(document_link_url = ifelse(document_link_url == "NA",NA,document_link_url)) %>%
  filter(!is.na(document_link_url))
dim(url.docs.gps.2) # 11638/22032

summary(as.factor(all.gps.sp.2.100m.100km$participating_org_narrative))

length(unique(c(url.docs.gps.1$iati_identifier,url.docs.gps.2$iati_identifier))) # 33880 projects with links to documents

# retrieving pdf when we have direct pdf link
url="http://documents.worldbank.org/curated/en/836341468212694602/pdf/PID-Appraisal-Print-P130492-04-11-2014-1397243722649.pdf"
test <- download.file(url, 'introductionToR.pdf', mode="wb")

# 
