# code to retrieve and correct GPS coordinates from the full database - in the descriptive narrative
source(here::here("analyses","001_Coastal_countries.R")) # load countries and cities in countries

# load raw data
all <- readRDS(here("data","raw-data","CADC.rds")) # 504348    254

#which(grepl("MG", all$recipient_country_code, fixed=TRUE))
#test <- all[which(grepl("MG", all$recipient_country_code, fixed=TRUE)),]
#test[which(test$iati_identifier == "XM-DAC-301-2-109281-001"),"recipient_country_code"]
#test <- all %>% filter(iati_identifier == "XM-DAC-41122-MADAGASCAR-2670/A0/06/600/101")
#dim(test)
#test$recipient_country_code
#length(unique(test$iati_identifier))
#dim(test)
#test$recipient_country_code
# remove duplicated projects
CADC <- all %>%
  dplyr::distinct() # 456723    254

# unique country vector
ctry <- unique(ctr$iso2)

################################################
#   extract gps data and location narrative    #
################################################

# count number of gps coordinates in position cell
# there information on whether the gps information are "exact location" or not. for now, it is not an information that has been considered, 
# but location narrative is provided
# extract both gps and location narrative for each project - 1 raw = 1 unique location (at present, 1 raw= 1 project, despite several locations)
# for now I did not removed the NAs or duplicated gps location, so it will be easier to match the "exact location information" with the gps coordinates, I hope...
CADC.db <- CADC %>%
  mutate(nstring_pos = str_count(location_point_pos, pattern = " ")) # count number of elements in pos vector

### check. why duplicated projects
length(unique(CADC.db$iati_identifier)) # 408644
#duplicated(CADC.db$iati_identifier) %>% which()
#CADC.db[c(19,22),]
  # beg/end of projects (final reports reported for ex) -> only consider the unique number of projects = 408644 unique projects (but can happen in several countries)

### how many countries with only GPS locations
rm(CADC.db.gps)
CADC.db.gps <- CADC.db %>%
  filter(!is.na(location_point_pos)) %>%
  mutate(n_ctry = str_length(recipient_country_code)) %>% # when 1 country per row,  (2 = number of strings in the cell)
  dplyr::filter(n_ctry == 2 & recipient_country_code %in% coastal.ctr$iso2) # keep only the coastal countries # 148391    256

  
#  filter(is.na(location_name_narrative))
#unique(CADC.db.gps$iati_identifier) %>% length() # 138922
#CADC.db.loc <- CADC.db %>%
#  filter(!is.na(location_name_narrative)) %>%
#  filter(is.na(location_point_pos))
#unique(CADC.db.loc$iati_identifier) %>% length() #40158

##### List for GPS locations
# transform position column into a list to extract latitude and longitude
rm(list.gps)
list.gps <- regmatches(CADC.db.gps$location_point_pos,
                       gregexpr("([0-9.-]+).+?([0-9.-]+)",CADC.db.gps$location_point_pos))
names(list.gps) <- CADC.db.gps$iati_identifier
list.gps <- lapply(list.gps,function(x) strsplit(x," ")) # create sublist
list.gps <- lapply(list.gps,unlist) # create vectors
list.gps <- lapply(list.gps, function(x) if(identical(x, character(0))) NA_character_ else x) #replace empty with NA

#list.gps[which(names(list.gps) == "FR-6-BR-pas de code prisme - subvention")]

# rm NAs
list.gps <-list.gps[!sapply(list.gps,is.null)] # ok - no null

# replace elements of list "" to NAs to avoid mistake
list.gps <- lapply(list.gps,function(x) replace(x, x=='', NA)) # create sublist

# remove empty string
list.gps <- lapply(list.gps,function(x) x[x != ""]) # create sublist

# find odd length and remove projects with uneven latitude/longitude information
is.odd <- function(x) x %% 2 != 0
list.gps <- list.gps[-which(is.odd(sapply(list.gps,length)))]
  # check all even
  # length(sapply(list.gps,length) %>% unique() %% 2 == 0) == length(sapply(list.gps,length) %>% unique())

# remove NULL list
#### WARNING "BE-BCE_KBO-0413119733-PROG2017-2021_SDI10-HAI" - coordonnées pas en decimal
### check certain lat/long are too large : on the 14/06/2023
list.gps <- compact(list.gps) # Remove all NULL entries from a list

# split each element of the list into sublist - a loop for now, can probably do better later with lapply - but no time (30/05/2023)
list.gps.full <- data.frame()

for (i in 1:length(list.gps)){
  
  print(paste("i=",i))
  
  df.temp =matrix(as.numeric(list.gps[[i]]), ncol=2, byrow = T)
  
  # names - unique identifier
  numb <- seq( from = 1, to = dim(df.temp)[1]) # unique identifier for on site projects (a,b,c,etc.)
  iati_bis <- paste0(names(list.gps)[i],"_",numb)
  # create a data.frame with 4 columns: IATI unique identifier, subprojects, latitude, longitude
  df.temp = cbind(rep(names(list.gps)[i],dim(df.temp)[1]),iati_bis,df.temp)
  colnames(df.temp) <- c("iati_identifier","iati_identifier_bis","latitude","longitude")
  
  # add in a full dataframe
  list.gps.full <- rbind(list.gps.full,df.temp) # create a full dataframe
  
  # clean objects
  rm(df.temp)
  rm(temp.list.dupl)
  rm(temp.names)
  
} # 450203      4

_# remove NAs in longitude and latitude
list.gps.full <-  list.gps.full %>%   
  mutate(longitude = as.numeric(longitude), latitude=as.numeric(latitude)) %>%
  dplyr::filter(!is.na(longitude),!is.na(latitude))    
# 448751      4

#list.gps.full %>% filter(iati_identifier == "FR-6-BR-pas de code prisme - subvention")

## remove distinct projects/gps
list.gps.full <- list.gps.full %>%
  distinct() #419546      4

length(unique(list.gps.full$iati_identifier)) # 187643 - unique projects with GPS coordinates
length(unique(list.gps.full$iati_identifier_bis)) # 419492

# look for issues in gps
#list.gps.full[which(list.gps.full$latitude >180),]
#list.gps.full[which(list.gps.full$latitude < -180),]

# extract countries for projects with GPS locations
# world polygons
world <- ne_countries(scale = "large", returnclass = "sf")

#crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
#projected.world = st_transform(world, 4326)

# project the GPS coordinates - create a sp - point dataframe
xy <- list.gps.full[,c("longitude","latitude")]
list.gps.full.proj <- SpatialPointsDataFrame(coords = xy, data = list.gps.full,
                                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# simple plot
world.2 <- ne_countries(type = "countries", scale = "large")
#sp::plot(world.2,xlim=c(12,13),ylim=c(-8,-7))
#points(CADC.db.filtered.gps.proj.sp,ylim=c(-180,180),ylim=c(-90,90))

# overlay points and countries to retrieve country information - still NAs since some points are outside the polygons
ctry.over <- over(list.gps.full.proj, world.2) #419546    168
# which(is.na(ctry.over$sovereignt))  %>% length()

#list.gps.full.proj[4787,]
#list.gps.full[4787,]

# merge GPS with new countries
list.gps.full.ctry <- cbind(list.gps.full,ctry.over %>% select(type,admin,iso_a2,iso_a3,pop_est,pop_rank,pop_year,gdp_md,gdp_year,economy,income_grp,continent,region_un,subregion,region_wb))

#list.gps.full.ctry %>% filter(latitude > 90 | latitude < -90 | longitude > 180 | longitude < -180 ) %>% head()
#list.gps.full.ctry %>% filter(latitude > 80 | latitude < -80 | longitude > 180 | longitude < -180 ) %>% summary()

# remove projects not occurring in coastal countries
rm(list.gps.full.ctry.coast)
list.gps.full.ctry.coast <- list.gps.full.ctry %>%
  mutate(iso_a2 = as.factor(iso_a2)) %>% 
  filter(list.gps.full.ctry$iso_a2 %in% coastal.ctr$iso2) # 291899     19

n.location.iati.ID <- list.gps.full.ctry.coast %>% select(iati_identifier,iati_identifier_bis) %>%
  mutate(iati_identifier = as.factor(iati_identifier),iati_identifier_bis = as.factor(iati_identifier_bis)) %>%
  dplyr::group_by(iati_identifier) %>%
  dplyr::summarize(n=n()) # certain projects are in a lot of locations (e.g. Nepal, etc.)

#########################################
#   merge GPS data with full database   #
#########################################

# less gps data (XX gps locations) than projects (XX), since not all the projects have gps information
dim(CADC.db) # 456723    255
length(unique(CADC.db$iati_identifier)) # 408644 unique projects in the full database
dim(list.gps.full.ctry)  # 419546     19
length(unique(list.gps.full.ctry$iati_identifier)) # 187643 unique projects with GPS locations - 8046 with no country information, since lat/long are not correct

# extract latitude and longitude from the list
CADC.db.V2 <- CADC.db %>%
  mutate(location_exactness_code_V2 = sapply(location_exactness_code,head,1))  %>% # extract first element of location exactness code
  mutate(Year = as.factor(substring(activity_date_iso_date,1,4))) # start year # 392946    257

# merge filtered database with gps location by iati_identifier
#rm(CADC.db.V3)
#CADC.db.V3 <- left_join(CADC.db.V2,list.gps.full.ctry.coast,by="iati_identifier",relationship = "many-to-many") %>%
#  distinct()
#dim(CADC.db.V3) # 621522    275
#saveRDS(CADC.db.V3,here("data","derived-data","CADC.db.V3.rds"))

#########################################
#      Extract country information      #
#########################################

#CADC.db.V3$location_name_narrative 
  # difficult to use: a mix of locations and countries, with different format - case by case use rather than automatize
#rm(CADC.db.V3)
#CADC.db.V3 <- readRDS(here("data","derived-data","CADC.db.V3.rds"))

# remove data with GPS
rm(CADC.db.gps.NA)
CADC.db.gps.NA <- CADC.db %>%
  dplyr::filter(is.na(location_point_pos)) %>%
  mutate(n_ctry = str_length(recipient_country_code)) %>% # when 1 country per row,  (2 = numner of strings in the cell)
  dplyr::filter(n_ctry == 2 & recipient_country_code %in% coastal.ctr$iso2) # keep only the coastal countries

# split the country list
rm(CTRY.list.db)
CTRY.list.db =strsplit(as.character(CADC.db.gps.NA$recipient_country_code),split=' | ',fixed=T)
#MONEY.list.db =strsplit(as.character(CADC.db.gps.NA$transaction_value),split=' | ',fixed=T)

# names the list - each element of the list = unique ID of the project
names(CTRY.list.db) <- CADC.db.gps.NA$iati_identifier

# list to long format dataframe for countries by iati IDs
rm(df)
df <- as.data.frame(do.call(cbind,CTRY.list.db)) %>% t() # 251505    175

# cbind project names and the dataframe
df <- cbind(as.character(rownames(df)),df)
rownames(df) <- NULL
df <- as.data.frame(df)
names(df)[1] <- "iati_identifier" # 168406      2
names(df)[2] <- "iso_a2" # 168406      2

# wide to long
rm(df_long)
df_long <- df %>% tidyr::gather(key = variable,iso_a2, -iati_identifier,factor_key=T,convert=T) %>%
  select(-variable) %>% 
  distinct() %>%
  mutate(iso_a2=as.factor(iso_a2)) %>% # remove duplicate countries for each project 
  mutate(iati_identifier=as.factor(iati_identifier)) # 218919 unique projects, 239 countries # 260640      2

# create unique project identifier for each project and country
df_long <- df_long %>%
  mutate(iati_identifier_bis = paste0(iati_identifier,"_",iso_a2))

#rm(df_long_n)
#df_long_n <- df_long %>%
#  dplyr::group_by(iati_identifier) %>%
  #dplyr::mutate(n=seq_along(iati_identifier)) %>%
#  ungroup()

# add non gps country information
rm(CADC.db.ctr)
CADC.db.ctr <- left_join(CADC.db.gps.NA,df_long,by="iati_identifier",relationship="many-to-many") %>% # many 2 many since there are duplicates iati-identifier
                    filter(iso_a2 %in% coastal.ctr$iso2) # filter coastal countries

CADC.db.ctr <- left_join(CADC.db.ctr,as.data.frame(world.2) %>% select(type,admin,iso_a2,iso_a3,pop_est,pop_rank,pop_year,gdp_year,income_grp,region_un,region_wb),by="iso_a2")
dim(CADC.db.ctr) # 168946    268
head(CADC.db.ctr) 

#CADC.db.ctr %>% select(iati_identifier,recipient_country_code,iso_a2,iati_identifier_bis) %>% head()
#CADC.db %>% filter(iati_identifier == "KR-GOV-090-1543000_2019_037") %>% head()

## bind gps and non_gps data
CADC.db.ctr.gps <- right_join(CADC.db.gps,list.gps.full.ctry.coast,by="iati_identifier",relationship="many-to-many") # many 2 many since there are duplicates iati-identifier 
              # 311373    273
#CADC.db.ctr.gps %>% select(iati_identifier,recipient_country_code,iso_a2,iati_identifier_bis,latitude,longitude) %>% tail(1000)
# merge the non-GPS and GPS data - should be about 734413 rows
CADC.db.final <- bind_rows(CADC.db.ctr,CADC.db.ctr.gps)
dim(CADC.db.final)
summary(as.data.frame(CADC.db.final$longitude))

#########################################
#                   Save                #
#########################################

saveRDS(CADC.db.final, here("data","derived-data","CADC.db.final.rds"))

# number of countries
length(unique(CADC.db.final$iso_a2)) # 179 countries
