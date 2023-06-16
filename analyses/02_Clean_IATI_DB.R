# source coastal countries code
source(here::here("analyses","001_Coastal_countries.R"))

# clean IATI -
  # keep coastal projects, rm irrigation projects
keywords <- c("\\bcoast","\\bmarin","\\bocean") # 3910
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac") # 6790
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based") # 7060
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based","\\bmpa\\b","\\bamp\\b","\\blmma") # 4845
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based","\\bmpa\\b","\\bamp\\b","\\blmma","conservation") # 11865
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based","\\bmpa\\b","\\bamp\\b","\\blmma","conservation","\\btouris") # 13743
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based","\\bmpa\\b","\\bamp\\b","\\blmma","conservation","\\btouris",
              "seabed mining","oil and gas") # 13901
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based","\\bmpa\\b","\\bamp\\b","\\blmma","conservation","\\btouris",
              "seabed mining","oil and gas","\\bdesal","offshore","\\bport\\b","shipping") # 15294
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based","\\bmpa\\b","\\bamp\\b","\\blmma","conservation","\\btouris",
              "seabed mining","oil and gas","\\bdesal","offshore","\\bport\\b","\\bshipping\\b","\\bbiotech","\\bpharmaceut","\\bbiochem") # 15850
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based","\\bmpa\\b","\\bamp\\b","\\blmma","conservation","\\btouris",
              "seabed mining","oil and gas","\\bdesal","offshore","\\bport\\b","\\bshipping\\b","\\bbiotech","\\bpharmaceut","\\bbiochem") # 15850
keywords <- c("\\bcoast","\\bmarine","\\bocean","\\bblue","\\bfish","\\baquac","coral","mangrove","seagrass","tidal marshes","nature-based","\\bmpa\\b","\\bamp\\b","\\blmma","conservation","\\btouris",
              "seabed mining","oil and gas","\\bdesal","offshore","\\bport\\b","\\bshipping\\b","\\bbiotech","\\bpharmaceut","\\bbiochem","plastic","tidal","\\bwave\\b","wastewater","climate",
              "\\badapt","\\brestoration")  # 16500 about

keywords_rm <- c("irrigation","drainage","earthquake","hospital")  # 13450

########################
#   load raw database #
########################

CADC.db <- readRDS(here("data","raw-data","CADC.rds"))

unique(CADC.db$iati_identifier) %>% length()
###########################
#   rm unrelated projects #
###########################
rm(CADC.db.filtered)
CADC.db.filtered <- CADC.db %>%
  dplyr::mutate(description_narrative = tolower(description_narrative)) %>%
  dplyr::filter(str_detect(description_narrative,paste0(keywords, collapse="|"))) %>% # projects to include
  dplyr::filter(!str_detect(description_narrative,paste0(keywords_rm, collapse="|"))) # projects to remove
dim(CADC.db.filtered)

# check
#str_detect(CADC.db.filtered$description_narrative[1],keywords[26])

#### form to categorized the projects in different categories
## on the 14/06/2023 - need to include: I don't know for each, in case there is not enough information in the title or narrative to answer
xxx = data.frame("narrative" = letters)

sub_set = CADC.db.filtered[1:2, c("iati_identifier","description_narrative")]

form <- function(data) {
  
  results = data.frame()
  
  for (i in 1:length(data)) {
    cat("Project #", i, "\n")
    cat(data[i,2], "\n")
    ans_coastal <- readline("Is it Costal?")
    
    if (tolower(ans_coastal) == "y") {
      
      ans_climate <- readline("Is it about Climate?")
      ans_fisheries <- readline("Is it about Fisheries?")
    } else {
      ans_climate = NA
      ans_fisheries = NA
    }
    
    results = rbind(results, 
                    data.frame("coastal" = ans_coastal,
                               "climate" = ans_climate,
                               "fisheries" = ans_fisheries,
                               row.names=data[i,1]))
  }
  results
}

form(sub_set)

########################
#   extract gps data   #
########################

# count number of gps coordinates in position cell
# there information on whether the gps information are "exact location" or not. for now, it is not an information that has been considered, 
# since the format for this information is messy. But to check for later - 
# for now I did not removed the NAs or duplicated gps location, so it will be easier to match the "exact location information" with the gps coordinates, I hope...
CADC.db.filtered <- CADC.db.filtered %>%
  mutate(nstring_pos = str_count(location_point_pos, pattern = " ")) # count number of elements in pos vector

# transform position column into a list to extract latitude and longitude
rm(list.gps)
list.gps <- regmatches(CADC.db.filtered$location_point_pos,
                       gregexpr("([0-9.-]+).+?([0-9.-]+)",CADC.db.filtered$location_point_pos))
names(list.gps) <- CADC.db.filtered$iati_identifier
list.gps <- lapply(list.gps,function(x) strsplit(x," ")) # create sublist
list.gps <- lapply(list.gps,unlist) # create vectors
list.gps <- lapply(list.gps, function(x) if(identical(x, character(0))) NA_character_ else x) #replace empty with NA

  # replace elements of list "" to NAs to avoid mistake
list.gps <- lapply(list.gps,function(x) replace(x, x=='', NA)) # create sublist

# rm NAs
list.gps <-list.gps[!sapply(list.gps,is.null)]

  # find odd length and remove projects with uneven latitude/longitude information
is.odd <- function(x) x %% 2 != 0
list.gps <- list.gps[-which(is.odd(sapply(list.gps,length)))]

# remove NULL list
  #### WARNING "BE-BCE_KBO-0413119733-PROG2017-2021_SDI10-HAI" - coordonnées pas en decimal
  ### check certain lat/long are too large : on the 14/06/2023
list.gps <- compact(list.gps)

      # split each element of the list into sublist - a loop for now, can probably do better later with lapply - but no time (30/05/2023)
        list.gps.full <- data.frame()
        
        for (i in 1:length(list.gps)){
          
          print(paste("i=",i))

          df.temp =matrix(as.numeric(list.gps[[i]]), ncol=2, byrow = T)

          # names - unique identifier
          let <- letters[seq( from = 1, to = dim(df.temp)[1])] # unique identifier for on site projects (a,b,c,etc.)
          iati_bis <- paste0(names(list.gps)[i],"_",let) ## WARNING rajouter des chiffres
          # create a data.frame with 4 columns: IATI unique identifier, subprojects, latitude, longitude
          df.temp = cbind(rep(names(list.gps)[i],dim(df.temp)[1]),iati_bis,df.temp)
          colnames(df.temp) <- c("iati_identifier","iati_identifier_bis","latitude","longitude")

          # add in a full dataframe
          list.gps.full <- rbind(list.gps.full,df.temp) # create a full dataframe
          
          # clean objects
          rm(df.temp)
          rm(temp.list.dupl)
          rm(temp.names)

        }
        
# remove NAs in longitude and latitude
list.gps.full <-  list.gps.full %>% 
  mutate(longitude = as.numeric(longitude), latitude=as.numeric(latitude)) %>%
  dplyr::filter(!is.na(longitude),!is.na(latitude))    


length(unique(list.gps.full$iati_identifier))
summary(list.gps.full$latitude)
#########################################
#   merge GPS data with full database   #
#########################################

# less gps data (2769 gps locations) than projects (3812), since not all the projects have gps information
dim(CADC.db.filtered) # 13450
length(unique(CADC.db.filtered$iati_identifier))
dim(list.gps.full)  # 11588

# extract latitude and longitude from the list
CADC.db.filtered.V2 <- CADC.db.filtered %>%
  mutate(location_exactness_code_V2 = sapply(location_exactness_code,head,1))  %>% # extract first element of location exactness code
  mutate(Year = as.factor(substring(activity_date_iso_date,1,4))) # start year

# merge filtered database with gps location by iati_identifier
CADC.db.filtered.V3 <- left_join(CADC.db.filtered.V2,list.gps.full,by="iati_identifier")
head(CADC.db.filtered.V3)
summary(as.numeric(CADC.db.filtered.V3$latitude))
summary(as.numeric(CADC.db.filtered.V3$longitude))

#########################################
#      Extract country information      #
#########################################

# split the country list
CTRY.list.db =strsplit(as.character(CADC.db.filtered.V3$recipient_country_code),' | ')

# names the list - each element of the list = unique ID of the project
names(CTRY.list.db) <- CADC.db.filtered.V3$iati_identifier

# list to long format dataframe for countries by iati IDs
rm(df)
df <- as.data.frame(do.call(cbind,CTRY.list.db)) %>% t()

# cbind project names and the dataframe
df <- cbind(as.character(rownames(df)),df)
rownames(df) <- NULL
df <- as.data.frame(df)
names(df)[1] <- "iati_identifier"

# wide to long
rm(df_long)
df_long <- df %>% tidyr::gather(key = variable,country, -iati_identifier,factor_key=T,convert=T) %>%
  select(-variable) %>% 
  distinct() %>% # remove duplicate countries
  filter(country != "|") # remove error inserted due to separators

# merge all list of countries with full database
CADC.db.filtered.V3 <- left_join(CADC.db.filtered.V3,df_long,by="iati_identifier")

# count number of countries by projects
#df2.count <- CADC.db.filtered.V3 %>%
#  dplyr::group_by(iati_identifier,country) %>%
#  duplicated() %>%
#  dplyr::summarize(n.country=n())

#CADC.db.filtered.V3 %>% filter(iati_identifier == "44000-P043195") %>%
#  select(country) %>%
#  dplyr::distinct() %>%
#  dplyr::summarize(n.country=n())



# merge number of countries by project
#CADC.db.filtered.V3 <- left_join(CADC.db.filtered.V3,df2.count,by="iati_identifier")
#dim(CADC.db.filtered)

# check if # countries > 1, longitude/latitude is provided or not. 
# if so, we need to extract the true countries from GPS coordinates, as it does not match the coordinates
#CADC.db.filtered.V3 %>% select(country,latitude,n.country) %>% 
#  mutate(latitude = as.numeric(latitude)) %>%
#  filter(n.country >1) %>% 
#  filter(!is.na(latitude)) %>%
#  dim()


##############################################
#      Fix countries by GPS coordinates      #
##############################################

# filter database with non NA gps coordinates 
CADC.db.filtered.gps <- CADC.db.filtered.V3 %>% 
  select(country,recipient_country_code,iati_identifier,iati_identifier_bis,latitude,longitude) %>%
  filter(!is.na(latitude)) %>%
  mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude),
         iati_identifier = as.factor(iati_identifier), iati_identifier_bis = as.factor(iati_identifier_bis)) 

CADC.db.filtered.V3$recipient_country_code
# world polygons
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

#crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)

projected.world = st_transform(world, 4326)

# project the GPS coordinates - create a sp - point dataframe
xy <- CADC.db.filtered.gps[,c("longitude","latitude")]
CADC.db.filtered.gps.proj.sp <- SpatialPointsDataFrame(coords = xy, data = CADC.db.filtered.gps %>% select(-latitude,-longitude),
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

# see the map of CADC - gps only
mapview(CADC.db.filtered.gps.proj.sp )

# simple plot
world.2 <- ne_countries(type = "countries", scale = "large")
sp::plot(world.2,xlim=c(12,13),ylim=c(-8,-7))
points(CADC.db.filtered.gps.proj.sp,ylim=c(-180,180),ylim=c(-90,90))

# overlay points and countries to retrieve country information - still NAs since some points are outside the polygons
ctry.over <- over(CADC.db.filtered.gps.proj.sp, world.2)
#which(is.na(ctry.over$sovereignt)) 
#CADC.db.filtered.gps[70:80,]

# merge GPS with new countries
CADC.db.filtered.gps <- cbind(CADC.db.filtered.gps,ctry.over)

# left_join with full database
df_to_join <- unique(CADC.db.filtered.gps[,c(4,11,51)])
CADC.db.filtered.final <- left_join(CADC.db.filtered.V3 %>% mutate(iati_identifier_bis = as.factor(iati_identifier_bis)),df_to_join,by="iati_identifier_bis")

#########################################
#                   Save                #
#########################################

saveRDS(CADC.db.filtered.final, here("data","derived-data","CADC.db.filtered.final.rds"))

# number of countries
length(unique(CADC.db.filtered.final$sov_a3))

