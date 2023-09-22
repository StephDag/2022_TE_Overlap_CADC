# code to retrieve and correct GPS coordinates from the full database - in the descriptive narrative
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R")) # countries and cities
source(here::here("R","find_locations.R"))
#source("analyses/02_Clean_GPS.R")
# load raw data
#all <- readRDS(here("data","derived-data","CADC.db.final.rds"))
all <- readRDS("/media/seagate/sdagata/CADC.db.final.rds")

all.loc <- all %>% 
  select(iso_a2,iati_identifier) %>%
  distinct() %>%
 group_by(iso_a2) %>%
  summarize(n =n()) %>% 
  as.data.frame()

all.ID <- all %>% 
  filter(iso_a2 == "ID")
  summarize(n =n()) %>% 
  as.data.frame()

# cities  - loaded from source file
head(cities)

# cities with same names as capitals
ctry.cap <- c("Andorra","Djibouti","Guatemala","Kuwait","Luxembourg","Mexico","Monaco","Panama","San Marino","Singapore","Vatican City")

# list of countries
#summary(as.factor(all$iso_a2))
ctry.list <- unique(all$iso_a2) %>% sort() # 179 countries

## start loop for countries
start_time <- Sys.time() 

# new data.frame to store cities/locations for each project that were cited in "descriptive_narrative"
loc.projects.df.gps.final <- data.frame()

# number of cores
n.cores <- 40


for (k in 71:length(ctry.list)){ # 70 is still with issues
#for (k in 1:2){

    print(paste("Country =",ctry.list[k],"k=",k))
    print("FIRST LOOP")
    # select projects for k country
    temp.country <- all %>% filter(iso_a2 == ctry.list[k])
    
    # locations for the country k
      # vector of cities, cleaned with parenthesis error, etc.
    temp.cities <- cities %>% filter(country_code == ctry.list[k])
    
    # combine the name, ascii and alternate names in one vector, and remove strings with parenthesis
      # name
    temp.name <- temp.cities$name
        if (length(grep("\\)",temp.name,fixed = TRUE)) == 0)  {
          temp.name = temp.name
            } else {
              temp.name = temp.name[-grep("\\)",temp.name,fixed = TRUE)]
            }

      # ascii
    temp.ascii <- temp.cities$asciiname
          if (length(grep("\\)",temp.ascii,fixed = TRUE)) == 0)  {
            temp.ascii = temp.ascii
              } else {
                temp.ascii = temp.ascii[-grep("\\)",temp.ascii,fixed = TRUE)]
              }
      # alternative names
      temp.alt <- temp.cities$alternatenames
      temp.alt <- strsplit(temp.alt,",")  # several locations by row: split it into a list
      temp.alt <- unlist(temp.alt) # unlist --> a vector
          if (length(grep("\\)",temp.alt,fixed = TRUE)) == 0)  {
            temp.alt = temp.alt
              } else {
                temp.alt = temp.alt[-grep("\\)",temp.alt,fixed = TRUE)]
              }
    # combined vector
    temp.cities.comb <- c(temp.name,temp.ascii,temp.alt)
    
      if (length(grep("\\(",temp.cities.comb,fixed = TRUE)) == 0)  {
        temp.cities.comb = temp.cities.comb
      } else {
        temp.cities.comb = temp.cities.comb[-grep("\\(",temp.cities.comb,fixed = TRUE)]
      }
    #temp.cities.comb <- gsub("\\(", "", temp.cities.comb)
    temp.cities.comb <- gsub("\\(", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\)", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\[", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\]", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\t", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\.", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\?", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\!", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\\\", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\+", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\:", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\*", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\:", "", as.character(temp.cities.comb))
    temp.cities.comb <- gsub("\\t", "", as.character(temp.cities.comb))
    
    # remove remnant weird patterns
    pattern <- "/|:|\\?|<|>|\\|\\\\|\\*"
    temp.cities.comb <- temp.cities.comb[-which(grepl(pattern, temp.cities.comb))]

      # locations = combined all cities
      locations <-temp.cities.comb %>% unique()

    # list of projects with geographical locations
    loc.projects=list()

    loc.projects <- mclapply(X=temp.country[,"description_narrative"],FUN=find_locations, locations = locations,mc.cores=n.cores)
    names(loc.projects) <- temp.country$iati_identifier_bis[1:dim(temp.country)[1]]

    ## dataframe with iati id, name of cities
    loc.projects <- lapply(loc.projects, function(x) if(identical(x, character(0))) NA_character_ else x) # change character(0) to NAs

    # locations of project as dataframe (wide)
    loc.projects.df <- stack(loc.projects)
    # reorder data.frame
    loc.projects.df <- loc.projects.df %>% 
      relocate(ind)
    
    if (dim(loc.projects.df)[1] == 0) next
    
    # rename columns
    names(loc.projects.df) <- c("iati_identifier","locations")

    # remove NAs in the loc.projects.df = when no cities in descriptive narrative
    loc.projects.df <- loc.projects.df %>% 
      filter(!is.na(locations)) %>% 
      distinct() %>%
      filter(locations != ctry.list[k])# %>% # remove locations when it matches the country iso_a2
    
    # correct for Antigua and Barbuda
    loc.projects.df$locations <- dplyr::case_when (
      loc.projects.df$locations %in% c("Antigua","Barbuda") ~"Antigua and Barbuda",
      TRUE ~ as.character(loc.projects.df$locations)
    )
    # remove locations that are country in english 
    row_index <- which(loc.projects.df$locations %in% ctr$country == T & loc.projects.df$locations %in% ctry.cap == F)
    
      # next loop if row_index= 0
    if (length(row_index) == 0) next 
    
    loc.projects.df <- loc.projects.df[-row_index,]

    # lat, longitude
    latlong <- data.frame(locations=factor(),latitude=numeric(),longitude=numeric())
    loc.unique <- unique(loc.projects.df$locations) 

      # next loop if no location
    if (length(loc.unique) == 0) next 
    
    # seconde loop to retrieve the gps coordinates in the country cities list - avoid retrieving coordinates of same cities in another country
    print("SECOND LOOP")
    for (j in 1:length(loc.unique)[1]){
    print(paste("k=",k,"j=",j))
    
      # latitude
    temp.lat <-   temp.cities %>%
    mutate(combined = str_c(name, asciiname, alternatenames, sep = ' ')) %>%
    filter(str_detect(combined, paste0("\\b",loc.unique[j],"\\b"))) %>%
    select(latitude) %>% as.matrix()
    
      # longitude
    temp.long <- temp.cities %>%
    mutate(combined = str_c(name, asciiname, alternatenames, sep = ' ')) %>%
    filter(str_detect(combined, paste0("\\b",loc.unique[j],"\\b"))) %>%
    select(longitude) %>% as.matrix()
    # store in a list
    df.temp = data.frame(locations=loc.unique[j],latitude=temp.lat[1],longitude=temp.long[1])
    latlong <- rbind(latlong,df.temp)
    }
    
    loc.projects.df.gps <- left_join(loc.projects.df,latlong,by= "locations")
    loc.projects.df.gps.final <- rbind(loc.projects.df.gps.final,loc.projects.df.gps)
    #saveRDS(loc.projects.df.gps.final,here("data","derived-data","loc.gps.projects.rds"))
    #return(loc.projects.df.gps.final)
    gc()
}

saveRDS(loc.projects.df.gps.final,here("data","derived-data","loc.gps.projects_b.rds"))



