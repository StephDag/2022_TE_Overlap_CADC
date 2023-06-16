# script to retrieve the IATI database (d-portal.org) for CADC initiatives
# Description
# IATI (international Aid Transparency International) database (https://d-portal.org/ctrack.html#view=search) 
# The IATI is updated every 24 hours. 

# 1. save API TOKEN
usethis::edit_r_environ()
Sys.getenv("IATI_KEY")

# 2. load list of coastal countries to avoid retrieving projects from non-coastal countries
source(here::here("analyses","001_Coastal_countries.R"))
  # Iso code for coastal countries
coast.ctr <- coastal.ctr$iso2 # remove empty - 192 countries

# 3. Retrieve CADC database from d-portal.org for coastal countries only
library("riati")
rm(all)
all <- data.frame()
# loop a
for (i in 1:length(coast.ctr)) {
  
  print(paste("i=",i))
  
  # query for i coastal country
  temp.query <- paste0("recipient_country_code:(",coast.ctr[i],")") # query for each country 
  
  # retrieve full dataset for i country
  temp <- try( get_records(temp.query, collection = "activity") ) # retrieving the full database without filters, skip errors when country is not in the database)
  print(dim(temp))
  # check dimension of the temp data.frame. If =0, skip the iteration and go to the next
  if (dim(temp)[1] == 0){
    next
  }
  
  # concatenate 
  all <- bind_rows(all,temp) # data.frame for each countries do not have the same number of rows
  
  # return focal object
  #return(all.a)
  # clean objects
  rm(temp.query)
  rm(temp)
  
}
saveRDS(all, here("data","raw-data","CADC.rds"))

