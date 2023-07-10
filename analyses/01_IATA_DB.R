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
ctr <- ctr.iati$code # for all countries - since impossible to catch country information when 1 project in several countries

# 3. Retrieve CADC database from d-portal.org for coastal countries only
rm(all)
all <- data.frame()

# loop a
for (i in 1:length(ctr)) {
  
  print(paste("i=",i))
  
  # query for i coastal country
  temp.query <- paste0("recipient_country_code:(",ctr[i],")") # query for each country 

  # retrieve full dataset for i country
  temp <- try(get_records(temp.query, collection = "activity") ) # retrieving the full database without filters, skip errors when country is not in the database)
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
  gc()
  
}
saveRDS(all, here("data","raw-data","CADC.rds"))

