### script to build full MPA database using shapefiles and points - National, marine, marine & terrestrial, designated
### Stephanie D'Agata, sept 2023
### output: shapefile of MPA- national

# load MPA data
MPA.0 <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_0","WDPA_WDOECM_Sep2023_Public_marine_shp-polygons.shp"))
MPA.1 <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_1","WDPA_WDOECM_Sep2023_Public_marine_shp-polygons.shp"))
MPA.2 <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_2","WDPA_WDOECM_Sep2023_Public_marine_shp-polygons.shp"))

# load points MPA
MPA.0.pts <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_0","WDPA_WDOECM_Sep2023_Public_marine_shp-points.shp"))
MPA.1.pts <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_1","WDPA_WDOECM_Sep2023_Public_marine_shp-points.shp"))
MPA.2.pts <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_2","WDPA_WDOECM_Sep2023_Public_marine_shp-points.shp"))

# filter for marine, and marine + terrestrial - and nationally 
#DESIG_TYPE == "National" #
#STATUS == "Designated" (by law) Established"  Proposed, Inscribed, Adopted
#MARINE %in% c(1,2)  #  0 (predominantly or entirely terrestrial),  1 (Coastal: marine and terrestrial), and 2 (predominantly or entirely marine)

# filter each, and merge into one shapefile
MPA.0.marine <- MPA.0 %>%
  filter(DESIG_TYPE == "National" & STATUS == "Designated" & MARINE %in% c(1,2)) # & VERIF == "State Verified")
MPA.1.marine <- MPA.1 %>%
  filter(DESIG_TYPE == "National" & STATUS == "Designated" & MARINE %in% c(1,2)) # & VERIF == "State Verified")
MPA.2.marine <- MPA.2 %>%
  filter(DESIG_TYPE == "National" & STATUS == "Designated" & MARINE %in% c(1,2)) # & VERIF == "State Verified")

# merge all shapefile
crs(MPA.0.marine) == crs(MPA.1.marine)
rm(MPA.full)
MPA.full <- rbind(MPA.0.marine, MPA.1.marine)
MPA.full <- rbind(MPA.full,MPA.2.marine)
dim(MPA.full)

# filter each and merge into one shapefile
MPA.0.marine.pts <- MPA.0.pts %>%
  filter(DESIG_TYPE == "National" & STATUS == "Designated" & MARINE %in% c(1,2)) # & VERIF == "State Verified")
MPA.1.marine.pts <- MPA.1.pts %>%
  filter(DESIG_TYPE == "National" & STATUS == "Designated" & MARINE %in% c(1,2)) # & VERIF == "State Verified")
MPA.2.marine.pts <- MPA.2.pts %>%
  filter(DESIG_TYPE == "National" & STATUS == "Designated" & MARINE %in% c(1,2)) # & VERIF == "State Verified")

# merge all shapefile pts
crs(MPA.0.marine) == crs(MPA.1.marine)
rm(MPA.full.pts)
MPA.full.pts <- rbind(MPA.0.marine.pts, MPA.1.marine.pts)
MPA.full.pts <- rbind(MPA.full.pts,MPA.2.marine.pts)
dim(MPA.full.pts)

# compute radius of buffer for each points MPA
MPA.full.pts <- MPA.full.pts %>%
  mutate(r = sqrt(REP_M_AREA/pi) %>% round(2))

# create marine MPA points buffer
buffer_sf_MPA <- st_buffer(MPA.full.pts$geometry,MPA.full.pts$r)

# join points and buffer
MPA.full.pts$geometry <- NULL
MPA.full.pts.buffer <- cbind(MPA.full.pts,buffer_sf_MPA)
MPA.full.pts.buffer$r <- NULL
head(MPA.full.pts.buffer)

# rbind shape and point shapefile
MPA.full.shp.pts <- bind_rows(MPA.full, MPA.full.pts.buffer)  # Bind as rows
dim(MPA.full.shp.pts) 
head(MPA.full.shp.pts)

# retrive crs from biodiversity raster
imp_count  <- rast(here("data","raw-data","Ohara2021_Science","impact_all_2013.tif"))

# MPA Mollweid
MPA.full.proj <- MPA.full.shp.pts %>%
  st_transform(crs(imp_count))

# transform to vector
#MPA.full.proj.vect <- vect(MPA.full.proj)

# save
# st_write(MPA.full.proj, here("data","derived-data","MPA.full.mollweide.shp")) too long

  # save as rds
saveRDS(MPA.full.proj,here("data","derived-data","MPA.full.mollweide.rds"))
