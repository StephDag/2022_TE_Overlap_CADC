# compute climate hazard composite index 
### Stephanie D'Agata, Nov 2023
### updates: Steph D'Agata & Camille Coux Jan 2024
### output: raster stack of climate change variables + composite index

library(here)
source(here("analyses","00_setup.R"))

source(here("analyses","001_Coastal_countries.R"))
source(here("R","NormMinMax.R"))

# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- rnaturalearth::ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)

# check with small countries are present
countries %>%
  filter(name_en == "American Samoa")
 
##########################################
#               Population               #
##########################################

pop.world.nc <- terra::rast(here::here("data","raw-data","World Population SEDAC 5km","gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc","gpw_v4_population_density_adjusted_rev11_2pt5_min.nc"))


# the 4th raster is the count population for 2015: 4	Population Count, v4.11 (2015) (see doc)
pop.world <- pop.world.nc[[4]] 
# plot(pop.world)

# mollweide projection
pop.world.proj <- terra::project(pop.world,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# terra::writeRaster(pop.world.proj, here::here("data", "derived-data", "Spatial rasters", "pop.world.proj.tif", overwrite=TRUE))
# plot(pop.world.proj)
pop.world.proj <- terra::rast( here::here("data", "derived-data", "Spatial rasters", "pop.world.proj.tif"))
##########################################
#               Biodiversity             #
##########################################

specie.grav <- terra::rast(here("data","derived-data","Spatial rasters","sp.count.rast.ter.grav.tif"))
# plot(specie.grav)

# project to mollweide
specie.grav.proj <- project(specie.grav,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# plot(specie.grav.proj)

# resample species grav to match population raster
specie.grav.resample <- resample(specie.grav, pop.world.proj, method="bilinear")
terra::writeRaster(specie.grav.resample, here::here("data", "derived-data", "Spatial rasters", "specie.grav.resample.tif"), overwrite=TRUE)
# plot(specie.grav.resample)
# terra::writeRaster(specie.grav.proj, here::here("data", "derived-data", "Spatial rasters", "specie.grav.resample.tif"))
specie.grav.resample <- terra::rast(here::here("data", "derived-data", "Spatial rasters", "specie.grav.resample.tif"))
##########################################
#      Coastal Population Crop           #
##########################################

# specie.grav.proj %>% res
# specie.grav.resample %>% res
# pop.world.proj %>% res


# crop to species gravity/coastal
# pop.world.proj.coastal <- crop(pop.world.proj, specie.grav.resample,mask=T)
# # plot(pop.world.proj.coastal)
# terra::writeRaster(pop.world.proj.coastal, here::here("data", "derived-data", "Spatial rasters", "pop.world.proj.coastal.tif"))
pop.world.proj.coastal <- terra::rast( here::here("data", "derived-data", "Spatial rasters", "pop.world.proj.coastal.tif"))

##########################################
#      Population change                 #
##########################################

# pop.world.nc.change <- terra::rast(here("data","raw-data","IPCC_Population","CMIP6 - Population density Change persons_km__2 - Near Term (2021-2040) SSP2 (rel. to 1995-2014) - Annual .tiff"))
# # plot(pop.world.nc.change)
# 
# # mollweide
# pop.world.change.proj <- project(pop.world.nc.change,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# # plot(pop.world.change.proj)
# 
# # crop to coastal -> doesn't work
# pop.world.change.proj.coastal <- terra::crop(pop.world.change. ,proj,specie.grav.resample,mask=T)
# 
# # resample species grav to match population
# pop.world.change.proj.resample <- resample(pop.world.change.proj, pop.world.proj, method="bilinear")
# # plot(pop.world.change.proj.resample)
# 
# crs(specie.grav.proj) == crs(pop.world.change.proj.resample)
# 
# # crop to species gravity/coastal 
# pop.world.change.coastal <- crop(pop.world.change.proj.resample, specie.grav.resample,mask=T)
# # plot(pop.world.change.coastal)
# terra::writeRaster(pop.world.change.coastal, here::here("data", "derived-data", "Spatial rasters", "pop.world.change.coastal.tif"))
pop.world.change.coastal <- terra::rast(here::here("data", "derived-data", "Spatial rasters","pop.world.change.coastal.tif"))

##########################################
#                 SLR                    #
##########################################

# mean SLR changes - (2021-2040) vs ( 1995 - 2014)
mean.SLR.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - Sea level rise (SLR) Change meters - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual .tiff"))
#plot(mean.SLR.change)

# mollweide
mean.SLR.change.proj <- project(mean.SLR.change,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
#plot(mean.SLR.change.proj)

# resample mean.SLR.change.proj to match population raster
mean.SLR.change.proj.resample <- resample(mean.SLR.change.proj, pop.world.proj, method="bilinear")
plot(mean.SLR.change.proj.resample)

# crop to species gravity/coastal 
mean.SLR.change.proj.coastal <- crop(mean.SLR.change.proj.resample, specie.grav.resample,mask=T)
# plot(pop.world.change.coastal)

##########################################
#             Poverty                    #
##########################################

# depriv <- terra::rast(here("data","raw-data","povmap-grdi-v1-geotiff","povmap-grdi-v1.tif"))
# plot(depriv$`povmap-grdi-v1`)
# 
# # projected
# depriv.proj <- project(depriv,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") ## takes ~5mins
# plot(depriv.proj)
# 
# # resample species grav to match population raster
# depriv.proj.resample <- resample(depriv.proj, pop.world.proj, method="bilinear")
# plot(depriv.proj.resample)
# 
# # # crop to species gravity/coastal 
# depriv.proj.resample.coastal <- crop(depriv.proj.resample, specie.grav.resample,mask=T)
# plot(depriv.proj.resample.coastal)
# depriv.proj.resample.coastal <- terra::rast("data", "derived")

# let's save this raster so we don't have to do this again:
#terra::writeRaster(depriv.proj.resample.coastal, "data/derived-data/Spatial rasters/depriv.proj.resample.coastal.tif")

# now simply load raster:
depriv.proj.resample.coastal <- terra::rast("data/derived-data/Spatial rasters/depriv.proj.resample.coastal.tif")

###########################################
#.          Marine dependency             #
###########################################

marine.dep <- read.csv(here("data","raw-data","Selig2019","Selig&al2019_Dependance_national_marine.csv"),sep=";")
head(marine.dep)
dim(marine.dep)

#############################################
#.          Enabling conditions             #
#############################################

enabling_CM <- read.csv(here("data","raw-data","Cisneiros Montemayor 2021","Enabling_conditions_equitable_sustainable_Blue_Economy_-_Cisneros-Montemayor_et_al_2021.csv"))

#Scale
#range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))} # Function MinMax 

fsi <- terra::readRDS(here::here("data","raw-data","Global_fsi_ineq.rds"))  %>%
  dplyr::mutate(ineq.drv = gender_inequality_index_2021 %>%
           as.factor() %>% as.numeric() %>%
           BBmisc::normalize())  
         

fsi.sf <- countries  %>%
  left_join(fsi,by=c("name_en" = "country"))

#### project
fsi.sf.proj <- st_transform(fsi.sf,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
crs(fsi.sf.proj) == crs(pop.world.proj)

#### 
fsi.sf.proj.ineq.gender <- rasterize(fsi.sf.proj,pop.world.proj, field="ineq.drv")
plot(fsi.sf.proj.ineq.gender)

fsi.sf.proj.ineq.gender.coastal <- crop(fsi.sf.proj.ineq.gender, specie.grav.resample, mask=T)
plot(fsi.sf.proj.ineq.gender.coastal)

terra::writeRaster(fsi.sf.proj.ineq.gender.coastal, here::here("data", "derived-data", "Spatial rasters", "fsi.sf.proj.ineq.gender.coastal.tif"), overwrite=TRUE)
# add it to the stack too

#############################################
##############################################
#.          Climate vulnerability            #
##############################################

ND_gain <- read.csv(here("data","raw-data","nd_gain_country_index_2023","resources","gain","gain.csv"))
head(ND_gain)
dim(ND_gain)

ND_gain_NA <- ND_gain %>%
  filter(!is.na(X1995))
dim(ND_gain_NA) # 185 countries

ND_gain_NA.sf <- countries  %>%
  left_join(ND_gain_NA,by=c("name_en" = "Name"))

#### project
ND_gain_NA.sf.proj <- st_transform(ND_gain_NA.sf,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
crs(ND_gain_NA.sf.proj) == crs(pop.world.proj)

#### 
ND_gain_NA.sf.proj.2015 <- rasterize(ND_gain_NA.sf.proj,pop.world.proj, field="X2015")
plot(ND_gain_NA.sf.proj.2015)

ND_gain_coastal <- crop(ND_gain_NA.sf.proj.2015, specie.grav.resample, mask=T)
plot(ND_gain_coastal)

terra::writeRaster(ND_gain_coastal, here::here("data", "derived-data", "Spatial rasters", "ND_gain_coastal.tif"), overwrite=TRUE)

##################################################
#.          Relative inequality index            # not sure if supposed to run this bit ? Don't think so...
##################################################
# 
# # relative wealth index
# ineq.list <- list.files(here("data","raw-data","relative-wealth-index-april-2021"),full.names = T)
# imp.func <- function (x) {
#  import(x) %>%
#   dplyr::mutate(ctry=substr(basename(x),1,3))
# }
# ineq.dat <- lapply(ineq.list,imp.func)
# rineq <- do.call(rbind.data.frame,ineq.dat)
# 
# # convert to spatial object
# rineq.vector <- vect(rineq, geom=c("longitude", "latitude"),crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# plot(rineq.vector)
# 
# # transform ## not sure what was the purpose here ?
# # rineq.vector.proj <- terra::rast(rineq.vector,pop.world.proj)
# 
# #crs(rineq.vector)
# # rasterize vector
# rineq.rast <- terra::rasterize(rineq.vector,pop.world.proj, field="rwi")
# plot(rineq.rast) # this doesn't show anything...
# 
# 
# #### project
# rineq.sf <-  st_as_sf(rineq.vector,
#                      coords=c("longitude","latitude"),
#                      crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", agr = NA_agr_, remove = FALSE)
# 
# # rasterize
# rineq.sf.raster <-st_rasterize(rineq.sf %>% dplyr::select(rwi, geometry))
# class(rineq.sf.raster)
# 
# #rineq.sf.raster.2 <- terra::rasterize(rineq.sf,pop.world.proj,filed= "rwi",fun="mean")
# #plot(rineq.sf.raster.2)
# 
# #rineq.sf.raster.proj <- st_transform(rineq.sf.raster,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# #crs(rineq.sf.raster.proj) == crs(pop.world.proj)

######################################################################
#   Create a data.frame for country level information and rasterize  #
######################################################################

# add enabling conditions
coastal.ctr.data <- coastal.ctr %>%
  left_join(fsi,by="country") # inequality and enabling conditions

# add  marine dependence
coastal.ctr.data <- coastal.ctr.data %>%
  left_join(marine.dep,by=c("country" = "Country"))

#### shapefile + rasterize
countries.data.sf <- countries  %>%
  left_join(coastal.ctr.data,by=c("iso_a3" = "alpha.3"))

#### project
countries.data.sf.proj <- st_transform(countries.data.sf,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
crs(countries.data.sf.proj) == crs(pop.world.proj)

countries.data.sf.proj.vt <- vect(countries.data.sf.proj)
plot(countries.data.sf.proj.vt)

####### nutritional
#   # rasterize polygon
# marine.dep.nutri <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Nutritional.dependence")
# 
# # crop to species gravity/coastal  - it works to crop a spatraster with coastal raster
# marine.dep.nutri.coastal <- crop(marine.dep.nutri, specie.grav.resample,mask=T)
# plot(marine.dep.nutri.coastal)
# 
# terra::writeRaster(marine.dep.nutri.coastal, here::here("data", "derived-data", "Spatial rasters", "marine.dep.nutri.coastal.tif"))
marine.dep.nutri.coastal <- terra::rast( here::here("data", "derived-data", "Spatial rasters", "marine.dep.nutri.coastal.tif"))

####### economic
# marine.dep.econ <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Economic.dependence")
# # crop to species gravity/coastal 
# marine.dep.econ.coastal <- crop(marine.dep.econ, specie.grav.resample,mask=T)
# plot(marine.dep.econ.coastal)
# 
# terra::writeRaster(marine.dep.econ.coastal, here::here("data", "derived-data", "Spatial rasters", "marine.dep.econ.coastal.tif"))
marine.dep.econ.coastal <- terra::rast( here::here("data", "derived-data", "Spatial rasters", "marine.dep.econ.coastal.tif"))

# terra::writeRaster(marine.dep.econ.coastal, here::here("data", "derived-data", "Spatial rasters", "marine.dep.econ.coastal.tif"), overwrite=TRUE)


##### merit_lecz raster to add to stack
# add final raster Steph mentionned
# merit
# merit <- terra::rast("data/derived-data/merit_leczs.tif")

# merit.proj <- terra::project(merit , "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# terra::writeRaster(merit.proj, here::here("data", "derived-data", "Spatial rasters", "merit.proj.tif"), overwrite=TRUE)
# merit.proj <- terra::rast(here::here("data", "derived-data", "Spatial rasters", "merit.proj.tif"))
# 
# 
# merit.resample <- resample(merit.proj, pop.world.proj, method="bilinear")
# terra::writeRaster(merit.resample, here::here("data", "derived-data", "Spatial rasters", "merit.resample.tif"), overwrite=TRUE)
# 
# 
# merit.coastal <-  crop(merit.resample, specie.grav.resample,mask=T)
# terra::writeRaster(merit.coastal, here::here("data", "derived-data", "Spatial rasters", "merit.coastal.tif"), overwrite=TRUE)

merit.coastal <- terra::rast(here::here("data", "derived-data", "Spatial rasters", "merit.coastal.tif"))




pop.world.nc.change <- terra::rast(here("data","raw-data","IPCC_Population","CMIP6 - Population density Change persons_km__2 - Near Term (2021-2040) SSP2 (rel. to 1995-2014) - Annual .tiff"))
# plot(pop.world.nc.change)

# mollweide
pop.world.change.proj <- project(pop.world.nc.change,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# plot(pop.world.change.proj)

# crop to coastal -> doesn't work
pop.world.change.proj.coastal <- terra::crop(pop.world.change.proj, specie.grav.resample, mask=T)

# resample species grav to match population
pop.world.change.proj.resample <- resample(pop.world.change.proj, pop.world.proj, method="bilinear")
# plot(pop.world.change.proj.resample)

crs(specie.grav.proj) == crs(pop.world.change.proj.resample)

# crop to species gravity/coastal 
pop.world.change.coastal <- crop(pop.world.change.proj.resample, specie.grav.resample,mask=T)
# plot(pop.world.change.coastal)
terra::writeRaster(pop.world.change.coastal, here::here("data", "derived-data", "Spatial rasters", "pop.world.change.coastal.tif"), overwrite=TRUE)
# add it to the stack too

#############################################
#        Stack on all raster                #
#############################################

# create stack raster
risk.stack <- list(pop.world.proj.coastal, # human population in 2015
                pop.world.change.coastal, # population change compare to 1994 - 2010
                specie.grav.resample, # species gravity
                depriv.proj.resample.coastal, #deprivation poverty index 
               # mean.temp.change.resample, # climate terrestrial
                marine.dep.nutri.coastal, # nutrional dependency to marine resources
                marine.dep.econ.coastal,#, # economic dependency to marine resources
                ND_gain_coastal, # national climate NDgain index
                #mean.SLR.change.proj.coastal, # SLR change
                fsi.sf.proj.ineq.gender.coastal,  # gender inequality
                merit.coastal) # merit.lecz = nb of ppl per pixel living near coast at < 10m elevation

#   # rename variables
tidyterra::rename.Spat(x) <- c("UN WPP-Adjusted Population Density, v4.11 (2000, 2005, 2010, 2015, 2020): 2.5 arc-minutes_raster=4" = "pop.world.proj.coastal", 
                       "pop.world.change.coastal", 
                       "specie.grav.resample",
                       "depriv.proj.resample.coastal",
                       "marine.dep.nutri.coastal", 
                       "marine.dep.econ.coastal",
                       "ND_gain_coastal", 
                       "fsi.sf.proj.ineq.gender.coastal",  
                       "merit.coastal")

lapply(risk.stack, function(x){
  terra::writeRaster(here::here("data", "deried-data", "Spatial rasters", paste(x)))
})



# save raster stack into netcdf file
terra::writeCDF(risk.stack, here::here("data","derived-data", "Spatial rasters", "risk.stack.nc"))
terra::writeRaster(risk.stack, here::here("data","derived-data", "Spatial rasters", "risk.stack.rds"))
saveRDS(risk.stack, here::here("data","derived-data", "Spatial rasters", "risk.stack.rds"))

# sample
#sr <- terra::spatSample(risk.stack, 1000,na.rm=T,as.points=T,values=T,xy=T,method="random") # sample 5000000 random grid cells
#dim(sr) #332583      6
# #saveRDS(sr,here("data","derived-data","risk.stack.sr.rds"))
risk.stack <- readRDS(here::here("data","derived-data","Spatial rasters","risk.stack.rds"))

# recreate scale raster stack
 risk.stack$mean.count.grav.V2.log.sc <- scale(risk.stack$mean.count.grav.V2.log); gc()
 risk.stack$povmap.grdi.v1.sc <- rescale01(risk.stack$povmap.grdi.v1); gc()
 risk.stack$Nutritional.dependence.sc <- rescale01(risk.stack$Nutritional.dependence); gc()
 risk.stack$Economic.dependence.sc <- rescale01(risk.stack$Economic.dependence); gc()
 risk.stack$ND_gain_NA.sf.proj.2015.sc <- rescale01(risk.stack$ND_gain_NA.sf.proj.2015); gc()
 #risk.stack$mean.SLR.change.proj.coastal.sc <- rescale01(risk.stack$SLR.change); gc()
 risk.stack$gender.ineq.sc <- rescale01(risk.stack$gender.ineq); gc()

 saveRDS(file="data/derived-data/Spatial rasters/risk.stack.rds",object=risk.stack) ## too big to be saved ?

 # free memory
 rm(depriv,depriv.proj,depriv.proj.resample,depriv.proj.resample.coastal,pop.world,
    pop.world.change.proj,pop.world.change.proj.coastal,pop.world.change.proj.resample,
    pop.world.change.coastal,pop.world.nc,pop.world.nc.change,pop.world.proj,
    mean.count.grav.V2.log.sc,mean.SLR.change.proj.coastal.sc)
 
# create a stack raster of normalized raster
 risk.stack.sc <- c(risk.stack$mean.count.grav.V2.log.sc,
                    risk.stack$povmap.grdi.v1.sc,
                    risk.stack$Nutritional.dependence.sc,
                    risk.stack$Economic.dependence.sc,
                    risk.stack$ND_gain_NA.sf.proj.2015.sc,
                    #risk.stack$mean.SLR.change.proj.coastal.sc,
                    risk.stack$gender.ineq.sc); gc()
 
 saveRDS(risk.stack.sc,"data/derived-data/Spatial rasters/risk.stack.sc.rds")
 
 #NAflag(risk.stack.sc$mean.count.grav.V2.log.sc) <- NA # bug
 r_stack_cor_5 <- focalPairs(risk.stack.sc,w=25, "pearson", na.rm=TRUE)
 r_stack_cor_5
 # save raster 
 saveRDS(risk.stack.sc,"data/derived-data/Spatial rasters/risk.stack.sc.rds")
 
# # correlation of normalized data (à faire après)
 #cor.ineq.sc <- terra::layerCor(risk.stack.sc, fun="pearson",use="masked.complete")

# # species gravity
# species.grav.poverty <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$povmap.grdi.v1.sc), # [MW]
#      use = "na.or.complete")
# species.grav.nutr.dep <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$Nutritional.dependence.sc), # [MW]
#                             use = "na.or.complete"); gc()
# species.grav.econ.dep <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$Economic.dependence.sc), # [MW]
#                              use = "na.or.complete"); gc()
# species.grav.climate <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$ND_gain_NA.sf.proj.2015.sc), # [MW]
#                              use = "na.or.complete"); gc()
# species.grav.slr <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
#                             use = "na.or.complete"); gc()
# species.grav.gender <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$gender.ineq.sc), # [MW]
#                             use = "na.or.complete"); gc()
# #poverty
# poverty.nutr <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$Nutritional.dependence.sc), # [MW]
#                             use = "na.or.complete")
# poverty.econ <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$Economic.dependence.sc), # [MW]
#                     use = "na.or.complete")
# poverty.climate <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$ND_gain_NA.sf.proj.2015.sc), # [MW]
#                     use = "na.or.complete")
# poverty.grav.slr <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
#                         use = "na.or.complete"); gc()
# poverty.gender <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$gender.ineq.sc), # [MW]
#                        use = "na.or.complete")
# #nutritional dependency
# nutr.econ <- cor(values(risk.stack$Nutritional.dependence.sc),values(risk.stack$Economic.dependence.sc), # [MW]
#                     use = "na.or.complete")
# nutr.climate <- cor(values(risk.stack$Nutritional.dependence.sc),values(risk.stack$ND_gain_NA.sf.proj.2015.sc), # [MW]
#                        use = "na.or.complete")
# nutr.slr <- cor(values(risk.stack$Nutritional.dependence.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
#                     use = "na.or.complete")
# nutr.gender <- cor(values(risk.stack$Nutritional.dependence.sc),values(risk.stack$gender.ineq.sc), # [MW]
#                       use = "na.or.complete")
# #econ dependency
# econ.climate <- cor(values(risk.stack$Economic.dependence.sc),values(risk.stack$ND_gain_NA.sf.proj.2015.sc), # [MW]
#                     use = "na.or.complete")
# econ.slr <- cor(values(risk.stack$Economic.dependence.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
#                 use = "na.or.complete")
# econ.gender <- cor(values(risk.stack$Economic.dependence.sc),values(risk.stack$gender.ineq.sc), # [MW]
#                    use = "na.or.complete")
# 
# # climate
# climate.slr <- cor(values(risk.stack$ND_gain_NA.sf.proj.2015.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
#                     use = "na.or.complete")
# climate.gender <- cor(values(risk.stack$ND_gain_NA.sf.proj.2015.sc),values(risk.stack$gender.ineq.sc), # [MW]
#                    use = "na.or.complete")
# 
# # slr
# slr.gender <- cor(values(risk.stack$mean.SLR.change.proj.coastal.sc),values(risk.stack$gender.ineq.sc), # [MW]
#                       use = "na.or.complete")
# 
# # add country information
# world.2 <- countries %>%
#   st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# 
#   # sr to sf
# risk.stack.sp.sf = st_as_sf(risk.stack)
# crs(risk.stack.sp.sf) == crs(world.2)

# intersecting points with country
# Convert the SpatRaster to a SpatialPointsDataFrame
#risk.stack_df <- as.data.frame(risk.stack)     
#
################# steph pense que c'est ça qu'il faut faire. st_intersection une fois qu'on a mis en spatial data frame. Puis merge avec shapefile du monde world 2. But = récupérer l'info de quel pxel dans quel pays, pour pouvoir calculer l'indice composite, 1 valeur composite par pixel, puis une distribution de valeurs par pays.
##################################################################################
##################################################################################
##################################################################################
#
#risk.stack.sp.sf.ctry <- intersect(world.2,risk.stack.sc) #
#risk.stack.sp.sf.ctry <- st_intersection(risk.stack.sc, world.2) #
#dim(risk.stack.sp.sf.ctry)  # 332583      6
#head(risk.stack.sp.sf.ctry)

# Transform raster grid to polygons grid
z <- terra::as.polygons(risk.stack.sc)
# Intersect with species
u <- terra::intersect(z,world.2)


# number of countries sampled
unique(risk.stack.sp.sf.ctry$name_en) %>% length()

# create composite risk score ##### INDICE COMPOSITE
risk.mat <- as.data.frame(sr.sp.sf.ctry) %>%
  select(ND_gain_NA.sf.proj.2015.sc,mean.count.grav.V2.log.sc,povmap.grdi.v1.sc,
         Nutritional.dependence.sc,Economic.dependence.sc,gender.ineq)
sr.sp.sf.ctry$risk.mat.score.sum <- apply(risk.mat, 1, sum)

# geometric mean
risk.mat.geom  <-  as.data.frame(sr.sp.sf.ctry) %>%
  mutate(risk.mat.score.geom = sqrt(mean.count.grav.V2.log.sc*povmap.grdi.v1.sc*Nutritional.dependence.sc*
                                      Economic.dependence.sc*ND_gain_NA.sf.proj.2015.sc*gender.ineq))
sr.sp.sf.ctry$risk.mat.score.geom <- risk.mat.geom$risk.mat.score.geom
sr.sp.sf.ctry

# saveRDS(sr.sp.sf.ctry,here("data","derived-data","risk.stack.sr.rds"))

# #################################
# # for supplemental, spatial PCA #
# #################################
# sr <- readRDS(here("data","derived-data","risk.stack.sr.rds"))
# 
#   # for spatial PCA
# # randomly select 5000 rows
# sample.2000 <- sample(seq(1,dim(sr.sp)[1],1),2000)
# sr.sp.2000 <- sr.sp[sample.2000,c(5:10)]
# 
# sr.sp.2000.sf = st_as_sf(sr.sp.2000)
# crs(sr.sp.2000.sf) == crs(world.2)
# 
# # intersecting points with country
# sr.sp.2000.sf.ctry <- st_intersection(sr.sp.2000.sf, world.2)
# dim(sr.sp.2000.sf.ctry) # 1986 175
# 
# sr.sp.2000.sf.ctry.pca <- sr.sp.2000.sf.ctry[,c(1:6,53,118)]
# head(sr.sp.2000.sf.ctry.pca)
# dim(sr.sp.2000.sf.ctry.pca)
# 
# # row to keep in original
# row2keep <- which(rownames(sr.sp.2000@data) %in% rownames(sr.sp.2000.sf.ctry.pca))
# 
# # new spatial points df
# sr.sp.2000.ctry <- sr.sp.2000[row2keep,]
# sr.sp.2000.ctry <- cbind(sr.sp.2000.ctry,sr.sp.2000.sf.ctry.pca[,c("iso_a3","name_en")] %>% st_drop_geometry())
# 
# # pca 
# 
# # bandwith
# bw.gw.pca <- GWmodel::bw.gwpca(sr.sp.2000.ctry[,1:6], 
#                       vars = names(sr.sp.2000.ctry[,1:7]),
#                       k = 3,
#                       robust = FALSE,
#                       adaptive = TRUE)
# 
# # geograph. weighted pcA
# gw.pca<- gwpca(sr.sp.2000.ctry[,1:6], 
#                vars = names(sr.sp.2000.ctry[,1:6]), 
#                bw=bw.gw.pca,
#                k = 3, 
#                robust = FALSE, 
#                adaptive = TRUE,
#                scores=T)
# 
# 
# # plot of the spatial PCA
# fviz_eig(gw.pca$pca, addlabels = TRUE, ylim = c(0, 50))
# 
# fviz_pca_var(gw.pca$pca,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 
# # Contributions of variables to PC1
# fviz_contrib(gw.pca$pca, choice = "var", axes = 1, top = 10,ylim=c(0,50))
# # Contributions of variables to PC2
# fviz_contrib(gw.pca$pca, choice = "var", axes = 2, top = 10,ylim=c(0,50))
# # Contributions of variables to PC3
# fviz_contrib(gw.pca$pca, choice = "var", axes = 3, top = 10,ylim=c(0,50))
# 
# # save normalize score for each PC
# #sr.sp.2000.ctry$PC1 <- normalize(gw.pca$pca$scores[,1])
# #sr.sp.2000.ctry$PC2 <- normalize(gw.pca$pca$scores[,2])
# #sr.sp.2000.ctry$PC3 <- normalize(gw.pca$pca$scores[,3])
# 
# # by country for each axis
# # Basic box plot
# p.PC1 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC1), y=PC1)) + 
#   geom_boxplot() +
#   coord_flip() +
#   xlab("Countries") +
#   theme_bw()
# p.PC1
# ggsave(here("figures","PC1_distrib_ctr.png"),p.PC1,height=10,width=10)
# p.PC2 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC2), y=PC2)) + 
#   geom_boxplot() +
#   coord_flip() +
#   xlab("Countries") +
#   theme_bw()
# p.PC2
# ggsave(here("figures","PC2_distrib_ctr.png"),p.PC2,height=10,width=10)
# p.PC3 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC3), y=PC3)) + 
#   geom_boxplot() +
#   coord_flip() +
#   xlab("Countries") +
#   theme_bw()
# p.PC3
# ggsave(here("figures","PC3_distrib_ctr.png"),p.PC3,height=10,width=10)
# 
# # 
# p.risk.score <- ggplot(as.data.frame(sr.sp.sf.ctry), aes(x=reorder(name_en,-risk.mat.score.sum), y=risk.mat.score.sum)) + 
#   geom_boxplot() +
#   coord_flip() +
#   xlab("Countries") +
#   ylab("Contextual Inequity Scores") +
#   theme_bw()
# p.risk.score
# ggsave(here("figures","Contextual_Ineq_score_distrib_ctr.png"),p.risk.score,height=12,width=12)
# 
# p.PC.score <- ggarrange(p.PC1,p.PC2,p.PC3,p.risk.score,ncol=2,nrow=2,labels=c("A","B","C","D"))
# ggsave(here("figures","double_exposure_ctry_distribution_2000samples.pdf"),p.PC.score,height=18,width=18)
# 
# ## add country
# fviz_pca_biplot(gw.pca$pca, repel = TRUE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969"  # Individuals color
# )
# 
# ###################
# fviz_pca_ind(gw.pca$pca,
#              geom.ind = "point", # show points only (nbut not "text")
#              col.ind = iris$Species, # color by groups
#              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#              addEllipses = TRUE, # Concentration ellipses
#              legend.title = "Groups"
# )
# 
# fviz_pca_ind(gw.pca$pca,
#              geom.ind = "point", # show points only (nbut not "text")
#              col.ind = iris$Species, # color by groups
#              palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#              addEllipses = TRUE,ellipse.type = "convex", # Concentration ellipses
#              legend.title = "Groups"
# )
# 
# fviz_pca_biplot(gw.pca$pca, 
#                 # Individuals
#                 geom.ind = "point",
#                 fill.ind = iris$Species, col.ind = "black",
#                 pointshape = 21, pointsize = 2,
#                 palette = "jco",
#                 addEllipses = TRUE,
#                 # Variables
#                 alpha.var ="contrib", col.var = "contrib",
#                 gradient.cols = "RdYlBu",
#                 
#                 legend.title = list(fill = "Species", color = "Contrib",
#                                     alpha = "Contrib")
# )
# 
# # PC1
# plot(sr.sp.5000$terrest.temp.change.sc,sr.sp.5000$risk.score.PC1)
# plot(sr.sp.5000$povmap.grdi.v1.sc,sr.sp.5000$risk.score.PC1)
# plot(sr.sp.5000$Nutritional.dependence.sc,sr.sp.5000$risk.score.PC1)
# plot(sr.sp.5000$Economic.dependence.sc,sr.sp.5000$risk.score.PC1)
# plot(sr.sp.5000$population.sc,sr.sp.5000$risk.score.PC1)
# plot(sr.sp.5000$mean.count.grav.V2.log.sc,sr.sp.5000$risk.score.PC1)
# 
# # PC2
# plot(sr.sp.5000$terrest.temp.change.sc,sr.sp.5000$risk.score.PC2)
# plot(sr.sp.5000$povmap.grdi.v1.sc,sr.sp.5000$risk.score.PC2)
# plot(sr.sp.5000$Nutritional.dependence.sc,sr.sp.5000$risk.score.PC2)
# plot(sr.sp.5000$Economic.dependence.sc,sr.sp.5000$risk.score.PC2)
# plot(sr.sp.5000$population.sc,sr.sp.5000$risk.score.PC2)
# plot(sr.sp.5000$mean.count.grav.V2.log.sc,sr.sp.5000$risk.score.PC2)
# 
# # function for calculation pproportion of variance 
# prop.var <- function(gwpca.obj, n.components) {
#   return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
# }
# var.gwpca <- prop.var(gw.pca, 3)
# mf$var.gwpca <- var.gwpca
# 
# 
# 
# # function for calculation pproportion of variance 
# prop.var <- function(gwpca.obj, n.components) {
#   return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
# }
# 
# var.gwpca <- prop.var(bw.gw.pca, 3)
# mf$var.gwpca <- var.gwpca
