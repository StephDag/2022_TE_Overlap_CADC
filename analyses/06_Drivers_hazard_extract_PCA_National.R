# generate scaled raster stack for contextual equity to compute composite score
### Stephanie D'Agata, Nov 2023
### last updates: Steph D'Agata & Camille Coux Feb 2024
### output: raster stack of climate change variables scaled 0 - 1

library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)
source(here("R","NormMinMax.R"))

# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)

# check with small countries are present
countries.shp.coastal %>%
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
# plot(pop.world.proj)

#df.pop <- terra::as.data.frame(pop.world.proj,xy=T)

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
# plot(specie.grav.resample)

##########################################
#      Coastal Population Crop           #
##########################################

# crop to species gravity/coastal
pop.world.proj.coastal <- crop(pop.world.proj, specie.grav.resample,mask=T)
# plot(pop.world.proj.coastal)
# 
# specie.grav.proj %>% res
# specie.grav.resample %>% res
# pop.world.proj %>% res

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
# # resample species grav to match population
# pop.world.change.proj.resample <- resample(pop.world.change.proj, pop.world.proj, method="bilinear")
# # plot(pop.world.change.proj.resample)
# 
# crs(specie.grav.proj) == crs(pop.world.change.proj.resample)
# 
# # crop to species gravity/coastal 
# pop.world.change.proj.resample.coastal <- crop(pop.world.change.proj.resample, specie.grav.resample,mask=T)
# # plot(pop.world.change.proj.resample.coastal)


##########################################
#                 SLR                    #
##########################################

# # mean SLR changes - (2021-2040) vs ( 1995 - 2014)
# mean.SLR.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - Sea level rise (SLR) Change meters - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual .tiff"))
# plot(mean.SLR.change)
# 
# # mollweide
# mean.SLR.change.proj <- project(mean.SLR.change,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# #plot(mean.SLR.change.proj)
# 
# # resample mean.SLR.change.proj to match population raster
# mean.SLR.change.proj.resample <- resample(mean.SLR.change.proj, pop.world.proj, method="bilinear")
# plot(mean.SLR.change.proj.resample)
# 
# # crop to species gravity/coastal
# mean.SLR.change.proj.coastal <- crop(mean.SLR.change.proj.resample, specie.grav.resample,mask=T)
# plot(mean.SLR.change.proj.coastal)

  # country level - from ND Gain
SLR.score <- read.csv(here("data","raw-data","nd_gain_country_index_2023","resources","indicators","id_infr_02","score.csv"),
                           header=T,sep=",")

SLR.score_NA <- SLR.score %>%
  filter(!is.na(X2015))
dim(SLR.score_NA) # 151 countries
head(SLR.score_NA) # 151 countries

# change names
names(SLR.score_NA)[3:29] <- paste("SLR",names(SLR.score_NA)[3:29],sep="_")

# in countries directly
countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(SLR.score_NA,by=c("iso_a3" = "ISO3"))
dim(countries.shp.coastal) # 119 countries

# #### project
# SLR.score_NA.sf.proj <- st_transform(SLR.score_NA,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# crs(SLR.score_NA.sf.proj) == crs(pop.world.proj)
# 
# #### 
# SLR.score_NA.sf.proj.2015 <- rasterize(SLR.score_NA.sf.proj,pop.world.proj, field="X2015")
# plot(SLR.score_NA.sf.proj.2015)
# 
# SLR.score_coastal <- crop(SLR.score_NA.sf.proj.2015, specie.grav.resample, mask=T)
# plot(SLR.score_coastal)

##########################################
#             Poverty                    #
##########################################
# library(wesanderson)
#  depriv <- terra::rast(here("data","raw-data","povmap-grdi-v1-geotiff","povmap-grdi-v1_VNL-2020.tif"))
#  plot(depriv$`povmap-grdi-v1_VNL-2020`,col=wes_palette("Zissou1", 10, type = "continuous"))
# 
# # projected
#  depriv.proj <- project(depriv,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") ## takes ~5mins
#  plot(depriv.proj)
#  summary(values(depriv$`povmap-grdi-v1`))
#  
# # # resample species grav to match population raster
#  depriv.proj.resample <- resample(depriv.proj, pop.world.proj, method="med",threads=T)
#  plot(depriv.proj.resample,col=wes_palette("Zissou1", 10, type = "continuous"))
#  
#  ggplot() +
#    geom_spatraster(data = depriv.proj.resample.coastal, aes(fill = `povmap-grdi-v1`),
#                    maxcell = 10e+05)  +
#    # You can use coord_sf
#    scale_fill_hypso_c()
# 
# # # # crop to species gravity/coastal 
#  depriv.proj.resample.coastal <- crop(depriv.proj.resample, specie.grav.resample,mask=T)
#  plot(depriv.proj.resample.coastal,col=wes_palette("Zissou1", 10, type = "continuous"))
# # 
# # # let's save this raster so we don't have to do this again:
# #terra::writeRaster(depriv.proj.resample.coastal, "data/derived-data/Spatial rasters/depriv.proj.resample.coastal.tif",overwrite=TRUE)

# now simply load raster:
depriv.proj.resample.coastal <- terra::rast("data/derived-data/Spatial rasters/depriv.proj.resample.coastal.tif")
plot(depriv.proj.resample.coastal)
summary(values(depriv.proj.resample.coastal))

# library(tidyterra)
# ggplot() +
#   geom_spatraster(data = depriv.proj.resample.coastal, aes(fill = `povmap-grdi-v1`))  +
#   # You can use coord_sf
#   scale_fill_hypso_c()

#############################################
#.          Enabling conditions             #
#############################################

enabling_CM <- read.csv(here("data","raw-data","Cisneiros Montemayor 2021","Enabling_conditions_equitable_sustainable_Blue_Economy_-_Cisneros-Montemayor_et_al_2021.csv"))

# load data
fsi <- terra::readRDS(here::here("data","raw-data","Global_fsi_ineq.rds"))  %>%
  dplyr::mutate(ineq.drv = gender_inequality_index_2021) %>%
  mutate(ineq.drv = as.numeric(ineq.drv)) %>%
  mutate(ineq.drv = case_when(
                    country == "Tanzania" ~ 0.560, # GII of 2021 - republic of tanzania
                    country == "South Korea" ~ 0.067, # republic of Korea in GII dataset
                    country == "Syria" ~ 0.477, # Syrian Arab Republic in GII dataset
                    country == "Bolivia" ~ 0.418, # Bolivia (Plurinational State of) in GII dataset
                    country == "Bolivia" ~ 0.418, # Bolivia (Plurinational State of) in GII dataset
                    country == "Congo Democratic Republic" ~ 0.601, # Congo (Democratic Republic of the) in GII dataset
                    country == "Congo Republic" ~ 0.564, # Congo in GII dataset
                    country == "Venezuela" ~ 0.492, # Venezuela (Bolivarian Republic of) in GII dataset
                    country == "Guinea Bissau" ~ 0.627, # Guinea-Bissau in GII dataset
                    country == "Iran" ~ 0.459, # Iran (Islamic Republic of) in GII dataset
                    country == "Turkey" ~ 0.272, # Türkiye in GII dataset
                    country == "Laos" ~ 0.478, # Lao People's Democratic Republic in GII dataset
                    country == "Russia" ~ 0.203, # Russian Federation in GII dataset
                    country == "Moldova (Republic of)" ~ 0.205, # Moldova (Republic of) in GII dataset
                    country == "Vietnam" ~ 0.296, # Viet Nam in GII dataset
                    TRUE   ~ ineq.drv))

# check countries without ineq. score
fsi[which(is.na(fsi$ineq.drv)==T),"country"] %>% as.vector()

# # check country names 
# fsi$country[which(fsi$country %in% countries$name_en == F)]
# countries[which(countries$iso_a3 == "GBR"),]
# sort(countries$name_en)

# modify countries name in fsi
fsi <- fsi %>%
  mutate(country.clean=case_when(
  country=="Congo Democratic Republic" ~ "Democratic Republic of the Congo",
  country=="Congo Republic" ~ "Republic of the Congo",
  country=="Guinea Bissau" ~ "Guinea-Bissau",
  country=="Côte d'Ivoire" ~ "Ivory Coast",
  country=="Swaziland" ~ "Swaziland",
  country=="Timor-Leste" ~ "East Timor",
  country=="Gambia" ~ "The Gambia",
  country=="Micronesia" ~ "Federated States of Micronesia",
  country=="Sao Tome and Principe" ~ "São Tomé and Príncipe",
  country=="China" ~ "People's Republic of China",
  country=="Cabo Verde" ~ "Cape Verde",
  country=="Brunei Darussalam" ~ "Brunei",
  country=="Bahamas" ~ "The Bahamas",
  country=="United States" ~ "United States of America",
  country=="Czechia" ~ "Czech Republic",
  TRUE ~  country))

countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(fsi,by=c("name_en" = "country.clean"))
dim(countries.shp.coastal) # 119 countries

countries.shp.coastal[which(is.na(countries.shp.coastal$ineq.drv)==T),"country"]

# #### project
# fsi.sf.proj <- st_transform(fsi.sf,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# crs(fsi.sf.proj) == crs(pop.world.proj)
# 
# #### 
# fsi.sf.proj.ineq.gender <- rasterize(fsi.sf.proj,pop.world.proj, field="ineq.drv")
# plot(fsi.sf.proj.ineq.gender)
# 
# fsi.sf.proj.ineq.gender.coastal <- crop(fsi.sf.proj.ineq.gender, specie.grav.resample, mask=T)
# plot(fsi.sf.proj.ineq.gender.coastal)

##############################################
#.          Disaster prep                   #
##############################################

# ND_gain <- read.csv(here("data","raw-data","nd_gain_country_index_2023","resources","gain","gain.csv"))
# head(ND_gain)
# dim(ND_gain)
# 
# ND_gain_NA <- ND_gain %>%
#   filter(!is.na(X1995))
# dim(ND_gain_NA) # 185 countries

dis.prep.score <- read.csv(here("data","raw-data","nd_gain_country_index_2023","resources","indicators","id_infr_06","score.csv"),
                     header=T,sep=",")

dis.prep_score_NA <- dis.prep.score %>%
   filter(!is.na(X2015))
dim(dis.prep_score_NA) # 136 countries

# modify names
dis.prep_score_NA <- dis.prep_score_NA %>%
  mutate(country.clean=case_when(
    Name=="Bolivia, Plurinational State of" ~ "Bolivia",
    Name=="Brunei Darussalam" ~ "Brunei",
    Name=="China" ~ "People's Republic of China",
    Name=="Cote d'Ivoire" ~ "Ivory Coast",
    Name=="Iran, Islamic Republic of" ~ "Iran",
    Name=="Korea, Republic of" ~ "South Korea",
    Name=="Lao People's Democratic Republic" ~ "Laos",
    Name=="Macedonia" ~ "North Macedonia",
    Name=="Micronesia, Federated States of" ~ "Federated States of Micronesia",
    Name=="Moldova, Republic of" ~ "Moldova",
    Name=="Swaziland" ~ "Swaziland",
    Name=="Syrian Arab Republic" ~ "Syria",
    Name=="Tanzania, United Republic of" ~ "Tanzania",
    Name=="Timor-Leste" ~ "East Timor",
    Name=="United States" ~ "United States of America",
    Name=="Venezuela, Bolivarian Republic o" ~ "Venezuela",
    Name=="Viet Nam" ~ "Vietnam",
    TRUE ~  Name))
# check country names
dis.prep_score_NA$country.clean[which(dis.prep_score_NA$country.clean %in% countries$name_en == F)]
# 
# dis.prep_score_NA <- countries  %>%
#   left_join(dis.prep_score_NA,by=c("name_en" = "country.clean"))
# dim(dis.prep_score_NA) # 119 countries
# 
# dis.prep_score_NA %>% filter(name_en =="Tanzania")
# 
# #### project
# dis.prep_score_NA.sf.proj <- st_transform(dis.prep_score_NA,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# crs(dis.prep_score_NA.sf.proj) == crs(pop.world.proj)
# 
# #### 
# dis.prep_score_NA.sf.proj.2015 <- rasterize(dis.prep_score_NA.sf.proj,pop.world.proj, field="X2015")
# plot(dis.prep_score_NA.sf.proj.2015)
# 
# dis.prep_score_coastal <- crop(dis.prep_score_NA.sf.proj.2015, specie.grav.resample, mask=T)
# plot(dis.prep_score_coastal)

# change names
names(dis.prep_score_NA)[3:29] <- paste("DPREP",names(dis.prep_score_NA)[3:29],sep="_")

# in countries directly
countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(dis.prep_score_NA,by=c("iso_a3" = "ISO3"),keep=F)
dim(countries) # 258 countries
names(countries.shp.coastal)
#######################################################
#.          Physical Climate vulnerability            #
#######################################################
# 
# # population in 10m lecz
# 
# leczs_5_10m <- terra::rast(here("data","raw-data",
#                                 "lecz-urban-rural-population-land-area-estimates-v3-merit-leczs-geotiff","lecz_v3_spatial_data","data","merit_leczs.tif"))
# 
# # project to mollweide -- does not work on the 25/01/2024
#       #Erreur : [project] cannot create dataset from source
#       #De plus : Message d'avis :
#       #../../../gdal-3.7.2/frmts/mem/memdataset.cpp, 1303: cannot allocate 1x66121344000 bytes (GDAL error 2)
# leczs_5_10m.proj <- project(f,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# plot(leczs_5_10m.proj)
# 
# # resample species grav to match population raster
# leczs_5_10m.proj.resample <- resample(leczs_5_10m.proj, pop.world.proj, method="bilinear")

###########################################
#.          Marine dependency             #
###########################################

marine.dep <- read.csv(here("data","raw-data","Selig2019","Selig&al2019_Dependance_national_marine.csv"),sep=";")
head(marine.dep)
dim(marine.dep)

marine.dep.NA <- marine.dep %>%
  filter(!is.na(Integrated.dependence))

  # check countries
marine.dep.NA[which(marine.dep.NA$Country %in% countries$name_en== F),"Country"]

marine.dep.NA <- marine.dep.NA %>%
  mutate(Country = as.character(Country)) %>%
      mutate(country.clean=case_when(
        Country=="Virgin Islands (U.S.)" ~ "United States Virgin Islands",
        Country=="Sao Tome and Principe" ~ "São Tomé and Príncipe",
        Country=="Cote D'Ivoire" ~ "Ivory Coast",
        Country=="Gambia, The" ~ "The Gambia",
        Country=="Congo" ~ "Republic of the Congo",
        Country=="Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
        Country=="Curacao" ~ "Curaçao",
        Country=="Wallis and Futuna Islands" ~ "Wallis and Futuna",
        Country=="Timor Leste" ~ "East Timor",
        Country=="French Southern Territories" ~ "French Southern and Antarctic Lands",
        Country=="China" ~ "People's Republic of China",
        Country=="French Guiana" ~ "Guyana",
        Country=="Brunei Darussalam" ~ "Brunei",
        Country=="Great Britain" ~ "United Kingdom",
        Country=="United States" ~ "United States of America",
        Country=="The Former Yugoslav Republic of Macedonia" ~ "Moldova",
        Country=="Swaziland" ~ "Swaziland",
        TRUE ~  Country))

# check countries
marine.dep.NA[which(marine.dep.NA$country.clean %in% countries$name_en== F),"country.clean"]

# add to countries
countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(marine.dep.NA,by=c("name_en" = "country.clean"))
dim(countries.shp.coastal) # 119 countries

countries.shp.coastal %>% filter(is.na(Nutritional.dependence))

###########################################
#.              Governance                #
###########################################

WB_gov <- read.csv(here("data","raw-data","WB_GOV_2015.csv"),header=T,sep=";")
head(WB_gov)

# add governance indicators
countries.shp.coastal <-  countries.shp.coastal %>%
  left_join(WB_gov,by=c("iso_a3" = "Code"))

saveRDS(countries.shp.coastal,here("data","derived-data","indicators_countries.shp.coastal.rds"))

######################################################################
#   Create a data.frame for country level information and rasterize  #
######################################################################

#### project
countries.data.sf.proj <- st_transform(countries.shp.coastal,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
crs(countries.data.sf.proj) == crs(pop.world.proj)

countries.data.sf.proj.vt <- vect(countries.data.sf.proj)
plot(countries.data.sf.proj.vt)

####### nutritional
  # rasterize polygon
marine.dep.nutri <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Nutritional.dependence")

# crop to species gravity/coastal  - it works to crop a spatraster with coastal raster
marine.dep.nutri.coastal <- crop(marine.dep.nutri, specie.grav.resample,mask=T)
plot(marine.dep.nutri.coastal)

####### economic
marine.dep.econ <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Economic.dependence")
# crop to species gravity/coastal 
marine.dep.econ.coastal <- crop(marine.dep.econ, specie.grav.resample,mask=T)
plot(marine.dep.econ.coastal)

###### governance
  # voice account
Voice_Account <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Voice_Account_2015")
# crop to species gravity/coastal 
Voice_Account.coastal <- crop(Voice_Account, specie.grav.resample,mask=T)
plot(Voice_Account.coastal)
  # polit stab
Polit_stab <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Polit8stab_2015")
# crop to species gravity/coastal 
Polit_stab.coastal <- crop(Polit_stab, specie.grav.resample,mask=T)
plot(Polit_stab.coastal)
  # polit stab
Gov_Effect <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Gov_Effect_2015")
# crop to species gravity/coastal 
Gov_Effect_coastal <- crop(Gov_Effect, specie.grav.resample,mask=T)
plot(Gov_Effect_coastal)
# Reg_quality_2015
Reg_quality <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Reg_quality_2015")
# crop to species gravity/coastal 
Reg_quality_coastal <- crop(Reg_quality, specie.grav.resample,mask=T)
plot(Reg_quality_coastal)

# Reg_quality_2015
Rule_Law <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Rule_Law_2015")
# crop to species gravity/coastal 
Rule_Law_coastal <- crop(Rule_Law, specie.grav.resample,mask=T)
plot(Rule_Law_coastal)

#Control_Corr_2015
Control_Corr <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Control_Corr_2015")
# crop to species gravity/coastal 
Control_Corr_coastal <- crop(Control_Corr, specie.grav.resample,mask=T)
plot(Control_Corr_coastal)

#Prep disaster
disaster.prep <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="DPREP_X2015")
# crop to species gravity/coastal 
disaster.prep_coastal <- crop(disaster.prep, specie.grav.resample,mask=T)
plot(disaster.prep_coastal)

# SLR
SLR.score <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="SLR_X2015")
# crop to species gravity/coastal 
SLR.score_coastal <- crop(SLR.score, specie.grav.resample,mask=T)
plot(SLR.score_coastal)

# gender ineq
ineq.score <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="ineq.drv")
# crop to species gravity/coastal 
ineq.score_coastal <- crop(ineq.score, specie.grav.resample,mask=T)
plot(ineq.score_coastal)

#############################################
#        Stack on all raster                #
#############################################

# create stack raster
risk.stack <- c(#pop.world.proj, # human population in 2015
                #pop.world.change.proj.resample, # population change compare to 1994 - 2010
                specie.grav.resample, # species gravity
                depriv.proj.resample.coastal, #deprivation poverty index 
               # mean.temp.change.resample, # climate terrestrial
                marine.dep.nutri.coastal, # nutrional dependency to marine resources
                marine.dep.econ.coastal,#, # economic dependency to marine resources
                Voice_Account.coastal, # voice account WB
                Polit_stab.coastal,# political stab
                Gov_Effect_coastal, # gov effectiveness
                Reg_quality_coastal,  #regularoty quality "
                Rule_Law_coastal, # "rule of law"
                Control_Corr_coastal, # control corruption
                disaster.prep, # nd gain disaster prep
                SLR.score_coastal, # SLR change
               ineq.score_coastal)  # gender inequality

#   # rename variables
names(risk.stack) <- c("mean.count.grav.V2.log","povmap.grdi.v1",
                       "Nutritional.dependence","Economic.dependence",
                       "Voice_account","Political_stab","Gov_effect","Reg_quality","Rule_law","control_corr",
                       "disaster_prep","SLR_change","gender.ineq")

# clean space of large raster that are not necessary anymore
rm(pop.world.nc,pop.world,specie.grav,specie.grav.proj,pop.world.nc.change,pop.world.change.proj,
   pop.world.change.proj.resample,pop.world.change.proj.resample.coastal,mean.SLR.change,
   mean.SLR.change.proj,mean.SLR.change.proj.resample,mean.SLR.change.proj.coastal,depriv.proj.resample.coastal,
   dis.prep.score,dis.prep_score_coastal,dis.prep_score_NA.sf.proj.2015,fsi.sf.proj.ineq.gender.coastal,fsi.sf.proj.ineq.gender,
   marine.dep.econ.coastal,marine.dep.nutri.coastal, pop.world.proj.coastal,Reg_quality_coastal,Rule_Law_coastal,SLR.score_coastal,SLR.score_NA.sf.proj.2015);gc()

# save raster 
terra::writeRaster(risk.stack,here("data","derived-data","Spatial rasters","risk_stack.tif"),overwrite=T)

# sample
#sr <- terra::spatSample(risk.stack, 1000,na.rm=T,as.points=T,values=T,xy=T,method="random") # sample 5000000 random grid cells
#dim(sr) #332583      6
rm(risk.stack)
risk.stack <- rast(here::here("data","derived-data","Spatial rasters","risk_stack.tif"))

# check normality of raster values
x <- values(risk.stack[[13]]); # log the 1,4,12
    plot(hist(x))
    plot(hist(log(x)))

# recreate scale raster stack
mean.count.grav.V2.log.sc <- rescale01(risk.stack$mean.count.grav.V2.log); gc() #1
povmap.grdi.v1.sc <- rescale01(risk.stack$povmap.grdi.v1); gc() #2
Nutritional.dependence.sc <- rescale01(risk.stack$Nutritional.dependence); gc() #3
Economic.dependence.sc <- rescale01(risk.stack$Economic.dependence); gc() #4
Voice_account.sc <- rescale01(risk.stack$Voice_account); gc() #5
Political_stab.sc <- rescale01(risk.stack$Political_stab); gc() #6
Gov_effect.sc <- rescale01(risk.stack$Gov_effect); gc() #7
Reg_quality.sc <- rescale01(risk.stack$Reg_quality); gc() #8
Rule_law.sc <- rescale01(risk.stack$Rule_law); gc() #9
control_corr.sc <- rescale01(risk.stack$control_corr); gc() #10
disaster_prep.sc <- rescale01(risk.stack$disaster_prep); gc() #11
SLR_change.sc <- rescale01(risk.stack$SLR_change); gc() #12
gender.ineq.sc <- rescale01(risk.stack$gender.ineq); gc() #13

 # free memory
 rm(risk.stack); gc()
 
# create a stack raster of normalized raster
 risk.stack.sc <- c(mean.count.grav.V2.log.sc,
                    povmap.grdi.v1.sc,
                    Nutritional.dependence.sc,
                    Economic.dependence.sc,
                    Voice_account.sc,
                    Political_stab.sc,
                    Gov_effect.sc,
                    Reg_quality.sc,
                    Rule_law.sc,
                    control_corr.sc,
                    disaster_prep.sc,
                    SLR_change.sc,
                    gender.ineq.sc); gc()
 
 #summary(risk.stack.sc)
terra::writeRaster(risk.stack.sc,here("data","derived-data","Spatial rasters","risk.stack_sc.tif"),overwrite=TRUE)
