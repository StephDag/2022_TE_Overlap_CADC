# compute climate hazard composite index 
### Stephanie D'Agata, Nov 2023
### output: raster stack of climate change variables + composite index

library(here)
source(here("analyses","00_setup.R"))
source(here("analyses","001_Coastal_countries.R"))

# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- ne_countries(returnclass = "sf",scale = 110)

#library(rworldmap)
#worldMap <- getMap()

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)

#coastal.ctr[which(coastal.ctr$iso2 %in% countries$iso_a2 == F),]
#"iso_3166.2"

countries %>%
  filter(name_en == "American Samoa")

##########################################
#               Population               #
##########################################

pop.world.nc <- rast(here("data","raw-data","Word Population count  SEDAC 5km","gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc","gpw_v4_population_count_adjusted_rev11_2pt5_min.nc"))

#pop.world.nc <- raster::brick(ncfname)

#pop.world.nc <- rast(here("data","raw-data","Word Population count  SEDAC 5km","gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc","gpw_v4_population_count_adjusted_rev11_2pt5_min.nc"),
#                     subds = "UN WP~ter=4")

# the 4th raster is the count population for 2015: 4	Population Count, v4.11 (2015) (see doc)
pop.world <- pop.world.nc[[4]] 
# plot(pop.world)

# mollweide
pop.world.proj <- project(pop.world,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# plot(pop.world.proj)
# check

# crop to species gravity/coastal 
#pop.world.proj.coastal <- crop(pop.world.proj, specie.grav.proj,mask=T)
#plot(pop.world.proj.coastal)

##########################################
#               Biodiversity             #
##########################################

# cyclones tracks
specie.grav <- rast(here("data","derived-data","Spatial rasters","sp.count.rast.ter.grav.tif"))
plot(specie.grav)

specie.grav.proj <- project(specie.grav,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

# resample species grav to match population
specie.grav.resample <- resample(specie.grav, pop.world.proj, method="bilinear")
plot(specie.grav.resample)

#####################################
#               Climate             #
#####################################

# load climate raster from ipcc atlas
# https://interactive-atlas.ipcc.ch/regional-information#eyJ0eXBlIjoiQVRMQVMiLCJjb21tb25zIjp7ImxhdCI6MzE1MjgwMCwibG5nIjotMjQ5OTk4MCwiem9vbSI6NiwicHJvaiI6IkVQU0c6NTQwMzAiLCJtb2RlIjoiY29tcGxldGVfYXRsYXMifSwicHJpbWFyeSI6eyJzY2VuYXJpbyI6InNzcDEyNiIsInBlcmlvZCI6Im5lYXIiLCJzZWFzb24iOiJ5ZWFyIiwiZGF0YXNldCI6IkNNSVA2IiwidmFyaWFibGUiOiJ0YXMiLCJ2YWx1ZVR5cGUiOiJBTk9NQUxZIiwiaGF0Y2hpbmciOiJTSU1QTEUiLCJyZWdpb25TZXQiOiJhcjYiLCJiYXNlbGluZSI6IkFSNiIsInJlZ2lvbnNTZWxlY3RlZCI6WzI3XX0sInBsb3QiOnsiYWN0aXZlVGFiIjoicGx1bWUiLCJzaG93aW5nIjp0cnVlLCJtYXNrIjoibm9uZSIsInNjYXR0ZXJZTWFnIjoiQU5PTUFMWSIsInNjYXR0ZXJZVmFyIjoidGFzIn19

# 1. load rasters
  # mean SST changes - (2021-2040) vs ( 1995 - 2014)
mean.SST.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - Sea Surface Temperature (SST) Change deg C - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual (28 models).tiff"))
plot(mean.SST.change)

# mean pH changes - (2021-2040) vs ( 1995 - 2014)
mean.pH.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - pH at surface (pH) Change pH - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual (10 models).tiff"))
plot(mean.pH.change) # inverse (less is more acidic)

# mean SLR changes - (2021-2040) vs ( 1995 - 2014)
mean.SLR.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - Sea level rise (SLR) Change meters - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual .tiff"))
plot(mean.SLR.change)

# mean terrestiral temp
mean.temp.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - Mean temperature (T) Change deg C - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual (34 models).tiff"))
plot(mean.temp.change)

mean.temp.change.proj <- project(mean.temp.change,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

  # resample terrestrial temp to match population
mean.temp.change.resample <- resample(mean.temp.change.proj, pop.world.proj, method="bilinear")
plot(mean.temp.change.resample)

  # crop to species gravity/coastal 
#mean.temp.change.resample.coastal <- crop(mean.temp.change.resample, specie.grav,mask=T)
#plot(mean.temp.change.resample.coastal)

# marine heatwaves

# 2. normalize between 0 and 1 for each raster
# standardized - min-max
source(here("R","NormMinMax.R"))

# SST changes
mean.SST.change.norm <- app(mean.SST.change,fun = normalize,na.rm=T)
plot(mean.SST.change.norm)

# resample SST change population
mean.SST.change.resample <- resample(mean.SST.change, pop.world.proj, method="bilinear")
plot(mean.SST.change.resample)

# mean pH Changes
mean.pH.change.norm <- app(-mean.pH.change,fun = normalize,na.rm=T)
plot(mean.pH.change.norm)

# mean SLR changes
mean.SLR.change.norm <- app(mean.SLR.change,fun = normalize,na.rm=T)
plot(mean.SLR.change.norm)

###### composite score
mean.cc.score <- mosaic(mean.SST.change.norm, mean.pH.change.norm, mean.SLR.change.norm,fun = "sum")

  # project to match population
mean.cc.score.proj <- project(mean.cc.score,sf_obj)
crs(mean.cc.score.proj) == crs(sf_obj)
distance(cbind(0,1), cbind(0,2), lonlat=TRUE)/1000

  # match EEZ

##########################################
#      Population change                 #
##########################################

pop.world.nc.change <- rast(here("data","raw-data","IPCC_Population","CMIP6 - Population density Change persons_km__2 - Near Term (2021-2040) SSP2 (rel. to 1995-2014) - Annual .tiff"))

# mollweide
pop.world.change.proj <- project(pop.world.nc.change,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# plot(pop.world.proj)

# crop to coastal
pop.world.change.proj.coastal <- terra::crop(pop.world.change.proj,specie.grav.proj,mask=T)

# resample species grav to match population
pop.world.change.proj.resample <- resample(pop.world.change.proj, pop.world.proj, method="bilinear")
plot(pop.world.change.proj.resample)

crs(specie.grav.proj) == crs(pop.world.change.proj.resample)

# crop to species gravity/coastal 
#pop.world.change.proj.resample.coastal <- crop(pop.world.change.proj.resample, vect(specie.grav.proj),mask=T)
#plot(pop.world.change.proj.resample.coastal)

##########################################
#             Poverty                    #
##########################################

depriv <- rast(here("data","raw-data","povmap-grdi-v1-geotiff","povmap-grdi-v1.tif"))
plot(depriv$`povmap-grdi-v1`)

depriv.proj <- project(depriv,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

# resample species grav to match population
depriv.proj.resample <- resample(depriv.proj, pop.world.proj, method="bilinear")
plot(depriv.proj.resample)

# crop to species gravity/coastal 
#depriv.proj.resample.coastal <- crop(depriv.proj.resample, specie.grav,mask=T)
#depriv.proj.resample.coastal <- project(depriv.proj.resample.coastal,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
#plot(depriv.proj.resample.coastal)

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
range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))} # Function MinMax 

fsi <- readRDS(here("data","raw-data","Global_fsi_ineq.rds"))  %>%
  mutate(ineq.drv = normalize(as.numeric(gender_inequality_index_2021,na.rm=T)))

fsi.sf <- countries  %>%
  left_join(fsi,by=c("name_en" = "country"))

#### project
fsi.sf.proj <- st_transform(fsi.sf,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
crs(fsi.sf.proj) == crs(pop.world.proj)

#### 
fsi.sf.proj.ineq.gender <- rasterize(fsi.sf.proj,pop.world.proj, field="ineq.drv")
plot(fsi.sf.proj.ineq.gender)

##############################################
#.          Climate vulnerability            #
##############################################

ND_gain <- read.csv(here("data","raw-data","nd_gain_country_index_2023","resources","vulnerability","vulnerability.csv"))
head(ND_gain)
dim(ND_gain)

ND_gain_NA <- ND_gain %>%
  filter(!is.na(X1995))
dim(ND_gain_NA)

ND_gain_NA.sf <- countries  %>%
  left_join(ND_gain_NA,by=c("name_en" = "Name"))

#### project
ND_gain_NA.sf.proj <- st_transform(ND_gain_NA.sf,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
crs(ND_gain_NA.sf.proj) == crs(pop.world.proj)

#### 
ND_gain_NA.sf.proj.2015 <- rasterize(ND_gain_NA.sf.proj,pop.world.proj, field="X2015")

##################################################
#.          Relative inequality index            #
##################################################

# relative wealth index
#ineq.list <- list.files(here("data","raw-data","relative-wealth-index-april-2021"),full.names = T)
#imp.func <- function (x) {
#  import(x) %>% 
#   dplyr::mutate(ctry=substr(basename(x),1,3))
#}
#ineq.dat <- lapply(ineq.list,imp.func)
#rineq <- do.call(rbind.data.frame,ineq.dat) 

#convert to spatial object
#rineq.vector <- vect(rineq, geom=c("longitude", "latitude"),crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
#plot(rineq.vector)

# transform
#rineq.vector.proj <- rast(rineq.vector,pop.world.proj)

#crs(rineq.vector)
# rasterize vector
#rineq.rast <- terra::rasterize(rineq.vector,pop.world.proj, field="rwi")

#### project
#rineq.sf <-  st_as_sf(rineq.vector,
#                      coords=c("longitude","latitude"), 
#                      crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs", agr = NA_agr_, remove = FALSE)

# rasterize
#rineq.sf.raster <-st_rasterize(rineq.sf %>% dplyr::select(rwi, geometry))
#class(rineq.sf.raster)

#rineq.sf.raster.2 <- terra::rasterize(rineq.sf,pop.world.proj,filed= "rwi",fun="mean")
#plot(rineq.sf.raster.2)

#rineq.sf.raster.proj <- st_transform(rineq.sf.raster,crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
#crs(rineq.sf.raster.proj) == crs(pop.world.proj)

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
#### convert to vector

####### nutritional
  # rasterize polygon
marine.dep.nutri <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Nutritional.dependence")
# crop to species gravity/coastal 
marine.dep.nutri.coastal <- crop(marine.dep.nutri, specie.grav,mask=T)
plot(marine.dep.nutri.coastal)

####### economic
marine.dep.econ <- rasterize(countries.data.sf.proj.vt,pop.world.proj, field="Economic.dependence")
# crop to species gravity/coastal 
#marine.dep.econ.coastal <- crop(marine.dep.econ, specie.grav,mask=T)
#plot(marine.dep.econ.coastal)

#############################################
#        PCA on all raster                  #
#############################################

# create stack raster
risk.stack <- c(#pop.world.proj, # human population in 2015
                #pop.world.change.proj.resample, # population change compare to 1994 - 2010
                specie.grav.resample, # species gravity
                depriv.proj.resample, #deprivation poverty index 
               # mean.temp.change.resample, # climate terrestrial
                marine.dep.nutri, # nutrional dependency to marine resources
                marine.dep.econ,#, # economic dependency to marine resources
                ND_gain_NA.sf.proj.2015, # national climate vulnerability index
                fsi.sf.proj.ineq.gender)  # gender inequality
 #    rineq.raster) # relative inequality

# remove large object to clean memory
rm(mean.pH.change,mean.pH.change,mean.pH.change.norm,mean.cc.score.proj,mean.cc.score,
   mean.SLR.change,mean.SLR.change.norm,mean.SST.change,mean.SST.change.norm,mean.SST.change.resample,
   pop.world,pop.world.change.proj,pop.world.change.proj.coastal,pop.world.change.proj.resample,
   pop.world.change.proj,pop.world.change.proj.coastal,pop.world.change.proj.resample,
   pop.world.proj.coastal,pop.world.nc,pop.world.proj,rineq,rineq.sf,
   species.grav,species.grav.proj,species.grav.resample,
   depriv,depriv.proj,depriv.proj.resample)

#saveRDS(risk.stack,here("data","derived-data","risk.stack.rds"))
risk.stack <- readRDS(here("data","derived-data","risk.stack.rds"))

# sample
sr <- terra::spatSample(risk.stack, 5000000,na.rm=T,as.points=T,values=T,xy=T,method="random") # sample 500 random grid cells
dim(sr)

  # rename variables
names(sr) <- c("mean.count.grav.V2.log","povmap.grdi.v1",
               "Nutritional.dependence","Economic.dependence","ND_gain_NA.sf.proj.2015","gender.ineq")
saveRDS(sr,here("data","derived-data","risk.stack.sr.rds"))
sr <- readRDS(here("data","derived-data","risk.stack.sr.rds"))

# recreate scale dataframe
#sr$population.sc <- normalize(sr$population)
#sr$population.change.sc <- normalize(sr$population.change)
sr$mean.count.grav.V2.log.sc <- normalize(sr$mean.count.grav.V2.log)
sr$povmap.grdi.v1.sc <- normalize(sr$povmap.grdi.v1)
#sr$terrest.temp.change.sc <- normalize(sr$terrest.temp.change)
sr$Nutritional.dependence.sc <- normalize(sr$Nutritional.dependence)
sr$Economic.dependence.sc <- normalize(sr$Economic.dependence)
sr$ND_gain_NA.sf.proj.2015.sc <- normalize(sr$ND_gain_NA.sf.proj.2015)
sr$gender.ineq %>% range()

# spatvector to sp
sr.sp <- as(sr, "Spatial")
names(sr.sp)

# randomly select 5000 rows
sample.2000 <- sample(seq(1,dim(sr.sp)[1],1),2000)
sr.sp.2000 <- sr.sp[sample.2000,c(5:10)]

# extract country information
world.2 <- ne_countries(type = "countries", scale = "large",return="sf") %>%
  st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

  # sr
sr.sp.sf = st_as_sf(sr.sp)
crs(sr.sp.sf) == crs(world.2)

# intersecting points with country
sr.sp.sf.ctry <- st_intersection(sr.sp.sf, world.2) # 
dim(sr.sp.sf.ctry) # 157076    181

  # for PCA
#sr.sp.2000.sf = st_as_sf(sr.sp.2000)
#crs(sr.sp.2000.sf) == crs(world.2)

# intersecting points with country
#sr.sp.2000.sf.ctry <- st_intersection(sr.sp.2000.sf, world.2) # 
#dim(sr.sp.2000.sf.ctry) # 1986 175

sr.sp.2000.sf.ctry.pca <- sr.sp.2000.sf.ctry[,c(1:6,53,118)]
head(sr.sp.2000.sf.ctry.pca)
dim(sr.sp.2000.sf.ctry.pca)

# row to keep in original
row2keep <- which(rownames(sr.sp.2000@data) %in% rownames(sr.sp.2000.sf.ctry.pca))

# new spatial points df
sr.sp.2000.ctry <- sr.sp.2000[row2keep,]
sr.sp.2000.ctry <- cbind(sr.sp.2000.ctry,sr.sp.2000.sf.ctry.pca[,c("iso_a3","name_en")] %>% st_drop_geometry())

# pca 

# bandwith
bw.gw.pca <- bw.gwpca(sr.sp.2000.ctry[,1:6], 
                      vars = names(sr.sp.2000.ctry[,1:7]),
                      k = 3,
                      robust = FALSE,
                      adaptive = TRUE)

# geograph. weighted pcA
gw.pca<- gwpca(sr.sp.2000.ctry[,1:6], 
               vars = names(sr.sp.2000.ctry[,1:6]), 
               bw=bw.gw.pca,
               k = 3, 
               robust = FALSE, 
               adaptive = TRUE,
               scores=T)


# plot of the spatial PCA
fviz_eig(gw.pca$pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(gw.pca$pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Contributions of variables to PC1
fviz_contrib(gw.pca$pca, choice = "var", axes = 1, top = 10,ylim=c(0,50))
# Contributions of variables to PC2
fviz_contrib(gw.pca$pca, choice = "var", axes = 2, top = 10,ylim=c(0,50))
# Contributions of variables to PC3
fviz_contrib(gw.pca$pca, choice = "var", axes = 3, top = 10,ylim=c(0,50))

# save normalize score for each PC
sr.sp.2000.ctry$PC1 <- normalize(gw.pca$pca$scores[,1])
sr.sp.2000.ctry$PC2 <- normalize(gw.pca$pca$scores[,2])
sr.sp.2000.ctry$PC3 <- normalize(gw.pca$pca$scores[,3])

# by country for each axis
# Basic box plot
p.PC1 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC1), y=PC1)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  theme_bw()
p.PC1
ggsave(here("figures","PC1_distrib_ctr.png"),p.PC1,height=10,width=10)
p.PC2 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC2), y=PC2)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  theme_bw()
p.PC2
ggsave(here("figures","PC2_distrib_ctr.png"),p.PC2,height=10,width=10)
p.PC3 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC3), y=PC3)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  theme_bw()
p.PC3
ggsave(here("figures","PC3_distrib_ctr.png"),p.PC3,height=10,width=10)

# create composite risk score
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

# 
p.risk.score <- ggplot(as.data.frame(sr.sp.sf.ctry), aes(x=reorder(name_en,-risk.mat.score.sum), y=risk.mat.score.sum)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  ylab("Contextual Inequity Scores") +
  theme_bw()
p.risk.score
ggsave(here("figures","Contextual_Ineq_score_distrib_ctr.png"),p.risk.score,height=12,width=12)

p.PC.score <- ggarrange(p.PC1,p.PC2,p.PC3,p.risk.score,ncol=2,nrow=2,labels=c("A","B","C","D"))
ggsave(here("figures","double_exposure_ctry_distribution_2000samples.pdf"),p.PC.score,height=18,width=18)

## add country
fviz_pca_biplot(gw.pca$pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

###################
fviz_pca_ind(gw.pca$pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

fviz_pca_ind(gw.pca$pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,ellipse.type = "convex", # Concentration ellipses
             legend.title = "Groups"
)

fviz_pca_biplot(gw.pca$pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)

# PC1
plot(sr.sp.5000$terrest.temp.change.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$povmap.grdi.v1.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$Nutritional.dependence.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$Economic.dependence.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$population.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$mean.count.grav.V2.log.sc,sr.sp.5000$risk.score.PC1)

# PC2
plot(sr.sp.5000$terrest.temp.change.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$povmap.grdi.v1.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$Nutritional.dependence.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$Economic.dependence.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$population.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$mean.count.grav.V2.log.sc,sr.sp.5000$risk.score.PC2)

# function for calculation pproportion of variance 
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
}
var.gwpca <- prop.var(gw.pca, 3)
mf$var.gwpca <- var.gwpca



# function for calculation pproportion of variance 
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
}

var.gwpca <- prop.var(bw.gw.pca, 3)
mf$var.gwpca <- var.gwpca

# non-spatial PCA
c
# run PCA on random sample with correlation matrix
# retx=FALSE means don't save PCA scores 
pca.risk <- prcomp(sr,  center = TRUE, scale = TRUE) 

# check eigenvalue
fviz_eig(pca.risk)

# plot variable
fviz_pca_var(pca.risk,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# correlation of variable on axis:
pca.risk$rotation


####################################
#         CADC Risk figures        #
####################################

ocean_ODA <- readRDS(here("data","derived-data","ocean_ODA_CADC_2010_2021_equity_sf.RDS"))
head(ocean_ODA)

# add coastal length
ocean_ODA <- left_join(ocean_ODA,coastal.ctr %>% select(country,iso2,coastline_wf) ,by=c("iso_a2" = "iso2"))

# filter by CADC
ocean_ODA_equity <- ocean_ODA %>%
  filter(cadc.type %in% c("equity CADC","CADC")) %>%
  st_drop_geometry() %>%
  group_by(iso_a3) %>%
  mutate(tot.project.CADC = sum(total.project)) %>%
  mutate(perc.equi = round(total.project/tot.project.CADC,3)) %>%
  as.data.frame()

ocean_ODA_CADC.eq <- ocean_ODA_equity %>%
  filter(cadc.type == "equity CADC")

# density of CADC (with world factbook)
#ocean_ODA_equity <- ocean_ODA_equity %>%
#  mutate(cadc.density = round(total.project/coastline_wri,2)) %>%
#  mutate(cadc.dollars.density = round(total.usd/coastline_wri,2))

# risk by country
ocean_ODA_CADC_risk.mean <- sr.sp.sf.ctry %>%
  as.data.frame() %>%
  dplyr::select(iso_a3,risk.mat.score.geom,risk.mat.score.sum) %>%
  group_by(iso_a3) %>%
  mutate(mean.risk.geom = mean(risk.mat.score.geom,na.rm=T)) %>% 
  mutate(mean.risk.sum = mean(risk.mat.score.sum,na.rm=T)) %>%
  select(-risk.mat.score.geom,-risk.mat.score.sum) %>%
  distinct()

ocean_ODA_CADC_risk.mean %>%
  arrange(-mean.risk.sum)

# merge with risk
rm(ocean_ODA_CADC_risk)
ocean_ODA_CADC_risk <- left_join(ocean_ODA_CADC_risk.mean,ocean_ODA_CADC.eq,by="iso_a3")
#ocean_ODA_CADC_risk$cadc.density.sc <- normalize(ocean_ODA_CADC_risk$cadc.density)

# scale risk and count/country
ocean_ODA_CADC_risk$mean.risk.geom.sc = normalize(ocean_ODA_CADC_risk$mean.risk.geom)
ocean_ODA_CADC_risk$total.project.sc = normalize(ocean_ODA_CADC_risk$total.project)
ocean_ODA_CADC_risk$mean.risk.sum.sc = normalize(ocean_ODA_CADC_risk$mean.risk.sum)

#  replace NA with 0 in perc.equi and total.project.sc
ocean_ODA_CADC_risk <- ocean_ODA_CADC_risk %>%
  mutate(total.project.sc = replace_na(total.project.sc, 0)) %>%
  mutate(total.project = replace_na(total.project, 0)) %>%
  mutate(perc.equi = replace_na(perc.equi, 0)) %>%
  as.data.frame()

ocean_ODA_CADC_risk <- ocean_ODA_CADC_risk %>%
  mutate(risk.CADC = sqrt(mean.risk.sum.sc^2 + total.project.sc^2)) %>%
  mutate(risk.CADC.sc = normalize(risk.CADC))
  
# re-add countries
ocean_ODA_CADC_risk <- ocean_ODA_CADC_risk %>%
  left_join(countries %>% select(iso_a3,name_en),by="iso_a3")

## PCA of the two, retrive 1st AXIS
#ocean_ODA_CADC_risk.PCA.df <- as.data.frame(ocean_ODA_CADC_risk) %>% 
#  select(mean.risk.sc,total.project.sc) %>%
#  filter(!is.na(mean.risk.sc))
#ocean_ODA_CADC_risk.PCA <- princomp(ocean_ODA_CADC_risk.PCA.df,cor=T)

# check eigenvalue
#fviz_eig(ocean_ODA_CADC_risk.PCA)

# plot variable
#fviz_pca_var(ocean_ODA_CADC_risk.PCA,
#             col.var = "contrib", # Color by contributions to the PC
#             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#             repel = TRUE     # Avoid text overlapping
#)

library(ggrepel)

# CADC - risk
sc.risk.CADC <- ggplot(ocean_ODA_CADC_risk,aes(x=total.project.sc,y=mean.risk.sum.sc,label=name_en)) +
  geom_point(aes(size=total.project))+
  geom_text_repel() +
  #geom_smooth(method="lm") +
  #scale_color_gradient(low="blue", high="red")+
  xlab("Standardized total number of projects by country") +
  ylab("Average contextual inequity by country") +
  xlim(0,1)  +  ylim(0,1) +
  geom_abline(slope=1,intercept=0,linetype = 2)+
  theme_bw()
sc.risk.CADC

# CADC risk - equity
CADC.risk.eq <- ggplot(ocean_ODA_CADC_risk,aes(x=perc.equi,y=risk.CADC.sc,label=name_en)) +
  geom_point(aes(size=total.project,color=mean.risk.sum.sc)) +
  scale_color_gradient(low="blue", high="red",name = "Contextual inequity")+
  #scale_colour_gradient(low = "lightblue",high = "blue") +
  xlab("Equity (% total of projects)") +
  geom_text_repel() +
  #geom_smooth(method="lm") +
  ylab("CADC change potential") +
  xlim(0,1) +  ylim(0,1) +
  geom_vline(xintercept=0.5,linetype = 2)+
  geom_hline(yintercept=0.5,linetype = 2)+
  theme_bw()
CADC.risk.eq

CADC.risk <- ggarrange(sc.risk.CADC,CADC.risk.eq,labels=c("a","b"),ncol=2,common.legend = T,legend = "bottom")
ggsave(here("figures","CADC.risk.equity.AB.pdf"),CADC.risk,width=18,height=10)

ggsave(here("figures","CADC.risk.equit.panelA.pdf"),sc.risk.CADC,width=10,height=10)
ggsave(here("figures","CADC.risk.equit.panelB.pdf"),CADC.risk.eq,width=10,height=10)

### contextual inequity with proecjt inequity
CADC.cont.eq <- ggplot(ocean_ODA_CADC_risk,aes(y=perc.equi,x=mean.risk.sum.sc,label=name_en)) +
  geom_point(aes(size=total.project)) +#color=mean.risk.sum.sc)) +
  #scale_color_gradient(low="blue", high="red",name = "Contextual inequity")+
  #scale_colour_gradient(low = "lightblue",high = "blue") +
  ylab("Equity (% total of projects)") +
  #geom_smooth(method="lm") +
  geom_text_repel() +
  xlab("Contextual inequity") +
  xlim(0,1) +  ylim(0,0.5) +
  #geom_vline(xintercept=0.5,linetype = 2)+
  #geom_hline(yintercept=0.5,linetype = 2)+
  theme_bw()
CADC.cont.eq
ggsave(here("figures","CADC.eq.cont.eq.pdf"),CADC.cont.eq,width=10,height=10)

### contextual inequity with proecjt inequity
CADC.cont.eq.n.projects <- ggplot(ocean_ODA_CADC_risk,aes(x=perc.equi,y=total.project,label=name_en)) +
  geom_point() +#color=mean.risk.sum.sc)) +
  #scale_color_gradient(low="blue", high="red",name = "Contextual inequity")+
  #scale_colour_gradient(low = "lightblue",high = "blue") +
  xlab("Equity (% total of projects)") +
  #geom_smooth(method="lm") +
  geom_text_repel() +
  ylab("Total number of projects") +
  xlim(0,0.5) +  #ylim(0,1) +
  #geom_vline(xintercept=0.5,linetype = 2)+
  #geom_hline(yintercept=0.5,linetype = 2)+
  theme_bw()
CADC.cont.eq.n.projects
ggsave(here("figures","CADC.eq.total.project.pdf"),CADC.cont.eq.n.projects,width=10,height=10)

CADC.risk.all <- ggarrange(sc.risk.CADC,CADC.risk.eq,CADC.cont.eq,CADC.cont.eq.n.projects,labels=c("a","b","c","d"),ncol=2,nrow=2,common.legend = T,legend = "bottom")
ggsave(here("figures","CADC.risk.equity.ABCD.pdf"),CADC.risk.all,width=18,height=18)


log.total.risk <- ggplot(ocean_ODA_CADC_risk,aes(x=log(total.project+1),y=mean.risk,label=name)) +
  geom_point() +
  geom_text_repel() +
  theme_bw()
log.total.risk

density.risk <- ggplot(ocean_ODA_CADC_risk,aes(x=log(cadc.density+1),y=mean.risk,label=name)) +
  geom_point() +
  geom_text_repel() +
  theme_bw()
density.risk

total.risk <- ggplot(ocean_ODA_CADC_risk,aes(x=total.project,y=mean.risk,label=name)) +
  geom_point() +
  geom_text_repel() +
  theme_bw()
total.risk
