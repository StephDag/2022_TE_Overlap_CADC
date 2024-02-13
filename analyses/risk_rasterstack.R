
library(magrittr)
# load spatial rasters Camille:

localpath = here::here("data", "derived-data", "Spatial rasters")
# 
# rast.names <- list("pop.world.proj.coastal",
#                "pop.world.change.coastal", 
#                "specie.grav.resample",
#                "depriv.proj.resample.coastal",
#                "marine.dep.nutri.coastal", 
#                "marine.dep.econ.coastal",
#                "ND_gain_coastal", 
#                "fsi.sf.proj.ineq.gender.coastal",  
#                "merit.coastal")
# 
# 
# risk.stack <- lapply(rast.names, function(x){
#   sp.rast <- paste0(localpath, "/", x, ".tif")
#   r1 <- terra::rast(sp.rast)
#   names(r1) <- paste(x)
#   return(r1)
# }) %>%
#   do.call(c, .)
# 
# # scale all raster layers
# risk.stack.sc <- lapply(rast.names, function(x){
#   sp.rast <- paste0(localpath, "/", x, ".tif")
#   r1 <- terra::rast(sp.rast)
#   r1 <- scale(r1)
#   names(r1) <- paste(x, ".sc")
#   return(r1)
# }) %>%
#   do.call(c, .)
# 
# # save to NetCDF
# terra::writeCDF(risk.stack, paste0(localpath, "/risk.stack.nc"), overwrite=TRUE)
# terra::writeCDF(risk.stack.sc, paste0(localpath, "/risk.stack.sc.nc"), overwrite=TRUE)
library(here)
source(here("analyses","00_setup.R"))

source(here("analyses","001_Coastal_countries.R"))
source(here("R","NormMinMax.R"))
rm(cities, codelist, ctr, ctr.iati, ctr.region, eez)
# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- rnaturalearth::ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)


# add country information
world.2 <- countries.shp.coastal %>%
  st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


# updated risk.stack from Steph:
risk.stack.sc <- terra::rast(here::here("data/derived-data/Spatial rasters/risk.stack_sc_steph.tif" ))
rast.names <- names(risk.stack.sc )



# # correlation of normalized data (à faire après)
risk.vals <- terra::values(risk.stack.sc)

risk.stack_df <- as.data.frame(risk.stack.sc)     
risk.vals.nona <- na.omit(risk.vals) # this no longer works coz the vector is too big ...




cor.risk <- cor(risk.vals.nona)
heatmap(cor.risk)

save.image(here::here("data/derived-data/risk_stack_session.rds"))


essai <- risk.stack.steph %>% terra::subset(., 1)
nas <- is.na(essai$mean.count.grav.V2.log)
miaou <- which(nas$mean.count.grav.V2.log==TRUE)


which(is.na(essai$mean.count.grav.V2.log[1:100]))
      
      
essai2 <- terra::extract(essai, points, xy=TRUE)
essai2 <- terra::crds(essai)
essai2$index <- 1:nrow(essai2)
essai2 <- which()

essai2 <- terra::crds(risk.stack.sc, na.rm=TRUE, na.all=TRUE)


# intersecting points with country
# Convert the SpatRaster to a SpatialPointsDataFrame essaye de paralléliser 
risk.stack.df <- as.data.frame(risk.stack)
risk.stack.df <- read.csv2(here::here("data/derived-data/Spatial rasters/xy_risk_stack.csv"))[,-1]
head(risk.stack.df)


################# steph pense que c'est ça qu'il faut faire. st_intersection une fois qu'on a mis en spatial data frame. Puis merge avec shapefile du monde world 2. But = récupérer l'info de quel pxel dans quel pays, pour pouvoir calculer l'indice composite, 1 valeur composite par pixel, puis une distribution de valeurs par pays. On n' a pas besoin d'avoir l'info spatiale pendant qu'on calcule les indices. On n'a pas besoin de stack.
# Then do the pca as planned. To have an idea of the relationship between variables. And have different colors per country.
#
##################################################################################
##################################################################################
##################################################################################


#risk.stack.sp.sf.ctry <- intersect(world.2,risk.stack.sc) #
#risk.stack.sp.sf.ctry <- st_intersection(risk.stack.sc, world.2) #
#dim(risk.stack.sp.sf.ctry)  # 332583      6
#head(risk.stack.sp.sf.ctry)

# essai avec le risk stack de steph

risk.stack.steph <- terra::rast(here::here("data/derived-data/Spatial rasters/risk.stack_sc_steph.tif"))

# extract coordinates from non-na cells:
rs_xy <- terra::crds(risk.stack.steph, na.rm=TRUE, na.all=TRUE) %>% 
  as.data.frame()
# turn into a sf 
rs_sf <- sf::st_as_sf(rs_xy, coords = c("x", "y"), crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# intersect:
essai <- sf::st_intersection(rs_sf[1:5000,], world.2)


#### WE ARE HERE      #############################################



library(here)
source(here("analyses","00_setup.R"))

source(here("analyses","001_Coastal_countries.R"))
source(here("R","NormMinMax.R"))
rm(cities, codelist, ctr, ctr.iati, ctr.region, eez)
# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- rnaturalearth::ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)


# add country information
world.2 <- countries.shp.coastal %>%
  st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


risk.stack.steph <- terra::rast(here::here("data/derived-data/Spatial rasters/risk.stack_sc_steph.tif"))








world.2$name %>% unique
#risk.stack.sp.sf.ctry <- intersect(world.2,risk.stack.steph) #
#risk.stack.sp.sf.ctry <- st_intersection(risk.stack.steph, world.2) #
#dim(risk.stack.sp.sf.ctry)  # 332583      6
#head(risk.stack.sp.sf.ctry)



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

