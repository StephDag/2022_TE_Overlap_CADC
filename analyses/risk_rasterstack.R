
library(magrittr)
# load spatial rasters:

localpath = here::here("data", "derived-data", "Spatial rasters")

rast.names <- list("pop.world.proj.coastal",
               "pop.world.change.coastal", 
               "specie.grav.resample",
               "depriv.proj.resample.coastal",
               "marine.dep.nutri.coastal", 
               "marine.dep.econ.coastal",
               "ND_gain_coastal", 
               "fsi.sf.proj.ineq.gender.coastal",  
               "merit.coastal")


risk.stack <- lapply(rast.names, function(x){
  sp.rast <- paste0(localpath, "/", x, ".tif")
  r1 <- terra::rast(sp.rast)
  names(r1) <- paste(x)
  return(r1)
}) %>%
  do.call(c, .)

# scale all raster layers
risk.stack.sc <- lapply(rast.names, function(x){
  sp.rast <- paste0(localpath, "/", x, ".tif")
  r1 <- terra::rast(sp.rast)
  r1 <- scale(r1)
  names(r1) <- paste(x, ".sc")
  return(r1)
}) %>%
  do.call(c, .)

# save to NetCDF
terra::writeCDF(risk.stack, paste0(localpath, "/risk.stack.nc"), overwrite=TRUE)
terra::writeCDF(risk.stack.sc, paste0(localpath, "/risk.stack.sc.nc"), overwrite=TRUE)

risk.stack.sc <- terra::rast(paste0(localpath, "/risk.stack.sc.nc"))
names(risk.stack.sc) <- rast.names

# # correlation of normalized data (à faire après)

risk.vals <- terra::values(risk.stack.sc)
risk.vals.nona <- na.omit(risk.vals)
colnames(risk.vals) <- rast.names

cor.risk <- cor(risk.vals.nona)
heatmap(cor.risk)


# add country information
countries <- rnaturalearth::ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

world.2 <- countries %>%
  sf::st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

  # sr to sf
risk.stars <- stars::st_as_stars(risk.stack.sc)
risk.stack.sp.sf = sf::st_as_sf(risk.stack)
crs(risk.stack.sp.sf) == crs(world.2)


essai <- risk.stack.sc %>% terra::subset(., 1)
essai2 <- terra::extract(essai, points, xy=TRUE)
essai2 <- terra::crds(essai, na.rm=TRUE)
essai2 <- terra::crds(risk.stack.sc, na.rm=TRUE, na.all=TRUE)


# intersecting points with country
# Convert the SpatRaster to a SpatialPointsDataFrame
risk.stack_df <- as.data.frame(risk.stack)


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


