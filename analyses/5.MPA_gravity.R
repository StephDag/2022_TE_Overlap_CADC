# Populated Terrestrial raster (2015 - 5km) of MPA gravity (based on MPA area in a buffer)
# outputs: 3 terrestrial raster of 5km resolution each
# Stephanie D'Agata
# Sept 2023
# Updates: Sept 2023
install.packages("here",dependencies=T)
source(here::here("analyses","00_setup.R"))

#### use of 10km biodiversity raster to retrieve closest coastal pixel
# ## Fig. 2A - species count
imp_count  <- rast(here("data","raw-data","Ohara2021_Science","impact_all_2013.tif"))
plot(imp_count)

#### load ropped 100km populated raster
pop.world.nc.100km.b <- rast(here("data","derived-data","pop.world.nc.100km.b_100km.tif"))
plot(pop.world.nc.100km.b)

# extract centroid of each pixel of coastal population
values.xy <- as.data.frame(pop.world.nc.100km.b,xy = TRUE)
names(values.xy) <- c("lon","lat","pop.count")
sf_obj <- st_as_sf(values.xy, coords = c("lon", "lat"),crs=crs(imp_count))

# load MPA database
MPA.full.proj <- readRDS(here("data","derived-data","MPA.full.mollweide.rds"))

#   # source the marine_terrestial_raster.r function: 1. find within the 120km buffer the closest imp_count cell, and 2. compute the mean imp.count and gravity in a 25km buffer around this point
source(here::here("R","marine_terrestial_mpa.R"))

# inputs
# exemple with 10 point
#point <- sf_obj[sample(1:8428193,1),]
# full populated raster
point=sf_obj
buffer_ter=150000 # 150km terrestrial raster
buffer_marine=20000 # 20km coastal raster
mpa=MPA.full.proj
biodiv=imp_count
n.cores=6
# standardized - min-max
source(here("R","NormMinMax.R"))

#################################
#         MPA gravity           #
#################################

results.mpa <- mclapply(split(point, seq(nrow(point))), FUN=marine_terrestrial_mpa, buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mc.cores=n.cores,mpa=mpa)
results.mpa.df <- do.call(rbind.data.frame, results.mpa)

#
results.mpa.df <- results.mpa.df %>%
  mutate(mean.count.grav.norm = normalize(log(results.mpa.df$mpa.grav+1)))

# transform to sf 
results.mpa.sf <- st_as_sf(x = results.mpa.df,                         
                                     geometry = results.mpa.df$point.geometry,
                                     crs = crs(imp_pct))

# rasterize 
results.mpa.rast.ter<-st_rasterize(results.mpa.sf %>% dplyr::select(mean.count.grav.norm, geometry),
                                st_as_stars(st_bbox(pop.world.nc.100km.b), dx=res(pop.world.nc.100km.b)[1],dy=res(pop.world.nc.100km.b)[2],
                                            values = NA_real_))
results.mpa.rast.ter <- rast(results.mpa.rast.ter)

# save as tiff
writeRaster(results.mpa.rast.ter, here("data","derived-data","results.mpa.rast.ter.gravity.tiff"),overwrite=T)



