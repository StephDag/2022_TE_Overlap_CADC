# Populated Terrestrial raster (2015 - 5km) of species at risk count, percentage and trends
# outputs: 3 terrestrial raster of 5km resolution each
# Stephanie D'Agata
# Sept 2023
# Updates: Sept 2023

# vulnerability frameword of IPCC 2019: hazards, exposure, contextual vulnerability

  # Ohara et al,  2021 - https://www.science.org/doi/full/10.1126/science.abe6731
  # already associate species with pressures: maps of at risk marine biodiversity, including all taxa, and all pressure
  # a good proxy of both hazard and a part of exposure linked to ecosystem integrity at 1km resolution

## load data from figure 2 linked to github: https://github.com/oharac/bd_chi/tree/master
  # https://github.com/oharac/bd_chi/tree/master/_output/rasters/impact_maps

# ## Fig. 2A - species count
imp_count  <- rast(here("data","raw-data","Ohara2021_Science","impact_all_2013.tif"))
plot(imp_count)

## Fig. 2B - % species
nspp <- rast(here("data","raw-data","Ohara2021_Science","n_spp_map.tif")) 
imp_pct <- imp_count / nspp
plot(imp_pct)

## Fig. 2C - intensification
incr <- rast(here("data","raw-data","Ohara2021_Science","intens_all_incr2.tif"))
decr <- rast(here("data","raw-data","Ohara2021_Science","intens_all_decr2.tif"))
nspp <- rast(here("data","raw-data","Ohara2021_Science","n_spp_map.tif")) 

int_pct <- (incr - decr) / nspp
plot(int_pct)

#### human population in coastal areas (100km, 100m elevation) from sedac - in mollweid projection
  # load the 100km coastal buffer
inlandWaters.100km <- readRDS(here::here("data","derived-data","inlandBuffer_100km.rds"))
inlandWaters.100km.vect <- vect(inlandWaters.100km)
inlandWaters.100km.vect <- project(inlandWaters.100km.vect,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

  # load population raster 2015
variable = "UN WPP-Adjusted Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 2.5 arc-minutes"
pop.world.nc <- rast(here("data","raw-data","Word Population count  SEDAC 5km","gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc","gpw_v4_population_count_adjusted_rev11_2pt5_min.nc"))

  # the 4th raster is the count population for 2015: 4	Population Count, v4.11 (2015) (see doc)
pop.world <- pop.world.nc[[4]] 

  # mollweid
pop.world.proj <- project(pop.world,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
plot(pop.world.proj)
 # check

  # intersect with 100km coastal buffer
      # check crs and extent
#crs(pop.world.proj) == crs(inlandWaters.100km.vect)
#ext(pop.world.proj); ext(inlandWaters.100km.vect)

      # crop the populated raster to 100km buffer
pop.world.nc.100km <- crop(pop.world.proj,ext(inlandWaters.100km.vect)+0.01)
pop.world.nc.100km.b <- mask(pop.world.nc.100km,inlandWaters.100km.vect)
writeRaster(pop.world.nc.100km.b, here("data","derived-data","pop.world.nc.100km.b_100km.tif"),overwrite=TRUE)

#plot(pop.world.nc.100km.b)
#lines(inlandWaters.100km.vect)

  # extract centroid of each pixel of coastal population
values.xy <- as.data.frame(pop.world.nc.100km.b,xy = TRUE)
names(values.xy) <- c("lon","lat","pop.count")
sf_obj <- st_as_sf(values.xy, coords = c("lon", "lat"),crs=crs(imp_count))

# Define the point coordinates 
# create a full function to apply lapply
  # source the marine_terrestial_raster.r function: 1. find within the 120km buffer the closest imp_count cell, and 2. compute the mean imp.count and gravity in a 25km buffer around this point
  source(here("R","marine_terrestial_raster.r"))

  # inputs
    # exemple with 1 point
point=sf_obj[sample(1:8428193,5),]
#point=sf_obj
buffer_ter=120000 # 120km terrestrial raster
buffer_marine=20000 # 20km coastal raster

  # species at risk - count
biodiv=imp_count
results.sp.count.risk <- mclapply(split(point, seq(nrow(point))), FUN=marine_terrestial_raster, buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mc.cores=6)
results.sp.count.risk.df <- do.call(rbind.data.frame, results.sp.count.risk)
  # standardized - min-max
source(here("R","NormMinMax.R"))

results.sp.count.risk.df <- results.sp.count.risk.df %>%
  mutate(mean.count.grav.norm = normalize(log(results.sp.count.risk.df$mean.count.grav+1)))

  # full join with full db


  # transform to sf 
results.sp.count.risk.sf <- st_as_sf(x = results.sp.count.risk.df,                         
                                           geometry = results.sp.count.risk.df$point.geometry,
                                           crs = crs(imp_pct))

  # sp.count.raster
sp.count.rast.ter<-st_rasterize(results.sp.count.risk.sf %>% dplyr::select(mean.count.grav.norm, geometry),st_as_stars(st_bbox(pop.world.nc.100km.b),values = NA_real_),dx=res(pop.world.nc.100km.b)[1],dy=res(pop.world.nc.100km.b)[2],nx= ncol(pop.world.nc.100km.b),nrow= nrow(pop.world.nc.100km.b))
sp.count.rast.ter <- rast(sp.count.rast.ter)


test <- rast(results.sp.count.risk.sf %>% dplyr::select(geometry,mean.count.grav.norm), type="xyz",
             nrows=nrow(pop.world.nc.100km.b),ncol=ncol(pop.world.nc.100km.b),
              extent=ext(pop.world.nc.100km.b),resolution=res(pop.world.nc.100km.b),type="xyz")                                        

# export as tiff
writeRaster(sp.count.rast.ter, here("data","derived-data","sp.count.rast.ter.tif"),overwrite=TRUE)

  # species at risk - percentage
biodiv=imp_pct
results.sp.perc.risk <- mclapply(split(point, seq(nrow(point))), FUN=marine_terrestial_raster, buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mc.cores=20)

  # species at risk - intensification
biodiv=int_pct
results.sp.perc.risk <- mclapply(split(point, seq(nrow(point))), FUN=marine_terrestial_raster, buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mc.cores=20)

