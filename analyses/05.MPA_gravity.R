# Populated Terrestrial raster (2015 - 5km) of MPA gravity (based on MPA area in a buffer)
# outputs: 3 terrestrial raster of 5km resolution each with stadarized MPA gravity (0 -1 value)
# Stephanie D'Agata
# Sept 2023
# Updates: Sept 2023

#install.packages("here",dependencies=T)
source(here::here("analyses","00_setup.R"))

#### use of 10km biodiversity raster to retrieve closest coastal pixel
# ## Fig. 2A - species count
imp_count  <- rast(here("data","raw-data","Ohara2021_Science","impact_all_2013.tif"))
plot(imp_count)

#### load cropped 100km populated raster
pop.world.nc.100km.b <- rast(here("data","derived-data","pop.world.nc.100km.b_100km.tif"))
plot(pop.world.nc.100km.b)

# extract centroid of each pixel of coastal population
values.xy <- as.data.frame(pop.world.nc.100km.b,xy = TRUE)
names(values.xy) <- c("lon","lat","pop.count")
sf_obj <- st_as_sf(values.xy, coords = c("lon", "lat"),crs=crs(imp_count))

# load MPA database
MPA.full.proj <- readRDS(here("data","derived-data","MPA.full.mollweide.rds"))
plot(MPA.full.proj)
#   # source the marine_terrestial_raster.r function: 1. find within the 120km buffer the closest imp_count cell, and 2. compute the mean imp.count and gravity in a 25km buffer around this point
source(here::here("R","marine_terrestrial_mpa.R"))

##### run by chunks
# Define the size of each chunk
chunk_size <- 1000
#chunk_size <- 20

# number of rows of sf
chunks.groups <- rep(seq_len(ceiling(dim(sf_obj)[1]/ chunk_size)),each = chunk_size,length.out = dim(sf_obj)[1])

# split by chunks
sf_obj.chunks <- split(sf_obj,f = chunks.groups)

# check dim (should be 1000 for each, and the last one == 549)
lapply(sf_obj.chunks,dim)

# inputs
# exemple with 10 point
# point <- sf_obj[sample(1:8428193,1),]

# full populated raster
point=sf_obj
buffer_ter=150000 # 150km terrestrial raster
buffer_marine=20000 # 20km coastal raster
mpa=MPA.full.proj
biodiv=imp_count
n.cores=20
# standardized - min-max
source(here("R","NormMinMax.R"))

#################################
#         MPA gravity           #
#################################


mcf<-function(f){ function(...) {tryCatch({f(...)} , error=function(e) e)}}
biodiv=imp_count
rm(results.mpa.df.full)
results.mpa.df.full <- data.frame()

for (i in 1:length(sf_obj.chunks)){
  # for (i in c(1, 701)){
  print(paste("i=",i))
  point <- sf_obj.chunks[[i]]
  results.mpa.temp <- mclapply(split(point, seq(nrow(point))), FUN=marine_terrestrial_mpa, buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mpa=mpa,mc.cores=n.cores)
  results.mpa.df.temp <- do.call(rbind.data.frame, results.mpa.temp)
  results.mpa.df.full <- rbind(results.mpa.df.full,results.mpa.temp)
  # save dataframe in case of crash : is it what is taking a lots of ram? is it necessary?
  saveRDS(results.mpa.df.full, here("data","derived-data","results.mpa.gravity.chunks.rds"))
  
}

# compute MPA gravity
results.mpa.df <- results.mpa.df.full %>%
  mutate(mean.MPA.grav = mpa.area.buffer/((distance/1000)^2)) %>%
  mutate(mean.MPA.grav.log = log(mean.MPA.grav+1)) %>%
  mutate(mean.MPA.grav.log.norm = normalize(mean.MPA.grav.log,na.rm = TRUE))

# transform to sf 
results.mpa.grav.sf <- st_as_sf(x = results.mpa.df,                         
                                     geometry = results.mpa.df$point.geometry,
                                     crs = crs(imp_pct))
saveRDS(results.mpa.grav.sf, here("data","derived-data","results.mpa.grav.sf.rds"))

# rasterize 
results.mpa.rast.ter<-st_rasterize(results.mpa.grav.sf %>% dplyr::select(mean.MPA.grav.log.norm, geometry),
                                st_as_stars(st_bbox(pop.world.nc.100km.b), dx=res(pop.world.nc.100km.b)[1],dy=res(pop.world.nc.100km.b)[2],
                                            values = NA_real_))
results.mpa.rast.ter <- rast(results.mpa.rast.ter)

# save as tiff
writeRaster(results.mpa.rast.ter, here("data","derived-data","results.mpa.rast.ter.gravity.tiff"),overwrite=T)
