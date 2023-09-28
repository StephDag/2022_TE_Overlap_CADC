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
point <- sf_obj[sample(1:8428193,10),]
# full populated raster
point=sf_obj
buffer_ter=150000 # 150km terrestrial raster
buffer_marine=20000 # 20km coastal raster
n.cores=8
# standardized - min-max
source(here("R","NormMinMax.R"))

