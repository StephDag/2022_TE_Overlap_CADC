# mapping raster of hazards, exposure, vulnerab at low resolution
# Stephanie D'Agata
# Sept 2023

# vulnerability frameword of IPCC 2019: hazards, exposure, contextual vulnerability

  # Ohara et al,  2021 - https://www.science.org/doi/full/10.1126/science.abe6731
  # already associate species with pressures: maps of at risk marine biodiversity, including all taxa, and all pressure
  # a good proxy of both hazard and a part of exposure linked to ecosystem integrity at 1km resolution

## load data from figure 2 linked to github: https://github.com/oharac/bd_chi/tree/master
  # https://github.com/oharac/bd_chi/tree/master/_output/rasters/impact_maps

# species count
imp_count  <- rast(here("data","raw-data","Ohara2021_Science","impact_all_2013.tif"))
plot(imp_count)

#xfm <- function(x) log10(x) ### how are we transforming count?
#imp_ct_df <- rasterToPoints(imp_count) %>%
#  as.data.frame() %>%
#  setNames(c('x', 'y', 'imp_ct')) %>%
#  mutate(xfm_imp_ct = xfm(imp_ct),
#         xfm_imp_ct = ifelse(is.infinite(xfm_imp_ct), NA, xfm_imp_ct))

#max_ct <- max(imp_ct_df$imp_ct, na.rm = TRUE)
#ct_labels <- c(1, 10, 100, max_ct)
#ct_breaks <- xfm(ct_labels)

#land_sf <- read_sf(here('_spatial/ne_10m_land/ne_10m_land_no_casp.shp')) %>%
#  st_transform(crs(imp_count))

## Fig. 2B - % species

nspp <- rast(here("data","raw-data","Ohara2021_Science","n_spp_map.tif")) 
imp_pct <- imp_count / nspp
plot(imp_pct)
#imp_pct_df <- rasterToPoints(imp_pct) %>%
#  as.data.frame() %>%
#  rename(imp_pct = layer)

#land_sf <- read_sf(here('_spatial/ne_10m_land/ne_10m_land_no_casp.shp')) %>%
#  st_transform(crs(imp_pct))

## Fig. 2C - intensification

incr <- rast(here("data","raw-data","Ohara2021_Science","intens_all_incr2.tif"))
decr <- rast(here("data","raw-data","Ohara2021_Science","intens_all_decr2.tif"))
nspp <- rast(here("data","raw-data","Ohara2021_Science","n_spp_map.tif")) 

int_pct <- (incr - decr) / nspp
plot(int_pct)

#int_pct_df <- rasterToPoints(int_pct) %>%
#  as.data.frame() %>%
#  rename(int_pct = layer)

#land_sf <- read_sf(here('_spatial/ne_10m_land/ne_10m_land_no_casp.shp')) %>%
#  st_transform(crs(int_pct))

#### human population in coastal areas (100km, 100m elevation) from sedac
  # load the 100km coastal buffer
inlandWaters.100km <- readRDS(here::here("data","derived-data","inlandBuffer_100km.rds"))
inlandWaters.100km.vect <- vect(inlandWaters.100km)
inlandWaters.100km.vect <- project(inlandWaters.100km.vect,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  # load population raster 2015
variable = "UN WPP-Adjusted Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 2.5 arc-minutes"
pop.world.nc <- rast(here("data","raw-data","Word Population count  SEDAC 5km","gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc","gpw_v4_population_count_adjusted_rev11_2pt5_min.nc"))

#pop.world.nc <- here("data","raw-data","Word Population count  SEDAC 5km","gpw-v4-population-count-adjusted-to-2015-unwpp-country-totals-rev11_totpop_2pt5_min_nc","gpw_v4_population_count_adjusted_rev11_2pt5_min.nc")

  # the 4th raster is the count population for 2015: 4	Population Count, v4.11 (2015) (see doc)
#robin = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
pop.world <- pop.world.nc[[4]] 

  # robin
#pop.world.proj <- project(pop.world,"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  # molleid
pop.world.proj <- project(pop.world,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
plot(pop.world.proj)
 # check

#crs="+init=EPSG:4326"

  # intersect with 100km coastal buffer
      # check crs
#crs(pop.world.proj) == crs(inlandWaters.100km.vect)
#ext(pop.world.proj); ext(inlandWaters.100km.vect)
      # 
pop.world.nc.100km <- crop(pop.world.proj,ext(inlandWaters.100km.vect)+0.01)
pop.world.nc.100km.b <- mask(pop.world.nc.100km,inlandWaters.100km.vect)
#plot(pop.world.nc.100km.b)
#lines(inlandWaters.100km.vect)

  # elevation for each - to do but not now, because too long (22/09/2023)
        #all.gps.sp <- st_as_sf(all.merge.gps, coords = c("longitude","latitude"))
        #st_crs(all.gps.sp) = 4326
        # get elevation points
        # all.gps.spdf_elev_epqs <- get_elev_point(all.gps.sp, prj = prj_dd, src = "aws")

#ncin <- nc_open(pop.world.nc)
#print(ncin)
#names(ncin$var)
#variables = names(ncin[['var']])

  # extract centroid of each pixel of coastal population
values.xy <- as.data.frame(pop.world.nc.100km.b,xy = TRUE)
names(values.xy) <- c("lon","lat","pop.count")
sf_obj <- st_as_sf(values.xy, coords = c("lon", "lat"),crs=crs(imp_count))

# Define the point coordinates 
  # example with 1 point
# create a full funciton to apply lapply
  # the function: 1. find within the 120km buffer the closest imp_count cell, and 2. compute the mean imp.count and gravity in a 25km buffer around this point

point=sf_obj[sample(1:8428193,10),]
buffer=1200000
buffer_radius_25=250000
biodiv=imp_count

process_gps_point <- function(point,buffer,buffer_radius_25,biodiv) { # point = sf dataframe, buffer.search = in meters, buffer.impact in meters
#point <- sf_obj[1000206,]
#point <- point
#crs(point) == crs(biodiv)
#ext(point); ext(biodiv)

# Define the buffer radius in meters: larger than 100km to find the closest coastal gps point, from which the 25km buffer should start
#buffer <- buffer

# Create a buffer around the point of 120km
buffer_sf <- st_buffer(point, buffer)
#crs(buffer_sf) == crs(biodiv)

# crop imp_count with buffer
#biodiv=biodiv
imp_count.buffer <- crop(biodiv,ext(buffer_sf)+0.01)
imp_count.buffer.b <- mask(imp_count.buffer,buffer_sf)
#plot(imp_count.buffer.b)
#points(point)

# Get the index of the nearest cell to the point in the 120km buffer
  # full matrix of distance
dist <- distance(imp_count.buffer.b,point)
#plot(dist)

dist.crop <- crop(dist,imp_count.buffer.b)
dist.crop.b <- mask(dist.crop,imp_count.buffer)
#plot(dist.crop.b)
#points(point)

  # find the locations of the pixel with shortest distance with the selected human pixel
index <- where.min(dist.crop.b)

  # extract the gps coordinates of the point
xy.index <- xyFromCell(imp_count.buffer.b, index[2])
names(xy.index) <- c("lon","lat")
  
  #  transfrom to sf object
xy.index <- xy.index %>%
  as.data.frame %>% 
  sf::st_as_sf(coords = c(1,2),crs=crs(imp_count))

  # plot the point in red: closest count species point to population point
#points(xy.index,col="red",pch=2)

  # from this point, draw a 25km bufferand estimate the mean number of species (or other)
buffer_radius_25 <- buffer_radius_25
buffer_sf_25 <- st_buffer(xy.index, buffer_radius_25)

  # 25km buffer 
imp_count.buffer_25 <- crop(imp_count,ext(buffer_sf_25)+0.01)
imp_count.buffer.25.b <- mask(imp_count.buffer_25,buffer_sf_25)

  # plot
#plot(imp_count.buffer.25.b)

#points(point)
#points(xy.index,col="red")
#crs(buffer_sf_25) == crs(imp_count)

 # average values
#summary(values(imp_count.buffer.25.b))
mean.count <- mean(values(imp_count.buffer.25.b),na.rm=T);mean.count

 # gravity of 
mean.count.grav <- mean.count/as.numeric(index[3]^2);mean.count.grav

# return as a lits
data.frame(point = point, xy.index = xy.index, mean.count=mean.count,distance=as.numeric(index[3]), mean.count.grav = mean.count.grav)

}

results <- lapply(split(point, seq(nrow(point))), FUN=process_gps_point, buffer=buffer, buffer_radius_25=buffer_radius_25, biodiv=biodiv)

### load crop potential
as.data.frame(v)
  
#### load human dependance on marine ressources from selig
human.dep <- read.csv2(here("data","raw-data","Selig2019","Selig&al2019_Dependance_national_marine.csv"),sep=";",header=T)
human.dep <- human.dep[-1,]

