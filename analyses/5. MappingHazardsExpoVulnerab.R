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
  # source the marine_terrestial_raster.r function: 1. find within the 120km buffer the closest imp_count cell, and 2. compute the mean imp.count and gravity in a 25km buffer around this point
  source(here("R","marine_terrestial_raster.r"))

  # inputs
point=sf_obj[sample(1:8428193,1),]
buffer_ter=120000
buffer_marine=20000
biodiv=imp_count

results.sp.risk <- mclapply(split(point, seq(nrow(point))), FUN=marine_terrestial_raster, buffer_ter=buffer_ter, buffer_marine=buffer_marine, biodiv=biodiv,mc.cores=4)

#### load human dependance on marine ressources from selig
human.dep <- read.csv2(here("data","raw-data","Selig2019","Selig&al2019_Dependance_national_marine.csv"),sep=";",header=T)
human.dep <- human.dep[-1,]

##### Exposure to MPAs
  # load MPA data
MPA.0 <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_0","WDPA_WDOECM_Sep2023_Public_marine_shp-polygons.shp"))
MPA.1 <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_1","WDPA_WDOECM_Sep2023_Public_marine_shp-polygons.shp"))
MPA.2 <- st_read(here::here("data","raw-data","WDPA_WDOECM_Sep2023_Public_marine_shp","WDPA_WDOECM_Sep2023_Public_marine_shp_2","WDPA_WDOECM_Sep2023_Public_marine_shp-polygons.shp"))

  # filter for marine, and marine + terrestrial - and nationally 
#DESIG_TYPE == "National" #
#STATUS == "Established" # Proposed, Inscribed, Adopted, Designated,
#MARINE %in% c(1,2)  #  0 (predominantly or entirely terrestrial),  1 (Coastal: marine and terrestrial), and 2 (predominantly or entirely marine)

      # filter each, and merge into one shapefile
MPA.0.marine <- MPA.0 %>%
  filter(DESIG_TYPE == "National" & STATUS == "Established" & MARINE %in% c(1,2))
MPA.1.marine <- MPA.1 %>%
  filter(DESIG_TYPE == "National" & STATUS == "Established" & MARINE %in% c(1,2))
MPA.2.marine <- MPA.2 %>%
  filter(DESIG_TYPE == "National" & STATUS == "Established" & MARINE %in% c(1,2))

    # merge all shapefile
crs(MPA.0.marine) == crs(MPA.1.marine)
MPA.full <- rbind(MPA.0.marine, MPA.1.marine)
MPA.full <- rbind(MPA.full,MPA.2.marine)
dim(MPA.full)

    # MPA Molleid
MPA.full.proj <- MPA.full %>%
  st_transform(crs(sf_obj))

# function to retrieve MPA % area for each populated pixel

    # test of the function when MPA are at vincinity 
# select the biggest MPA and extract the extend box
    #which(MPA.full.proj$GIS_AREA > 108288)
    #st_bbox(MPA.full.proj[10,"geometry"])

#get the bounding box of the two x & y coordinates, make sfc
  #bounding_box <- st_bbox(MPA.full.proj[10,"geometry"]) %>% st_as_sfc()
# subset point of populated places
  #sf_points_subset <- st_intersection(sf_obj, bounding_box)
  #plot(sf_points_subset)

#dim(sf_points_subset)

  # sample X points to test the function
#point=sf_points_subset[sample(1:70865,5),]

point=sf_obj
buffer=1200000
buffer_radius_25=250000
mpa=MPA.full.proj

process_gps_point.mpa <- function(point,buffer,buffer_radius_2,mpa) { # point = sf dataframe, buffer.search = in meters, buffer.impact in meters
  #point <- sf_obj[1000206,]
  #point <- point
  #crs(point) == crs(mpa)
  #ext(point); ext(mpa)
  
  # Define the buffer radius in meters: larger than 100km to find the closest coastal gps point, from which the 25km buffer should start
  #buffer <- buffer
  
  # Create a buffer around the point of 120km
  buffer_sf <- st_buffer(point, buffer)
  #crs(buffer_sf) == crs(mpa)
  
  # crop imp_count with buffer - imp count is our support for marine raster at one km - not used after, only to find the closest marine gps point
  imp_count.buffer <- crop(imp_count,ext(buffer_sf)+0.01)
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
  
  # from this point, draw a 25km buffer and estimate the total area of MPA within the 25km buffer
  buffer_radius_2 <- buffer_radius_2
  buffer_sf <- st_buffer(xy.index, buffer_radius_25)

  # 25km buffer  on intersect with MPA
  mpa.buffer <- terra::intersect(vect(mpa),ext(buffer_sf)+0.01)
  
  # assess area of mpa in the buffer
  plot(mpa.buffer)
  points(xy.index,col="red")

  if (is.null(dim(mpa.buffer)) == T) {
    mpa.area = 0
  } else {
    mpa.area = expanse(mpa.buffer,unit="m")
  }
 
  # plot
 
  #points(point)
  #points(xy.index,col="red")
  #crs(mpa.buffer_25) == crs(imp_count)

  # gravity ofmpa
  mpa.grav <- mpa.area/as.numeric(index[3]^2);mpa.grav
  
  # return as a lits
  data.frame(point = point, xy.index = xy.index, mpa.area.buffer=mpa.area,distance=as.numeric(index[3]), mpa.grav = mpa.grav)
  
}

results <- lapply(split(point, seq(nrow(point))), FUN=process_gps_point.mpa, buffer=buffer, buffer_radius_2=buffer_radius_2, mpa=mpa)



