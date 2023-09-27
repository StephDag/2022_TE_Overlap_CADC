





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



