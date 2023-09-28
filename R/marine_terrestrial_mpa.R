
# test
#point <- sf_obj[sample(1:8428193,10),]
#buffer_ter=150000 # 150km
#buffer_marine=20000 # 20km
#mpa=MPA.full.proj
#biodiv=imp_count

marine_terrestrial_mpa <- function(point,buffer_ter,buffer_marine,mpa,biodiv) { # point = sf dataframe, buffer.search = in meters, buffer.impact in meters
  #point <- sf_obj[1000206,]
  #point <- point
  #crs(point) == crs(mpa)
  #ext(point); ext(mpa)
  
  # Define the buffer radius in meters: larger than 100km to find the closest coastal gps point, from which the 25km buffer should start
  #buffer <- buffer
  
  # Create a buffer around the point of 120km
  buffer_sf <- st_buffer(point, buffer_ter)
  #crs(buffer_sf) == crs(mpa)
  
  # crop imp_count with buffer - imp count is our support for marine raster at one km - not used after, only to find the closest marine gps point
  imp_count.buffer <- crop(biodiv,buffer_sf)
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
  buffer_marine <- buffer_marine
  buffer_sf_marine <- st_buffer(xy.index, buffer_marine)
  
  # 20km buffer  on intersect with MPA
  mpa.buffer <- terra::intersect(vect(mpa),buffer_marine)
  
  # assess area of mpa in the buffer
  #plot(mpa.buffer)
  #points(xy.index,col="red")
  
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
  data.frame(point = point, mpa.area.buffer=mpa.area,distance=as.numeric(index[3]), mpa.grav = mpa.grav)
  
}