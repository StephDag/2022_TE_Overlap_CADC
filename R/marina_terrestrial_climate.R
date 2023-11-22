
# test
#point <- sf_obj[sample(1:8428193,10),]
buffer_ter=150000 # 150km
buffer_marine=100000 # 100km
climate=mean.cc.score.proj

st_crs(climate)
st_crs(point)



marine_terrestrial_climate <- function(point,buffer_ter,buffer_marine,climate) { # point = sf dataframe, buffer.search = in meters, buffer.impact in meters
  #point <- sf_obj[1000206,]
  #point <- point
  #crs(point) == crs(mean.cc.score.proj)
  #ext(point); ext(mean.cc.score)
  
  # Define the buffer radius in meters: larger than 100km to find the closest coastal gps point, from which the 25km buffer should start
  #buffer <- buffer
  
  # Create a buffer around the point of 120km
  buffer_sf <- st_buffer(point, buffer_ter)
  crs(buffer_sf) == crs(climate)

  # crop imp_count with buffer - imp count is our support for marine raster at one km - not used after, only to find the closest marine gps point
  imp_count.buffer <- terra::crop(climate,buffer_sf)
  imp_count.buffer.b <- terra::mask(imp_count.buffer,buffer_sf)
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
  
  # marine buffer 
  imp_count.buffer <- crop(climate,ext(buffer_sf_marine)+0.01)
  imp_count.buffer.b <- mask(imp_count.buffer,buffer_sf_marine)
  
  plot(imp_count.buffer.b)
  # assess area of mpa in the buffer
  #plot(mpa.buffer)
  #points(xy.index,col="red")
  
  # average values
  #summary(values(imp_count.buffer.b))
  mean.climate <- mean(values(imp_count.buffer.b),na.rm=T);mean.climate
  
  # gravity of 
  mean.climate.grav <- mean.climate/as.numeric(index[3]^2);mean.climate.grav

  # return as a list
  data.frame(point = point, mean.climate=mean.climate,marine.pt=xy.index,distance=as.numeric(index[3]), mean.climate.grav = mean.climate.grav)
  
}