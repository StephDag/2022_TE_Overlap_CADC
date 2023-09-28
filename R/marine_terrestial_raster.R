# FUNction to estimate the mean gravity value of any marine raster around a buffer_radius of X, for any terrestrial pixel
# output: mean raster buffer X/(distance^sq)
# Stephanie D'Agata
# Sept 2023

  # point: gps point ( format)
  # buffer_ter: terrestrial buffer use to lower the search zone of the closest marine pixel
  # buffer_marine:marine buffer starting from the closest marine pixel
  # biodiv: marine raster

buffer_ter=150000 # 150km terrestrial raster
buffer_marine=20000 # 20km coastal raster
biodiv = imp_count

marine_terrestial_raster <- function(point,buffer_ter,buffer_marine,biodiv) { # point = sf dataframe, buffer.search = in meters, buffer.impact in meters
  #point <- sf_obj[1000206,]
  #point <- point
  #crs(point) == crs(biodiv)
  #ext(point); ext(biodiv)
  
  # Define the terrestrial buffer radius in meters: larger than 100km to find the closest coastal gps point, from which the XXkm marine buffer should start

  # Create a search buffer around the point of 120km
  buffer_sf <- st_buffer(point, buffer_ter)

  # change to actual gps grid
  buffer_sf <- st_transform(buffer_sf,crs=crs(biodiv))
  
  #mapview::mapview(buffer_sf) +
    mapview::mapview(point)
  
  #crs(buffer_sf) == crs(biodiv)
  
  # crop imp_count with buffer
  #biodiv=biodiv
  imp_count.buffer <- crop(biodiv,ext(buffer_sf)+1)
  
  imp_count.buffer.b <- mask(imp_count.buffer,buffer_sf)
  #plot(imp_count.buffer.b)
  #points(point)
  
  #library(stars)
  #library(mapview)
  #r = st_as_stars(imp_count.buffer.b)
  #mapview(r) +
  #mapview::mapview(point)

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
  
  # from this point, draw a XXkm marine buffer and estimate the mean number of species (or other)
  buffer_marine <- buffer_marine
  buffer_sf_marine <- st_buffer(xy.index, buffer_marine)
  
  # marine buffer 
  imp_count.buffer <- crop(imp_count,ext(buffer_sf_marine)+0.01)
  imp_count.buffer.b <- mask(imp_count.buffer,buffer_sf_marine)
  
  # plot
  #plot(imp_count.buffer.b)
  
  #points(point)
  #points(xy.index,col="red")
  #crs(buffer_sf_marine) == crs(imp_count)
  
  # average values
  #summary(values(imp_count.buffer.b))
  mean.count <- mean(values(imp_count.buffer.b),na.rm=T);mean.count
  
  # gravity of 
  mean.count.grav <- mean.count/as.numeric(index[3]^2);mean.count.grav
  
  # return as a lits
  data.frame(point = point, mean.count=mean.count,distance=as.numeric(index[3]), mean.count.grav = mean.count.grav)
  
}
