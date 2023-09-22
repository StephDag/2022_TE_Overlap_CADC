# 100km coastlines buffer 

library(rgdal)
library(rnaturalearth)
library(sf)
library(ggplot2)

robin = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
prj_dd <- "EPSG:4326"
st_crs(all.gps.sp.1) = 4326
# world
rm(ROI)
ROI = ne_countries(returnclass = 'sf',scale = "large") %>%
  st_combine() %>%
  st_transform(crs=robin) %>%
  st_make_valid()

# check valid to perform union
sf::st_is_valid(ROI)

# union countries
ROI.comb <- st_union(ROI)

# inland buffer 100km
rm(inlandWaters)
inlandWaters = ROI.comb %>%
  st_buffer(-100000) # 100km inland

#### loop to make buffer from 10 to 100km from the coast
buffer <- seq(10000,100000,10000)

st_crs(ROI.comb)$units

for (i in 1:length(buffer)) {
  # inland full buffer
  inlandWaters.temp = ROI.comb %>%
    st_buffer(-buffer[i])
  
  # intersection
  rm(inlandWaters.int.temp)
  inlandWaters.int.temp <- st_difference( ROI.comb,inlandWaters.temp)
  
  mapview::mapView(inlandWaters.int.temp)
  plot(inlandWaters.int.temp)

  # save buffer
  saveRDS(inlandWaters.int.temp,here::here("data","derived-data",paste0("inlandBuffer_",buffer[i]/1000,"km.rds")))
  
}

inlandWaters.100km <- st_difference( ROI.comb,coastalWaters)
mapview::mapView(inlandWaters.100km)

# test match crs to plot
st_crs(ROI.comb) == st_crs(inlandWaters.temp)

# plot inland with 
plot.inland <- ggplot() +
geom_sf(data = inlandWaters.100km) +
geom_sf(data = inlandWaters.100km, fill = "lightblue", col = "transparent") +
geom_sf(data = la_ams,
          aes(geometry = geometry),
          size = 3,
          color = "red")
plot.inland


