
library(here)
source(here("analyses","00_setup.R"))

source(here("analyses","001_Coastal_countries.R"))
source(here("R","NormMinMax.R"))
rm(cities, codelist, ctr, ctr.iati, ctr.region, eez)
# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- rnaturalearth::ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)


# add country information
world.2 <- countries.shp.coastal %>%
  st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


risk.stack.steph <- terra::rast(here::here("data/derived-data/Spatial rasters/risk.stack_sc_steph.tif"))

# 
# 
# # extract coordinates from non-na cells:
# 
# rs_xy <- terra::crds(risk.stack.steph, na.rm=TRUE, na.all=TRUE) %>% 
#   as.data.frame()
# # turn into a sf 
# rs_sf <- sf::st_as_sf(rs_xy, coords = c("x", "y"), crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# # intersect: but we need the rs values.
# 



# create a smaller bbox, crop the risk stack with it, add coordinates, remove NAs, merge all.


mosaic_rasterstack <- function(xmin1, xmax1, ymin1, ymax1){
  # bbox <- sf::st_bbox(c(xmin = -18040096, ymin =  0, 
  #                       xmax =    0, ymax = 9020048),
  #                     crs = sf::st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  bbox <- sf::st_bbox(c(xmin = xmin1, ymin =  ymin1, 
                        xmax =    xmax1, ymax = ymax1),
                      crs = sf::st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  bbox <- sf::st_as_sf(sf::st_as_sfc(bbox))
  
  bbox_v <- terra::vect(bbox)
  
  essai <- terra::crop(risk.stack.steph, bbox_v)
  
  x=essai[] # to extract values from raster
  
  essai_xy <- terra::xyFromCell(essai, 1:terra::ncell(essai))
  
  b1 <- cbind(essai_xy, x)
  
  
  # remove NAs
  
  miaou <- apply(b1[, 3:ncol(b1)], 1, function(k){
    all(is.na(k))
  })
  
  b2 <- b1[which(miaou ==FALSE),]
  
  return(b2)
}

# topleft <- mosaic_rasterstack(-18040096, 0, 0, 9020048)
# topright <- mosaic_rasterstack(0, 18040096, 0, 9020048) rossie start 15h39
# bottomright <- mosaic_rasterstack(0, 18040096, -9020048, 0) # rossie start 15h42
# bottomleft <- mosaic_rasterstack(-18040096, 0, -9020048, 0) # rossie start 15h44

# world.2 can be cropped too:
crop_by_bbox <- function(shape, xmin1, xmax1, ymin1, ymax1){
  bbox <- sf::st_bbox(c(xmin = xmin1, ymin =  ymin1, 
                        xmax =    xmax1, ymax = ymax1),
                      crs = sf::st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  bbox <- sf::st_as_sf(sf::st_as_sfc(bbox))
  
  bbox_v <- terra::vect(bbox)
  shape <- terra::vect(shape)
  
  if (terra::crs(shape) == terra::crs(bbox_v)){
    essai <- terra::crop(shape, bbox_v)
  }else{
    stop("crs don't match")
  }
  return(essai)
}
w.topleft <- crop_by_bbox(world.2, -18040096, 0, 0, 9020048) %>%
  sf::st_as_sf()
w.topright <- crop_by_bbox(world.2, 0, 18040096, 0.1, 9020048)%>% # for some reason doesn't work on Rossie
  sf::st_as_sf()
w.bottomright <- crop_by_bbox(world.2, 0, 18040096, -9020048, 0)%>%
  sf::st_as_sf()
w.bottomleft <- crop_by_bbox(world.2, -18040096, 0, -9020048, 0)%>%
  sf::st_as_sf()



# let's just try it out on a small part:

# 
# alb <- world.2 %>% dplyr::filter(name_en == "Albania")
# alb <- st_transform(alb, crs=4326)
# alb_pts <- data.frame(x=c(19, 20, 20 ), y=c(40, 41, 42 )) %>%
#   sf::st_as_sf(., coords=c("x", "y"), crs=4326, remove=FALSE)
# ggplot(data=alb) + geom_sf() + geom_point(data=alb_pts, aes(x = x, y=y))
# 




# then intersect with the countries

bottomleft_sf <- bottomleft %>%
  as.data.frame() %>%
  st_as_sf(.,  coords = c("x", "y"), 
           remove=FALSE, 
           crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs" )

sf::st_crs(world.2) == sf::st_crs(bottomleft_sf)

Sys.time()
topright_countries <- sf::st_intersection(topright_sf, w.topright %>% dplyr::select(name_en, geometry))
Sys.time()
# 15h20 début Rossinante topleft
# 
# 
head(world.2)

w2 <- world.2[ , c("name", "geometry") ]

b2 <- as.data.frame(b1[1400274:1400350,])
b2$mean.count.grav.V2.log %>%
  is.na %>%
  sum


bloo <- which(!is.na(b2$mean.count.grav.V2.log))
bloo %>% head(50)


k = b2[2, 3:15]







