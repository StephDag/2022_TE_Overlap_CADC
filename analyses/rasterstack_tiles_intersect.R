
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

# try for the 1st col: extract values
# problem = some layers have more NAs, so if I use them 1 by 1 they won't have the right length.
small.stack <- terra::subset(risk.stack.steph, 1:2)

boo <- as.data.frame(small.stack[1:10,], xy=TRUE, na.rm=TRUE)

 
small.stack %>% terra::crds()


layers <- list()
Sys.time()
for (i in 1:2){
  layers[[i]] <- terra::values(risk.stack.steph[, i], na.rm=TRUE, dataframe=TRUE )
}
Sys.time()


apply(risk.stack.steph[,1:2], 2, )



# extract coordinates from non-na cells:

rs_xy <- terra::crds(risk.stack.steph, na.rm=TRUE, na.all=TRUE) %>% 
  as.data.frame()
# turn into a sf 
rs_sf <- sf::st_as_sf(rs_xy, coords = c("x", "y"), crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# intersect: but we need the rs values.
essai <- sf::st_intersection(rs_sf[1:5000,], world.2 %>% dplyr::select(name, geometry))




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
  
  x=essai[] # to extract values from ratser
  
  essai_xy <- terra::xyFromCell(essai, 1:terra::ncell(essai))
  
  b1 <- cbind(essai_xy, x)
  
  
  # remove NAs
  
  miaou <- apply(b1[, 3:ncol(b1)], 1, function(k){
    all(is.na(k))
  })
  
  b2 <- b1[which(miaou ==FALSE),]
  
  return(b2)
}

topleft <- b2



# then intersect with the countries:

topito <- head(topleft) %>%
  as.data.frame() %>%
  st_as_sf(.,  coords = c("x", "y"), crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs" )

topito_countries <- terra::intersect(topito, w2)
head(world.2)

w2 <- world.2[ , c("name", "geometry") ]

b2 <- as.data.frame(b1[1400274:1400350,])
b2$mean.count.grav.V2.log %>%
  is.na %>%
  sum


bloo <- which(!is.na(b2$mean.count.grav.V2.log))
bloo %>% head(50)


k = b2[2, 3:15]







