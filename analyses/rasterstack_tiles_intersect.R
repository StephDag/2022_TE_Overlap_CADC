

sf::sf_use_s2(FALSE)
sf::st_make_valid()

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

# load the risk raster stack updated by Steph
risk.stack.steph <- terra::rast(here::here("data/derived-data/Spatial rasters/risk.stack_sc_steph.tif"))


# Intersect risk.stack with the world.2 coastal countries to identify which pixels fall in which countries
# pb: rasters are too big --> crop them into smaller chuncks. Here: 4 equal sized boxes:


# create a smaller bbox, crop the risk stack with it, add coordinates, remove NAs, merge all.
mosaic_rasterstack <- function(xmin1, xmax1, ymin1, ymax1){
  # create bbox
  bbox_v <- sf::st_bbox(
    c(  xmin = xmin1, ymin =  ymin1,  
        xmax =    xmax1, ymax = ymax1),
    crs = sf::st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")) %>%
    # turn into sfc object
    sf::st_as_sfc() %>% 
    # transform into terra vector file
    terra::vect() 
  
  # crop through all layers of risk.stack with the bbox to get a SpatRaster
  essai <- terra::crop(risk.stack.steph, bbox_v)
  # extract values from raster
  x=essai[] 
  # extract xy coordinates from raster
  essai_xy <- terra::xyFromCell(essai, 1:terra::ncell(essai)) 
  # bind together in a df
  b1 <- cbind(essai_xy, x) 
  
  # remove NAs
  miaou <- apply(b1[, 3:ncol(b1)], 1, function(k){
    all(is.na(k))
  })
  b2 <- b1[which(miaou ==FALSE),]
  
  return(b2)
}

# xmin1=-18040096
# xmax1=0
# ymin1=0
# ymax1=9020048

# topleft <- mosaic_rasterstack(-18040096, 0, 0, 9020048)
# topright <- mosaic_rasterstack(0, 18040096, 0, 9020048)
# bottomright <- mosaic_rasterstack(0, 18040096, -9020048, 0) 
# bottomleft <- mosaic_rasterstack(-18040096, 0, -9020048, 0)





# Crop world.2 coastal country data by the bbox too so that the intersect in between 2 smaller objects :
crop_by_bbox <- function(sf_shape, xmin1, xmax1, ymin1, ymax1){
  # create bbox and translate into sf object
  bbox_v <- sf::st_bbox(
    c(xmin = xmin1, ymin =  ymin1,  xmax =    xmax1, ymax = ymax1),
    crs = sf::st_crs("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")) %>%
    st_as_sfc() %>% 
    st_as_sf() %>%
    sf::st_make_valid()
    
  
  
  if(sf::st_crs(bbox_v) == sf::st_crs(sf_shape)){
    essai <- sf::st_crop(sf_shape %>% sf::st_make_valid(), bbox_v) %>%
      sf::st_make_valid()
  }else{
    stop("crs don't match")
  }
return(essai)
}
  
sf_shape <- world.2

xmin1=0
xmax1=18040096
ymin1=0
ymax1=9020048

w.topleft2 <- crop_by_bbox(world.2, -18040096, 0, 0, 9020048) %>%
  sf::st_as_sf() 
w.topright <- crop_by_bbox(world.2, 0, 18040096, 0, 9020048) %>% # for some reason doesn't work on Rossie
  sf::st_as_sf()
w.bottomright <- crop_by_bbox(world.2, 0, 18040096, -9020048, 0)%>%
  sf::st_as_sf()
w.bottomleft <- crop_by_bbox(world.2, -18040096, 0, -9020048, 0)%>%
  sf::st_as_sf()



# then intersect with the countries

topright_sf <- topright %>%
  as.data.frame() %>%
  st_as_sf(.,  coords = c("x", "y"), 
           remove=FALSE, 
           crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs" )

sf::st_crs(w.topright) == sf::st_crs(topright_sf)


# run this part on Rossinante, takes a long time
Sys.time()
topright_countries <- 
  sf::st_intersection(
    topright_sf, 
    w.topright %>% dplyr::select(name_en, geometry))  %>% 
  sf::st_make_valid() # this is just in case you get that stupid error with sf about geom not being valid
Sys.time()



# now read in the gpkg files:
topleft <- sf::st_read("data/derived-data/topleft.gpkg")
bottomleft <- sf::st_read("data/derived-data/bottomleft.gpkg")
bottomright <- sf::st_read("data/derived-data/bottomright.gpkg")

topleft_df <- as.data.frame(topleft)
bottomleft_df <- as.data.frame(bottomleft)
bottomright_df <- as.data.frame(bottomright)


mega <- rbind(topleft_df, bottomleft_df, bottomright_df)


