# script to retrieve bathymetry /elevetion information for each gps coordinates

  # 26/07/2023 - test on GPS database - still not adding gps location from retrieving location in the narrative

# load raw data
all <- readRDS(here("data","derived-data","CADC.db.final.rds"))

# filter only GPS data
all.gps <- all %>% 
 filter(!is.na(latitude))  # 304252/473198

# elevation for all project with GPS location
library(elevatr)

prj_dd <- "EPSG:4326"

# Create an example SpatialPoints
all.gps.coord <- all.gps %>% select(longitude,latitude)
all.gps.sp <- SpatialPoints(all.gps.coord, proj4string = CRS(prj_dd))

# Create an example SpatialPointsDataFrame
all.gps.spdf <- SpatialPointsDataFrame(all.gps.sp, data = all.gps)

# get elevation points
all.gps.spdf_elev_epqs <- get_elev_point(all.gps.spdf, prj = prj_dd, src = "aws")
