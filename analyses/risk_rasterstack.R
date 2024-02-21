# Camille to Staphanie:
# the only interesting thing about this script is the correlation matrix between your risk variables, and the heatmap.
# but since the rasterstack was updated with more variables, it's too big now to use the na.omit command. You'd need to break it down.
# 


library(magrittr)

localpath = here::here("data", "derived-data", "Spatial rasters")

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


# updated risk.stack from Steph:
risk.stack.sc <- terra::rast(here::here("data/derived-data/Spatial rasters/risk.stack_sc_steph.tif" ))
rast.names <- names(risk.stack.sc )



# # correlation of normalized data (à faire après)
risk.vals <- terra::values(risk.stack.sc)

risk.stack_df <- as.data.frame(risk.stack.sc)     
risk.vals.nona <- na.omit(risk.vals) # this no longer works coz the vector is too big ...




cor.risk <- cor(risk.vals.nona)
heatmap(cor.risk)

save.image(here::here("data/derived-data/risk_stack_session.rds"))

