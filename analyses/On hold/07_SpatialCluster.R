# spatial cluster of initiatives

# load data CADC filter data
# 14/09/2023 - test on all GPS data, unfiltered - need to update the data with actual categorized dataset
all.gps.sp.100m.100km <- readRDS(here("data","derived-data","iati_GPS_db_100km_100m.rds"))

# load admin layers
    # National (level 0)
    # State/province/equivalent (level 1)
    # County/district/equivalent (level 2)
    # Commune/municipality/equivalent (level 3)
    # smaller Levels 4 and 5.

# install.packages("remotes")


### exemple Kenya
world_admlvl1_boundaries.KE <- geoboundaries(
  country = "Kenya",
  adm_lvl = "adm1",
  type = NULL,
  version = NULL,
  quiet = TRUE
)

#st_crs(world_admlvl1_boundaries.KE) <- robin
st_crs(world_admlvl1_boundaries.KE) <- 4326

#world_admlvl1_boundaries.KE <- world_admlvl1_boundaries.KE
crs(world_admlvl1_boundaries.KE)
#st_crs(world_admlvl1_boundaries.KE)$proj4string

### Province
#world_admlvl1_boundaries.KE <- spTransform(world_admlvl1_boundaries.KE, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#plot(germG, axes=T)

#st_crs(world_admlvl1_boundaries.KE) = robin

##### District
world_admlvl2_boundaries.KE <- gb_adm2(country = "Kenya", type = NULL, version = NULL, quiet = TRUE)
st_crs(world_admlvl2_boundaries.KE) <- 4326

world_admlvl3_boundaries.KE <- gb_adm3(country = "Kenya", type = NULL, version = NULL, quiet = TRUE)
st_crs(world_admlvl3_boundaries.KE) <- 4326

# load MPA database

  # filter for marine and terrestrial


# 
robin = CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# test: filter for Kenya
all.gps.sp.100m.100km.KE <- all.gps.sp.100m.100km %>%
  filter(iso_a2 == "KE")  %>%
  st_transform(crs=4326)

st_crs(all.gps.sp.100m.100km.KE) == st_crs(world_admlvl1_boundaries.KE)

  # plot
Kenya.map <- ggplot(world_admlvl2_boundaries.KE) +
  geom_sf() +
  geom_sf(data = all.gps.sp.100m.100km.KE,
          aes(geometry = geometry),
          size = 1,
          color = "lightblue") 
Kenya.map 

# st valid? make valid the country polygon
all(sf::st_is_valid(world_admlvl2_boundaries.KE))
world_admlvl2_boundaries.KE <- st_make_valid(world_admlvl2_boundaries.KE)
all(sf::st_is_valid(world_admlvl2_boundaries.KE))
# join the country polygon with points - on the 15/09/2023 = NAs due to points out to sea - no direct overlap with the country polygon 
    # maybe deal with the problem by hand later, once the final set of projects have been selected
KE.adm2 <- st_join(all.gps.sp.100m.100km.KE,world_admlvl2_boundaries.KE)

  # NAS- no overlap, out to sea
KE.adm2.NA <- KE.adm2 %>%
  filter(is.na(shapeName))

  # summarize the number of 
KE.adm2.n <- KE.adm2 %>%
  group_by(shapeName) %>%
  summarize(n = n())

Kenya.map.admin <- ggplot(world_admlvl2_boundaries.KE) +
  geom_sf() +
  geom_sf(data = KE.adm2,
          aes(geometry = geometry,color=as.factor(shapeName)),
          size = 1)  +
  guides(color=guide_legend(title="Districts")) +
  ggtitle("Kenya - Initiatives by district") +
  theme_bw()
Kenya.map.admin 

####### dbscan test 


# to estimate eps
kNNdistplot(st_coordinates(KE.adm2$geometry), k = 2)
abline(h=.15, col = "red", lty=2)

# exemple on what they consider a "knee"
#set.seed(2)
#n <- 400
#x <- cbind(
#   x = runif(4, 0, 1) + rnorm(n, sd = 0.1),
#   y = runif(4, 0, 1) + rnorm(n, sd = 0.1)
#   )
#true_clusters <- rep(1:4, time = 100)
#plot(x, col = true_clusters, pch = true_clusters)
#kNNdistplot(x, k = 2)
#abline(h=.06, col = "red", lty=2)

  # dbscan
KE.db <- dbscan(st_coordinates(KE.adm2$geometry), eps = 0.15, MinPts = 4)
KE.db
KE.db$cluster
KE.db$minPts

KE.adm2 <- cbind(KE.adm2,KE.db$cluster)
KE.adm2 <- KE.adm2 %>%
  mutate(KE.db.cluster <- as.factor(KE.db.cluster))

Kenya.map.db <- ggplot(world_admlvl2_boundaries.KE) +
  geom_sf() +
  geom_sf(data = KE.adm2,
          aes(geometry = geometry,color=as.factor(KE.db.cluster)),
          size = 1) +
  guides(color=guide_legend(title="dbscan clusters")) +
  ggtitle("Kenya - Initiatives by dbscan clusters") +
  theme_bw()
Kenya.map.db 
summary(as.factor(all.gps.sp.100m.100km.KE$KE.db.cluster))

  # hdbscan
KE.hdb <- hdbscan(st_coordinates(all.gps.sp.100m.100km.KE$geometry), minPts = 4)
KE.hdb

all.gps.sp.100m.100km.KE <- cbind(all.gps.sp.100m.100km.KE,KE.hdb$cluster)
all.gps.sp.100m.100km.KE <- all.gps.sp.100m.100km.KE %>%
  mutate(KE.hdb.cluster <- as.factor(KE.hdb.cluster))

Kenya.map.hdb <- ggplot(world_admlvl2_boundaries.KE) +
  geom_sf() +
  geom_sf(data = all.gps.sp.100m.100km.KE,
          aes(geometry = geometry,color=as.factor(KE.hdb.cluster)),
          size = 1) +
        guides(color=guide_legend(title="hdbscan clusters")) +
      ggtitle("Kenya - Initiatives by hdbscan clusters")
Kenya.map.hdb 

### check district/cluster
table(all.gps.sp.100m.100km.KE$KE.hdb.cluster,all.gps.sp.100m.100km.KE$shapeName)

# optics
KE.optics <- optics(st_coordinates(all.gps.sp.100m.100km.KE$geometry), minPts = 4)

# plot the ordering
plot(KE.optics)
plot(st_coordinates(all.gps.sp.100m.100km.KE$geometry), col = "grey")
polygon(st_coordinates(all.gps.sp.100m.100km.KE$geometry)[KE.optics$order, ])

# 
#KE.optics.cl <- extractDBSCAN(res, eps_cl = .1)
KE.optics.cl <- extractXi(res, xi = 0.07)
plot(KE.optics.cl)
hullplot(st_coordinates(all.gps.sp.100m.100km.KE$geometry), KE.optics.cl)

all.gps.sp.100m.100km.KE <- cbind(all.gps.sp.100m.100km.KE,KE.hdb$cluster)
all.gps.sp.100m.100km.KE <- all.gps.sp.100m.100km.KE %>%
  mutate(KE.hdb.cluster <- as.factor(KE.hdb.cluster))

Kenya.map.hdb <- ggplot(world_admlvl2_boundaries.KE) +
  geom_sf() +
  geom_sf(data = all.gps.sp.100m.100km.KE,
          aes(geometry = geometry,color=as.factor(KE.hdb.cluster)),
          size = 1)
Kenya.map.hdb 


optics(x, eps = NULL, minPts = 5, ...)

