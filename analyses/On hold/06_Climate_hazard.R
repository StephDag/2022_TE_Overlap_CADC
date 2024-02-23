# compute climate hazard composite index 
### Stephanie D'Agata, Nov 2023
### output: raster stack of climate change variables + composite index

# load climate raster from picc atlas
# https://interactive-atlas.ipcc.ch/regional-information#eyJ0eXBlIjoiQVRMQVMiLCJjb21tb25zIjp7ImxhdCI6MzE1MjgwMCwibG5nIjotMjQ5OTk4MCwiem9vbSI6NiwicHJvaiI6IkVQU0c6NTQwMzAiLCJtb2RlIjoiY29tcGxldGVfYXRsYXMifSwicHJpbWFyeSI6eyJzY2VuYXJpbyI6InNzcDEyNiIsInBlcmlvZCI6Im5lYXIiLCJzZWFzb24iOiJ5ZWFyIiwiZGF0YXNldCI6IkNNSVA2IiwidmFyaWFibGUiOiJ0YXMiLCJ2YWx1ZVR5cGUiOiJBTk9NQUxZIiwiaGF0Y2hpbmciOiJTSU1QTEUiLCJyZWdpb25TZXQiOiJhcjYiLCJiYXNlbGluZSI6IkFSNiIsInJlZ2lvbnNTZWxlY3RlZCI6WzI3XX0sInBsb3QiOnsiYWN0aXZlVGFiIjoicGx1bWUiLCJzaG93aW5nIjp0cnVlLCJtYXNrIjoibm9uZSIsInNjYXR0ZXJZTWFnIjoiQU5PTUFMWSIsInNjYXR0ZXJZVmFyIjoidGFzIn19

# 1. load rasters
  # mean SST changes - (2021-2040) vs ( 1995 - 2014)
mean.SST.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - Sea Surface Temperature (SST) Change deg C - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual (28 models).tiff"))
plot(mean.SST.change)

# mean pH changes - (2021-2040) vs ( 1995 - 2014)
mean.pH.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - pH at surface (pH) Change pH - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual (10 models).tiff"))
plot(mean.pH.change) # inverse (less is more acidic)

# mean SLR changes - (2021-2040) vs ( 1995 - 2014)
mean.SLR.change  <- rast(here("data","raw-data","IPCC_Climate","CMIP6 - Sea level rise (SLR) Change meters - Near Term (2021-2040) SSP2-4.5 (rel. to 1995-2014) - Annual .tiff"))
plot(mean.SLR.change)

# marine heatwaves


# cyclones tracks
specie.grav  <- rast(here("data","derived-data","Spatial rasters","sp.count.rast.ter.grav.tif"))
plot(specie.grav)

# 2. normalize between 0 and 1 for each raster
# standardized - min-max
source(here("R","NormMinMax.R"))

# SST changes
mean.SST.change.norm <- app(mean.SST.change,fun = normalize,na.rm=T)
plot(mean.SST.change.norm)

# mean pH Changes
mean.pH.change.norm <- app(-mean.pH.change,fun = normalize,na.rm=T)
plot(mean.pH.change.norm)

# mean SLR changes
mean.SLR.change.norm <- app(mean.SLR.change,fun = normalize,na.rm=T)
plot(mean.SLR.change.norm)

###### composite score
mean.cc.score <- mosaic(mean.SST.change.norm, mean.pH.change.norm, mean.SLR.change.norm,fun = "sum")

  # project to match population
mean.cc.score.proj <- project(mean.cc.score,sf_obj)
crs(mean.cc.score.proj) == crs(sf_obj)
distance(cbind(0,1), cbind(0,2), lonlat=TRUE)/1000

# 


