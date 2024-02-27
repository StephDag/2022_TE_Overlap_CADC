# compute climate hazard composite index 
### Stephanie D'Agata, Nov 2023
### updates: Steph D'Agata Feb 2024
### output: Create df to compute contextual inequity composite index

library(here)
source(here::here("analyses","00_setup.R"))

source(here::here("analyses","001_Coastal_countries.R"),echo=T)

# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)
# 
# # load df file from Camille
# riskstack_countries_df <- read.delim2(here::here("data","derived-data","riskstack_countries_df.txt"))
# dim(riskstack_countries_df) # 7,783,171      17
# summary(as.data.frame(riskstack_countries_df)) # 7,783,171      17
# riskstack_countries_df$povmap.grdi.v1 %>% as.numeric %>% summary()

# # load raster sc
 risk.stack.sc <- terra::rast(here("data","derived-data","Spatial rasters","risk.stack_sc.tif"))

# #
# # # to spatial dataframe
#  df <- st_as_sf(x = df.risk.stack.sc,
#                 coords = c("x", "y"),
#                 crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# #
# # country sf information
# world.2 <- countries %>%
#   st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
#
# world.2$economy %>% as.factor() %>% summary()
# world.2$income_grp %>% as.factor() %>% summary()
#
# # # intersecting raster points with country
# df.risk.stack.sc.ctry <- st_intersection(df, world.2) # take very long
#
# # left join by rownames
# df.risk.stack.sc.ctry <- df.risk.stack.sc.ctry %>%
#     mutate(ID = rownames(df.risk.stack.sc.ctry))
# df.risk.stack.sc.2 <- df.risk.stack.sc.2 %>%
#   mutate(ID = rownames(df.risk.stack.sc.2))
#
# df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.2,df.risk.stack.sc.ctry, by="ID")
# dim(df.risk.stack.sc.ctry.full)
#
# # reload stack
rm(df.risk.stack.sc.ctry.full)
 df.risk.stack.sc.ctry.full <- readRDS(here("data","derived-data","Spatial rasters","df.risk_stack_ctry.rds"))
 df.risk.stack.sc.ctry.full <- df.risk.stack.sc.ctry.full %>%
   select(-mean.count.grav.V2.log.x,-povmap.grdi.v1.x,     
-Nutritional.dependence.x,-Economic.dependence.x,-mean.count.grav.V2.log.y,-povmap.grdi.v1.y,     
-Nutritional.dependence.y,-Economic.dependence.y,-Voice_account,-Political_stab,        
-Gov_effect,-Reg_quality,-Rule_law,-control_corr, -SLR_change)
names(df.risk.stack.sc.ctry.full); gc()
names(risk.stack.sc)

# # add the other layers
# names(risk.stack.sc)
# # extract xy from 1st layer and transform it to dataframe
  df.risk.stack.sc.temp.0 <- terra::as.data.frame(risk.stack.sc[[c(1,2)]],xy=T)
 df.risk.stack.sc.temp <- terra::as.data.frame(risk.stack.sc[[c(3,4)]],xy=T); gc()
 df.risk.stack.sc.temp.1 <- terra::as.data.frame(risk.stack.sc[[c(5,6)]],xy=T); gc()
 df.risk.stack.sc.temp.2 <- terra::as.data.frame(risk.stack.sc[[c(7,8)]],xy=T); gc()
 df.risk.stack.sc.temp.3 <- terra::as.data.frame(risk.stack.sc[[c(9,10)]],xy=T); gc()
 df.risk.stack.sc.temp.4 <- terra::as.data.frame(risk.stack.sc[[c(11,12,13)]],xy=T); gc()
# #
# # left join with main
 # gravity + poverty
 gc()
 df.risk.stack.sc.temp.0 <- df.risk.stack.sc.temp.0 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.0))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.0 %>% select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)
#   # 0
 df.risk.stack.sc.temp <- df.risk.stack.sc.temp %>%
   mutate(ID = rownames(df.risk.stack.sc.temp)); gc()
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp %>% select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)

# # 1
 df.risk.stack.sc.temp.1 <- df.risk.stack.sc.temp.1 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.1)); gc()
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.1 %>% select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)
 df.risk.stack.sc.ctry.full$Voice_account.x <- NULL
 df.risk.stack.sc.ctry.full$Political_stab.x <- NULL
 names(df.risk.stack.sc.ctry.full)[177:178] <- gsub(".y","",names(df.risk.stack.sc.ctry.full)[177:178])
 
 
# # 2
 df.risk.stack.sc.temp.2 <- df.risk.stack.sc.temp.2 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.2))
  df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.2 %>% select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)
#
# # clean memory
 rm(df.risk.stack.sc.temp,df.risk.stack.sc.temp.1,df.risk.stack.sc.temp.2)
#
# # 3
 df.risk.stack.sc.temp.3 <- df.risk.stack.sc.temp.3 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.3))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.3%>% select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full)
 rm(df.risk.stack.sc.temp.3)
# # 4
 df.risk.stack.sc.temp.4 <- df.risk.stack.sc.temp.4 %>%
   mutate(ID = rownames(df.risk.stack.sc.temp.4))
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.4 %>% select(-x,-y), by="ID")
 names(df.risk.stack.sc.ctry.full) 
 rm(df.risk.stack.sc.temp.4)

 # save sf dataframe - all countries, all variables
 saveRDS(df.risk.stack.sc.ctry.full,here("data","derived-data","Spatial rasters","sf_df.risk_stack_ctry.rds"))

# # load df ctry
 df.risk.stack.sc.ctry.full <- readRDS(here("data","derived-data","Spatial rasters","sf_df.risk_stack_ctry.rds"))

# select only variables, country/eez names, remove all geometry to work on a simplified dataframe for the rest of the computation (and clean the space)
df.risk.stack.sc.ctry.ind <- df.risk.stack.sc.ctry.full %>%
  st_drop_geometry(x) %>%
  select(ID,sovereignt,name,iso_a2,iso_a3,economy,income_grp,
         continent,region_un,subregion,region_wb,
         mean.count.grav.V2.log,povmap.grdi.v1,Nutritional.dependence,Economic.dependence,
         Voice_account,Political_stab,Gov_effect,Reg_quality,Rule_law,control_corr,disaster_prep,SLR_change,gender.ineq)

# coastal
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind %>%
  filter(iso_a3 %in% countries.shp.coastal$iso_a3)
summary(df.risk.stack.sc.ctry.ind.coastal)
# save clean df
saveRDS(df.risk.stack.sc.ctry.ind.coastal,here("data","derived-data","df.cont.inequity.compo.coastal.rds"))


# check number of countries
df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(povmap.grdi.v1)) %>%
  select(iso_a3) %>% unique() %>% dim() # 171 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(Nutritional.dependence)) %>%
  select(iso_a3) %>% unique() %>% dim() # 132 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(Economic.dependence)) %>%
  select(iso_a3) %>% unique() %>% dim() # 149 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(gender.ineq)) %>%
  select(iso_a3) %>% unique() %>% dim() # 113 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(SLR_change)) %>%
  select(iso_a3) %>% unique() %>% dim() # 148 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(disaster_prep)) %>%
  select(iso_a3) %>% unique() %>% dim() # 92 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(control_corr)) %>%
  select(iso_a3) %>% unique() %>% dim() # 178 countries

