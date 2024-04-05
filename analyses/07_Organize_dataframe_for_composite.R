# compute climate hazard composite index 
### Stephanie D'Agata, Nov 2023
### updates: Steph D'Agata Feb 2024
### output: Create df to compute contextual inequity composite index

library(here)
source(here::here("analyses","00_setup.R"))

source(here::here("analyses","001_Coastal_countries.R"),echo=T)

# # coastal countries shapefile
# # Get the country boundaries data - sf dataframe
# countries <- ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories
# 
# # filter by coastal countries
# countries.shp.coastal <- countries %>%
#   filter(iso_a2 %in% coastal.ctr$iso2)
# dim(coastal.ctr)
# dim(countries.shp.coastal)
# # 
# # # load df file from Camille
# # riskstack_countries_df <- read.delim2(here::here("data","derived-data","riskstack_countries_df.txt"))
# # dim(riskstack_countries_df) # 7,783,171      17
# # summary(as.data.frame(riskstack_countries_df)) # 7,783,171      17
# # riskstack_countries_df$povmap.grdi.v1 %>% as.numeric %>% summary()
# 
# # # load raster sc
 risk.stack.sc.1 <- terra::rast(here("data","derived-data","Spatial rasters","risk.stack_sc_1.tif"))
 names(risk.stack.sc.1) <- c("mean.count.grav.V2","povmap.grdi.v1",
                             "Nutritional.dependence","Economic.dependence",
                             "Voice_account","Political_stab")


 risk.stack.sc.2 <- terra::rast(here("data","derived-data","Spatial rasters","risk.stack_sc_2.tif"))
 names(risk.stack.sc.2) <- c("Gov_effect","Reg_quality","Rule_law","control_corr",
                             "disaster_prep","pop.lla.10m","gender.ineq")

# # # # country sf information
# world.2 <- countries %>%
#   st_transform(crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# 
# world.2$economy %>% as.factor() %>% summary()
# world.2$income_grp %>% as.factor() %>% summary()
#  
# # # # left join by rownames
# # df.risk.stack.sc.ctry <- df.risk.stack.sc.ctry %>%
# #     mutate(ID = rownames(df.risk.stack.sc.ctry))
# 
# # 
# # df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.2,df.risk.stack.sc.ctry, by="ID")
# # dim(df.risk.stack.sc.ctry.full)
# # 
# # # # reload stack
 rm(df.risk.stack.sc.ctry.full)
 df.risk.stack.sc.ctry.full <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.rds"))
  df.risk.stack.sc.ctry.full <- df.risk.stack.sc.ctry.full %>%
    dplyr::select(ID:region_wb)
 names(df.risk.stack.sc.ctry.full); gc()

# # add the other layers
# names(risk.stack.sc)
# # extract xy from 1st layer and transform it to dataframe
df.risk.stack.sc.temp.0 <- terra::as.data.frame(risk.stack.sc.1[[c(1,2)]],xy=T); gc()
df.risk.stack.sc.temp <- terra::as.data.frame(risk.stack.sc.1[[c(3,4)]],xy=T); gc()
df.risk.stack.sc.temp.1 <- terra::as.data.frame(risk.stack.sc.1[[c(5,6)]],xy=T); gc()
df.risk.stack.sc.temp.2 <- terra::as.data.frame(risk.stack.sc.2[[c(1,2)]],xy=T); gc()
df.risk.stack.sc.temp.3 <- terra::as.data.frame(risk.stack.sc.2[[c(3,4)]],xy=T); gc()
df.risk.stack.sc.temp.4 <- terra::as.data.frame(risk.stack.sc.2[[c(5,6,7)]],xy=T); gc()


# lla 
risk.stack.lla <- rast(here("data","derived-data","Spatial rasters","risk.stack.lla.tif"))
df.risk.stack.lla <- terra::as.data.frame(risk.stack.lla,xy=T); gc()

  # create new pop vector based on merit data
df.risk.stack.lla <- df.risk.stack.lla %>%
  mutate(merit.10m.pop = pop.world.coastal) %>%
  mutate(merit.10m.pop = ifelse(merit_leczs > 10,0,merit.10m.pop)) 
df.risk.stack.lla <- df.risk.stack.lla %>%
  mutate(ID = rownames(df.risk.stack.lla))

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
 # df.risk.stack.sc.ctry.full$Voice_account.x <- NULL
 # df.risk.stack.sc.ctry.full$Political_stab.x <- NULL
 # names(df.risk.stack.sc.ctry.full)[177:178] <- gsub(".y","",names(df.risk.stack.sc.ctry.full)[177:178])
 # 
 
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
 
 # modify llo population
 # rm old one
 df.risk.stack.sc.ctry.full <- readRDS(here("data","derived-data","Spatial rasters","sf_df.risk_stack_ctry.rds"))
 
 df.risk.stack.sc.ctry.full <- df.risk.stack.sc.ctry.full %>%
   select(-pop.lla.10m)
 
 # add the new one
 df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.lla %>% select(-x,-y,-merit_leczs,-pop.world.coastal), by="ID")
 saveRDS(df.risk.stack.sc.ctry.full,here("data","derived-data","Spatial rasters","sf_df.risk_stack_ctry.rds"))
 
 names(df.risk.stack.sc.ctry.full)
 summary(df.risk.stack.sc.ctry.full)
 
 # rm old one
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind.coastal %>%
  select(-pop.lla.10m)

# add the new one
df.risk.stack.sc.ctry.ind.coastal <- left_join(df.risk.stack.sc.ctry.ind.coastal,df.risk.stack.lla %>% select(-x,-y,-merit_leczs,-pop.world.coastal), by="ID")
saveRDS(df.risk.stack.sc.ctry.ind.coastal,here("data","derived-data","df.cont.inequity.compo.coastal.rds"))

names(df.risk.stack.sc.ctry.ind.coastal)
summary(df.risk.stack.sc.ctry.ind.coastal)

# # load df ctry
 df.risk.stack.sc.ctry.full <- readRDS(here("data","derived-data","Spatial rasters","sf_df.risk_stack_ctry.rds"))

# select only variables, country/eez names, remove all geometry to work on a simplified dataframe for the rest of the computation (and clean the space)
df.risk.stack.sc.ctry.ind <- df.risk.stack.sc.ctry.full %>%
  st_drop_geometry(x) %>%
  select(ID,sovereignt,name,iso_a2,iso_a3,economy,income_grp,
         continent,region_un,subregion,region_wb,
         mean.count.grav.V2,povmap.grdi.v1,Nutritional.dependence,Economic.dependence,
         Voice_account,Political_stab,Gov_effect,Reg_quality,Rule_law,control_corr,disaster_prep,pop.lla.10m,gender.ineq)

# coastal
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind %>%
  filter(iso_a3 %in% countries.shp.coastal$iso_a3)

df.risk.stack.sc.ctry.ind.coastal %>% filter(iso_a3 == "TLS")

# reverse governance indicators
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind.coastal %>%
   mutate(Voice_account = 1-Voice_account) %>%
   mutate(Political_stab = 1-Political_stab) %>%
   mutate(Gov_effect = 1-Gov_effect) %>%
   mutate(Reg_quality = 1-Reg_quality) %>%
   mutate(Rule_law = 1-Rule_law) %>%
   mutate(control_corr = 1-control_corr)
 
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
  select(iso_a3) %>% unique() %>% dim() # 125 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(pop.lla.10m)) %>%
  select(iso_a3) %>% unique() %>% dim() # 178 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(disaster_prep)) %>%
  select(iso_a3) %>% unique() %>% dim() # 104 countries

df.risk.stack.sc.ctry.ind.coastal %>% filter(!is.na(control_corr)) %>%
  select(iso_a3) %>% unique() %>% dim() # 178 countries

# add the new coastal pop index (05/04/2024)
# save clean df
df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.rds"))

# rm old one
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind.coastal %>%
  select(-pop.lla.10m)

# add the new one
df.risk.stack.sc.ctry.ind.coastal <- left_join(df.risk.stack.sc.ctry.ind.coastal,df.risk.stack.lla %>% select(-x,-y,-merit_leczs,-pop.world.coastal), by="ID")
saveRDS(df.risk.stack.sc.ctry.ind.coastal,here("data","derived-data","df.cont.inequity.compo.coastal.rds"))

names(df.risk.stack.sc.ctry.ind.coastal)
summary(df.risk.stack.sc.ctry.ind.coastal)
