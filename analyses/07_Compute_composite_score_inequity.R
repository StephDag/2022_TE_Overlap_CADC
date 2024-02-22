# compute climate hazard composite index 
### Stephanie D'Agata, Nov 2023
### updates: Steph D'Agata Feb 2024
### output: composite index

library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)
source(here("R","NormMinMax.R"))
source(here("R","cbind_fill.R"))

# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

# filter by coastal countries
countries.shp.coastal <- countries %>%
  filter(iso_a2 %in% coastal.ctr$iso2)
dim(coastal.ctr)
dim(countries.shp.coastal)

# check with small countries are present
countries %>%
  filter(name_en == "American Samoa")

# load df file from Camille
riskstack_countries_df <- read.delim2(here::here("data","derived-data","riskstack_countries_df.txt"))
dim(riskstack_countries_df) # 7,783,171      17
summary(as.data.frame(riskstack_countries_df)) # 7,783,171      17
riskstack_countries_df$povmap.grdi.v1 %>% as.numeric %>% summary()

# # load raster sc
# risk.stack.sc <- terra::rast(here("data","derived-data","Spatial rasters","risk.stack_sc.tif"))
# 
# # extract xy from 1st layer and transform it to dataframe
# df.risk.stack.sc.2 <- terra::as.data.frame(risk.stack.sc[[c(1,2)]],xy=T)
# 
# # to spatial dataframe
# df <- st_as_sf(x = df.risk.stack.sc,                         
#                coords = c("x", "y"),
#                crs="+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
# 
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
# df.risk.stack.sc.ctry.full <- readRDS(here("data","derived-data","Spatial rasters","df.risk_stack_ctry.rds"))
# df.risk.stack.sc.ctry.full$SLR_change <- NULL
# 
# # test poverty # country
# df.risk.stack.sc.ctry.full$povmap.grdi.v1
# 
# gc()
# # add the other layers
# names(risk.stack.sc)
# df.risk.stack.sc.temp <- terra::as.data.frame(risk.stack.sc[[c(3,4)]],xy=T)
# df.risk.stack.sc.temp.1 <- terra::as.data.frame(risk.stack.sc[[c(5,6)]],xy=T); gc()
# df.risk.stack.sc.temp.2 <- terra::as.data.frame(risk.stack.sc[[c(7,8)]],xy=T); gc()
# df.risk.stack.sc.temp.3 <- terra::as.data.frame(risk.stack.sc[[c(9,10)]],xy=T); gc()
# df.risk.stack.sc.temp.4 <- terra::as.data.frame(risk.stack.sc[[c(11,12,13)]],xy=T); gc()
# # 
# # left join with main
#   # 0
# df.risk.stack.sc.temp <- df.risk.stack.sc.temp %>%
#   mutate(ID = rownames(df.risk.stack.sc.temp))
# df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp %>% select(-x,-y), by="ID")
# names(df.risk.stack.sc.ctry.full)
# 
# df.risk.stack.sc.ctry.full$mean.count.grav.V2.log.y <- NULL
# # 1
# df.risk.stack.sc.temp.1 <- df.risk.stack.sc.temp.1 %>%
#   mutate(ID = rownames(df.risk.stack.sc.temp.1))
# df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.1 %>% select(-x,-y), by="ID")
# names(df.risk.stack.sc.ctry.full)
# # 2
# df.risk.stack.sc.temp.2 <- df.risk.stack.sc.temp.2 %>%
#   mutate(ID = rownames(df.risk.stack.sc.temp.2))
# df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.2 %>% select(-x,-y), by="ID")
# names(df.risk.stack.sc.ctry.full)
# 
# # clean memory
# rm(df.risk.stack.sc.temp,df.risk.stack.sc.temp.1,df.risk.stack.sc.temp.2)
# 
# # 3
# df.risk.stack.sc.temp.3 <- df.risk.stack.sc.temp.3 %>%
#   mutate(ID = rownames(df.risk.stack.sc.temp.3))
# df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.3%>% select(-x,-y), by="ID")
# names(df.risk.stack.sc.ctry.full)
# rm(df.risk.stack.sc.temp.3)
# # 4
# df.risk.stack.sc.temp.4 <- df.risk.stack.sc.temp.4 %>%
#   mutate(ID = rownames(df.risk.stack.sc.temp.4))
# df.risk.stack.sc.ctry.full <- left_join(df.risk.stack.sc.ctry.full,df.risk.stack.sc.temp.4 %>% select(ID,SLR_change), by="ID")
# names(df.risk.stack.sc.ctry.full)
# rm(df.risk.stack.sc.temp.4,df.risk.stack.sc.temp.3,df.risk.stack.sc.temp.2,df.risk.stack.sc.temp.1,df.risk.stack.sc.temp)
# 
# names(df.risk.stack.sc.ctry.full) <- sub("_3.*", "", df1[,1])
# 
# saveRDS(df.risk.stack.sc.ctry.full,here("data","derived-data","Spatial rasters","df.risk_stack_ctry.rds"))

# load df ctry
df.risk.stack.sc.ctry.full <- readRDS(here("data","derived-data","Spatial rasters","df.risk_stack_ctry.rds"))

# clean variables
df.risk.stack.sc.ctry.full$Nutritional.dependence.x <- NULL
df.risk.stack.sc.ctry.full$Economic.dependence.x <- NULL
df.risk.stack.sc.ctry.full$Voice_account.x <- NULL; gc()
df.risk.stack.sc.ctry.full$Political_stab.x <- NULL; gc()
df.risk.stack.sc.ctry.full$Gov_effect.x <- NULL; gc()
df.risk.stack.sc.ctry.full$Reg_quality.x <- NULL; gc()
df.risk.stack.sc.ctry.full$Rule_law.x <- NULL; gc()
df.risk.stack.sc.ctry.full$control_corr.x <- NULL; gc()
df.risk.stack.sc.ctry.full$SLR_change.x <- NULL; gc()

# rm .y
names(df.risk.stack.sc.ctry.full)[177:185] <- gsub(".y","",names(df.risk.stack.sc.ctry.full)[177:185])
  # filter for coastal countries

# select only variables, country/eez names, remove all geometry to work on a simplified dataframe for the rest of the computation (and clean the space)
df.risk.stack.sc.ctry.ind <- df.risk.stack.sc.ctry.full %>%
  st_drop_geometry(x) %>%
  select(ID,sovereignt,name,iso_a2,iso_a3,economy,income_grp,
         continent,region_un,subregion,region_wb,
         mean.count.grav.V2.log,povmap.grdi.v1,Nutritional.dependence,Economic.dependence,
         Voice_account,Political_stab,Gov_effect,Reg_quali,Rule_law,control_corr,disaster_prep,SLR_change,gender.ineq) #%>%
        #drop_na()
# coastal
df.risk.stack.sc.ctry.ind.coastal <- df.risk.stack.sc.ctry.ind %>%
  filter(iso_a3 %in% countries.shp.coastal$iso_a3)

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

# create composite risk score ##### INDICE COMPOSITE with disaster prep
risk.mat <- df.risk.stack.sc.ctry.ind %>%
  select(mean.count.grav.V2.log,povmap.grdi.v1,Nutritional.dependence,Economic.dependence,
         Voice_account,Political_stab,Gov_effect,Reg_quali,Rule_law,control_corr,disaster_prep,SLR_change,gender.ineq)
df.risk.stack.sc.ctry.ind$risk.mat.score.sum.with.dprep <- apply(risk.mat, 1, sum)

# create composite risk score ##### INDICE COMPOSITE without disaster prep
rm(risk.mat)
risk.mat <- df.risk.stack.sc.ctry.ind %>%
  select(mean.count.grav.V2.log,povmap.grdi.v1,Nutritional.dependence,Economic.dependence,
         Voice_account,Political_stab,Gov_effect,Reg_quali,Rule_law,control_corr,SLR_change,gender.ineq)
df.risk.stack.sc.ctry.ind$risk.mat.score.sum.without.dprep <- apply(risk.mat, 1, sum)


length(unique(df.risk.stack.sc.ctry.ind.coastal$iso_a3)) #69 countries for all variables 
summary(df.risk.stack.sc.ctry.ind)
dim(df.risk.stack.sc.ctry.ind)

# plot distribution
  # n countries
ctry.length <- as.data.frame(df.risk.stack.sc.ctry.ind) %>% filter(!is.na(risk.mat.score.sum.without.dprep)) %>% select(name) %>% unique() 

p.risk.score.without.disaster <- ggplot(as.data.frame(df.risk.stack.sc.ctry.ind) %>% filter(!is.na(risk.mat.score.sum.without.dprep)), aes(x=reorder(name,-risk.mat.score.sum.without.dprep), y=risk.mat.score.sum.without.dprep/12)) + 
  geom_boxplot() +
  coord_flip() +
  ylim(0,1) +
  xlab("Countries") +
  ylab("Contextual Inequity Scores") +
  ggtitle("without disaster prep. - 95 coastal countries") +
  theme_bw()
p.risk.score.without.disaster
ggsave(here("figures","Contextual_Ineq_score_distrib_ctr_without_disaster_95ctries.png"),p.risk.score.without.disaster,height=12,width=12)

# with disaster
ctry.length <- as.data.frame(df.risk.stack.sc.ctry.ind) %>% filter(!is.na(risk.mat.score.sum.with.dprep)) %>% select(name) %>% unique() 

p.risk.score.with.disaster <- ggplot(as.data.frame(df.risk.stack.sc.ctry.ind) %>% filter(!is.na(risk.mat.score.sum.with.dprep)), aes(x=reorder(name,-risk.mat.score.sum.with.dprep), y=risk.mat.score.sum.with.dprep/13)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  ylab("Contextual Inequity Scores") +
  ggtitle("with disaster prep. - 69 coastal countries") +
  ylim(0,1) +
  theme_bw()
p.risk.score.with.disaster
ggsave(here("figures","Contextual_Ineq_score_distrib_ctr_with_disaster_69ctries.png"),p.risk.score.with.disaster,height=12,width=12)

# check with OECD database
# load dataset with potential projects in coastal countries
oecd.dat <- import(here("data","derived-data","ocean_ODA_CADC_2010_2021_equity.csv"))
#names(oecd)
summary(oecd.dat)

ctry.oecd.db <- oecd.dat$ctry %>%
  unique()
ctry.oecd.db[-grep("regional",ctry.oecd.db)]

ctry.oecd.db.coastal <- ctry.oecd.db[which(ctry.oecd.db %in% coastal.ctr$country)] # 109 coastal countries

ctry.oecd.db.coastal[which(ctry.oecd.db.coastal %in% df.risk.stack.sc.ctry.ind$name)] %>% length()

###### END of the scritpt #############


################ check # countries
# poverty check - 
pov.ctry <- df.risk.stack.sc.ctry.ind %>% select(name, povmap.grdi.v1) %>%
  group_by(name) %>%
  summarize(mean.pov = mean(povmap.grdi.v1,na.rm=T)) %>% # 170 countries/territories
  drop_na()
dim(pov.ctry)

# SLR check - 
SLR.ctry <- df.risk.stack.sc.ctry.ind %>% select(name, SLR_change) %>%
  group_by(name) %>%
  summarize(mean.SLR = mean(SLR_change,na.rm=T)) %>% # 178 countries/territories
  drop_na()
dim(SLR.ctry) # 128 countries

# nutr dep check - 
nutr.ctry <- df.risk.stack.sc.ctry.ind %>% select(name, Nutritional.dependence) %>%
  group_by(name) %>%
  summarize(mean.nutr = mean(Nutritional.dependence,na.rm=T)) %>% # 115 countries/territories
  drop_na()
dim(nutr.ctry) # 115 countries

# econ dep check - 
econ.ctry <- df.risk.stack.sc.ctry.ind %>% select(name, Economic.dependence) %>%
  group_by(name) %>%
  summarize(mean.nutr = mean(Economic.dependence,na.rm=T)) %>% # 115 countries/territories
  drop_na()
dim(econ.ctry) # 128 countries

# gender eq check - 
gender.ctry <- gender.ineq %>% select(name, gender.ineq) %>%
  group_by(name) %>%
  summarize(mean.gender = mean(gender.ineq,na.rm=T)) %>% # 115 countries/territories
  drop_na()
dim(gender.ctry) # 113 countries

# disaster eq check - 
disaster.ctry <- df.risk.stack.sc.ctry.ind %>% select(name, disaster_prep) %>%
  group_by(name) %>%
  summarize(mean.disaster = mean(disaster_prep,na.rm=T)) %>% # 115 countries/territories
  drop_na()
dim(disaster.ctry) # 92 countries
# common countries
rm(common_ctry)
common_ctry <- intersect(pov.ctry$name,SLR.ctry$name)
common_ctry <- intersect(common_ctry,nutr.ctry$name)
common_ctry <- intersect(common_ctry,econ.ctry$name)
common_ctry <- intersect(common_ctry,gender.ineq$Country)
common_ctry <- intersect(common_ctry,disaster.ctry$name)

ctry.oecd.db.coastal[which(ctry.oecd.db.coastal %in% common_ctry)] %>% length()

length(common_ctry)

# End of the script
# ***********************************************************

# test with large sample
sr <- terra::spatSample(risk.stack.sc, 10000000,na.rm=T,as.points=T,values=T,xy=T,method="random") # sample 5000000 random grid cells
dim(sr) #332583      6

# spatial sr
sr.sf = st_as_sf(sr)
crs(sr.sf) == crs(world.2)

# intersecting points with country
sr.sp.ctry <- st_intersection(sr.sf, world.2)
dim(sr.sp.ctry) # 1986 175

# create composite risk score ##### INDICE COMPOSITE
risk.mat <- as.data.frame(sr.sp.ctry) %>%
  select(ND_gain_NA.sf.proj.2015.sc,mean.count.grav.V2.log.sc,povmap.grdi.v1.sc,
         Nutritional.dependence.sc,Economic.dependence.sc,gender.ineq)
sr.sp.ctry$risk.mat.score.sum <- apply(risk.mat, 1, sum)

# geometric mean
risk.mat.geom  <-  as.data.frame(sr.sp.ctry) %>%
  mutate(risk.mat.score.geom = sqrt(mean.count.grav.V2.log.sc*povmap.grdi.v1.sc*Nutritional.dependence.sc*
                                      Economic.dependence.sc*ND_gain_NA.sf.proj.2015.sc*gender.ineq))
sr.sp.ctry$risk.mat.score.geom <- risk.mat.geom$risk.mat.score.geom
sr.sp.ctry

# saveRDS(sr.sp.sf.ctry,here("data","derived-data","risk.stack.sr.rds"))














# correlation of normalized data 
#cor.ineq.sc <- terra::layerCor(risk.stack.sc, fun="pearson",use="masked.complete")

# species gravity
species.grav.poverty <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$povmap.grdi.v1.sc), # [MW]
                            use = "na.or.complete")
species.grav.nutr.dep <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$Nutritional.dependence.sc), # [MW]
                             use = "na.or.complete"); gc()
species.grav.econ.dep <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$Economic.dependence.sc), # [MW]
                             use = "na.or.complete"); gc()
species.grav.climate <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$ND_gain_NA.sf.proj.2015.sc), # [MW]
                            use = "na.or.complete"); gc()
species.grav.slr <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
                        use = "na.or.complete"); gc()
species.grav.gender <- cor(values(risk.stack$mean.count.grav.V2.log.sc),values(risk.stack$gender.ineq.sc), # [MW]
                           use = "na.or.complete"); gc()
#poverty
poverty.nutr <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$Nutritional.dependence.sc), # [MW]
                    use = "na.or.complete")
poverty.econ <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$Economic.dependence.sc), # [MW]
                    use = "na.or.complete")
poverty.climate <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$ND_gain_NA.sf.proj.2015.sc), # [MW]
                       use = "na.or.complete")
poverty.grav.slr <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
                        use = "na.or.complete"); gc()
poverty.gender <- cor(values(risk.stack$povmap.grdi.v1.sc),values(risk.stack$gender.ineq.sc), # [MW]
                      use = "na.or.complete")
#nutritional dependency
nutr.econ <- cor(values(risk.stack$Nutritional.dependence.sc),values(risk.stack$Economic.dependence.sc), # [MW]
                 use = "na.or.complete")
nutr.climate <- cor(values(risk.stack$Nutritional.dependence.sc),values(risk.stack$ND_gain_NA.sf.proj.2015.sc), # [MW]
                    use = "na.or.complete")
nutr.slr <- cor(values(risk.stack$Nutritional.dependence.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
                use = "na.or.complete")
nutr.gender <- cor(values(risk.stack$Nutritional.dependence.sc),values(risk.stack$gender.ineq.sc), # [MW]
                   use = "na.or.complete")
#econ dependency
econ.climate <- cor(values(risk.stack$Economic.dependence.sc),values(risk.stack$ND_gain_NA.sf.proj.2015.sc), # [MW]
                    use = "na.or.complete")
econ.slr <- cor(values(risk.stack$Economic.dependence.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
                use = "na.or.complete")
econ.gender <- cor(values(risk.stack$Economic.dependence.sc),values(risk.stack$gender.ineq.sc), # [MW]
                   use = "na.or.complete")

# climate
climate.slr <- cor(values(risk.stack$ND_gain_NA.sf.proj.2015.sc),values(risk.stack$mean.SLR.change.proj.coastal.sc), # [MW]
                   use = "na.or.complete")
climate.gender <- cor(values(risk.stack$ND_gain_NA.sf.proj.2015.sc),values(risk.stack$gender.ineq.sc), # [MW]
                      use = "na.or.complete")

# slr
slr.gender <- cor(values(risk.stack$mean.SLR.change.proj.coastal.sc),values(risk.stack$gender.ineq.sc), # [MW]
                  use = "na.or.complete")


# Transform raster grid to polygons grid
z <- terra::as.polygons(risk.stack.sc)
# Intersect with species
u <- terra::intersect(z,world.2)


# number of countries sampled
unique(risk.stack.sp.sf.ctry$name_en) %>% length()

#################################
# for supplemental, spatial PCA #
#################################
sr <- readRDS(here("data","derived-data","risk.stack.sr.rds"))

# for spatial PCA
# randomly select 5000 rows
sample.2000 <- sample(seq(1,dim(sr.sp)[1],1),2000)
sr.sp.2000 <- sr.sp[sample.2000,c(5:10)]

sr.sp.2000.sf = st_as_sf(sr.sp.2000)
crs(sr.sp.2000.sf) == crs(world.2)

# intersecting points with country
sr.sp.2000.sf.ctry <- st_intersection(sr.sp.2000.sf, world.2)
dim(sr.sp.2000.sf.ctry) # 1986 175

sr.sp.2000.sf.ctry.pca <- sr.sp.2000.sf.ctry[,c(1:6,53,118)]
head(sr.sp.2000.sf.ctry.pca)
dim(sr.sp.2000.sf.ctry.pca)

# row to keep in original
row2keep <- which(rownames(sr.sp.2000@data) %in% rownames(sr.sp.2000.sf.ctry.pca))

# new spatial points df
sr.sp.2000.ctry <- sr.sp.2000[row2keep,]
sr.sp.2000.ctry <- cbind(sr.sp.2000.ctry,sr.sp.2000.sf.ctry.pca[,c("iso_a3","name_en")] %>% st_drop_geometry())

# pca 

# bandwith
bw.gw.pca <- GWmodel::bw.gwpca(sr.sp.2000.ctry[,1:6], 
                               vars = names(sr.sp.2000.ctry[,1:7]),
                               k = 3,
                               robust = FALSE,
                               adaptive = TRUE)

# geograph. weighted pcA
gw.pca<- gwpca(sr.sp.2000.ctry[,1:6], 
               vars = names(sr.sp.2000.ctry[,1:6]), 
               bw=bw.gw.pca,
               k = 3, 
               robust = FALSE, 
               adaptive = TRUE,
               scores=T)


# plot of the spatial PCA
fviz_eig(gw.pca$pca, addlabels = TRUE, ylim = c(0, 50))

fviz_pca_var(gw.pca$pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Contributions of variables to PC1
fviz_contrib(gw.pca$pca, choice = "var", axes = 1, top = 10,ylim=c(0,50))
# Contributions of variables to PC2
fviz_contrib(gw.pca$pca, choice = "var", axes = 2, top = 10,ylim=c(0,50))
# Contributions of variables to PC3
fviz_contrib(gw.pca$pca, choice = "var", axes = 3, top = 10,ylim=c(0,50))

# save normalize score for each PC
#sr.sp.2000.ctry$PC1 <- normalize(gw.pca$pca$scores[,1])
#sr.sp.2000.ctry$PC2 <- normalize(gw.pca$pca$scores[,2])
#sr.sp.2000.ctry$PC3 <- normalize(gw.pca$pca$scores[,3])

# by country for each axis
# Basic box plot
p.PC1 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC1), y=PC1)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  theme_bw()
p.PC1
ggsave(here("figures","PC1_distrib_ctr.png"),p.PC1,height=10,width=10)
p.PC2 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC2), y=PC2)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  theme_bw()
p.PC2
ggsave(here("figures","PC2_distrib_ctr.png"),p.PC2,height=10,width=10)
p.PC3 <- ggplot(as.data.frame(sr.sp.2000.ctry), aes(x=reorder(name_en,-PC3), y=PC3)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  theme_bw()
p.PC3
ggsave(here("figures","PC3_distrib_ctr.png"),p.PC3,height=10,width=10)

# 
p.risk.score <- ggplot(as.data.frame(sr.sp.sf.ctry), aes(x=reorder(name_en,-risk.mat.score.sum), y=risk.mat.score.sum)) + 
  geom_boxplot() +
  coord_flip() +
  xlab("Countries") +
  ylab("Contextual Inequity Scores") +
  theme_bw()
p.risk.score
ggsave(here("figures","Contextual_Ineq_score_distrib_ctr.png"),p.risk.score,height=12,width=12)

p.PC.score <- ggarrange(p.PC1,p.PC2,p.PC3,p.risk.score,ncol=2,nrow=2,labels=c("A","B","C","D"))
ggsave(here("figures","double_exposure_ctry_distribution_2000samples.pdf"),p.PC.score,height=18,width=18)

## add country
fviz_pca_biplot(gw.pca$pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

###################
fviz_pca_ind(gw.pca$pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

fviz_pca_ind(gw.pca$pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE,ellipse.type = "convex", # Concentration ellipses
             legend.title = "Groups"
)

fviz_pca_biplot(gw.pca$pca, 
                # Individuals
                geom.ind = "point",
                fill.ind = iris$Species, col.ind = "black",
                pointshape = 21, pointsize = 2,
                palette = "jco",
                addEllipses = TRUE,
                # Variables
                alpha.var ="contrib", col.var = "contrib",
                gradient.cols = "RdYlBu",
                
                legend.title = list(fill = "Species", color = "Contrib",
                                    alpha = "Contrib")
)

# PC1
plot(sr.sp.5000$terrest.temp.change.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$povmap.grdi.v1.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$Nutritional.dependence.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$Economic.dependence.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$population.sc,sr.sp.5000$risk.score.PC1)
plot(sr.sp.5000$mean.count.grav.V2.log.sc,sr.sp.5000$risk.score.PC1)

# PC2
plot(sr.sp.5000$terrest.temp.change.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$povmap.grdi.v1.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$Nutritional.dependence.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$Economic.dependence.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$population.sc,sr.sp.5000$risk.score.PC2)
plot(sr.sp.5000$mean.count.grav.V2.log.sc,sr.sp.5000$risk.score.PC2)

# function for calculation pproportion of variance 
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
}
var.gwpca <- prop.var(gw.pca, 3)
mf$var.gwpca <- var.gwpca



# function for calculation pproportion of variance 
prop.var <- function(gwpca.obj, n.components) {
  return((rowSums(gwpca.obj$var[, 1:n.components]) /rowSums(gwpca.obj$var)) * 100)
}

var.gwpca <- prop.var(bw.gw.pca, 3)
mf$var.gwpca <- var.gwpca
