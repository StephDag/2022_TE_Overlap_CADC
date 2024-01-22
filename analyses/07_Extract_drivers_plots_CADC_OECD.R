# outputs: OECD coastal, non GPS file - summarize by country of OECD initiatives and CADC - with drivers, output ready for figures
# Stephanie D'Agata
# Nov 2023
# Updates

# source country files
source(here("analyses","001_Coastal_countries.R"))

##### extract drivers information for each projects
# load relevant projects
  # CLIMATE
rm(CADC.OECD)
CADC.OECD <- read.delim(here("data","raw-data","OECD","ocean_ODA_2010_2021.csv"),sep=";",dec=",")

# Summarize by country : number of project by coutry, CADC - with sumarize buget
  # wide to long to summarize CADC type in 1 column

CADC.OECD %>%
  filter(ocean_ODA == T & sustainable_ocean_ODA == F)

  # create unique project ID
CADC.OECD <- CADC.OECD %>% 
  mutate(ID = paste0(year,"_",seq(1,61102,1),sep="")) %>%
  mutate(ID = as.factor(ID)) %>%
  # create new variables for simple coding for categories
  mutate(environment.V2 = ifelse(environment  %in% c(1,2),"e","0")) %>%
  mutate(biodiversity.V2 = ifelse(biodiversity  %in% c(1,2),"b","0")) %>%
  mutate(climate.V2 = ifelse(mitigation | adaptation  %in% c(1,2),"c","0")) %>%
  # combination of categories by project
  mutate(CADC.comb = paste0(environment.V2,biodiversity.V2,climate.V2,sep='')) %>%
  # number of categories by projects 
  mutate(n.CADC = nchar(gsub("\\d", "", CADC.comb))) %>%
  # weigth project number
  mutate(weight.sum = ifelse(n.CADC >=1,round(1/n.CADC,2),0)) %>%
  # create new variables for simple coding for categories
  mutate(environment.V3 = ifelse(environment  %in% c(1,2),1,0)) %>%
  mutate(biodiversity.V3 = ifelse(biodiversity  %in% c(1,2),1,0)) %>%
  mutate(climate.V3 = ifelse(mitigation | adaptation  %in% c(1,2),1,0))

CADC.OECD %>% filter(ocean_ODA == T & n.CADC >0) %>% dim()

CADC.OECD.long <- CADC.OECD %>% 
  gather(key="CADC",value="presence_CADC",gender_equality,environment, biodiversity,mitigation,adaptation) %>%
  mutate(CADC = as.factor(CADC), presence_CADC=as.factor(presence_CADC)) %>%
  mutate(presence_merge_CADC = presence_CADC) %>%
  mutate(presence_merge_CADC = dplyr::recode(presence_merge_CADC, "2" = "1")) %>%
  filter(ocean_ODA == T) %>%
  filter(presence_merge_CADC != 0)

CADC.OECD.long %>% filter(ID== "2010_8")
CADC.OECD %>% filter(ID== "2010_30")

# number of projects = 61102

## count number by country and CADC
rm(CADC.OECD.ocean)
CADC.OECD.ocean <- CADC.OECD.long %>%
  select(recipient,CADC,presence_merge_CADC,ocean_ODA,sustainable_ocean_ODA,landbased_ODA,CADC.comb,n.CADC,weight.sum) %>%
  filter(CADC != "gender_equality") %>%
  droplevels() %>%
  # modify CADC name to match typo from triple exposure
  mutate(CADC.V2 = ifelse(CADC == "environment","Development",
                          ifelse(CADC == "mitigation" | CADC == "adaptation","Climate",
                                 ifelse(CADC == "biodiversity","Biodiversity",NA)))) %>%
  mutate(CADC.V2 = as.factor(CADC.V2)) %>%
  # number of projects by country and CADC
  dplyr::group_by(recipient,CADC.V2) %>%
  dplyr::summarize(n.CADC = n()) %>%
  as.data.frame() %>%
  complete(recipient, CADC.V2,fill=list(n = 0))  %>%
  as.data.frame()

# long to wide
CADC.OECD.ocean.wide <- CADC.OECD.ocean %>%
  spread(key=CADC.V2,n.CADC,fill=0) %>% 
  filter(!str_detect(recipient, c("regional"))) %>%
  filter(!str_detect(recipient, c("Bilateral")))

CADC.OECD.ocean.wide.proj <- projected.world %>%
  left_join(CADC.OECD.ocean.wide,by=c("name_en"="recipient")) %>%
  filter(!is.na(Biodiversity),!is.na(Climate))

    # triple 
source(tricolore.R)
tric <- Tricolore(CADC.OECD.ocean.wide.proj, p1 = 'Biodiversity', p2 = 'Climate', p3 = 'Development',breaks = 2)
#tric <- Tricolore(euro_example, p1 = 'ed_0to2', p2 = 'ed_3to4', p3 = 'ed_5to8')
plot(tric$rgb)
tric$key

CADC.OECD.ocean.wide.proj$rgb <- tric$rgb

CADC.OECD.ocean.wide.proj.ctr.sf <- projected.world %>%
  st_join(CADC.OECD.ocean.wide.proj,by="name_en")

cum_CADC_2021_trip <- ggplot(data =  projected.world) +
  geom_sf() +
  geom_sf(data=CADC.OECD.ocean.wide.proj,aes(fill = rgb)) +
  #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
  scale_fill_identity() +
  #annotation_custom(
  #  ggplotGrob(tric$key),
  #  xmin = -16808080, xmax = -6000000, ymin = 16927050 , ymax = -8000000
  #) +
  theme_bw()

#-16808080 ymin: -8625155 xmax: 16927050 ymax: 8342353

cum_CADC_2021_trip
ggsave(here("figures","OECD_sust_cum_CADC_2021_triple_50.png"),cum_CADC_2021_trip)
ggsave(here("figures","OECD_sust_cum_CADC_2021_triple_legend_50.png"),tric$key)


# shannon diversity - effective number of CADC

ven.all <- CADC.OECD %>%
  filter(ocean_ODA == T & n.CADC != 0) %>%
  group_by(CADC.comb) %>%
  summarize(n = n())
sum(ven.all$n) # 19905

# ven plot for global count
ven.plot <- draw.triple.venn(area1 = 6786+4817+4834+2463,area2 = 837+146+4834+2463,area3 = 472+146+4834+4817,
                 n12 = 2463 + 4834,n23 = 146 + 4834,n13 = 4817+4834,n123 = 4834,
                 category = c("Development", "Climate", "Biodiversity"),
                 fill = c("blue4", "deeppink3", "aquamarine3"),
                 lty = "blank",
                 cex = 2,
                 cat.cex = 2,
                 cat.col = c("blue4", "deeppink3", "aquamarine3"))
ggsave(here('figures',"ven.CADC.global.png"),ven.plot,height=10,width=10)



  dplyr::ungroup() %>% 
  # total number of projects by country and CADC
  dplyr::group_by(recipient,CADC.V2,presence_merge_CADC,ocean_ODA,sustainable_ocean_ODA) %>%
  dplyr::mutate(n_tot_CADC = sum(weight.sum,na.rm=T)) %>%
  dplyr::ungroup() %>% 
  as.data.frame()
  #select(-commitment_usd_million_defl,-disbursement_usd_million_defl) %>%
  distinct() %>%
  ungroup() %>%
  arrange(year,recipient) %>%
  # cumulative number of project by country and year
  #group_by(recipient,year) %>%
  #mutate(cum_n = cumsum(n)) %>%
  #ungroup() %>%
  # total number of project by country
  group_by(recipient) %>%
  mutate(n_tot_ctry=sum(n)) %>%
  ungroup() %>%
  # complete all combination country and CADC.V2
  complete(recipient, CADC.V2,fill=list(n_tot_CADC = NA)) %>%
  as.data.frame()

filter(presence_merge_CADC != 0) %>%
  
head(CADC.OECD.sust)

# number of CADC by project
CADC.OECD.sust.n.CADC <- CADC.OECD.long %>%
  select(ID,CADC,presence_merge_CADC) %>%
  filter(presence_merge_CADC == 1) %>%
  filter(CADC != "gender_equality") %>%
  group_by(ID) %>%
  summarize(n = n())
summary(CADC.OECD.sust.n.CADC)
    # mean number of CADC by project = 1.8, median = 2, range = 1 to 4

# left join eez + coastal countries
eez <- eez %>%
  mutate(rgn_name = as.factor(rgn_name))
coastal.ctr <- coastal.ctr %>%
  mutate(country = as.factor(country))
CADC.OECD.sust.eez <- dplyr::left_join(CADC.OECD.sust,eez,by=c("recipient" = "rgn_name")) # EEZ
CADC.OECD.sust.eez.ctry <- dplyr::left_join(CADC.OECD.sust.eez,coastal.ctr,by=c("recipient" = "country")) # EEZ

# left join wide OECD with EEZ
CADC.OECD <- left_join(CADC.OECD,coastal.ctr,by=c("recipient" = "country"))

# distribution of projects by country
CADC.OECD.all <- CADC.OECD %>%
  filter(n.CADC !=0 & ocean_ODA == T) %>%
  group_by(recipient) %>%
  summarize(n.ctry = n())

# project
CADC.OECD.tot.proj <- projected.world %>%
  left_join(CADC.OECD.all,by=c("name_en"="recipient"))# %>%

### cumulative - all CADC
CADC.OECD.tot.proj.map <- ggplot(data = CADC.OECD.tot.proj) + 
  geom_sf(aes(fill = n.ctry)) +
  scale_fill_viridis_c(option = "plasma",na.value = "whitesmoke") +
  ggtitle("Cumulative number of projects by contry: 2010 - 2021") +
  #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
  theme_bw()
CADC.OECD.tot.proj.map
ggsave(here("figures","OECD_cum_CADC_2021_ALL.png"),CADC.OECD.tot.proj.map)

# sum number of projects n.CADC = 1
CADC.OECD.1 <- CADC.OECD %>%
  filter(n.CADC == 1) %>%
  group_by(recipient) %>%
  summarize(n = n())

  # project
CADC.OECD.1.proj <- projected.world %>%
  left_join(CADC.OECD.1,by=c("name_en"="recipient"))# %>%

### cumulative - all CADC
CADC.OECD.1.proj.map <- ggplot(data = CADC.OECD.1.proj) + 
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma",na.value = "whitesmoke") +
  ggtitle("Cumulative number of projects with only 1 CADC category: 2010 - 2021") +
  #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
  theme_bw()
  CADC.OECD.1.proj.map
ggsave(here("figures","OECD_cum_CADC_2021_1CAT.png"),CADC.OECD.1.proj.map)

# sum number of projects n.CADC = 2
CADC.OECD.2 <- CADC.OECD %>%
  filter(n.CADC == 2) %>%
  group_by(recipient) %>%
  summarize(n = n())

# project
CADC.OECD.2.proj <- projected.world %>%
  left_join(CADC.OECD.2,by=c("name_en"="recipient"))# %>%

### cumulative - all CADC
CADC.OECD.2.proj.map <- ggplot(data = CADC.OECD.2.proj) + 
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma",na.value = "whitesmoke") +
  ggtitle("Cumulative number of projects with 2 CADC categories: 2010 - 2021") +
  #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
  theme_bw()
CADC.OECD.2.proj.map
ggsave(here("figures","OECD_cum_CADC_2021_2CAT.png"),CADC.OECD.2.proj.map)

# sum number of projects n.CADC = 3
CADC.OECD.3 <- CADC.OECD %>%
  filter(n.CADC == 3) %>%
  group_by(recipient) %>%
  summarize(n = n())

# project
CADC.OECD.3.proj <- projected.world %>%
  left_join(CADC.OECD.3,by=c("name_en"="recipient"))# %>%

### cumulative - all CADC
CADC.OECD.3.proj.map <- ggplot(data = CADC.OECD.3.proj) + 
  geom_sf(aes(fill = n)) +
  scale_fill_viridis_c(option = "plasma",na.value = "whitesmoke") +
  ggtitle("Cumulative number of projects with only 3 CADC categories: 2010 - 2021") +
  #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
  theme_bw()
CADC.OECD.3.proj.map
ggsave(here("figures","OECD_cum_CADC_2021_3CAT.png"),CADC.OECD.3.proj.map)

## map cumulative number of initiativesb- 3 CADC
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)

crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
projected.world = st_transform(world, crs.val.robinson)

#join # projects by year and country data - Projected 
rm(CADC.OECD.proj)
CADC.OECD.proj <- projected.world %>%
  left_join(CADC.OECD,by=c("name_en"="recipient"))# %>%
#st_set_crs(crs.val.robinson) %>%
#st_make_valid() %>%

### cumulative - all CADC
cum_CADC_2021_all <- ggplot(data = CADC.OECD.sust.eez.ctry.map.proj %>% 
                              select(iso_a3,CADC.V2,n_tot,tot_commit_dollars_period,tot_disb_dollars_period)) +
  geom_sf(aes(fill = n_tot)) +
  scale_fill_viridis_c(option = "plasma",na.value = "whitesmoke") +
  ggtitle("Cumulative number of sustainable aid initiatives: 2010 - 2021") +
  #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
  theme_bw()
cum_CADC_2021_all
ggsave(here("figures","OECD_sust_cum_CADC_2021.png"),cum_CADC_2021_all)

##########################
#  Drivers + Inequality  #
##########################

# FSI composite indicators (? add ISO3 country code to fsi dataframe to join with ctry)
# standardized - min-max
source(here("R","NormMinMax.R"))

#Scale
fsi <- readRDS(here("data","raw-data","Global_fsi_ineq.rds"))  %>%
  mutate(ineq.drv = normalize(as.numeric(gender_inequality_index_2021)))

# add cumulative impact
#### cululative impacts
cum_imp_2015 <- read_excel(here("data","raw-data","Halpern_2015_cumulative_impacts.xlsx")) %>%
  clean_names()  %>% 
  as.data.frame()

# scale cumulative impact - Halpern et al 2015 - Spatial and temporal changes in cumulative human impacts on the world's ocean
cum_imp_2015_scale <- cum_imp_2015 %>%
  mutate(dev.drv=direct_human_impact+organic_pollution+nutrient_pollution+inorganic_pollution+shipping,
         dev.drv=range01(dev.drv),
         climate.drv=sea_surface_temperature+sea_level_rise+ocean_acidification,
         climate.drv=range01(climate.drv),
         cum.drv=dev.drv+climate.drv)
dim(cum_imp_2015_scale)
head(cum_imp_2015_scale)

# OHI data
# add biodiversity/habitat from OHI index
OHI.global <- read.csv(here(here("data","raw-data","OHI_2020","OHI_final_formatted_scores_2020-10-01.csv")))

OHI.global.biodiv <- OHI.global %>%
  filter(long_goal == "Biodiversity" & scenario == "2020") %>%
  filter(dimension == "trend")
head(OHI.global.biodiv)

# merge eez and OHI to retrieve country information
rm(OHI.global.eez)
OHI.global.eez <- OHI.global.biodiv %>%
  left_join(eez,by=c("region_name"="rgn_name"))
# merge OHI and impacts
rm(OHI.global.eez.impact)
OHI.global.eez.impact <- OHI.global.eez %>%
  left_join(cum_imp_2015_scale,by=c("region_name"="eez_name")) # eez name, not ISO3

# keep only main territory for now
OHI.global.eez.impact.short <- OHI.global.eez.impact[which(OHI.global.eez.impact$region_name %in% ctr$country),]

## add poverty index from MDI - WB
poverty <- read.csv(here("data","raw-data","poverty-data-main","datasets","pip_dataset.csv"))
head(poverty)

rm(poverty.coastal)
poverty.coastal <- poverty[which(poverty$country %in% ctr$country),]
poverty.coastal <- poverty.coastal %>% 
  #filter(welfare_type == "income") %>%
  group_by(country) %>%
  slice(which.max(year)) %>%
  select(country,headcount_ratio_international_povline)
head(poverty.coastal)
dim(poverty.coastal)

## HDI index
rm(HDI)
HDI <- read.csv(here("data","raw-data","UNDP timeseries","HDR21-22_Composite_indices_complete_time_series.csv"))
HDI.2020 <- HDI %>%
  select(country,hdi_2020)

### merge with fsi -- full country db
# add country + region
rm(ctr.fsi.ineq.biodiv.hdi)
ctr.fsi.ineq.biodiv.hdi <- fsi %>%
  left_join(ctr.region,by=c("country"="name"))
dim(ctr.fsi.ineq.biodiv.hdi)
head(ctr.fsi.ineq.biodiv.hdi)

# add biodiversity trend
ctr.fsi.ineq.biodiv.hdi <- ctr.fsi.ineq.biodiv.hdi %>%
  left_join(OHI.global.eez.impact.short,by=c("country"="region_name"))
# add poverty
ctr.fsi.ineq.biodiv.hdi <- ctr.fsi.ineq.biodiv.hdi %>%
  left_join(poverty %>% 
              #filter(welfare_type == "income") %>%
              group_by(country) %>%
              slice(which.max(year)) %>%
              select(country,headcount_ratio_international_povline),by="country")
ctr.fsi.ineq.biodiv.hdi %>% dim()
ctr.fsi.ineq.biodiv.hdi %>% head()
# check not duplicated names
duplicated(ctr.fsi.ineq.biodiv.hdi$country)

# remove non coastal countries
ctr.fsi.ineq.biodiv.hdi <- ctr.fsi.ineq.biodiv.hdi %>%
  filter(!is.na(value))
dim(ctr.fsi.ineq.biodiv.hdi) # 129 countries

############################
#    MAP initiatives       #
############################
  
  #################################
  #       CADC by country         #
  #################################
  
  CADC.OECD.sust.eez.ctry
  
    # 3 maps sustainable projects a) environment, b) biodiversity, c) climate (mitigation + adaptation)
    # inputs: total number of project, by country and CADC across the period 2010 - 2021
  
  ## map cumulative number of initiatives
  world <- ne_countries(scale = "large", returnclass = "sf")
  class(world)
  
  crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
  projected.world = st_transform(world, crs.val.robinson)
  
  #join # projects by year and country data - Projected 
  rm(CADC.OECD.sust.eez.ctry.map.proj)
  CADC.OECD.sust.eez.ctry.map.proj <- projected.world %>%
    left_join(CADC.OECD.sust.eez.ctry,by=c("name_en"="recipient"))# %>%
  #st_set_crs(crs.val.robinson) %>%
  #st_make_valid() %>%
  
  ### cumulative - all CADC
  cum_CADC_2021_all <- ggplot(data = CADC.OECD.sust.eez.ctry.map.proj %>% 
                                select(iso_a3,CADC.V2,n_tot,tot_commit_dollars_period,tot_disb_dollars_period)) +
    geom_sf(aes(fill = n_tot)) +
    scale_fill_viridis_c(option = "plasma",na.value = "whitesmoke") +
    ggtitle("Cumulative number of sustainable aid initiatives: 2010 - 2021") +
    #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
    theme_bw()
  cum_CADC_2021_all
  ggsave(here("figures","OECD_sust_cum_CADC_2021.png"),cum_CADC_2021_all)
  
  # Climate
  OECD.climate <- CADC.OECD.sust.eez.ctry %>% 
    select(recipient,CADC.V2,n_tot_CADC,tot_commit_dollars_period,tot_disb_dollars_period) %>%
    filter(CADC.V2 == "Climate") %>%
    distinct()
  OECD.climate.sf <- projected.world %>%
    left_join(OECD.climate,by=c("name_en"="recipient"))
  
  cum_CADC_2021_CLIMATE <- ggplot(data =  OECD.climate.sf) +
    geom_sf(aes(fill = n_tot_CADC)) +
    scale_fill_viridis_c(option = "plasma","Climate",na.value = "whitesmoke") +
    ggtitle("Cumulative number of sustainable climate aid initiatives: 2010 - 2021") +
    #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
    theme_bw()
  cum_CADC_2021_CLIMATE
  ggsave(here("figures","OECD_sust_cum_CADC_2021_CLIMATE.png"),cum_CADC_2021_CLIMATE)
  
  # sust dev / environment
  OECD.DEV <- CADC.OECD.sust.eez.ctry %>% 
    select(recipient,CADC.V2,n_tot_CADC,tot_commit_dollars_period,tot_disb_dollars_period) %>%
    filter(CADC.V2 == "Development") %>%
    distinct()
  OECD.env.sf <- projected.world %>%
    left_join(OECD.DEV,by=c("name_en"="recipient"))
  
  cum_CADC_2021_DEV <- ggplot(data =  OECD.env.sf) +
    geom_sf(aes(fill = n_tot_CADC)) +
    scale_fill_viridis_c(option = "plasma","Development",na.value = "whitesmoke") +
    ggtitle("Cumulative number of sustainable development aid initiatives: 2010 - 2021") +
    #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
    theme_bw()
  cum_CADC_2021_DEV
  ggsave(here("figures","OECD_sust_cum_CADC_2021_DEV.png"),cum_CADC_2021_DEV)
  
  # conservation
  OECD.CONS <- CADC.OECD.sust.eez.ctry %>% 
    select(recipient,CADC.V2,n_tot_CADC,tot_commit_dollars_period,tot_disb_dollars_period) %>%
    filter(CADC.V2 == "Biodiversity") %>%
    distinct()
  OECD.cons.sf <- projected.world %>%
    left_join(OECD.CONS,by=c("name_en"="recipient"))
  
  cum_CADC_2021_CONS <- ggplot(data =  OECD.cons.sf) +
    geom_sf(aes(fill = n_tot_CADC)) +
    scale_fill_viridis_c(option = "plasma","Conservation",na.value = "whitesmoke") +
    ggtitle("Cumulative number of conservation aid initiatives: 2010 - 2021") +
    #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
    theme_bw()
  cum_CADC_2021_CONS
  ggsave(here("figures","OECD_sust_cum_CADC_2021_CONS.png"),cum_CADC_2021_DEV)
  

  # plot altogether
  p <- ggarrange(cum_CADC_2021_all,cum_CADC_2021_CLIMATE,cum_CADC_2021_DEV,cum_CADC_2021_CONS,labels = c("a","b","c","d"),nrow=2,ncol=2,
                 width=15,height=15)
  p
  ggsave(here("figures","OECD_sust_cum_CADC_2021_full_figure.png"),p)

    # biplot with size of plot being the third CADC, and country name
  CADC.OECD.sust.eez.ctry.wide.count <- CADC.OECD.sust.eez.ctry %>%
    select(recipient,CADC.V2,n_tot_CADC) %>%
    mutate(recipient = as.factor(recipient)) %>%
    distinct() %>%
    tidyr::spread(key=CADC.V2,value=n_tot_CADC)
  
  biplot_CADC_tot <- ggplot(data = CADC.OECD.sust.eez.ctry.wide.count,aes(x=Biodiversity,y=Climate,
                                                                          size=Development,label=recipient)) +
    geom_point() +
    geom_text_repel() +
    theme(legend.position = "none") +
    theme_bw() +
    xlim(0,700) + ylim(0,500) +
    geom_abline(slope=1,intercept=0)
    
  biplot_CADC_tot
  ggsave(here("figures","OECD_biplot_CADC_tot.png"),biplot_CADC_tot)
  

    # cumulative barplot for each country, or region, with dot lines for each the total number of project across the period
  barplot.OECD.2021 <- ggplot(CADC.OECD.sust.eez.ctry %>% filter(CADC.V2 != "Gender_equality") %>% 
                              mutate(CADC.V2 = as.factor(CADC.V2) %>%
                              droplevels()),
                              mapping=aes(x=recipient,y=n_tot_CADC,fill=CADC.V2)) +
    geom_bar(position="stack", stat="identity")  +
    theme_bw()
  
  # trivariate maps
  # color-code the data set and generate a color-key
  CADC.OECD.sust.eez.ctry.wide.count.NA <- CADC.OECD.sust.eez.ctry.wide.count %>%
    filter(!is.na(Climate)) %>%
    filter(!is.na(Biodiversity))
  
    # remove all regions
  CADC.OECD.sust.eez.ctry.wide.count.NA.ctr <- CADC.OECD.sust.eez.ctry.wide.count.NA %>%
    filter(!str_detect(recipient, c("regional"))) %>%
    filter(!str_detect(recipient, c("Bilateral")))

  
  source(tricolore.R)
  
  tric <- Tricolore(CADC.OECD.sust.eez.ctry.wide.count.NA.ctr, p1 = 'Biodiversity', p2 = 'Climate', p3 = 'Development',breaks = 2)
  #tric <- Tricolore(euro_example, p1 = 'ed_0to2', p2 = 'ed_3to4', p3 = 'ed_5to8')
  plot(tric$rgb)
  
  CADC.OECD.sust.eez.ctry.wide.count.NA.ctr$rgb <- tric$rgb
  
  CADC.OECD.sust.eez.ctry.wide.count.NA.ctr.sf <- projected.world %>%
    left_join(CADC.OECD.sust.eez.ctry.wide.count.NA.ctr,by=c("name_en"="recipient"))
  
  cum_CADC_2021_trip <- ggplot(data =  CADC.OECD.sust.eez.ctry.wide.count.NA.ctr.sf) +
    geom_sf(aes(fill = rgb)) +
    ggtitle("Cumulative number aid initiatives: 2010 - 2021") +
    #coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
    scale_fill_identity() +
    #annotation_custom(
    #  ggplotGrob(tric$key),
    #  xmin = -16808080, xmax = -6000000, ymin = 16927050 , ymax = -8000000
    #) +
    theme_bw()

  #-16808080 ymin: -8625155 xmax: 16927050 ymax: 8342353
  
  cum_CADC_2021_trip
  ggsave(here("figures","OECD_sust_cum_CADC_2021_triple_50.png"),cum_CADC_2021_trip)
  ggsave(here("figures","OECD_sust_cum_CADC_2021_triple_legend_50.png"),tric$key)
  
  
  p <- ggarrange(cum_CADC_2021_trip,tric$key,nrow=2)
  
  #############################
  #       DEVELOPMENT         #
  #############################
  
  ## add CADC in global drivers/inequity map database
  rm(ctr.fsi.ineq.biodiv.hdi.CADC.DEV)
  ctr.fsi.ineq.biodiv.hdi.CADC.DEV <- ctr.fsi.ineq.biodiv.hdi %>%
    left_join(n.ctry.CADC.DEV,by=c("country")) %>%
    mutate(`2022` = replace_na(`2022`,0)) %>% # 3192 projects
    mutate(PA_CADC = ifelse(`2022` == 0,0,1)) %>%
    mutate(PA_CADC = as.factor(PA_CADC))
  
  saveRDS(ctr.fsi.ineq.biodiv.hdi.CADC.DEV,here("data","derived-data","DEV.drivers.N.projects.rds"))
  
  # n countries with CADC
  n.ctry.CADC.DEV <- length(which(ctr.fsi.ineq.biodiv.hdi.CADC.DEV$`2022` != 0)) # 111 countries
  
  ## map cumulative number of initiatives
  
  world <- ne_countries(scale = "large", returnclass = "sf")
  class(world)

  crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
  projected.world = st_transform(world, crs.val.robinson)
  
  #join # projects by year and country data - Projected 
  rm(ctr.fsi.ineq.biodiv.hdi.CADC.map.proj.DEV)
  ctr.fsi.ineq.biodiv.hdi.CADC.map.proj.DEV <- projected.world %>%
    left_join(ctr.fsi.ineq.biodiv.hdi.CADC.DEV,by=c("name_en"="country"))# %>%
  #st_set_crs(crs.val.robinson) %>%
  #st_make_valid() %>%
  
  #join # projects by year and country data - Non Projected 
  rm(ctr.fsi.ineq.biodiv.hdi.CADC.map.DEV)
  ctr.fsi.ineq.biodiv.hdi.CADC.map.DEV <- world %>%
    left_join(ctr.fsi.ineq.biodiv.hdi.CADC.DEV,by=c("name_en"="country"))# %>%
  #st_set_crs(crs.val.robinson) %>%
  #st_make_valid() %>%
  
  # join transaction cost by year and country data
  
  # plot cumulative number of CADC
  cum_CADC_2022.DEV <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.proj.DEV) +
    geom_sf(aes(fill = `2022`)) +
    scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
    ggtitle("Cumulative number of development (fisheries, aquaculture) aid initiatives in 2022") +
    theme_bw()
  #scale_fill_gradient(low = "white", high = "purple", na.value = "grey")
  cum_CADC_2022.DEV
  ggsave(here("figures","DEVELOPMENT_Cum_CADC_2022_world.png"),cum_CADC_2022.DEV)
  # comments: make a zoom on the pacific, cariabean, etc. 

  # Coral triangle
  cum_CADC_2022_CT.DEV <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.DEV) +
    geom_sf(aes(fill = `2022`)) +
    scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
    ggtitle("Cumulative number of development aid initiatives - Coral triangle") +
    coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
    theme_bw()
  cum_CADC_2022_CT.DEV
  ggsave(here("figures","DEVELOPMENT_cum_CADC_2022_CT.DEV.png"),cum_CADC_2022_CT.DEV)
  #cum_CADC_2022_CT
  
  #WIO
  cum_CADC_2022_WIO.DEV <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.DEV) +
    geom_sf(aes(fill = `2022`)) +
    scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
    ggtitle("Cumulative number of development aid initiatives - Western Indian Ocean") +
    coord_sf(xlim = c(20, 70), ylim = c(-34, 14), expand = FALSE) +
    theme_bw()
  cum_CADC_2022_WIO.DEV
  ggsave(here("figures","DEVELOPMENT_cum_CADC_2022_WIO.png"),cum_CADC_2022_WIO.DEV)
  #cum_CADC_2022_WIO
  
  ##### Histogram # CADC by countries
  
  CADC_ctry_barplot.DEV <- ggplot(ctr.fsi.ineq.biodiv.hdi.CADC.map.DEV %>% filter(`2022` != 0),aes(x=reorder(name,-`2022`),y=`2022`,fill=`2022`)) +
    geom_bar(stat="identity") +
    scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
    xlab("Countries") + ylab("Total number of initiatives by country (2022)") +
    ggtitle("57 countries") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60,hjust = 1, vjust = 1),legend.position = "none",axis.text = element_text(size = 10))
  
  #### map + barplot
  rm(p)
  p <- ggarrange(cum_CADC_2022.DEV,CADC_ctry_barplot.DEV,labels = c("A","B"),common.legend=T,nrow=2,ncol=1,align="v",legend="bottom")
  p
  ggsave(here("figures","CADC_CTRY_MAP_BARPLOT.DEV.png"),p)

  
  #############################
  #         CLIMATE           #
  #############################
  
  # 11/06 - something wrong with number of projects
  # need to check when less tired
  
  ## add CADC in global drivers/inequity map database
  rm(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE)
  ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE <- ctr.fsi.ineq.biodiv.hdi %>%
    left_join(n.ctry.CADC.CLIMATE,by=c("country")) %>%
    mutate(`2022` = replace_na(`2022`,0)) %>% # 3192 projects
    mutate(PA_CADC = ifelse(`2022` == 0,0,1)) %>%
    mutate(PA_CADC = as.factor(PA_CADC))
  
  saveRDS(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE,here("data","derived-data","CLIMATE.drivers.N.projects.rds"))
  
  # n countries with CADC
  n.ctry.CADC.CLIMATE <- length(which(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE$`2022` != 0)) # 28 countries
  
  ## map cumulative number of initiatives
  #join # projects by year and country data - Projected 
  rm(ctr.fsi.ineq.biodiv.hdi.CADC.map.proj.CLIMATE)
  ctr.fsi.ineq.biodiv.hdi.CADC.map.proj.CLIMATE <- projected.world %>%
    left_join(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE,by=c("name_en"="country"))# %>%
  #st_set_crs(crs.val.robinson) %>%
  #st_make_valid() %>%
  
  #join # projects by year and country data - Non Projected 
  rm(ctr.fsi.ineq.biodiv.hdi.CADC.map.CLIMATE)
  ctr.fsi.ineq.biodiv.hdi.CADC.map.CLIMATE <- world %>%
    left_join(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE,by=c("name_en"="country"))# %>%
  #st_set_crs(crs.val.robinson) %>%
  #st_make_valid() %>%
  
  # join transaction cost by year and country data
  
  # plot cumulative number of CADC
  cum_CADC_2022.CLIMATE <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.proj.CLIMATE) +
    geom_sf(aes(fill = `2022`)) +
    scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
    ggtitle("Cumulative number of development (fisheries, aquaculture) aid initiatives in 2022") +
    theme_bw()
  #scale_fill_gradient(low = "white", high = "purple", na.value = "grey")
  cum_CADC_2022.CLIMATE
  ggsave(here("figures","DEVELOPMENT_Cum_CADC_2022_world.png"),cum_CADC_2022.DEV)
  # comments: make a zoom on the pacific, cariabean, etc. 
  
  # Coral triangle
  cum_CADC_2022_CT.DEV <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.DEV) +
    geom_sf(aes(fill = `2022`)) +
    scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
    ggtitle("Cumulative number of development aid initiatives - Coral triangle") +
    coord_sf(xlim = c(90, 165), ylim = c(-14, 20), expand = FALSE) +
    theme_bw()
  cum_CADC_2022_CT.DEV
  ggsave(here("figures","DEVELOPMENT_cum_CADC_2022_CT.DEV.png"),cum_CADC_2022_CT.DEV)
  #cum_CADC_2022_CT
  
  #WIO
  cum_CADC_2022_WIO.DEV <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.DEV) +
    geom_sf(aes(fill = `2022`)) +
    scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
    ggtitle("Cumulative number of development aid initiatives - Western Indian Ocean") +
    coord_sf(xlim = c(20, 70), ylim = c(-34, 14), expand = FALSE) +
    theme_bw()
  cum_CADC_2022_WIO.DEV
  ggsave(here("figures","DEVELOPMENT_cum_CADC_2022_WIO.png"),cum_CADC_2022_WIO.DEV)
  #cum_CADC_2022_WIO
  
  ##### Histogram # CADC by countries
  
  CADC_ctry_barplot.DEV <- ggplot(ctr.fsi.ineq.biodiv.hdi.CADC.map.DEV %>% filter(`2022` != 0),aes(x=reorder(name,-`2022`),y=`2022`,fill=`2022`)) +
    geom_bar(stat="identity") +
    scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
    xlab("Countries") + ylab("Total number of initiatives by country (2022)") +
    ggtitle("57 countries") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60,hjust = 1, vjust = 1),legend.position = "none",axis.text = element_text(size = 10))
  
  #### map + barplot
  rm(p)
  p <- ggarrange(cum_CADC_2022.DEV,CADC_ctry_barplot.DEV,labels = c("A","B"),common.legend=T,nrow=2,ncol=1,align="v",legend="bottom")
  p
  ggsave(here("figures","CADC_CTRY_MAP_BARPLOT.DEV.png"),p)
  
  
  ## Map the drivers for coastal countries
  
  ## Biodiversity
  #fake_scico <- scico(3, palette = "vik")
  #Biodiversity_global <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.proj) +
  #  geom_sf(aes(fill = value)) +
  #  #scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
  #  ggtitle("OHI - Biodiversity") +
  #  scale_fill_gradient2(low = fake_scico[1], mid = fake_scico[2], high = fake_scico[3],
                         na.value = "whitesmoke") +
  #  theme_bw()
  #ggsave(here("2_Preliminary_results","Biodiversity_global_drv.png"),Biodiversity_global)
  
  ### Climate
 # Climate_global <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.proj) +
#    geom_sf(aes(fill = climate.drv)) +
#    #scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
#    ggtitle("Climate Change Impacts") +
#    scico::scale_fill_scico(palette = "cork",midpoint = 0,na.value = "whitesmoke") + # the default
#    theme_bw()
#  ggsave(here("2_Preliminary_results","Climate_global_drv.png"),Climate_global)
  
  ### POverty
#  Poverty_global <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.proj) +
#    geom_sf(aes(fill = headcount_ratio_international_povline)) +
#    #scale_fill_viridis_c(option = "plasma","2022",na.value = "whitesmoke") +
#    ggtitle("Poverty - % headcount < $2.3") +
#    scico::scale_fill_scico(palette = "lajolla",na.value = "whitesmoke",name="Poverty") +
#    theme_bw()
#  ggsave(here("2_Preliminary_results","Poverty_global_drv.png"),Poverty_global)
  
  ### Inequity
 # Ineq_global <- ggplot(data = ctr.fsi.ineq.biodiv.hdi.CADC.map.proj) +
#    geom_sf(aes(fill = ineq.drv)) +
#    ggtitle("Gender inequity") +
#    scale_fill_continuous(low="lightblue",high="darkblue",na.value = "whitesmoke",name="GI") +
#    theme_bw()
#  ggsave(here("2_Preliminary_results","Inequity_global_drv.png"),Ineq_global)
  
 # library(ggpubr)
#  rm(p.map)
#  p.map <-  ggarrange(Biodiversity_global,Climate_global,Poverty_global,Ineq_global,
#                      ncol = 2, nrow=2, labels = c("A","B", "C","D"))
#  p.map
#  ggsave(here("2_Preliminary_results","CADC_Drivers.png"),p.map)

###############################
#   # projects by countries.  #
###############################

####################
#   DEVELOPMENT    #
####################

# number of projects by country

## WARNING
# check for NAs in year
# check year = 0015, 0016

## Number of projects from IATI db
rm(n.proj.year.DEV)
n.proj.year.DEV <- CADC.db.DEV.relevant %>%
  mutate(CTR_1st = as.factor(substring(recipient_country_code,1,2))) %>% # check for NAs in year
  group_by(CTR_1st,Year) %>%
  summarize(n= n()) %>%
  arrange(CTR_1st,Year) %>%
  group_by(CTR_1st) %>%
  mutate(cum_sum_CADC = cumsum(n)) # cumulative sum of projects to map

# remove NAs for now
n.proj.year.DEV <- n.proj.year.DEV %>%
  filter(!is.na(Year))
sum(n.proj.year$n) # 3797 projects without cleaning

# long to wide by year - compute cumulative number of projects by projects across the year (from 1975 to 2026)
rm(n.proj.year.wide.DEV)
n.proj.year.wide.DEV <-  n.proj.year.DEV %>%
  select(-cum_sum_CADC) %>%
  spread(Year, n,fill=0) %>%
  gather("Year","n",-CTR_1st) %>% # to retrieve all information by year, even missing year for some countries
  arrange(CTR_1st,Year) %>% 
  # sort by country and year
  group_by(CTR_1st) %>%
  mutate(cum_n = cumsum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  spread(Year, cum_n,fill=0) %>%
  as.data.frame()
n.proj.year.wide.DEV
# check number of projects - OK
#sum(n.proj.year.wide$`2022`) # 385

# add country information
n.ctry.CADC.DEV <- n.proj.year.wide.DEV %>%
  left_join(coastal.ctr,by=c("CTR_1st"="iso2"))

# plot
n.proj.year.long.DEV <- n.proj.year.wide.DEV %>%
  gather("Year","n",-CTR_1st) %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(ctr.region,by=c("CTR_1st"="alpha.2")) %>%
  mutate(region = as.factor(region))

# plot by country and year
n.ctry.year.DEV <- ggplot(n.proj.year.long,aes(x=Year,y=n,color=CTR_1st)) +
  geom_line()  +
  theme_bw()
#n.ctry.year 

# # countries
unique(n.proj.year.long.DEV$name) %>% length()

# plot by region and year
rm(n.proj.year.region.DEV)
n.proj.year.region.DEV <- n.proj.year.DEV %>%
  left_join(ctr.region,by=c("CTR_1st"="alpha.2")) %>%
  mutate(region = as.factor(region)) %>%
  dplyr::group_by(region,Year) %>%
  dplyr::summarize(n_region = sum(n)) %>%
  arrange(region, Year) %>% 
  mutate(cum_n_region = cumsum(n_region)) %>%
  as.data.frame() %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  filter(Year <= 2022)

## region and year
# using color blind palette
library(ggokabeito)
palette_okabe_ito(order = 1:9, alpha = NULL, recycle = FALSE)
n.region.year.DEV <- ggplot(n.proj.year.region.DEV %>% filter(!is.na(region)) %>% droplevels(),aes(x=Year,y=cum_n_region,color=region)) +
  geom_point(size=3)  +
  geom_line(size=2) +
  theme_bw() +
  ggtitle("Cumulative number of development projects (fisheries, aquaculture) by regions from 1978 to 2022 in 57 countries") +
  ylab("Total number of coastal aid projects")  +
  scale_color_okabe_ito()
n.region.year.DEV
ggsave(here("figures","DEVELOPMENT_n.region.year.png"),n.region.year.DEV)


####################
#     CLIMATE      #
####################

# number of projects by country
CADC.OECD.sust
## WARNING
# check for NAs in year
# check year = 0015, 0016

## Number of projects from OECD db
rm(n.proj.year.CLIMATE)
n.proj.year.CLIMATE <- CADC.db.CLIMATE.relevant %>%
  mutate(CTR_1st = as.factor(substring(recipient_country_code,1,2))) %>% # check for NAs in year
  group_by(CTR_1st,Year) %>%
  summarize(n= n()) %>%
  arrange(CTR_1st,Year) %>%
  group_by(CTR_1st) %>%
  mutate(cum_sum_CADC = cumsum(n)) # cumulative sum of projects to map

# remove NAs for now
n.proj.year.CLIMATE <- n.proj.year.CLIMATE %>%
  filter(!is.na(Year))
sum(n.proj.year$n) # 3797 projects without cleaning

# long to wide by year - compute cumulative number of projects by projects across the year (from 1975 to 2026)
rm(n.proj.year.wide.CLIMATE)
n.proj.year.wide.CLIMATE <-  n.proj.year.CLIMATE %>%
  select(-cum_sum_CADC) %>%
  spread(Year, n,fill=0) %>%
  gather("Year","n",-CTR_1st) %>% # to retrieve all information by year, even missing year for some countries
  arrange(CTR_1st,Year) %>% 
  # sort by country and year
  group_by(CTR_1st) %>%
  mutate(cum_n = cumsum(n)) %>%
  ungroup() %>%
  select(-n) %>%
  spread(Year, cum_n,fill=0) %>%
  as.data.frame()
n.proj.year.wide.CLIMATE
# check number of projects - OK
#sum(n.proj.year.wide$`2022`) # 385

# add country information
n.ctry.CADC.CLIMATE <- n.proj.year.wide.CLIMATE %>%
  left_join(coastal.ctr,by=c("CTR_1st"="iso2"))

# plot
n.proj.year.long.CLIMATE <- n.proj.year.wide.CLIMATE %>%
  gather("Year","n",-CTR_1st) %>%
  mutate(Year = as.numeric(Year)) %>%
  left_join(ctr.region,by=c("CTR_1st"="alpha.2")) %>%
  mutate(region = as.factor(region))

# plot by country and year
n.ctry.year.CLIMATE <- ggplot(n.proj.year.long,aes(x=Year,y=n,color=CTR_1st)) +
  geom_line()  +
  theme_bw()
#n.ctry.year 

# # countries
unique(n.proj.year.long.CLIMATE$name) %>% length() # 41 projects

# plot by region and year
rm(n.proj.year.region.CLIMATE)
n.proj.year.region.CLIMATE <- n.proj.year.CLIMATE %>%
  left_join(ctr.region,by=c("CTR_1st"="alpha.2")) %>%
  mutate(region = as.factor(region)) %>%
  dplyr::group_by(region,Year) %>%
  dplyr::summarize(n_region = sum(n)) %>%
  arrange(region, Year) %>% 
  mutate(cum_n_region = cumsum(n_region)) %>%
  as.data.frame() %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  filter(Year <= 2022)

## region and year
# using color blind palette
library(ggokabeito)
palette_okabe_ito(order = 1:9, alpha = NULL, recycle = FALSE)
n.region.year.CLIMATE <- ggplot(n.proj.year.region.CLIMATE %>% filter(!is.na(region)) %>% droplevels(),aes(x=Year,y=cum_n_region,color=region)) +
  geom_point(size=3)  +
  geom_line(size=2) +
  theme_bw() +
  ggtitle("Cumulative number of climate projects by regions from 2001 to 2022 in 41 countries") +
  ylab("Total number of coastal aid projects")  +
  scale_color_okabe_ito()
n.region.year.CLIMATE
ggsave(here("figures","CLIMATE_n.region.year.png"),n.region.year.CLIMATE)


