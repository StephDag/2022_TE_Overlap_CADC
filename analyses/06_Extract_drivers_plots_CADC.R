# source country files
source(here("analyses","001_Coastal_countries.R"))

##### extract drivers information for each projects

# load relevant projects
  # CLIMATE
CADC.db.CLIMATE.relevant <- readRDS(here("data","derived-data","CADC.db.CLIMATE.relevant.rds")) # 470 locations
      # rm duplicate names
CADC.db.CLIMATE.relevant <- CADC.db.CLIMATE.relevant[,-c(which(duplicated(names(CADC.db.CLIMATE.relevant))))]

  #  DEVELOPMENT - filter for project relevance == 1
CADC.db.DEV.relevant <- readRDS(here("data","derived-data","CADC.db.DEV.relevant.rds")) # 385 locations
CADC.db.DEV.relevant <- CADC.db.DEV.relevant[,-c(which(duplicated(names(CADC.db.DEV.relevant))))]

##########################
#  Drivers + Inequality  #
##########################

# FSI composite indicators (? add ISO3 country code to fsi dataframe to join with ctry)
range01 <- function(x){(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))} # Function MinMax 

#Scale
fsi <- readRDS(here("data","raw-data","Global_fsi_ineq.rds"))  %>%
  mutate(ineq.drv = range01(as.numeric(gender_inequality_index_2021)))

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

# add biodiversity/habitat from OHI index
OHI.global <- read.csv(here("data","raw-data","OHI_2020","OHI_final_formatted_scores_2020-10-01.csv"))

  # dimension to use: 
#future  pressures resilience      score     status      trend 

OHI.global.biodiv <- OHI.global %>%
  filter(long_goal == "Biodiversity" & scenario == "2020") %>%
  filter(dimension == "trend") # to change depending on what we want
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
  
  ## WARNING
  # check for NAs in year
  # check year = 0015, 0016
  
  ## Number of projects from IATI db
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
  

############################
#    MAP initiatives       #
############################
  
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

