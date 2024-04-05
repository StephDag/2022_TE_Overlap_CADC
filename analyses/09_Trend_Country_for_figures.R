# Trend by countries
### Stephanie D'Agata 
### updates: Steph D'Agata
### output: positive or negative investment trend by countries

library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)

#### load last vesion (21/12/2023) of the OECD db with keywords check
# check with OECD database
# load dataset with potential projects in coastal countries
oecd.dat.sf.ctry <- readRDS(here("data","derived-data","ocean_ODA_CADC_2010_2021_equity_sf.RDS"))
#names(oecd)
summary(as.factor(oecd.dat.sf.ctry$cadc.type))
names(oecd.dat.sf.ctry)
head(oecd.dat.sf.ctry)
dim(oecd.dat.sf.ctry)

# summarize by country, year and cadc.type for the trend
oecd.dat.sf.ctry.all.trend <- oecd.dat.sf.ctry %>%
  dplyr::select(year,iso_a3,name_en,cadc.type,usd.comm,usd.disb) %>%
  st_drop_geometry() %>%
  dplyr::filter(!is.na(cadc.type)) %>%
  filter(cadc.type != "other ocean economy") %>%
  droplevels() %>%
  distinct() %>%
  dplyr::group_by(year,iso_a3,name_en) %>%
  dplyr::summarise(usd.comm.year.CADC=sum(usd.comm,na.rm=T) %>% round(2),
                   usd.disb.year.CADC=sum(usd.disb,na.rm=T) %>% round(2)) %>%
  ungroup() %>%
  group_by(iso_a3) %>%
  mutate(n = n()) %>%
  dplyr::arrange(iso_a3,year) %>%
  as.data.frame() %>%
  #filter(n >7) %>% # filter for low n years (<8 year records) %>%
  droplevels()
oecd.dat.sf.ctry.all.trend

quantile(oecd.dat.sf.ctry.all.trend$n,probs = seq(0, 1, 0.01))

# plot temporal trends by country
p.trend.all.ctry <- ggplot(oecd.dat.sf.ctry.all.trend,aes(x=year,y=usd.comm.year.CADC,color=name_en)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none") 
p.trend.all.ctry

# model trend by country
  # list unique countries
ctry <- unique(oecd.dat.sf.ctry.all.trend$iso_a3)

# dataframe to save models parameters 
N <- length(ctry)
df.out.trend <- data.frame(iso_a3=ctry,
                           trend=numeric(N), #2
                           coef.int.025 = numeric(N),#3
                           coef.int.975 = numeric(N),#4
                           coef.int.25 = numeric(N),#5
                           coef.int.75 = numeric(N),#6
                           AR.1=numeric(N), #7
                           coef.int.025 = numeric(N),#8
                           coef.int.975 = numeric(N),#9
                           coef.int.25 = numeric(N),#10
                           coef.int.75 = numeric(N),#11
                           AR.2=numeric(N),#12
                           coef.int.025 = numeric(N),#13
                           coef.int.975 = numeric(N),#14
                           coef.int.25 = numeric(N),#15
                           coef.int.75 = numeric(N),#16
                           Adj.Rsq = numeric(N),#17
                           significant.90 = character(N),#18
                           significant.50 = character(N),#19
                           stringsAsFactors=FALSE)

# test for 1 country
  
  for (i in 1:length(ctry)) {
    
  skip_to_next <- FALSE
 
  print(paste("i=",i))

  df.temp.ctry = oecd.dat.sf.ctry.all.trend %>% dplyr::filter(iso_a3 == ctry[i])
  print(df.temp.ctry)

  tryCatch({ fit_envcpt = envcpt(df.temp.ctry$usd.comm.year.CADC)}, error=function(e){skip_to_next <<- TRUE})  # Fit all models at once
  
  if(skip_to_next) { next } 

# save trend coeff
df.out.trend[i,c(2,7,12)] <- round(fit_envcpt$trendar2$coefficients[2:4],2) 
# 95%
df.out.trend[i,3:4] <- confint(fit_envcpt$trendar2, level=0.90)[2,] %>% round(2)
df.out.trend[i,8:9] <- confint(fit_envcpt$trendar2, level=0.90)[3,]%>% round(2)
df.out.trend[i,13:14] <- confint(fit_envcpt$trendar2, level=0.90)[4,]%>% round(2)
# 50%
df.out.trend[i,5:6] <- confint(fit_envcpt$trendar2, level=0.50)[2,] %>% round(2)
df.out.trend[i,10:11] <- confint(fit_envcpt$trendar2, level=0.50)[3,]%>% round(2)
df.out.trend[i,15:16] <- confint(fit_envcpt$trendar2, level=0.50)[4,]%>% round(2)
# Rsq
df.out.trend[i,17] <- glance(fit_envcpt$trendar2)[2] %>% round(3)
# NS regarding CI 50 and CI 95
df.out.trend[i,18] <- ifelse(df.out.trend$coef.int.025[i] <0 & df.out.trend$coef.int.975[i] > 0,"NS","S")
df.out.trend[i,19] <- ifelse(df.out.trend$coef.int.25[i] <0 & df.out.trend$coef.int.75[i] > 0,"NS","S")
rm(df.temp.ctry,fit_envcpt)

  }
df.out.trend$significant.90[df.out.trend$significant.90==""] <- NA
df.out.trend$significant.90 <- as.factor(df.out.trend$significant.90)
df.out.trend$significant.50[df.out.trend$significant.50==""] <- NA
df.out.trend$significant.50 <- as.factor(df.out.trend$significant.50)
df.out.trend$significant.confidence <- ifelse(df.out.trend$significant.90 == "S" & df.out.trend$significant.50 == "S",2,
                                              ifelse(df.out.trend$significant.90 == "NS" & df.out.trend$significant.50 == "S",1,0))
df.out.trend$significant.confidence <- as.factor(df.out.trend$significant.confidence)
summary(df.out.trend)

# total CADC investment by country over the period
oecd.invest <- oecd.dat.sf.ctry.all.trend %>%
  group_by(iso_a3) %>%
  summarize(usd.comm.year.CADC.total = sum(usd.comm.year.CADC),
            usd.disb.year.CADC.total = sum(usd.disb.year.CADC))
sum(oecd.invest$usd.comm.year.CADC.total)

# left_join
df.out.trend <- df.out.trend %>%
  left_join(oecd.invest,by="iso_a3")
write.csv(df.out.trend,here("outputs","prop_investment_trend_coeff_by_ctry.csv"))

