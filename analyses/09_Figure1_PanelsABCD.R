# Figure 1 - panel A, B, C and D
### Stephanie D'Agata & David Gill, Nov 2023
### updates: Steph D'Agata March 2024
### output: Figure 1 + trend OECD

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

  # summarize by year and cadc.type for the trend
oecd.dat.sf.ctry.trend <- oecd.dat.sf.ctry %>%
  dplyr::select(year,cadc.type,usd.comm,usd.disb) %>%
  st_drop_geometry() %>%
  dplyr::filter(!is.na(cadc.type)) %>%
  droplevels() %>%
  distinct() %>%
  dplyr::group_by(year,cadc.type) %>%
  dplyr::summarise(usd.comm.year=sum(usd.comm,na.rm=T) %>% round(2),
            usd.disb.year=sum(usd.disb,na.rm=T) %>% round(2)) %>%
  dplyr::arrange(cadc.type,year) %>%
  dplyr::group_by(cadc.type) %>%
  dplyr::mutate(cum.usd.comm=cumsum(usd.comm.year)%>% round(2),
         cum.usd.disb=cumsum(usd.disb.year)%>% round(2)) %>%
  dplyr::arrange(cadc.type,year) %>% 
  dplyr::group_by(year) %>% 
  dplyr::mutate(pct.comm=100*round(usd.comm.year/sum(usd.comm.year),4),
         pct.disb=100*round(usd.disb.year/sum(usd.disb.year),4)) %>% #equity % 11% in 2010, 42% in 2021
  gather(var,val,usd.comm.year:pct.disb) %>%
  as.data.frame()

oecd.dat.sf.ctry.trend %>% filter(year == 2010 & var == "usd.comm.year")
oecd.dat.sf.ctry.trend %>% filter(year == 2021 & var == "usd.comm.year")

# color blind
hcl_palettes(plot = TRUE)
# number of categories
q3 <- qualitative_hcl(4, palette = "Harmonic")
q3

library(rcartocolor)
display_carto_pal(8, "Teal")
my_colors = carto_pal(8, "Teal")
my_colors_3 = my_colors[c(3,5,8)]

######################### FIGURE 1A #############################

# plot temporal trend
p.oecd.trend <- ggplot(data=oecd.dat.sf.ctry.trend,aes(x = year, y = val, fill=cadc.type)) +
  geom_area() +
  labs(title=" 'equity` CADC vs other CADC", y="USD million committed") +
  facet_wrap(.~var, scales = "free_y") +
  #scale_fill_carto_d(palette = "Teal") +
  scale_fill_manual(values =my_colors_3) +
  #scale_fill_discrete_qualitative(palette = "Dark 2", nmax = 5, order = 3:5) +
  labs(fill = "CADC types") +
  theme_classic()
p.oecd.trend
ggsave(here("figures","SUPP_Figure1A.png"))
  
  # select only perc committed
p.oecd.trend.perc.commited <- oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm") %>%
  droplevels() %>%
  ggplot(aes(x = year, y = val, fill=cadc.type)) +
  geom_area() +
  #scale_fill_discrete_qualitative(palette = "Dark 2", nmax = 5, order = 3:5) +
  #scale_fill_carto_d(palette = "Teal") +
  scale_fill_manual(values =my_colors_3) +
  geom_hline(yintercept=75, linetype="dashed", color = "grey") +
  geom_hline(yintercept=50, linetype="dashed", color = "grey") +
  geom_hline(yintercept=25, linetype="dashed", color = "grey") +
  labs(y="Percentage committed investment (%)",x="Year") +
  labs(fill = "CADC types") +
  xlim(2010,2021) +
  theme_classic() +
  theme(legend.position="right") 
p.oecd.trend.perc.commited
ggsave(here("figures","SUPP_Figure1A.png"))

# select only perc committed - lines plots
p.oecd.trend.perc.commited.lines <- oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm") %>%
  droplevels() %>%
  ggplot(aes(x = year, y = val, color=cadc.type)) +
  geom_point() +
  geom_smooth(method="gam") +
  #scale_fill_discrete_qualitative(palette = "Dark 2", nmax = 5, order = 3:5) +
  #scale_color_carto_d(palette = "Teal") +
  scale_color_manual(values =my_colors_3) +
  geom_hline(yintercept=75, linetype="dashed", color = "grey") +
  geom_hline(yintercept=50, linetype="dashed", color = "grey") +
  geom_hline(yintercept=25, linetype="dashed", color = "grey") +
  labs(y="Percentage committed investment (%)",x="Year") +
  labs(fill = "CADC types") +
  xlim(2010,2021) +
  ylim(0,75)+
  theme_classic() +
  theme(legend.position="right") 
p.oecd.trend.perc.commited.lines
ggsave(here("figures","Figure1A_gam_perc.png"))

#
# detect change trend in % investment and save results in table
# dataframe to save AIC + weights of each model
N <- 12
df.out <- data.frame(models=character(N),
                           AIC.eq=numeric(N), 
                           AIC.weights.eq=numeric(N), 
                           AIC.CADC=numeric(N ), 
                           AIC.weights.CADC=numeric(N ), 
                           AIC.other.ec=numeric(N ), 
                           AIC.weights.other.ec=numeric(N ), 
                           AIC.commit.dollars=numeric(N ), 
                           AIC.weights.commit.dollars=numeric(N ), 
                           stringsAsFactors=FALSE) 

# dataframe to save models parameters 
N <- 6
df.out.trend <- data.frame(CADC=c("equity","other_CADC","CADC","other_ocean_economy","total committed_investment","CADC committed_investment"),
                     trend=numeric(N), 
                     coef.int.25 = numeric(N),
                     coef.int.975 = numeric(N),
                     AR.1=numeric(N), 
                     coef.int.25 = numeric(N),
                     coef.int.975 = numeric(N),
                     AR.2=numeric(N),
                     coef.int.25 = numeric(N),
                     coef.int.975 = numeric(N),
                     Adj.Rsq = numeric(N),
                     stringsAsFactors=FALSE)

# equity
library(EnvCpt)
df.perc.eq.CADC = oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm" & cadc.type == "equity CADC") %>% 
  dplyr::select(year,val)
fit_envcpt = envcpt(df.perc.eq.CADC$val)  # Fit all models at once

# AIC and weights
df.out$models <- names(AIC(fit_envcpt))

  # save trend coeff
df.out.trend[1,c(2,5,8)] <- round(fit_envcpt$trendar2$coefficients[2:4],2) 
df.out.trend[1,3:4] <- confint(fit_envcpt$trendar2, level=0.95)[2,] %>% round(2)
df.out.trend[1,6:7] <- confint(fit_envcpt$trendar2, level=0.95)[3,]%>% round(2)
df.out.trend[1,9:10] <- confint(fit_envcpt$trendar2, level=0.95)[4,]%>% round(2)
df.out.trend[1,11] <- glance(fit_envcpt$trendar2)[2] %>% round(3)

# other CADC
df.perc.other.CADC = oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm" & cadc.type == "other CADC") %>% 
  dplyr::select(year,val)

ggplot(df.perc.other.CADC,aes(x = year, y = val)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

rm(fit_envcpt)
fit_envcpt = envcpt(df.perc.other.CADC$val)  # Fit all models at once

summary(fit_envcpt$trendar2)

df.out$AIC.CADC <- AIC(fit_envcpt) %>% round(2)
df.out$AIC.weights.CADC <- AICweights(fit_envcpt) %>% round(2)

# save trend coeff
df.out.trend[2,c(2,5,8)] <- round(fit_envcpt$trendar2$coefficients[2:4],2) 
df.out.trend[2,3:4] <- confint(fit_envcpt$trendar2, level=0.95)[2,] %>% round(2)
df.out.trend[2,6:7] <- confint(fit_envcpt$trendar2, level=0.95)[3,]%>% round(2)
df.out.trend[2,9:10] <- confint(fit_envcpt$trendar2, level=0.95)[4,]%>% round(2)
df.out.trend[2,11] <- glance(fit_envcpt$trendar2)[2] %>% round(3)

############ all CADC
all.CADC.dat.sf.ctry.trend <- oecd.dat.sf.ctry.trend %>% 
  filter(cadc.type != "other ocean economy") %>%
  group_by(year,var) %>%
  dplyr::summarize(val.CADC = sum(val)) %>%
  as.data.frame()
all.CADC.dat.sf.ctry.trend


# check 
oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm" & cadc.type %in% c("equity CADC","other CADC")) %>% 
  dplyr::select(year,val) %>% arrange(year)

df.perc.CADC = all.CADC.dat.sf.ctry.trend %>% filter(var == "pct.comm") %>% 
  dplyr::select(year,val.CADC)

all.cadc.df <- oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm" & cadc.type != "other ocean economy") %>%
  dplyr::select(-var) %>% 
  spread(cadc.type,val) %>%
  mutate(CADC = rowSums(.[2:3])) %>%
  gather(perc.comm,val.CADC,-year)

ggplot(all.cadc.df,aes(x = year, y = val.CADC,color=perc.comm)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

rm(fit_envcpt.CADC)
fit_envcpt.CADC = envcpt(df.perc.CADC$val.CADC)  # Fit all models at once
AIC(fit_envcpt.CADC) %>% which.min()
AICweights(fit_envcpt.CADC) 
summary(fit_envcpt.CADC$trendar2)
summary(fit_envcpt.CADC$trendar1)
summary(fit_envcpt.CADC$trend)
plot(fit_envcpt.CADC$trendar2)

hist(df.perc.CADC$val.CADC)

df.out$AIC.CADC <- AIC(fit_envcpt) %>% round(2)
df.out$AIC.weights.CADC <- AICweights(fit_envcpt) %>% round(2)

plot(fit_envcpt,type='fit') # plots the fits

# save trend coeff
df.out.trend[3,c(2,5,8)] <- round(fit_envcpt.CADC$trendar2$coefficients[2:4],2) 
df.out.trend[3,3:4] <- confint(fit_envcpt.CADC$trendar2, level=0.95)[2,] %>% round(2)
df.out.trend[3,6:7] <- confint(fit_envcpt.CADC$trendar2, level=0.95)[3,]%>% round(2)
df.out.trend[3,9:10] <- confint(fit_envcpt.CADC$trendar2, level=0.95)[4,]%>% round(2)
df.out.trend[3,11] <- glance(fit_envcpt.CADC$trendar2)[2] %>% round(3)

# other ocean economy
df.perc.other.eco = oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm" & cadc.type == "other ocean economy") %>% 
  dplyr::select(year,val)
rm(fit_envcpt)
fit_envcpt = envcpt(df.perc.other.eco$val)  # Fit all models at once
AIC(fit_envcpt)
AICweights(fit_envcpt) 

df.out$AIC.other.ec <- AIC(fit_envcpt) %>% round(2)
df.out$AIC.weights.other.ec <- AICweights(fit_envcpt) %>% round(2)

# save trend coeff
df.out.trend[4,c(2,5,8)] <- round(fit_envcpt$trendar2$coefficients[2:4],2) 
df.out.trend[4,3:4] <- confint(fit_envcpt$trendar2, level=0.95)[2,] %>% round(2)
df.out.trend[4,6:7] <- confint(fit_envcpt$trendar2, level=0.95)[3,]%>% round(2)
df.out.trend[4,9:10] <- confint(fit_envcpt$trendar2, level=0.95)[4,]%>% round(2)
df.out.trend[4,11] <- glance(fit_envcpt$trendar2)[2] %>% round(3)

#***********************************************************************

# detect change trend in $USD investment
#
rm(df.total)
df.total = oecd.dat.sf.ctry.trend %>% filter(var == "usd.comm.year") %>% 
  dplyr::select(year,val) %>%
  group_by(year) %>%
  summarize(tot.com.year = sum(val)) %>%
  as.data.frame()

library(ggpmisc)
p.oecd.total.USD <- ggplot(df.total,aes(x = year, y = tot.com.year)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()
p.oecd.total.USD

# testing non stationary ull hypothesis - pvalue provide the proba that what is observed is due to chance
adf.test(df.total$tot.com.year) # p-value < 0.05 indicates the TS is stationary

library(EnvCpt)
rm(fit_envcpt)
fit_envcpt = envcpt(df.total$tot.com.year)  # Fit all models at once

df.out$AIC.commit.dollars <- AIC(fit_envcpt) %>% round(2)
df.out$AIC.weights.commit.dollars <- AICweights(fit_envcpt) %>% round(2)

# save trend coeff
df.out.trend[5,c(2,5,8)] <- round(fit_envcpt$trendar2$coefficients[2:4],2) 
df.out.trend[5,3:4] <- confint(fit_envcpt$trendar2, level=0.95)[2,] %>% round(2)
df.out.trend[5,6:7] <- confint(fit_envcpt$trendar2, level=0.95)[3,]%>% round(2)
df.out.trend[5,9:10] <- confint(fit_envcpt$trendar2, level=0.95)[4,]%>% round(2)
df.out.trend[5,11] <- glance(fit_envcpt$trendar2)[2] %>% round(3)

# only CADC
rm(df.total.cadc)
df.total.cadc = oecd.dat.sf.ctry.trend %>% filter(var == "usd.comm.year") %>% 
  filter(cadc.type != "other ocean economy")  %>% 
  dplyr::select(year,val) %>%
  group_by(year) %>%
  summarize(tot.com.year = sum(val)) %>%
  as.data.frame()

# testing non stationary ull hypothesis - pvalue provide the proba that what is observed is due to chance
adf.test(df.total.cadc$tot.com.year) # p-value < 0.05 indicates the TS is stationary

library(EnvCpt)
rm(fit_envcpt)
fit_envcpt = envcpt(df.total.cadc$tot.com.year)  # Fit all models at once
AICweights(fit_envcpt)

# save trend coeff
df.out.trend[6,c(2,5,8)] <- round(fit_envcpt$trendar2$coefficients[2:4],2) 
df.out.trend[6,3:4] <- confint(fit_envcpt$trendar2, level=0.95)[2,] %>% round(2)
df.out.trend[6,6:7] <- confint(fit_envcpt$trendar2, level=0.95)[3,]%>% round(2)
df.out.trend[6,9:10] <- confint(fit_envcpt$trendar2, level=0.95)[4,]%>% round(2)
df.out.trend[6,11] <- glance(fit_envcpt$trendar2)[2] %>% round(3)

# save the dataframe 
write.csv(df.out,here("outputs","prop_investment_models_comparisons.csv"))
write.csv(df.out.trend,here("outputs","prop_investment_trend_coeff.csv"))

#### investment by regions
oecd.dat.invest.by.subregion <- oecd.dat.sf.ctry %>%
  filter(cadc.type != "other ocean economy") %>%
  dplyr::select(region_un,subregion,region_wb,cadc.type,usd.comm,usd.disb) %>%
  st_drop_geometry() %>%
  dplyr::filter(!is.na(cadc.type)) %>%
  droplevels() %>%
  dplyr::group_by(region_un,subregion) %>%
  dplyr::summarise(usd.comm.subregion.cadc=sum(usd.comm,na.rm=T) %>% round(2),
                   usd.disb.subregion.cadc=sum(usd.disb,na.rm=T) %>% round(2)) %>%
  ungroup() %>%
  mutate(perc.comm = round(100*usd.comm.subregion.cadc/sum(usd.comm.subregion.cadc * !duplicated(usd.comm.subregion.cadc)),2), 
         perc.disb = round(100*usd.disb.subregion.cadc/sum(usd.disb.subregion.cadc * !duplicated(usd.disb.subregion.cadc)),2)) %>%
  mutate(cum.perc.comm = cumsum(perc.comm), 
         cum.perc.disb = cumsum(perc.disb)) %>%
  group_by(region_un) %>%
  dplyr::mutate(usd.comm.region.cadc=sum(usd.comm.subregion.cadc,na.rm=T) %>% round(2),
                   usd.disb.region.cadc=sum(usd.disb.subregion.cadc,na.rm=T) %>% round(2)) %>%
  arrange(desc(usd.comm.region.cadc)) %>%
  ungroup() %>%
  mutate(cum.invest.comm_usd.regions = cumsum(usd.comm.region.cadc * !duplicated(usd.comm.region.cadc)),
         cum.invest.disb.usd.regions = cumsum(usd.disb.region.cadc * !duplicated(usd.disb.region.cadc))) %>% 
  ungroup() %>%
  mutate(perc.comm.regions = round(100*usd.comm.region.cadc/sum(usd.comm.region.cadc * !duplicated(usd.comm.region.cadc)),2), 
         perc.disb.regions = round(100*usd.disb.region.cadc/sum(usd.disb.region.cadc * !duplicated(usd.disb.region.cadc)),2)) %>%
  mutate(cum.perc.comm.regions = cumsum(perc.comm.regions * !duplicated(perc.comm.regions)), 
         cum.perc.disb.regions = cumsum(perc.disb.regions * !duplicated(perc.disb.regions))) %>%
  #dplyr::arrange(desc(cum.invest.comm_usd.regions)) %>%
  as.data.frame()

oecd.dat.invest.by.subregion
write.csv(oecd.dat.invest.by.subregion,here("outputs","investment_subregion.csv"))

sum(oecd.dat.invest.by.subregion$usd.comm.subregion.cadc) # 32.6 billions

#### investment by country by year
oecd.dat.invest.by.country <- oecd.dat.sf.ctry %>%
  dplyr::select(iso_a3,name_en,cadc.type,usd.comm,usd.disb) %>%
  st_drop_geometry() %>%
  dplyr::filter(!is.na(cadc.type)) %>%
  droplevels() %>%
  dplyr::group_by(iso_a3,name_en,cadc.type) %>%
  dplyr::summarise(usd.comm.ctry.cadc=sum(usd.comm,na.rm=T) %>% round(2),
                   usd.disb.ctry.cadc=sum(usd.disb,na.rm=T) %>% round(2)) %>%
  dplyr::arrange(iso_a3,name_en,cadc.type) %>%
  filter(cadc.type != "other ocean economy") %>%
  droplevels() %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::mutate(tot.usd.comm.ctry=sum(usd.comm.ctry.cadc,na.rm=T)%>% round(2),
         tot.usd.disb.ctry=sum(usd.disb.ctry.cadc,na.rm=T)%>% round(2)) %>%
  ungroup() %>%
  arrange(-tot.usd.comm.ctry) %>%
  as.data.frame()
oecd.dat.invest.by.country
oecd.dat.invest.by.country %>% filter(iso_a3 == "MAR")

write.csv(oecd.dat.invest.by.country,here("outputs","investment_ctry.csv"))

# cum sum by country 
rm(oecd.dat.invest.by.country.cumsum)
oecd.dat.invest.by.country.cumsum <- oecd.dat.sf.ctry %>%
  dplyr::select(iso_a3,name_en,cadc.type,usd.comm,usd.disb) %>%
  st_drop_geometry() %>%
  dplyr::filter(!is.na(cadc.type)) %>%
  filter(cadc.type != "other ocean economy") %>%
  droplevels() %>%
  dplyr::group_by(iso_a3,name_en,cadc.type) %>%
  dplyr::summarise(usd.comm.ctry.cadc=sum(usd.comm,na.rm=T) %>% round(2),
                   usd.disb.ctry.cadc=sum(usd.disb,na.rm=T) %>% round(2)) %>%
  dplyr::arrange(iso_a3,name_en,cadc.type) %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::mutate(tot.usd.comm.ctry=sum(usd.comm.ctry.cadc,na.rm=T)%>% round(2),
                tot.usd.disb.ctry=sum(usd.disb.ctry.cadc,na.rm=T)%>% round(2)) %>%
  arrange(desc(tot.usd.comm.ctry)) %>%
  dplyr::select(iso_a3,tot.usd.comm.ctry,tot.usd.disb.ctry) %>%
  distinct() %>%
  ungroup() %>%
  dplyr::mutate(cum.usd.comm.ctry=cumsum(as.numeric(tot.usd.comm.ctry))%>% round(2),
                cum.usd.disb.ctry=cumsum(tot.usd.disb.ctry)%>% round(2)) %>%
  mutate(perc.cumsum.comm = cum.usd.comm.ctry/sum(tot.usd.comm.ctry),
         perc.cumsum.disb = cum.usd.disb.ctry/sum(tot.usd.disb.ctry))
oecd.dat.invest.by.country.cumsum

# total CADC investment
sum(oecd.dat.invest.by.country.cumsum$tot.usd.comm.ctry)

# left join with main
oecd.dat.invest.by.country <- oecd.dat.invest.by.country %>%
  left_join(oecd.dat.invest.by.country.cumsum %>% dplyr::select(iso_a3,perc.cumsum.comm,perc.cumsum.disb),by="iso_a3")
oecd.dat.invest.by.country

# simple
oecd.dat.invest.by.country.dist <- oecd.dat.invest.by.country %>%
  dplyr::select(iso_a3,name_en,tot.usd.comm.ctry,perc.cumsum.comm) %>%
  distinct()

ymax=max(oecd.dat.invest.by.country.dist$tot.usd.comm.ctry)

# plot
oecd.dat.invest.by.country.plot <- ggplot(oecd.dat.invest.by.country,
                                          aes(x=reorder(name_en,-usd.comm.ctry.cadc))) +
  geom_bar(position="stack", stat="identity",aes(y=usd.comm.ctry.cadc,fill=cadc.type)) +
  scale_fill_carto_d(palette = "Teal") +
  labs(y="committed CADC investment billions ($)",x="Countries") +
  labs(fill = "CADC types") +
  geom_point(mapping=aes(x=reorder(name_en,-perc.cumsum.comm), y=perc.cumsum.comm*ymax)) + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  scale_y_continuous(name = 'Number of users', sec.axis = sec_axis(~./max(oecd.dat.invest.by.country$tot.usd.comm.ctry), 
                                                                   name = "Cumulative percentage of routes [%]",labels = scales::percent))+
  
  theme_bw()
oecd.dat.invest.by.country.plot # correct st helena
ggsave(here("figures","SUPP_Figure_country_invest.png"))

#################################################
# Extra
#################################################

# barplot
p.oecd.trend.perc.commited.barplot <- oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm") %>%
  droplevels() %>%
  ggplot(aes(x = year, y = val, fill=cadc.type,label = round(val,1))) +
  geom_bar(position="stack", stat="identity") +
  #scale_fill_discrete_qualitative(palette = "Dark 2", nmax = 5, order = 3:5) +
  scale_fill_carto_d(palette = "Teal") +
  geom_text(position = position_stack(vjust=0.5), size=4) +
  #geom_hline(yintercept=75, linetype="dashed", color = "grey") +
  #geom_hline(yintercept=50, linetype="dashed", color = "grey") +
  #geom_hline(yintercept=25, linetype="dashed", color = "grey") +
  labs(y="Percentage committed investment (%)",x="Year") +
  labs(fill = "CADC types") +
  #xlim(2009,2022) +
  theme_classic() +
  theme(legend.position="right") 
p.oecd.trend.perc.commited.barplot
ggsave(here("figures","SUPP_Figure1A_barplot_perc.png"))
# supplemental $ committed by year
# select only perc committed
p.oecd.trend.usd.commited <- oecd.dat.sf.ctry.trend %>% filter(var == "usd.comm.year") %>%
  droplevels() %>%
  ggplot(aes(x = year, y = val/1000, fill=cadc.type)) +
  geom_area() +
  #scale_fill_discrete_qualitative(palette = "Dark 3", nmax = 6, order = 3:5) +
  scale_fill_carto_d(palette = "Teal") +
  labs(y="billions USD committed investment ($)",x="Year") +
  labs(fill = "CADC types") +
  xlim(2010,2021) +
  theme_classic() +
  theme(legend.position=c(0.2,0.9)) 
p.oecd.trend.usd.commited
ggsave(here("figures","Figure1A_invest.png"))

# plot temporal trend
p.oecd.trend <- oecd.trend %>% 
  filter()
ggplot( aes(x = year, y = val, fill=cadc.type)) +
  geom_area() +
  labs(title=" 'equity` CADC vs other CADC", y="USD million committed") +
  facet_wrap(.~var, scales = "free_y") +
  theme_classic()
p.oecd.trend

######################### FIGURE 1B #############################
# maps of total commited investment by countries
# select only perc committed
rm(oecd.dat.sf.ctry.total.commited)
oecd.dat.sf.ctry.total.commited <- oecd.dat.sf.ctry %>%
  dplyr::filter(cadc.type != "other ocean economy") %>%
  dplyr::select(iso_a3,year,cadc.type,usd.comm,usd.disb) %>%
  dplyr::group_by(iso_a3,year) %>%
  dplyr::summarise(usd.comm.year=sum(usd.comm,na.rm=T) %>% round(2),
            usd.disb.year=sum(usd.disb,na.rm=T) %>% round(2)) %>%
  arrange(iso_a3,year) %>%
  ungroup() %>%
  complete(iso_a3,year,fill = list(usd.comm.year = 0,usd.disb.year=0)) %>%
  group_by(iso_a3) %>%
  mutate(cum.usd.comm=cumsum(usd.comm.year)%>% round(2),
         cum.usd.disb=cumsum(usd.disb.year)%>% round(2)) %>%
  ungroup() %>%
  as.data.frame()
oecd.dat.sf.ctry.total.commited %>% filter(iso_a3 == "MAR")

## map cumulative number of initiatives
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
crs.val.robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs" # The World Robinson projection (ESRI 54030)
projected.world = st_transform(world, crs.val.robinson)

oecd.dat.sf.ctry.total.commited.sf <- projected.world %>%
  left_join(oecd.dat.sf.ctry.total.commited,by="iso_a3")

p.map.2021.cum.ctry <- ggplot(data =  projected.world) +
  geom_sf(color="black",fill = "white") +
  geom_sf(data=oecd.dat.sf.ctry.total.commited.sf %>% filter(year == "2021"),aes(fill = cum.usd.comm)) +
  scale_fill_viridis_c(option = "plasma",na.value = "whitesmoke") +
  labs(fill = "Investment ($)") +
  theme_bw()
p.map.2021.cum.ctry
ggsave(here("figures","SUPP_Figure1B.png"),p.map.2021.cum.ctry)

######################### FIGURE 1C and 1D #############################

# create df - 1 raw = 1 country = mean contextual equity, % equity CADC, total investment (cum2021) # oecd.dat.sf.ctry.total.commited
oecd.dat.sf.ctry.equity <- oecd.dat.sf.ctry %>%
  dplyr::filter(cadc.type %in% c("equity CADC","other CADC")) %>%
  st_drop_geometry() %>%
  dplyr::select(iso_a3,name_en,cadc.type,total.project) %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::mutate(tot.project.CADC = sum(total.project)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(iso_a3,cadc.type) %>%
  dplyr::mutate(tot.project.eq=sum(total.project)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(perc.equi = 100*round(tot.project.eq/tot.project.CADC,3)) %>%
  dplyr::select(-total.project) %>%
  distinct() %>%
  as.data.frame()
oecd.dat.sf.ctry.equity

# add total investment in 2021
oecd.dat.sf.ctry.equity.commit <- oecd.dat.sf.ctry.equity %>%
  left_join(oecd.dat.sf.ctry.total.commited %>% filter(year == "2021"),by="iso_a3")

# load composite scores
df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.scores.rds"))

# load trend investment by country
df.out.trend.ctr <- read.csv(here("outputs","prop_investment_trend_coeff_by_ctry.csv"))

# SUMMARIZE one indicator by country
rm(cont.ineq.ctry)
cont.ineq.ctry <- df.risk.stack.sc.ctry.ind.coastal %>%
  dplyr::group_by(iso_a3) %>%
  dplyr::mutate(cont.eq.score.mean = mean(risk.mat.score.mean.without.dprep,na.rm=T) %>% round(2),
            cont.eq.sd =  sd(risk.mat.score.mean.without.dprep,na.rm=T) %>% round(2)) %>%
  mutate(cont.eq.score.CV = 100*(cont.eq.sd/cont.eq.score.mean) %>% round(4))%>%
  ungroup() %>%
  dplyr::select(iso_a3,cont.eq.score.mean,cont.eq.sd,cont.eq.score.CV) %>%
  distinct() %>%
  dplyr::filter(!is.na(cont.eq.score.mean)) %>%
  as.data.frame()

head(cont.ineq.ctry)
dim(cont.ineq.ctry)
summary(cont.ineq.ctry)

# join world
cont.ineq.ctry.sf <- projected.world %>%
  left_join(cont.ineq.ctry,by="iso_a3")

# add trend data
cont.ineq.ctry.sf <- cont.ineq.ctry.sf %>%
  left_join(df.out.trend.ctr,by="iso_a3")


# add add color columns
cont.ineq.ctry.sf <- cont.ineq.ctry.sf %>%
  mutate(significant.50 = as.factor(significant.50),
         significant.90 = as.factor(significant.90)) %>%
  mutate(color.vect.50 = ifelse(trend > 0 & significant.50 == "S","pos",
                              ifelse(trend < 0 & significant.50 == "S","neg",
                                     ifelse(trend > 0 & significant.50 == "NS","NS.pos",
                                            ifelse(trend < 0 & significant.50 =="NS","NS.neg",NA))))) %>%
  mutate(color.vect.90 = ifelse(trend > 0 & significant.90 == "S","pos",
                                ifelse(trend < 0 & significant.90 == "S","neg",
                                  ifelse(trend > 0 & significant.90 == "NS","NS.pos",
                                    ifelse(trend < 0 & significant.90 =="NS","NS.neg",NA)))))
# remove NAs from trend color
cont.ineq.ctry.sf$color.vect.50 <- addNA(cont.ineq.ctry.sf$color.vect.50)  
cont.ineq.ctry.sf$color.vect.90 <- addNA(cont.ineq.ctry.sf$color.vect.90)  

# change order levels
levels(cont.ineq.ctry.sf$color.vect.50) <- c("Negative","Negative.uncertain","Positive.uncertain","Positive","NA")
levels(cont.ineq.ctry.sf$color.vect.90) <- c("Negative","Negative.uncertain","Positive.uncertain","Positive","NA")
cont.ineq.ctry.sf$color.vect.50 <- factor(cont.ineq.ctry.sf$color.vect.50 ,levels=c("Negative","Negative.uncertain","Positive.uncertain","Positive","NA"))
cont.ineq.ctry.sf$color.vect.90 <- factor(cont.ineq.ctry.sf$color.vect.90 ,levels=c("Negative","Negative.uncertain","Positive.uncertain","Positive","NA"))

# reduce number of factors
cont.ineq.ctry.sf$color.vect.50.V2 <- fct_collapse(cont.ineq.ctry.sf$color.vect.50, Uncertain = c("Negative.uncertain", "Positive.uncertain"))
cont.ineq.ctry.sf$color.vect.90.V2 <- fct_collapse(cont.ineq.ctry.sf$color.vect.90, Uncertain = c("Negative.uncertain", "Positive.uncertain"))

tail(cont.ineq.ctry.sf)
dim(cont.ineq.ctry.sf)

# remove NA in contextual equity
cont.ineq.ctry.sf <- cont.ineq.ctry.sf %>%
  drop_na(cont.eq.score.mean) %>%
  drop_na(usd.comm.year.CADC.total)

p.map.ctry.equity <- ggplot(data =  projected.world) +
  geom_sf(color="black",fill = "white") +
  geom_sf(data=cont.ineq.ctry.sf,aes(fill = cont.eq.score.mean)) +
  #scale_fill_viridis_c(option = "magma",na.value = "whitesmoke",direction = -1) +
  scale_fill_continuous_diverging(palette = "Blue-Red 3",na.value = "whitesmoke")+
  labs(colour = "Mean contextual equity") +
  theme_bw()
p.map.ctry.equity
ggsave(here("figures","SUPP_Figure1C.png"),p.map.ctry.equity)

### chloropeth map mean cont inequity and commited investment
## install just cowplot and sf
install.packages(c("cowplot"))
library(cowplot)
## install all suggested dependencies
install.packages("biscale")

library(biscale)

# create classes
N <- 4
pal = "PinkGrn"
data.eq <- bi_class(cont.ineq.ctry.sf, x = usd.comm.year.CADC.total, y = cont.eq.score.mean, style = "jenks", dim = N)

break_vals.eq <- bi_class_breaks(cont.ineq.ctry.sf, style = "jenks",
                              x = usd.comm.year.CADC.total, y = cont.eq.score.mean, dim = N, dig_lab = c(x = 4, y = 5),
                              split = TRUE)

map <- ggplot(data =  projected.world) +
  geom_sf(color="black",fill = "white") +
  geom_sf(data = data.eq, mapping = aes(fill = bi_class), color = "black", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = pal, dim = N) +
  theme(plot.margin = margin(-2, 0, -2, 0, "cm"))+
  bi_theme()
map

legend <- bi_legend(pal = pal,
                    dim = N,
                    xlab = "Committed investment (millions $) ",
                    ylab = "Average Contextual Vulnerability ",
                    size = 8,
                    breaks = break_vals.eq)

finalPlot.cont.ineq <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, .2, 0.3, 0.3)
finalPlot.cont.ineq
ggsave(here("figures","Figure1B_chloropeth.png"),finalPlot.cont.ineq,width=8,height = 8)


g <- ggplot_build(map)

colors.legend <- cbind(g$data[[2]]["fill"],g$data[[2]]["group"],g$data[[2]]["geometry"]) %>%
  distinct() %>%
  arrange(group)

test.color <- data.eq %>% 
  dplyr::select(iso_a3,geometry) %>%
  distinct()

test.color <- test.color %>%
  left_join(colors.legend,by="geometry")

# # CADC invest - contextual ineq - log
# sc.risk.CADC.dollars.log <- ggplot(oecd.dat.sf.ctry.equity.commit.ineq %>% filter(cadc.type=="equity CADC"),aes(x=log10(cum.usd.comm+1),y=cont.eq.score.mean,label=name_en)) +
#   geom_point()+
#   geom_text_repel() +
#   #geom_smooth(method="lm") +
#   #scale_color_gradient(low="blue", high="red")+
#   xlab("log(Total investment by country) ($)") +
#   ylab("Average contextual inequity by country") +
#   ylim(0,0.7) + #   xlim(0,1)  + 
#   theme(legend.position = "bottom")+
#   theme_bw()
# sc.risk.CADC.dollars.log

# CADC invest - contextual ineq
# CI 50%
sc.risk.CADC.dollars.50 <- ggplot(cont.ineq.ctry.sf,aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean,label=name_en,color=color.vect.50.V2,size=abs(trend))) +
  geom_point()+
  geom_text_repel( size = 4,force=4) +
  #geom_smooth(method="lm") +
  #scale_color_gradient(low="blue", high="red")+
  geom_abline(slope=0,intercept=0.5,col="black",linetype=2)+
  xlab("Total investment by country ($)") +
  ylab("Average contextual vulnerability by country") +
  ylim(0,0.75) + #   xlim(0,1)  + 
  scale_color_manual(values = c("firebrick4","grey","blue4","ivory")) +
  labs(color = "Investment direction",size="Investment level (millions USD/year)") +
  theme(legend.position = c(0.7, 0.1),legend.direction="horizontal",
        panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "lightgrey"),
        legend.box.background = element_rect(colour = "black"))

sc.risk.CADC.dollars.50
ggsave(here("figures","Figure1C_trend_50CI.simple.png"),sc.risk.CADC.dollars.50,width=8,height = 8)
# CI 90%
sc.risk.CADC.dollars.90 <- ggplot(cont.ineq.ctry.sf,aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean,label=name_en,color=color.vect.90.V2,size=abs(trend))) +
  geom_point()+
  geom_text_repel( size = 4,force=4,max.overlaps=10) +
  #geom_smooth(method="lm") +
  #scale_color_gradient(low="blue", high="red")+
  geom_abline(slope=0,intercept=0.5,col="black",linetype=2)+
  xlab("Total investment by country ($)") +
  ylab("Average contextual vulnerability by country") +
  ylim(0,0.75) + #   xlim(0,1)  + 
  scale_color_manual(values = c("firebrick4","darkgrey","blue4","lightgrey")) +
  labs(color = "Investment direction",size="Investment level (millions USD/year)") +
  theme(legend.position = c(0.5, 0.1),legend.direction="horizontal",
        panel.background = element_rect(fill = "white",
                                        colour = "grey",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "lightgrey"),
        legend.box.background = element_rect(colour = "black"))

sc.risk.CADC.dollars.90
ggsave(here("figures","Figure1C_trend_90CI.simple.png"),sc.risk.CADC.dollars.90,width=8,height = 8)

# # CI 50%
# sc.risk.CADC.dollars.50 <- ggplot(cont.ineq.ctry.sf,aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean,label=name_en,color=color.vect.50,size=abs(trend))) +
#   geom_point()+
#   geom_text_repel( size = 4,force=4) +
#   #geom_smooth(method="lm") +
#   #scale_color_gradient(low="blue", high="red")+
#   geom_abline(slope=0,intercept=0.5,col="black",linetype=2)+
#   xlab("Total investment by country ($)") +
#   ylab("Average contextual inequity by country") +
#   ylim(0,0.75) + #   xlim(0,1)  + 
#   scale_color_manual(values = c("firebrick4","indianred2","lightblue2","blue4","ivory")) +
#   labs(color = "Investment direction",size="Investment level (millions USD/year)") +
#   theme(legend.position = c(0.7, 0.1),legend.direction="horizontal",
#         panel.background = element_rect(fill = "white",
#                                         colour = "grey",
#                                         size = 0.5, linetype = "solid"),
#         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                         colour = "lightgrey"), 
#         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                         colour = "lightgrey"),
#         legend.box.background = element_rect(colour = "black"))
# 
# sc.risk.CADC.dollars.50
# ggsave(here("figures","Figure1C_trend_50CI.png"),sc.risk.CADC.dollars.50,width=10,height=10)
# # CI 90%
# sc.risk.CADC.dollars.90 <- ggplot(cont.ineq.ctry.sf,aes(x=usd.comm.year.CADC.total,y=cont.eq.score.mean,label=name_en,color=color.vect.90,size=abs(trend))) +
#   geom_point()+
#   geom_text_repel( size = 4,force=4,max.overlaps=10) +
#   #geom_smooth(method="lm") +
#   #scale_color_gradient(low="blue", high="red")+
#   geom_abline(slope=0,intercept=0.5,col="black",linetype=2)+
#   xlab("Total investment by country ($)") +
#   ylab("Average contextual inequity by country") +
#   ylim(0,0.75) + #   xlim(0,1)  + 
#   scale_color_manual(values = c("firebrick4","indianred2","lightblue2","blue4","ivory")) +
#   labs(color = "Investment direction",size="Investment level (millions USD/year)") +
#   theme(legend.position = c(0.5, 0.1),legend.direction="horizontal",
#         panel.background = element_rect(fill = "white",
#                                         colour = "grey",
#                                         size = 0.5, linetype = "solid"),
#         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                         colour = "lightgrey"), 
#         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                         colour = "lightgrey"),
#         legend.box.background = element_rect(colour = "black"))
# 
# sc.risk.CADC.dollars.90
# ggsave(here("figures","Figure1C_trend_90CI.png"),sc.risk.CADC.dollars.90,width=10,height=10)

# # logged
# sc.risk.CADC.dollars.log <- ggplot(cont.ineq.ctry.sf,aes(x=log10(usd.comm.year.CADC.total),y=cont.eq.score.mean,label=name_en,color=color.vect,size=abs(trend))) +
#   geom_point()+
#   geom_text_repel( size = 4,force=4) +
#   #geom_smooth(method="lm") +
#   #scale_color_gradient(low="blue", high="red")+
#   geom_abline(slope=0,intercept=0.5,col="black",linetype=2)+
#   xlab("Total investment by country ($)") +
#   ylab("Average contextual inequity by country") +
#   ylim(0,0.75) + #   xlim(0,1)  + 
#   scale_color_manual(values = c("firebrick4","darkgrey","blue4")) +
#   labs(color = "Investment trend",size="Investment level") +
#   theme(legend.position = c(0.7, 0.1),legend.direction="horizontal",
#         panel.background = element_rect(fill = "white",
#                                         colour = "grey",
#                                         size = 0.5, linetype = "solid"),
#         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                         colour = "lightgrey"), 
#         panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                         colour = "lightgrey"),
#         legend.box.background = element_rect(colour = "black"))
# 
# sc.risk.CADC.dollars.log
# ggsave(here("figures","Figure1C_trend_log.png"),sc.risk.CADC.dollars.log,width=10,height=10)

# CADC risk - equity
  # join contextual vulnerability
oecd.dat.sf.ctry.equity.commit.vulnerab <- oecd.dat.sf.ctry.equity.commit %>%
  left_join(data.eq %>% st_drop_geometry() %>% dplyr::select(iso_a3,cont.eq.score.mean,cont.eq.sd,cont.eq.score.CV,bi_class))

oecd.dat.sf.ctry.equity.commit.vulnerab <- oecd.dat.sf.ctry.equity.commit.vulnerab %>%
  left_join(test.color,by="iso_a3") %>%
  filter(!is.na(cont.eq.score.mean)) %>%
  filter(cadc.type=="equity CADC") %>%
  droplevels()
oecd.dat.sf.ctry.equity.commit.vulnerab$fill

library(loon)
l_colorName(c("#FFFF00000000", "#FF00FF", "blue"))
oecd.dat.sf.ctry.equity.commit.vulnerab$colorname <-  l_colorName(as.character(oecd.dat.sf.ctry.equity.commit.vulnerab$fill))

# create color scale
color.biclass <- oecd.dat.sf.ctry.equity.commit.vulnerab %>%
  dplyr::select(bi_class,fill) %>%
  distinct() %>%
  mutate(bi_class = as.factor(bi_class))

CADC.risk.eq.dollars <- ggplot(oecd.dat.sf.ctry.equity.commit.vulnerab %>% filter(cadc.type=="equity CADC"),aes(x=perc.equi,y=cont.eq.score.mean,label=name_en)) +
  geom_point(aes(colour = colorname),size=5) +
  scale_color_identity() +
 # scale_size_continuous(limits=c(2, 5), breaks=seq(2, 5, by=0.5))
 # scale_color_gradient(low="blue", high="red",name = "Investment ($)")+
  xlab("Equity (% total of CADC projects)") +
  geom_text_repel(force_pull=2) +
  ylab("Average contextual vulnerability by country") +
  xlim(0,50) +  ylim(0,0.75) +
  geom_vline(xintercept=50,linetype = 2)+
  geom_hline(yintercept=0.5,linetype = 2)+
  theme_bw()
CADC.risk.eq.dollars
ggsave(here("figures","Figure1D.png"),CADC.risk.eq.dollars,width=8,height = 8)

# full panel
CADC.full.panel.1 <- ggarrange(p.oecd.trend.usd.commited,
                               finalPlot.cont.ineq,
                               sc.risk.CADC.dollars.90,
                             CADC.risk.eq.dollars,
                             labels=c("a","b","c","d"),ncol=2,nrow=2)
ggsave(here("figures","CADC.risk.equity.ABCD_1.pdf"),CADC.full.panel.1,width=12,height=12)
ggsave(here("figures","CADC.risk.equity.ABCD_1.tiff"),CADC.full.panel.1,width=12,height=12)

# three panel
CADC.full.panel.2 <- ggarrange(finalPlot.cont.ineq,
                               ggarrange(sc.risk.CADC.dollars.90,
                               CADC.risk.eq.dollars,labels=c("B","C")),
                               nrow = 2, 
                               labels = "A",align="hv")
ggsave(here("figures","CADC.risk.equity.ABC_2.pdf"),CADC.full.panel.2,width=10,height=10)
ggsave(here("figures","CADC.risk.equity.ABC_2.tiff"),CADC.full.panel.2,width=10,height=10)

# using draw_plot
CADC.full.panel.3 <-ggdraw() +
  draw_plot(finalPlot.cont.ineq, x = 0, y = .5, width = 1, height = .5) +
  draw_plot(sc.risk.CADC.dollars.90, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(CADC.risk.eq.dollars, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), size = 15,
                  x = c(0, 0, 0.5), y = c(1, 0.5, 0.5))
ggsave(here("figures","CADC.risk.equity.ABC_3.pdf"),CADC.full.panel.3,width=12,height=12)
ggsave(here("figures","CADC.risk.equity.ABC_3.png"),CADC.full.panel.3,width=12,height=12)

# full panel
CADC.full.panel.2 <- ggarrange(p.oecd.trend.perc.commited,
                               p.map.2021.cum.ctry,
                               sc.risk.CADC.dollars,
                               CADC.risk.eq.dollars,
                               labels=c("a","b","c","d"),ncol=2,nrow=2)
ggsave(here("figures","CADC.risk.equity.ABCD.pdf"),CADC.full.panel.1,width=10,height=10)
ggsave(here("figures","CADC.risk.equity.ABCD.png"),CADC.full.panel.1,width=10,height=10)

library(cowplot)

plot_grid(p.oecd.trend.perc.commited,
          p.map.2021.cum.ctry,
          sc.risk.CADC.dollars,
          CADC.risk.eq.dollars, ncol=2, align="hv",labels=c("a","b","c","d"))

file4 <- tempfile("file4", fileext = ".pdf")
save_plot(file4, p4, ncol = 2, base_asp = 1.1)
### supplementals - correlation between disbursement and committed
disb_comm_ctry <- oecd.dat.sf.ctry %>%
  select(iso_a3,total.comm.usd,total.distr.usd) %>%
  st_drop_geometry %>%
  group_by(iso_a3) %>%
  summarize(sum.disb = sum(total.distr.usd,na.rm=T),sum.com = sum(total.comm.usd,na.rm=T)) %>%
  distinct() %>%
  as.data.frame()
disb_comm_ctry <- disb_comm_ctry[-1,]

cor_disb_committed <- ggplot(disb_comm_ctry,aes(x=sum.com,y=sum.disb,label=as.factor(iso_a3))) +
  geom_point() +
  geom_abline(slope=1,intercept=0) +
  theme_bw() +
  xlab("Total commited investment ($) by country") +
  ylab("Total disbursed investment ($) by country") +
  geom_text_repel() +
  xlim(0,3000) + ylim(0,3000)  +
  geom_smooth(method = "lm", 
              formula = y ~ x) +
  stat_cor(label.y = 1300,label.x=2450)#+
 # stat_regline_equation(label.y = 1400,label.x=2450) 
cor_disb_committed
ggsave(here("figures","SUPP_cor_commited_disb_ctry.png"),cor_disb_committed)

# Figure 1A - trends
#---- summaries ----
  # disbursement
(oecd.trend.disb <- oecd.dat.sf.ctry.scores %>% 
   group_by(year,cadc.type) %>% 
   summarise(usd.disb=sum(disbursement_usd_million_defl,na.rm=T)) %>% 
   ggplot( aes(x = year, y = usd.disb, fill=cadc.type)) +
   geom_area() +
   labs(title="`non-equity` CADC vs `equity` CADC vs other ocean economy", y="USD million disbursment")+
   theme_classic())
ggsave("oecd_trend.png",width = 8, height = 4)

oecd.cadc.ctry.trend <- oecd %>% 
  group_by(year,ctry,cadc) %>% 
  summarise(usd.disb=sum(disbursement_usd_million_defl,na.rm=T)) %>% 
  filter(cadc=="cadc") %>% 
  group_by(ctry) %>% 
  arrange(year) %>% 
  summarise(start.yr=first(usd.disb),
            end.yr=last(usd.disb),
            total=sum(usd.disb)) %>% 
  mutate(trend=((end.yr)-(start.yr))) %>% 
  filter(ctry%in%projected.world$name_en) %>% 
  arrange(-trend)
head(oecd.cadc.ctry.trend)


(p.cadc.trend.usd <- ggplot(oecd.cadc.ctry.trend[1:20,],aes(x=reorder(ctry,trend),y=trend)) +
    geom_bar(stat = "identity")+
    labs(title="Top 20 increase in ODA 2010-2021",x="country",y="increase in $USD disbursed")+
    coord_flip()+
    theme_classic())
ggsave("top_cadc_trend.png",width = 8, height = 4)

(p.cadc.total.usd <- oecd.cadc.ctry.trend%>% 
    arrange(-total) %>% 
    slice(1:20) %>% 
    ggplot(aes(x=reorder(ctry,total),y=total)) +
    geom_bar(stat = "identity")+
    labs(title="Top 20 in ODA 2010-2021",x="country",y="$USD disbursed total")+
    coord_flip()+
    theme_classic())
ggsave("top_cadc_total.png",width = 8, height = 4)

# Figure 1B - maps
(p.cadc.ctry.proj <- oecd.dat.sf.ctry.scores %>% 
    # convert everything else to NAs to retain the other countries on map
    mutate(cadc.project=ifelse(cadc.type%in%c("equity CADC","not equity CADC"),NA,total.project)) %>% 
    ggplot() +
    geom_sf(aes(fill = cadc.project))  + 
    theme_void() +
    labs(title = "# CADC projects per country") + 
    scale_fill_distiller(palette = "Spectral", na.value = "white"))

(p.cadc.ctry.usd <- oecd.dat.sf.ctry %>% 
    # convert everything else to NAs to retain the other countries on map
    mutate(cadc.usd=ifelse(cadc.type%in%c("equity CADC","not equity CADC"),NA,total.usd)) %>% 
    ggplot() +
    geom_sf(aes(fill = cadc.usd))  + 
    theme_void() +
    labs(title = "CADC $USD million per country") + 
    scale_fill_distiller(palette = "Spectral", na.value = "white"))

cowplot::plot_grid(p.cadc.ctry.proj,p.cadc.ctry.usd, nrow = 2)
ggsave("cadc_ctry_map.png",width = 4, height = 8)

#----- maps ---- 
# summarise values 
oecd.ctry <- oecd %>% 
  group_by(ctry,cadc.type) %>%  
  summarise(total.project=n(),
            total.usd=sum(disbursement_usd_million_defl,na.rm=T))
export(oecd.ctry,here("data","derived-data","ocean_ODA_CADC_2010_2021_equity.csv"))

# join oecd data to map
CADC.OECD.ocean.wide.proj.ctr.sf <- projected.world %>%
  left_join(oecd.ctry,by=c("name_en"="ctry")) %>% 
  rename(ctry=name_en)


# join oecd trend data to map
#CADC.OECD.ocean.wide.proj.ctr.sf.trend <- projected.world %>%
#   left_join(oecd.cadc.ctry.trend,by=c("name_en"="ctry")) %>% 
#   rename(ctry=name_en)
# 
# (p.cadc.ctry.trend.usd <- CADC.OECD.ocean.wide.proj.ctr.sf.trend %>% 
#     mutate(trend.usd=ifelse(cadc!="CADC",NA,trend)) %>% 
#     ggplot() +
#     geom_sf(aes(fill = trend.usd))  + 
#     theme_void() +
#     scale_fill_distiller(palette = "Spectral", na.value = "white"))




