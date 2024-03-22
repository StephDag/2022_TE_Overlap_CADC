# Figure Panel 
### Stephanie D'Agata, Dec 2023
### output: Figure panels 1,B,C,D
library(here)
source(here("analyses","00_setup.R"))
source(here("analyses","001_Coastal_countries.R"))
source(here("R","NormMinMax.R"))

# coastal countries shapefile
# Get the country boundaries data - sf dataframe
countries <- ne_countries(returnclass = "sf",scale = 10) # 258 countries and territories

####################################
#         CADC Risk figures        #
####################################

# load country contextual inequity
sr.sp.sf.ctry <- readRDS(here("data","derived-data","risk.stack.sr.rds"))

# load ODA data
ocean_ODA <- readRDS(here("data","derived-data","ocean_ODA_CADC_2010_2021_equity_sf.RDS"))
head(ocean_ODA)

# add coastal length
ocean_ODA <- left_join(ocean_ODA,coastal.ctr %>% select(country,iso2,coastline_wf) ,by=c("iso_a2" = "iso2"))

# filter by CADC
ocean_ODA_equity <- ocean_ODA %>%
  filter(cadc.type %in% c("equity CADC","CADC")) %>%
  st_drop_geometry() %>%
  group_by(iso_a3) %>%
  mutate(tot.project.CADC = sum(total.project)) %>%
  mutate(perc.equi = round(total.project/tot.project.CADC,3)) %>%
  as.data.frame()

ocean_ODA_CADC.eq <- ocean_ODA_equity %>%
  filter(cadc.type == "equity CADC")

# density of CADC (with world factbook)
#ocean_ODA_equity <- ocean_ODA_equity %>%
#  mutate(cadc.density = round(total.project/coastline_wri,2)) %>%
#  mutate(cadc.dollars.density = round(total.usd/coastline_wri,2))

# risk by country
ocean_ODA_CADC_risk.mean <- sr.sp.sf.ctry %>%
  as.data.frame() %>%
  dplyr::select(iso_a3,risk.mat.score.geom,risk.mat.score.sum) %>%
  group_by(iso_a3) %>%
  mutate(mean.risk.geom = mean(risk.mat.score.geom,na.rm=T)) %>% 
  mutate(mean.risk.sum = mean(risk.mat.score.sum,na.rm=T)) %>%
  select(-risk.mat.score.geom,-risk.mat.score.sum) %>%
  distinct()

ocean_ODA_CADC_risk.mean %>%
  arrange(-mean.risk.sum)

# merge with risk
rm(ocean_ODA_CADC_risk)
ocean_ODA_CADC_risk <- left_join(ocean_ODA_CADC_risk.mean,ocean_ODA_CADC.eq,by="iso_a3")
#ocean_ODA_CADC_risk$cadc.density.sc <- normalize(ocean_ODA_CADC_risk$cadc.density)

# scale risk and count/country
ocean_ODA_CADC_risk$mean.risk.geom.sc = normalize(ocean_ODA_CADC_risk$mean.risk.geom)
ocean_ODA_CADC_risk$total.project.sc = normalize(ocean_ODA_CADC_risk$total.project)
ocean_ODA_CADC_risk$mean.risk.sum.sc = normalize(ocean_ODA_CADC_risk$mean.risk.sum)

#  replace NA with 0 in perc.equi and total.project.sc
ocean_ODA_CADC_risk <- ocean_ODA_CADC_risk %>%
  mutate(total.project.sc = replace_na(total.project.sc, 0)) %>%
  mutate(total.project = replace_na(total.project, 0)) %>%
  mutate(perc.equi = replace_na(perc.equi, 0)) %>%
  as.data.frame()

ocean_ODA_CADC_risk <- ocean_ODA_CADC_risk %>%
  mutate(risk.CADC = sqrt(mean.risk.sum.sc^2 + total.project.sc^2)) %>%
  mutate(risk.CADC.sc = normalize(risk.CADC))

# re-add countries
ocean_ODA_CADC_risk <- ocean_ODA_CADC_risk %>%
  left_join(countries %>% select(iso_a3,name_en),by="iso_a3")

# CADC - risk
sc.risk.CADC.dollars <- ggplot(ocean_ODA_CADC_risk,aes(x=total.usd,y=mean.risk.sum.sc,label=name_en)) +
  geom_point()+
  geom_text_repel() +
  #geom_smooth(method="lm") +
  #scale_color_gradient(low="blue", high="red")+
  xlab("Total investment by country ($)") +
  ylab("Average contextual inequity by country") +
  ylim(0,1) + #   xlim(0,1)  + 
  geom_abline(slope=1,intercept=0,linetype = 2)+
  theme_bw()
sc.risk.CADC.dollars

# CADC risk - equity
CADC.risk.eq.dollars <- ggplot(ocean_ODA_CADC_risk,aes(x=perc.equi,y=mean.risk.sum.sc,label=name_en)) +
  geom_point(aes(size=total.usd,color=total.usd)) +
  scale_color_gradient(low="blue", high="red",name = "Investment")+
  #scale_colour_gradient(low = "lightblue",high = "blue") +
  xlab("Equity (% total of projects)") +
  geom_text_repel() +
  #geom_smooth(method="lm") +
  ylab("Average contextual inequity by country") +
  xlim(0,1) +  ylim(0,1) +
  geom_vline(xintercept=0.5,linetype = 2)+
  geom_hline(yintercept=0.5,linetype = 2)+
  theme_bw()
CADC.risk.eq.dollars


CADC.risk <- ggarrange(sc.risk.CADC.dollars,CADC.risk.eq.dollars,labels=c("c","d"),ncol=2,common.legend = T,legend = "bottom")
ggsave(here("figures","CADC.risk.equity.CD.pdf"),CADC.risk,width=18,height=10)

ggsave(here("figures","CADC.risk.equit.panelC.pdf"),sc.risk.CADC.dollars,width=10,height=10)
ggsave(here("figures","CADC.risk.equit.panelD.pdf"),CADC.risk.eq.dollars,width=10,height=10)

#########################
#        unpacking      #
#########################

### contextual inequity with proecjt inequity
CADC.cont.eq <- ggplot(ocean_ODA_CADC_risk,aes(y=perc.equi,x=mean.risk.sum.sc,label=name_en)) +
  geom_point(aes(size=total.project)) +#color=mean.risk.sum.sc)) +
  #scale_color_gradient(low="blue", high="red",name = "Contextual inequity")+
  #scale_colour_gradient(low = "lightblue",high = "blue") +
  ylab("Equity (% total of projects)") +
  #geom_smooth(method="lm") +
  geom_text_repel() +
  xlab("Contextual inequity") +
  xlim(0,1) +  ylim(0,0.5) +
  #geom_vline(xintercept=0.5,linetype = 2)+
  #geom_hline(yintercept=0.5,linetype = 2)+
  theme_bw()
CADC.cont.eq
ggsave(here("figures","CADC.eq.cont.eq.pdf"),CADC.cont.eq,width=10,height=10)

### contextual inequity with proecjt inequity
CADC.cont.eq.n.projects <- ggplot(ocean_ODA_CADC_risk,aes(x=perc.equi,y=total.project,label=name_en)) +
  geom_point() +#color=mean.risk.sum.sc)) +
  #scale_color_gradient(low="blue", high="red",name = "Contextual inequity")+
  #scale_colour_gradient(low = "lightblue",high = "blue") +
  xlab("Equity (% total of projects)") +
  #geom_smooth(method="lm") +
  geom_text_repel() +
  ylab("Total number of projects") +
  xlim(0,0.5) +  #ylim(0,1) +
  #geom_vline(xintercept=0.5,linetype = 2)+
  #geom_hline(yintercept=0.5,linetype = 2)+
  theme_bw()
CADC.cont.eq.n.projects
ggsave(here("figures","CADC.eq.total.project.pdf"),CADC.cont.eq.n.projects,width=10,height=10)

CADC.risk.all <- ggarrange(sc.risk.CADC,CADC.risk.eq,CADC.cont.eq,CADC.cont.eq.n.projects,labels=c("a","b","c","d"),ncol=2,nrow=2,common.legend = T,legend = "bottom")
ggsave(here("figures","CADC.risk.equity.ABCD.pdf"),CADC.risk.all,width=18,height=18)


log.total.risk <- ggplot(ocean_ODA_CADC_risk,aes(x=log(total.project+1),y=mean.risk,label=name)) +
  geom_point() +
  geom_text_repel() +
  theme_bw()
log.total.risk

density.risk <- ggplot(ocean_ODA_CADC_risk,aes(x=log(cadc.density+1),y=mean.risk,label=name)) +
  geom_point() +
  geom_text_repel() +
  theme_bw()
density.risk

total.risk <- ggplot(ocean_ODA_CADC_risk,aes(x=total.project,y=mean.risk,label=name)) +
  geom_point() +
  geom_text_repel() +
  theme_bw()
total.risk
