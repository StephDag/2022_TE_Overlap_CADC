# Figure 1 - panel A, B, C and D
### Stephanie D'Agata & David Gill, Nov 2023
### updates: Steph D'Agata Feb 2024
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
  select(year,cadc.type,usd.comm,usd.disb) %>%
  st_drop_geometry() %>%
  filter(!is.na(cadc.type)) %>%
  droplevels() %>%
  distinct() %>%
  group_by(year,cadc.type) %>%
  summarise(usd.comm.year=sum(usd.comm,na.rm=T) %>% round(2),
            usd.disb.year=sum(usd.disb,na.rm=T) %>% round(2)) %>%
  arrange(cadc.type,year) %>%
  group_by(cadc.type) %>%
  mutate(cum.usd.comm=cumsum(usd.comm.year)%>% round(2),
         cum.usd.disb=cumsum(usd.disb.year)%>% round(2)) %>%
  arrange(cadc.type,year) %>% 
  group_by(year) %>% 
  mutate(pct.comm=100*round(usd.comm.year/sum(usd.comm.year),4),
         pct.disb=100*round(usd.disb.year/sum(usd.disb.year),4))%>% #equity % 11% in 2010, 42% in 2021
  gather(var,val,usd.comm.year:pct.disb) %>%
  as.data.frame()

oecd.dat.sf.ctry.trend %>% filter(year == 2010 & var == "usd.comm.year")
oecd.dat.sf.ctry.trend %>% filter(year == 2021 & var == "usd.comm.year")

# color blind
hcl_palettes(plot = TRUE)
# number of categories
q3 <- qualitative_hcl(3, palette = "Harmonic")
q3

######################### FIGURE 1A #############################

# plot temporal trend
p.oecd.trend <- ggplot(data=oecd.dat.sf.ctry.trend,aes(x = year, y = val, fill=cadc.type)) +
  geom_area() +
  labs(title=" 'equity` CADC vs other CADC", y="USD million committed") +
  facet_wrap(.~var, scales = "free_y") +
  scale_fill_discrete_qualitative(palette = "Dark 3", nmax = 6, order = 3:5) +
  labs(fill = "CADC types") +
  theme_classic()
p.oecd.trend
ggsave(here("figures","SUPP_Figure1A.png"))
  
  # select only perc committed
p.oecd.trend.perc.commited <- oecd.dat.sf.ctry.trend %>% filter(var == "pct.comm") %>%
  droplevels() %>%
  ggplot(aes(x = year, y = val, fill=cadc.type)) +
  geom_area() +
  scale_fill_discrete_qualitative(palette = "Dark 3", nmax = 6, order = 3:5) +
  geom_hline(yintercept=75, linetype="dashed", color = "grey") +
  geom_hline(yintercept=50, linetype="dashed", color = "grey") +
  geom_hline(yintercept=25, linetype="dashed", color = "grey") +
  labs(y="Percentage committed investment (%)",x="Year") +
  labs(fill = "CADC types") +
  xlim(2010,2021) +
  theme_classic() +
  theme(legend.position="top") 
p.oecd.trend.perc.commited
ggsave(here("figures","Figure1A.png"))

# supplemental $ committed by year
# select only perc committed
p.oecd.trend.usd.commited <- oecd.dat.sf.ctry.trend %>% filter(var == "usd.comm.year") %>%
  droplevels() %>%
  ggplot(aes(x = year, y = val, fill=cadc.type)) +
  geom_area() +
  scale_fill_discrete_qualitative(palette = "Dark 3", nmax = 6, order = 3:5) +
  labs(y="USD committed investment ($)",x="Year") +
  labs(fill = "CADC types") +
  xlim(2010,2021) +
  theme_classic() +
  theme(legend.position="top") 
p.oecd.trend.usd.commited
ggsave(here("figures","SUPP_Figure1A.png"))

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
  filter(cadc.type != "other ocean economy") %>%
  select(iso_a3,year,cadc.type,usd.comm,usd.disb) %>%
  group_by(iso_a3,year) %>%
  summarise(usd.comm.year=sum(usd.comm,na.rm=T) %>% round(2),
            usd.disb.year=sum(usd.disb,na.rm=T) %>% round(2)) %>%
  arrange(iso_a3,year) %>%
  ungroup() %>%
  complete(iso_a3,year,fill = list(usd.comm.year = 0,usd.disb.year=0)) %>%
  group_by(iso_a3) %>%
  mutate(cum.usd.comm=cumsum(usd.comm.year)%>% round(2),
         cum.usd.disb=cumsum(usd.disb.year)%>% round(2)) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  as.data.frame() %>%
  select(-geometry)

oecd.dat.sf.ctry.total.commited %>% filter(iso_a3 == "CHL")

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
ggsave(here("figures","Figure1B.png"),p.map.2021.cum.ctry)

######################### FIGURE 1C and 1D #############################

# create df - 1 raw = 1 country = mean contextual equity, % equity CADC, total investment (cum2021) # oecd.dat.sf.ctry.total.commited
oecd.dat.sf.ctry.equity <- oecd.dat.sf.ctry %>%
  filter(cadc.type %in% c("equity CADC","other CADC")) %>%
  st_drop_geometry() %>%
  select(iso_a3,name_en,cadc.type,total.project) %>%
  group_by(iso_a3) %>%
  mutate(tot.project.CADC = sum(total.project)) %>%
  ungroup() %>%
  group_by(iso_a3,cadc.type) %>%
  mutate(tot.project.eq=sum(total.project)) %>%
  ungroup() %>%
  mutate(perc.equi = 100*round(tot.project.eq/tot.project.CADC,3)) %>%
  select(-total.project) %>%
  distinct() %>%
  as.data.frame()
oecd.dat.sf.ctry.equity

# add total investment in 2021
oecd.dat.sf.ctry.equity.commit <- oecd.dat.sf.ctry.equity %>%
  left_join(oecd.dat.sf.ctry.total.commited %>% filter(year == "2021"),by="iso_a3")
  
# load composite scores
df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.scores.rds"))

# SUMMARIZE one indicator by country
rm(cont.ineq.ctry)
cont.ineq.ctry <- df.risk.stack.sc.ctry.ind.coastal %>%
  group_by(iso_a3) %>%
  mutate(cont.eq.score.mean = mean(risk.mat.score.mean.without.dprep,na.rm=T) %>% round(2),
            cont.eq.sd =  sd(risk.mat.score.mean.without.dprep,na.rm=T) %>% round(2)) %>%
  ungroup() %>%
  select(iso_a3,cont.eq.score.mean,cont.eq.sd) %>%
  distinct() %>%
  filter(!is.na(cont.eq.score.mean)) %>%
  as.data.frame()

head(cont.ineq.ctry)
dim(cont.ineq.ctry)

# add contextual inequity
rm(oecd.dat.sf.ctry.equity.commit.ineq)
oecd.dat.sf.ctry.equity.commit.ineq <- oecd.dat.sf.ctry.equity.commit %>%
  full_join(cont.ineq.ctry,by="iso_a3") 
  # mutate(tot.project.CADC = replace_na(tot.project.CADC, 0)) %>%
  # mutate(tot.project.eq = replace_na(tot.project.eq, 0)) %>%
  # mutate(perc.equi = replace_na(perc.equi, 0)) %>%
  # mutate(cum.usd.comm = replace_na(cum.usd.comm, 0))
summary(oecd.dat.sf.ctry.equity.commit.ineq)

# CADC invest - contextual ineq - log
sc.risk.CADC.dollars.log <- ggplot(oecd.dat.sf.ctry.equity.commit.ineq %>% filter(cadc.type=="equity CADC"),aes(x=log10(cum.usd.comm+1),y=cont.eq.score.mean,label=name_en)) +
  geom_point()+
  geom_text_repel() +
  #geom_smooth(method="lm") +
  #scale_color_gradient(low="blue", high="red")+
  xlab("log(Total investment by country) ($)") +
  ylab("Average contextual inequity by country") +
  ylim(0,0.7) + #   xlim(0,1)  + 
  theme_bw()
sc.risk.CADC.dollars.log

# CADC invest - contextual ineq
sc.risk.CADC.dollars <- ggplot(oecd.dat.sf.ctry.equity.commit.ineq %>% filter(cadc.type=="equity CADC"),aes(x=cum.usd.comm,y=cont.eq.score.mean,label=name_en)) +
  geom_point()+
  geom_text_repel() +
  #geom_smooth(method="lm") +
  #scale_color_gradient(low="blue", high="red")+
  xlab("Total investment by country ($)") +
  ylab("Average contextual inequity by country") +
  ylim(0,0.75) + #   xlim(0,1)  + 
  theme_bw()
sc.risk.CADC.dollars
ggsave(here("figures","Figure1C.png"),sc.risk.CADC.dollars)

# CADC risk - equity
CADC.risk.eq.dollars <- ggplot(oecd.dat.sf.ctry.equity.commit.ineq %>% filter(cadc.type=="equity CADC"),aes(x=perc.equi,y=cont.eq.score.mean,label=name_en)) +
  geom_point(aes(size=cum.usd.comm,color=cum.usd.comm)) +
  scale_color_viridis_c(option = "plasma",name = "Investment ($)") +
 # scale_color_gradient(low="blue", high="red",name = "Investment ($)")+
  xlab("Equity (% total of CADC projects)") +
  geom_text_repel() +
  ylab("Average contextual inequity by country") +
  xlim(0,50) +  ylim(0,0.75) +
  geom_vline(xintercept=50,linetype = 2)+
  geom_hline(yintercept=0.5,linetype = 2)+
  theme_bw()
CADC.risk.eq.dollars
ggsave(here("figures","Figure1D.png"),CADC.risk.eq.dollars)


# full panel
CADC.full.panel <- ggarrange(p.oecd.trend.perc.commited,
                             p.map.2021.cum.ctry,
                             sc.risk.CADC.dollars,
                             CADC.risk.eq.dollars,
                             labels=c("a","b","c","d"),ncol=2,nrow=2)
ggsave(here("figures","CADC.risk.equity.ABCD.pdf"),CADC.full.panel,width=10,height=10)
ggsave(here("figures","CADC.risk.equity.ABCD.png"),CADC.full.panel,width=10,height=10)

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




