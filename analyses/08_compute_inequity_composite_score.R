# compute ocean equity composite index 
### Stephanie D'Agata, Nov 2023
### updates: Steph D'Agata Feb 2024
### output: composite index

library(here)
source(here::here("analyses","00_setup.R"))
source(here::here("analyses","001_Coastal_countries.R"),echo=T)

# load data
# # save clean df
 df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.rds"))

# # create composite risk score ##### INDICE COMPOSITE without disaster prep
 rm(risk.mat)
 risk.mat <- df.risk.stack.sc.ctry.ind.coastal %>%
   dplyr::select(mean.count.grav.V2,povmap.grdi.v1,Nutritional.dependence,Economic.dependence,
          Voice_account,Political_stab,Gov_effect,Reg_quality,Rule_law,control_corr,merit.10m.pop,gender.ineq)
 df.risk.stack.sc.ctry.ind.coastal$risk.mat.score.mean.without.dprep <- apply(risk.mat, 1, mean,na.rm=T)
 
# # save df with composite scores
saveRDS(df.risk.stack.sc.ctry.ind.coastal,here("data","derived-data","df.cont.inequity.compo.coastal.scores.rds"))

# load composite scores
df.risk.stack.sc.ctry.ind.coastal <- readRDS(here("data","derived-data","df.cont.inequity.compo.coastal.scores.rds"))

# plot distribution
  # define color blind palette for country - OECD or not
hcl_palettes(plot = TRUE)
  # number of categories
summary(as.factor(df.risk.stack.sc.ctry.ind.coastal$income_grp))
q5 <- sequential_hcl(5, palette = "Purple-Yellow")
q5

# n countries
ctry.length <- as.data.frame(df.risk.stack.sc.ctry.ind.coastal) %>% filter(!is.na(risk.mat.score.mean.without.dprep)) %>% select(name) %>% unique() 

p.risk.score.without.disaster <- ggplot(as.data.frame(df.risk.stack.sc.ctry.ind.coastal) %>% filter(!is.na(risk.mat.score.mean.without.dprep)), aes(x=reorder(name,-risk.mat.score.mean.without.dprep), y=risk.mat.score.mean.without.dprep,color=income_grp)) + 
  geom_boxplot() +
  scale_color_discrete_sequential(palette = "Purple-Yellow", nmax = 6, order = 2:6) +
  coord_flip() +
  ylim(0,1) +
  xlab("Countries") +
  ylab("Contextual Vulnerability Scores") +
  labs(color = "Countries income") +
  ggtitle("without disaster prep. - 95 coastal countries") +
  theme_bw()
p.risk.score.without.disaster
ggsave(here("figures","SUPP_Contextual_Ineq_score_distrib_ctr_without_disaster_95ctries.png"),p.risk.score.without.disaster,height=12,width=12)

# check with OECD database
# load dataset with potential projects in coastal countries
ctry.oecd.db <- import(here("data","derived-data","ocean_ODA_CADC_2010_2021_equity.csv"))
#names(oecd)
summary(oecd.dat)

ctry.oecd.db.coastal <- ctry.oecd.db[which(ctry.oecd.db$ctry %in% coastal.ctr$country)] # 109 coastal countries

ctry.oecd.db.coastal[which(ctry.oecd.db.coastal %in% df.risk.stack.sc.ctry.ind.coastal$name)] %>% length()

###### END of the script #############

