# finance data: load and rind together for each country, and summarize for each project and country
file_paths <- list.files("data/raw-data/IATIFinance", pattern = ".xlsx$", full.names = TRUE)

iati.budget <- do.call(rbind, lapply(file_paths, function(x) 
  transform(readxl::read_excel(x), store = sub(".xlsx$", "", basename(x)))))

# check if all countries are listed
source(here::here("analyses","001_Coastal_countries.R")) # load countries and cities in countries
ctr <- ctr.iati$code # for all countries - since impossible to catch country information when 1 project in several countries

    # check
ctr.check <- ctr[which(ctr$iso2 %in% iati.budget$store  == F),]

# extract countries for all projects
#iati.ctr <- iati.budget %>%
#  select(IATI.Identifier,store) %>%
#  distinct()
#write.csv2(iati.ctr, here("data","derived-data","ID_ctry_iati.csv"),row.names=F)

# summarize - by year
iati.budget.ID.USD <- iati.budget %>%
  dplyr::group_by(IATI.Identifier, store,Reporting.Organisation,Provider.Organisation,Receiver.Organisation,Recipient.Country.or.Region,Calendar.Year) %>%
  dplyr::summarize(budget_USD =sum(Value..USD.))
head(iati.budget.ID.USD)
dim(iati.budget.ID.USD)

# summarize - along the projects
iati.budget.ID.USD.all <- iati.budget %>%
  dplyr::group_by(IATI.Identifier, store,Reporting.Organisation,Provider.Organisation,Receiver.Organisation,Recipient.Country.or.Region) %>%
  dplyr::summarize(budget_USD =sum(Value..USD.))
head(iati.budget.ID.USD.all)
dim(iati.budget.ID.USD.all)

# alluvial plot - reporting, provider, receiver organisation - % of projects
org.iati.budget <- iati.budget %>% 
  select(IATI.Identifier,Reporting.Organisation,Provider.Organisation,Provider.Organisation.Type,Receiver.Organisation,Receiver.Organisation.Type) %>%
  distinct() #%>%
  #filter(Reporting.Organisation != "No data") %>%
  #filter(Provider.Organisation != "No data") #%>%
  #filter(Receiver.Organisation != "No data")
  #replace_na(list(Reporting.Organisation = "No data")) %>%
  #replace_na(list(Provider.Organisation = "No data")) %>%
  #replace_na(list(Receiver.Organisation = "No data"))

# summarize for alluvial plot
org.iati.budget.summarize <- org.iati.budget %>%
  group_by(Reporting.Organisation,Provider.Organisation,Provider.Organisation.Type,Receiver.Organisation,Receiver.Organisation.Type) %>%
  summarize(n.iati_ID = n_distinct(IATI.Identifier)) %>%  # count unique number of projects for each organisation level
  arrange(desc(n.iati_ID)) %>%
  as.data.frame()

org.iati.budget.summarize.type <- org.iati.budget %>%
  group_by(Provider.Organisation.Type,Receiver.Organisation.Type) %>%
  summarize(n.iati_ID = n_distinct(IATI.Identifier)) %>%  # count unique number of projects for each organisation level
  arrange(desc(n.iati_ID)) %>%
  as.data.frame()

# histograms
provider_hist <- ggplot(org.iati.budget.summarize.type %>% filter(n.iati_ID >1000),aes(x=reorder(Provider.Organisation.Type,n.iati_ID),y=n.iati_ID)) +
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip() +
  theme_classic() +
  xlab("Provider organisations") + ylab("# projects")

receiver_hist <- ggplot(org.iati.budget.summarize.type %>% filter(n.iati_ID >1000),aes(x=reorder(Receiver.Organisation.Type,n.iati_ID),y=n.iati_ID)) +
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip() +
  theme_classic() +
  xlab("Receiver organisations") + ylab("# projects")  

  # barplot
## load ATI index 2022
rm(ATI)
ATI <- read.csv(here::here("data","raw-data","ATI_index.csv"),";",header=T)

  ATI <- ATI %>% 
    mutate(Score=as.numeric(Score),Category=as.factor(Category))
head(ATI)
summary(ATI)

# organisation to choose
ATI.sub <- ATI %>%
  filter(Category %in% c("Poor","Very poor") == F) %>%
  droplevels()
summary(ATI.sub)

  #
test <- org.iati.budget[which(str_detect(org.iati.budget,ATI.sub$Name[1])),]

which(str_detect(org.iati.budget$Provider.Organisation,ATI.sub$Name[1]))

which(org.iati.budget$Provider.Organisation == "Afdb")

org.iati <- ggplot(org.iati.budget.summarize filter(n.iati_ID > 500), aes(x=Provider.Organisation, y=n.iati_ID)) + 
  geom_bar(stat="identity", position=position_dodge())+
  coord_flip()

  # identical
org.iati.budget.summarize.n <- org.iati.budget %>%
  group_by(Reporting.Organisation,Provider.Organisation,Receiver.Organisation) %>%
  summarize(n.iati_ID = n()) %>% 
  arrange(desc(n.iati_ID)) %>%
  as.data.frame()

length(unique(org.iati.budget$IATI.Identifier)) # 579188 projects
dim(org.iati.budget)

tail(org.iati.budget.summarize)

