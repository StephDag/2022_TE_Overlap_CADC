
## Null models for climate and development projects
  # source the function to randomize the projects across countries
source(here("R","RandVec.R"))

#########################
#         CLIMATE       #
#########################

ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE <- readRDS(here("data","derived-data","CLIMATE.drivers.N.projects.rds"))

# climate
rm(random.CADC)
random.CADC <- RandVec(a=0, b=400, s=400, n=dim(ctr.fsi.ineq.biodiv.hdi.CADC)[1], m=1000, Seed=sample(1:1000, size = 1))
random.CADC <- apply(random.CADC$RandVecOutput,2,round)
colSums(random.CADC)

### create full random climate
rm(all.random)
all.random <- cbind(ctr.fsi.ineq.biodiv.hdi.CADC,random.CADC)

clim.random.mean <- vector(length=1000)
clim.random.median <- vector(length=1000)
for (i in 1:1000){
  clim.random.mean[i] <- mean(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE[which(random.CADC[,i] !=0),"climate.drv"]$climate.drv,na.rm=T)
  clim.random.median[i] <- median(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE[which(random.CADC[,i] !=0),"climate.drv"]$climate.drv,na.rm=T)
}

# CI climate drivers
mean_value_climate <- mean(clim.random.mean)
n_climate <- length(clim.random.mean)

# Find the standard deviation
standard_deviation <- sd(clim.random.mean)

# Find the standard error
standard_error <- standard_deviation / sqrt(n_climate)
alpha = 0.05
degrees_of_freedom = n_climate - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error

# Calculating lower bound and upper bound
lower_bound_climate <- mean_value_climate - margin_error
upper_bound_climate <- mean_value_climate + margin_error

# climate
n.CADC.random.cc <- 100*round(sum(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE[which(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE$climate.drv > mean_value_climate),"2022"])/sum(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE$`2022`),2)
# histogramme
climate.distrib <- ggplot(ctr.fsi.ineq.biodiv.hdi.CADC.CLIMATE,aes(x=climate.drv)) +
  geom_histogram(binwidth=0.02) +
  geom_vline(aes(xintercept=mean_value_climate),
             color="lightgrey", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(climate.drv,na.rm=T)),
             color="blue", linetype="dashed", size=1) +
  xlim(range(ctr.fsi.ineq.biodiv.hdi.CADC$climate.drv,na.rm=T)) +
  ggtitle(paste0("CLIMATE: ",n.CADC.random.cc,"% projects over random climate change threshold (grey)")) + 
  ylab("Number of coastal aid projects") + 
  xlab("Climate Change Impact") +
  theme_bw()


### Poverty
rm(all.random)
all.random <- cbind(ctr.fsi.ineq.biodiv.hdi.CADC,random.CADC)

pov.random.mean <- vector(length=1000)
pov.random.median <- vector(length=1000)

for (i in 1:1000){
  pov.random.mean[i] <- mean(ctr.fsi.ineq.biodiv.hdi.CADC[which(random.CADC[,i] !=0),"headcount_ratio_international_povline"]$headcount_ratio_international_povline,na.rm=T)
  pov.random.median[i] <- median(ctr.fsi.ineq.biodiv.hdi.CADC[which(random.CADC[,i] !=0),"headcount_ratio_international_povline"]$headcount_ratio_international_povline,na.rm=T)
}

# CI poverty drivers
mean_value_poverty <- mean(pov.random.mean)
n_poverty <- length(pov.random.mean)

# Find the standard deviation
standard_deviation <- sd(pov.random.mean)

# Find the standard error
standard_error <- standard_deviation / sqrt(n_poverty)
alpha = 0.05
degrees_of_freedom = n_poverty - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error

# Calculating lower bound and upper bound
lower_bound_poverty <- mean_value_poverty - margin_error
upper_bound_poverty <- mean_value_poverty + margin_error

### biodiversity loss
rm(all.random)
all.random <- cbind(ctr.fsi.ineq.biodiv.hdi.CADC,random.CADC)

biodiv.random.mean <- vector(length=1000)
biodiv.random.median <- vector(length=1000)

for (i in 1:1000){
  biodiv.random.mean[i] <- mean(ctr.fsi.ineq.biodiv.hdi.CADC[which(random.CADC[,i] !=0),"value"]$value,na.rm=T)
  biodiv.random.median[i] <- median(ctr.fsi.ineq.biodiv.hdi.CADC[which(random.CADC[,i] !=0),"value"]$value,na.rm=T)
}

# CI poverty drivers
mean_value_biodiv <- mean(biodiv.random.mean)
n_biodiv <- length(biodiv.random.mean)

# Find the standard deviation
standard_deviation <- sd(biodiv.random.mean)

# Find the standard error
standard_error <- standard_deviation / sqrt(n_biodiv)
alpha = 0.05
degrees_of_freedom = n_biodiv - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error

# Calculating lower bound and upper bound
lower_bound_biodiv <- mean_value_biodiv - margin_error
upper_bound_biodiv <- mean_value_biodiv + margin_error

### equity
rm(all.random)
all.random <- cbind(ctr.fsi.ineq.biodiv.hdi.CADC,random.CADC)

eq.random.mean <- vector(length=1000)
eq.random.median <- vector(length=1000)

for (i in 1:1000){
  eq.random.mean[i] <- mean(ctr.fsi.ineq.biodiv.hdi.CADC[which(random.CADC[,i] !=0),"ineq.drv"]$ineq.drv,na.rm=T)
  eq.random.median[i] <- median(ctr.fsi.ineq.biodiv.hdi.CADC[which(random.CADC[,i] !=0),"ineq.drv"]$ineq.drv,na.rm=T)
}

# CI poverty drivers
mean_value_eq <- mean(eq.random.mean)
n_eq <- length(eq.random.mean)

# Find the standard deviation
standard_deviation <- sd(eq.random.mean)

# Find the standard error
standard_error <- standard_deviation / sqrt(n_eq)
alpha = 0.05
degrees_of_freedom = n_eq - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
margin_error <- t_score * standard_error

# Calculating lower bound and upper bound
lower_bound_eq <- mean_value_eq - margin_error
upper_bound_eq <- mean_value_eq + margin_error

########################
#     DEVELOPMENT      #
########################

saveRDS(ctr.fsi.ineq.biodiv.hdi.CADC.DEV,here("data","derived-data","DEV.drivers.N.projects.rds"))
