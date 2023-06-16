# load packages for the project
pacman::p_load(here,dplyr,janitor,tidyverse,tidytext,strex,fraser,star,readxl,ggpubr,scico,ggrepel)
pacman::p_load(rnaturalearth,rnaturalearthdata,rnaturalearthhires,leaflet,sf,ggmap,rio,terra,maps,sp,wdpar)
pacman::p_load(p_author(wordcloud,RColorBrewer,wordcloud2,tm))

x <- c("here","dplyr","janitor","tidyverse","tidytext","strex","fraser","star","readxl","ggpubr","scico","ggrepel",
       "rnaturalearth","rnaturalearthdata","rnaturalearthhires","leaflet","sf","ggmap","rio","terra","maps","sp","wdpar",
       "wordcloud","RColorBrewer","wordcloud2","tm")
lapply(x, require, character.only = TRUE)

# load CESAB IATI package
#install.packages("remotes")
#remotes::install_github("frbcesab/riati",force = TRUE)
library("riati")

# geonames packages
install.packages("devtools") # if you don't have devtools yet
require(devtools)
install.packages("rjson") #-- gets this dependency from CRAN
install_github("ropensci/geonames")
library(rjson)
library(geonames)


