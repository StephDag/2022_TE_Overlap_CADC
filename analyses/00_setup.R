# load packages for the project
# pacman::p_load(here,janitor,tidytext,strex,fraser,star,readxl,ggpubr,scico,ggrepel,
#                rnaturalearth,rnaturalearthdata,rnaturalearthhires,leaflet,ggmap,rio,terra,maps,sp,wdpar,
#                wordcloud,RColorBrewer,wordcloud2,tm,dbscan,rgeoboundaries,foreach,doParallel,doSNOW,tcltk,
#                ncdf4,RANN,mapview,sf,stars,tidyverse,"pdftools")

x <- c("here","dplyr","janitor","tidyverse","tidytext","strex","readxl","ggpubr","scico","ggrepel",
       "rnaturalearth","rnaturalearthdata","rnaturalearthhires","leaflet","sf","ggmap","rio","terra","maps","sp","wdpar",
       "wordcloud","RColorBrewer","wordcloud2","tm","data.table","rutils",
       "foreach","doParallel","doSNOW","tcltk","svMisc","elevatr",
       "dbscan","rgeoboundaries","ncdf4","RANN","mapview","sf","stars",
       "pdftools")

lapply(x, require, character.only = TRUE)

# load CESAB IATI package
#install.packages("remotes")
#remotes::install_github("frbcesab/riati",force = TRUE)
#library("riati")

# geonames packages
#install.packages("devtools") # if you don't have devtools yet
#require(devtools)
#install.packages("rjson") #-- gets this dependency from CRAN
#install_github("ropensci/geonames")
#library(rjson)
#library(geonames)

# gptchatter
#install.packages(c("devtools", "openai"))
#library(devtools)
#install_github("isinaltinkaya/gptchatteR")
#library(gptchatteR)

# rutils CESAB
#remotes::install_github("frbcesab/rutils")

#library(remotes)
#remotes::install_github("wmgeolab/rgeoboundaries")

