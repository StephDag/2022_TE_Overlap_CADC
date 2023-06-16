## certain aid projects are occurring in several countries, but countries are listed in one row only
## A script to extract projects information that are embedded in one raw

one_raw_2_long <- function(x,y) {
    #database,y=unique_ID
    # transform cell with several countries to list
    temp.list <- strsplit(as.character(x),' | ')
    # names list by unique ID
    names(temp.list) <- y
    # list to wide format
    temp.db <- as.data.frame(do.call(cbind,temp.list)) %>% t()
    # wide to long format
    temp.db.long <- temp.db %>% tidyr::gather(key = variable,country, -y,factor_key=T,convert=T) %>%
      select(-variable) %>% 
      distinct() %>% # remove duplicate countries
      filter(country != "|") # remove error inserted due to separators
}

### load data
all.filter <- read.csv(here("1_CADC_db","CADC_all.csv"))
dim(all.filter)
# create a list of countries for each project
CTRY.list.db =strsplit(as.character(all.filter$recipient_country_code),' | ')
# names the list - each element of the list = unique ID of the project
names(CTRY.list.db) <- all.filter$iati_identifier

# list to long format dataframe for countries by iati IDs
rm(df)
df <- as.data.frame(do.call(cbind,CTRY.list.db)) %>% t()
rownames(df)
head(df)
df <- cbind(as.character(rownames(df)),df)
rownames(df) <- NULL
df <- as.data.frame(df)
summary(df$iati_identifier)
names(df)[1] <- "iati_identifier"

# wide to long
rm(df_long)
df_long <- df %>% tidyr::gather(key = variable,country, -iati_identifier,factor_key=T,convert=T) %>%
  select(-variable) %>% 
  distinct() %>% # remove duplicate countries
  filter(country != "|") # remove error inserted due to separators

