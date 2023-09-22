# check organisation list - bias in publications/country
all <- readRDS(here("data","derived-data","CADC.db.final.rds"))

# list all columns with "org in it" to retrieve all potential columns
col.org <- names(all)[which(str_detect_all(names(all),"org"))]

  # global definition:
    # IATI standard allows for the recording of information on all organisations that participate in any part 
    # of the lifecycle of an aid activity: inter alia donors, beneficiaries, extending and implementing agencies

  # in iati standard
      # reporting organisations (reporting-org) = reporting
      # receiving funds (receiver-org) = beneficiaries
      # providing funds (provider-org) = donors 
      # otherwise involved in iati-activities (using participating-org) = codes
              # 1	Funding	The government or organisation which provides funds to the activity.
              # 2	Accountable	An organisation responsible for oversight of the activity and its outcomes
              # 3	Extending	An organisation that manages the budget and direction of an activity on behalf of the funding organisation
              # 4	Implementing	The organisation that physically carries out the activity or intervention.

  # check
  summary(as.factor(all[,col.org[1]]))
  
# country file for each project
  list.ctry <- lapply(all[,"recipient_country_code"],function(x) strsplit(x,' \\| ')) # create sublist
  names(list.ctry) <- all[,"iati_identifier"]
  list.ctry <- lapply(list.ctry,unlist) # create vectors
  list.ctry <- lapply(list.ctry,unique) # remove duplicates
  list.ctry <- list.ctry[!is.na(list.ctry)] # remove NAs
    
      # as.data.frame
      list.ctry <- compact(list.ctry) # Remove all NULL entries from a list
      rm(list.ctry.df)
      list.ctry.df <-   as.data.frame(do.call(rbind, list.ctry))  
  
  # transaction recipient country
  list.ctry.2 <- lapply(all[,"transaction_recipient_country_code"],function(x) strsplit(x,' \\| ')) # create sublist
  names(list.ctry.2) <- all[,"iati_identifier"]
  list.ctry.2 <- lapply(list.ctry.2,unlist) # create vectors
  list.ctry.2 <- lapply(list.ctry.2,unique) # remove duplicates
  list.ctry.2 <- list.ctry[!is.na(list.ctry.2)] # remove NAs
  
      # as.data.frame
      list.ctry.2 <- compact(list.ctry.2) # Remove all NULL entries from a list
      rm(list.ctry.df.2)
      list.ctry.df.2 <-   as.data.frame(do.call(rbind, list.ctry.2))
      
  # rbind and unique list.ctry.df
      list.ctry.df.all <- rbind(list.ctry.df,list.ctry.df.2)
      list.ctry.df.all <- rownames_to_column(list.ctry.df.all)
      names(list.ctry.df.all) <- c("iati_identfiier","CTRY")
      list.ctry.df.all <- distinct(list.ctry.df.all)
      dim(list.ctry.df.all)
      
# clean the participating organisation
  
    # donors
  list.provider <- lapply(all[,"transaction_provider_org_ref"],function(x) strsplit(x,' \\| ')) # create sublist
  names(list.provider) <- all[,"iati_identifier"]
  list.provider <- lapply(list.provider,unlist) # create vectors
  list.provider <- lapply(list.provider,unique)
  list.provider <- list.provider[!is.na(list.provider)] # remove NAs
  
    # as.data.frame
    list.provider <- compact(list.provider) # Remove all NULL entries from a list
    list.provider <- lapply(list.provider,as.data.frame)
    list.provider.df <- as.data.frame(do.call(rbind, list.provider)) 
    list.provider.df <- rownames_to_column(list.provider.df)
    names(list.provider.df) <- c("iati_identifier_V2","donors")
      
        # recreate unique identifier
    list.provider.df <- list.provider.df %>%
      mutate(iati_identifier = str_extract_part(iati_identifier_V2, before = TRUE, pattern = ".")) %>%
      select(-iati_identifier_V2) %>%
      select(iati_identifier,donors)
    
      # number of projects by donors
    list.provider.df.n <- list.provider.df %>%
      group_by(donors) %>%
      summarize(n= n()) %>%
      arrange(n = desc(n)) %>%
      filter(donors != "") %>% 
      as.data.frame() %>%
      mutate(donors = as.factor(donors))
    
      ## plot
    list.provider.plot <- ggplot(list.provider.df.n,aes(x=donors,y=n)) +
      geom_bar(stat="identity") + 
      coord_flip()

      

  
    lapply(list.provider,class)
    
    
    head(list.provider.df)
  
  
  
  
  planned_disbursement_provider_org_narrative
  planned_disbursement_provider_org_ref
  planned_disbursement_provider_org_type
  transaction_provider_org_narrative
  transaction_provider_org_ref
  transaction_provider_org_type