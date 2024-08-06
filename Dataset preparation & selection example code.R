#library
library(data.table)
library(tidyverse)

#import raw data
Taiwan_Moth_data<- data.table::fread("Moth Raw data/Taiwan_Moth_data.csv",encoding = "UTF-8", colClasses = "character")
China_Moth_data<- data.table::fread("Moth Raw data/China_Moth_data.csv",encoding = "UTF-8", colClasses = "character")
Malaysia_Moth_data<- data.table::fread("Moth Raw data/Malaysia_Moth_data.csv",encoding = "UTF-8", colClasses = "character")

#taiwan_dt:species, number of observed sites, total individuals
taiwan_dt<-Taiwan_Moth_data %>%
  setDT() %>%
  .[, .(, number_of_observered_sites := unique(altitude),
        total_individuals := .N),
    by = Species]
  
#select data--number of site more than 3 sites; individuals more than 10
taiwan_dt_ls<-taiwan_dt %>%
  filter(as.numeric(number_of_observered_sites)>3) %>%
  filter(as.numeric(total_individuals)>10)

#fill the datas to sort
dt_taiwan_filt<-Taiwan_Moth_data %>% left_join(taiwan_dt_ls, by = "Species")

# china_dt
china_dt<-China_Moth_data %>%
  setDT() %>%
  .[, .(, number_of_observered_sites := unique(altitude),
        total_individuals := .N),
    by = Species]
  
#select data--number of site more than 3 sites; individuals more than 10
china_dt_ls<-china_dt %>%
  filter(as.numeric(number_of_observered_sites)>3) %>%
  filter(as.numeric(total_individuals)>10)

#fill the datas to sort
dt_china_filt<-China_Moth_data %>% left_join(china_dt_ls, by = "Species")

#malaysia_dt
malaysia_dt<-Malaysia_Moth_data %>%
  setDT() %>%
  .[, .(, number_of_observered_sites := unique(altitude),
        total_individuals := .N),
    by = Species]
  
#select data--number of site more than 3 sites; individuals more than 10
malaysia_dt_ls<-malaysia_dt %>%
  filter(as.numeric(number_of_observered_sites)>3) %>%
  filter(as.numeric(total_individuals)>10)

#fill the datas to sort
dt_malaysia_filt<-Malaysia_Moth_data %>% left_join(malaysia_dt_ls, by = "Species")
