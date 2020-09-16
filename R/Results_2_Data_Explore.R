#Results_2_Data Explore


### load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
library(xtable)
library(nlme)
library(scales)
library(zoo)
Sys.setenv(TZ='UTC')

#load data
#########################################

full_data <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw_cleaned.csv") %>% 
  left_join(fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
              distinct(duid, .keep_all = TRUE) %>% select(duid, dispatch_type, technology_type_descriptor), by = "duid") %>% 
  filter(dispatch_type == "Generator") %>% #remove loads
  select(-dispatch_type) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  group_by(settlementdate) %>% 
  mutate(qw_lmp = sum(initialmw*lmp)/sum(initialmw)) #add QW_LMP

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

# TABLES
###############################

cut(full_data$lmp, 
    breaks=c(-Inf, -10000, -1000, -100, 0, 100, 1000, 10000, Inf)) %>% table() %>% 
  data.frame() %>% 
  rename("lmp_range" = ".") %>% 
  mutate(Percent = 100*Freq/sum(Freq))

cut(full_data$lmp, 
    breaks=c(-Inf, -1000, 15000, Inf)) %>% table() %>% 
  data.frame() %>% 
  rename("lmp_range" = ".") %>% 
  mutate(Percent = 100*Freq/sum(Freq))

