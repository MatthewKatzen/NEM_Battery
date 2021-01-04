#Load all data and merge 

#load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
library(xtable)
Sys.setenv(TZ='UTC')

#
generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE) %>% 
  select(duid, classification, technology_type_primary, station_name, region, dispatch_type) %>% 
  mutate(region = substr(region, 1, nchar(region)-1)) %>% 
  filter(technology_type_primary == "Storage")

#load rrp data
price_data <- map_dfr(list.files("D:/Data/RAW/AEMO/NEMWEB/DISPATCHPRICE/", full.names = T) %>% 
               grep(2019, ., value = TRUE),
             ~fread(.x) %>% 
               clean_names() %>% 
               filter(intervention == 0) %>% 
               mutate(settlementdate = ymd_hms(settlementdate)) %>% 
               group_by(interval = ceiling_date(settlementdate, "30 min"), regionid) %>% 
               mutate(rrp30 = mean(rrp),
                      region = substr(regionid, 1, nchar(regionid)-1)) %>% 
               ungroup() %>% 
               select(settlementdate, region, rrp, rrp30, raise6secrrp, raise60secrrp, raise5minrrp, raiseregrrp,
                      lower6secrrp, lower60secrrp, lower5minrrp, lowerregrrp) %>% 
               rename(energy5min = rrp, energy30min = rrp30, raise6sec = raise6secrrp, raise60sec = raise60secrrp, raise5min = raise5minrrp, raisereg = raiseregrrp,
                      lower6sec = lower6secrrp, lower60sec = lower60secrrp, lower5min = lower5minrrp, lowerreg = lowerregrrp)) %>% 
  pivot_longer(cols = c(-settlementdate, -region), #make long form
               names_to = "var", values_to = "price") 
  

#load dispatch data
dispatch_data <-  map_dfr(list.files("D:/Data/RAW/AEMO/NEMWEB/DISPATCHLOAD/", full.names = T) %>% 
                       grep(2019, ., value = TRUE),
                     ~fread(.x) %>% 
                       clean_names() %>% 
                       filter(intervention == 0,
                              duid %in% generator_details_AEMO$duid) %>% 
                       mutate(energy5min = initialmw, energy30min = initialmw) %>% 
                       mutate(settlementdate = ymd_hms(settlementdate)) %>% 
                       select(settlementdate, duid, energy5min, energy30min,
                              lower5min, lower60sec, lower6sec, 
                              raise5min, raise60sec, raise6sec,
                              lowerreg, raisereg)) %>% 
  left_join(generator_details_AEMO %>% select(duid, region, dispatch_type), by = "duid") %>% 
  pivot_longer(cols = c(-settlementdate, -duid, -region, -dispatch_type), #make long form
               names_to = "var", values_to = "value") 

  
#merged
full_data <- dispatch_data %>% left_join(price_data, by = c("settlementdate", "region", "var")) 

fwrite(full_data, "D:/Data/Cleaned/Battery_2019_data.csv")

