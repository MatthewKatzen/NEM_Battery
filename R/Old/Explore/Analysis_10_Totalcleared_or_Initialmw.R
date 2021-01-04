#Analysis_10_Totalcleared_or_Initialmw

#trying to figure out which is used for revenue
#comparing quarterly figures to AEMO's QED Q4 2019 figure 26

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
Sys.setenv(TZ='UTC')

#load initialmw data
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")
battery_duids <- generator_details %>% filter(fuel_type %in% c("Battery (Discharge)", "Battery (Charge)")) %>% .[["duid"]]

f <- function(x, pos) x %>% clean_names() %>%  #only keep half hourly rows
  filter(initialmw > 0,
         duid %in% battery_duids) %>% #only keep pos initialmw
  group_by(settlementdate, duid) %>% 
  filter(intervention == max(intervention)) %>% #if intervention, keep it and remove int==0)
  ungroup() %>% 
  select(-intervention) #remove intervention var

initialmw_battery_2019 <- map_dfr(paste0("D:/NEM_LMP/Data/RAW/INITIALMW/2019-",str_pad(c(1:12), 2, pad = "0"),".csv"),
                                     ~read_csv_chunked(.,DataFrameCallback$new(f), chunk_size = 2000000)) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  mutate(interval = ceiling_date(settlementdate, "30 minutes")) %>% 

#rrp
  
f <- function(x, pos) x %>% clean_names() %>% 
  filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything. Just an indicator
  #for intervention dispatch quantity (which is definitely different)
  mutate(region = substr(regionid, 1, nchar(regionid)-1)) %>% 
  select(settlementdate, region, rrp)

rrp_2019_data <- read_csv_chunked("D:/NEM_LMP/Data/Raw/dispatchprice_24-01-2020.csv", DataFrameCallback$new(f), chunk_size = 200000) %>% 
  filter((year(settlementdate)==2019) | (settlementdate == "2020-01-01 00:00:00")) %>%   #keep only 2019
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  mutate(interval = ceiling_date(settlementdate, "30 minutes")) %>% 
  group_by(interval, region) %>% 
  mutate(rrp30 = mean(rrp))

#merge to get rev and exp

initialmw_merged_2019 <- initialmw_battery_2019 %>% 
  left_join(generator_details %>% select(duid, station, region, type), by = "duid") %>% 
  left_join(rrp_2019_data, by = c("settlementdate","region", "interval")) %>% 
  mutate(initialmw = ifelse(type == "Gen", initialmw, -initialmw))

#quarterly rev exp
initialmw_merged_2019 %>% group_by(station, interval, rrp30) %>% 
  summarise(net = sum(initialmw)/12) %>% 
  mutate(rev = ifelse(net>0, rrp30*net, 0),
         exp = ifelse(net<0, rrp30*net, 0)) %>% 
  group_by(quarter = lubridate::quarter(interval)) %>% 
  summarise(total_rev = sum(rev),
            total_exp = sum(exp))

initialmw_merged_2019 %>% group_by(station, settlementdate, rrp30) %>% 
  summarise(net = sum(initialmw)/12) %>% 
  mutate(rev = ifelse(net>0, rrp30*net, 0),
         exp = ifelse(net<0, rrp30*net, 0)) %>% 
  group_by(quarter = lubridate::quarter(settlementdate)) %>% 
  summarise(total_rev = sum(rev),
            total_exp = sum(exp))


### Creat Totalcleared
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")
battery_duids <- generator_details %>% filter(fuel_type %in% c("Battery (Discharge)", "Battery (Charge)")) %>% .[["duid"]]

f <- function(x, pos) x %>% clean_names() %>%  #only keep half hourly rows
  filter(totalcleared > 0,
         duid %in% battery_duids) %>% #only keep pos initialmw
  group_by(settlementdate, duid) %>% 
  filter(intervention == max(intervention)) %>% #if intervention, keep it and remove int==0)
  ungroup() %>% 
  select(-intervention) #remove intervention var

totalcleared_battery_2019 <- map_dfr(paste0("D:/NEM_LMP/Data/RAW/dispatchload_2009-2020/output/dispatchload_2019-",str_pad(c(1:12), 2, pad = "0"),".csv"),
                             ~read_csv_chunked(.,DataFrameCallback$new(f), chunk_size = 2000000)) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  mutate(interval = ceiling_date(settlementdate, "30 minutes"))


#merge to get rev and exp

totalcleared_merged_2019 <- totalcleared_battery_2019 %>% 
  left_join(generator_details %>% select(duid, station, region, type), by = "duid") %>% 
  left_join(rrp_2019_data, by = c("settlementdate","region", "interval")) %>% 
  mutate(totalcleared = ifelse(type == "Gen", totalcleared, -totalcleared))
  
  
totalcleared_merged_2019 %>% group_by(station, settlementdate, rrp30) %>% 
  summarise(net = sum(totalcleared)/12) %>% 
  mutate(rev = ifelse(net>0, rrp30*net, 0),
         exp = ifelse(net<0, rrp30*net, 0)) %>% 
  group_by(quarter = lubridate::quarter(settlementdate)) %>% 
  summarise(total_rev = sum(rev),
            total_exp = sum(exp))
