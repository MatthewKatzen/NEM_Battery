#Results_1_Merge_datasets


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

# Create Data
###############################


# Uncapped LMPs
RRP <-  fread("D:/Data/Cleaned/RRP/2019_RRP.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate))

generator_details <- fread("D:/Data/RAW/Nemsight/generator_details.csv") %>% clean_names() %>% 
  select(duid, type, station, region, capacity, fuel_type, schedule_type) %>% 
  filter(schedule_type != "Non-scheduled")

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE) %>% select(duid, station_name, dispatch_type)

adjustment <- fread(paste0("D:/Data/Raw/AEMC/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(-locally_constrained) %>% 
  filter(year(settlementdate)==2019) 

merged_data <- RRP %>% 
  left_join(generator_details, by = "region") %>% #merge dataframes
  left_join(adjustment, by = c("settlementdate", "duid")) %>% 
  mutate(local_price_adjustment = coalesce(local_price_adjustment, 0)) %>% #convert NA to 0
  mutate(lmp = case_when(type == "Gen" ~ rrp + local_price_adjustment,
                         type == "Load" ~ rrp - local_price_adjustment))

fwrite(merged_data, "D:/Data/Cleaned/LMP/full_lmp_uncapped.csv")


#Add initialmw

uncapped_lmp <- fread("D:/Data/Cleaned/LMP/full_lmp_uncapped.csv") %>% 
  select(-schedule_type, -type, -station, -fuel_type, -local_price_adjustment) %>% mutate(settlementdate = ymd_hms(settlementdate))


map2(paste0("D:/Data/RAW/AEMC/INITIALMW/2019-",str_pad(c(1:12), 2, pad = "0"),".csv"), #data from
     paste0("D:/Data/Cleaned/INITIALMW/Monthly_Uncapped/INITIALMW_LMP_UNCAPPED_2019-",str_pad(c(1:12), 2, pad = "0"),".csv"), #data to
     ~fread(.x) %>% clean_names() %>%  
       mutate(settlementdate = ymd_hms(settlementdate)) %>% 
       filter(intervention == 0) %>% #keep non-intervention
       inner_join(uncapped_lmp, by = c("settlementdate", "duid")) %>% #merge with lmp, only keeps duids which are in both lmp and initialmw
       fwrite(.y))

# merge into yearly file
full_data_uncapped <- map_df(list.files("D:/Data/Cleaned/INITIALMW/Monthly_Uncapped/", full.names = TRUE),
                             ~fread(.x) %>% 
                               mutate(settlementdate = ymd_hms(settlementdate))) 


fwrite(full_data_uncapped, "D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw.csv")

# Merge duids with same LMP and remove Loads
####################################
full_data_uncapped <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw.csv")

#find stations to merge
duplicated_station <- full_data_uncapped %>% left_join(generator_details_AEMO, by = "duid") %>% 
  filter(dispatch_type == "Generator") %>% #remove loads
  group_by(station_name, settlementdate) %>% summarise(duids = n_distinct(duid),
                                                         dif = max(lmp)-min(lmp)) %>% 
  group_by(station_name) %>% summarise(duids = mean(duids), #how many duids?
                                       mean = mean(dif)) %>%  #did the lmp differ
  filter(mean==0, duids!=1)

#merge at the month level
map(c(1:12),
    ~ full_data_uncapped %>% filter(month(settlementdate)==.x) %>% 
      left_join(generator_details_AEMO %>% select(duid, station_name), by = "duid") %>% #add station names
      group_by(station_name, settlementdate) %>% 
      mutate(initialmw = ifelse(station_name %in% duplicated_station$station_name, #combine mw if in duplicate duid
                                sum(initialmw),
                                initialmw)) %>% #merge duids w same lmp
      filter(!((station_name %in% duplicated_station$station_name) & (duid != duid[1]))) %>%  #only keep first duid
      select(-station_name) %>% 
      fwrite(paste0("D:/Data/Cleaned/INITIALMW/Monthly_Uncapped_cleaned/", .x, ".csv")))

#merge all months
full_lmp_uncapped_initialmw_cleaned <- map_dfr(list.files("D:/Data/Cleaned/INITIALMW/Monthly_Uncapped_cleaned/", full.names = TRUE),
                                          ~fread(.x)) %>% select(-intervention, - station_name)

fwrite(full_lmp_uncapped_initialmw_cleaned, "D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw_cleaned.csv")
station