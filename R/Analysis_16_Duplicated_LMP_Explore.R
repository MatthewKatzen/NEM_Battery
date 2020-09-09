# Remove LMPs of DUIDS which are similar

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



# WHO ACTUALLY PRODUCES IN 2019
##########################################

full_initialmw <- map_dfr(paste0("D:/Data/RAW/AEMC/INITIALMW/2019-",str_pad(c(1:12), 2, pad = "0"),".csv"),
                          fread)

duid_produce <- full_initialmw %>% filter(initialmw>0) %>% .[["duid"]] %>% 
  unique() 

fwrite(duid_produce %>% data.frame(), "D:/Data/Cleaned/Duplication Removal/duids_produce.csv")

# DUPLICATES
###########################################

uncapped_lmp <- fread("D:/Data/Cleaned/LMP/full_lmp_uncapped.csv") %>% select(-schedule_type) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(settlementdate, duid, station, lmp) %>% 
  filter(duid %in% duid_produce)

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

generator_details <- fread("D:/Data/RAW/Nemsight/generator_details.csv") %>% clean_names() 

big_stations <- generator_details %>% filter(type == "Gen", duid %in% duid_produce) %>% #remove loads and non porducing duids 
  group_by(station) %>% filter(n_distinct(duid) >= 2) %>% select(station, duid)


temp <- uncapped_lmp %>% filter(duid %in% big_stations$duid) %>% 
  group_by(settlementdate, station) %>% summarise(dif = max(lmp)-min(lmp))

duplicate_stations <- temp %>% group_by(station) %>% summarise(mean = mean(dif)) %>% filter(mean == 0)

duplicate_data <- big_stations %>% filter(station %in% duplicate_stations$station)
#These 86 gens have LMPs same between duids

fwrite(duplicate_data, "D:/Data/Cleaned/Duplication Removal/duplicate_duids_data.csv")


#combine initialmw of gens which have non-varying lmps
#################################################################

duplicate_station <- fread("D:/Data/Cleaned/Duplication Removal/duplicate_duids_data.csv") %>% .[["station"]] %>% unique()
duid_produce <- fread("D:/Data/Cleaned/Duplication Removal/duids_produce.csv") %>% .[[1]]
generator_details <- fread("D:/Data/RAW/Nemsight/generator_details.csv") %>% clean_names() 

weighted_data_uncapped_cleaned <- fread("D:/Data/Cleaned/Quantity Weighted/full_lmp_uncapped_quantity_weighted.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  filter(duid %in% duid_produce)  #remove non-producing


map(c(1:12),
    ~ weighted_data_uncapped_cleaned %>% filter(month(settlementdate)==.x) %>% 
      left_join(generator_details %>% select(duid, station), by = "duid") %>% #add station names
      group_by(station, settlementdate) %>% 
      mutate(initialmw = ifelse(station %in% duplicate_station, #combine mw if in duplicate duid
                                sum(initialmw),
                                initialmw)) %>% #merge duids w same lmp
      filter(!((station %in% duplicate_station) & (duid != duid[1]))) %>%  #only keep first duid
      select(-station) %>% 
      fwrite(paste0("D:/Data/Cleaned/Quantity Weighted/Duplication/Month/", .x, ".csv")))


weighted_data_uncapped_cleaned <- map_dfr(list.files("D:/Data/Cleaned/Quantity Weighted/Duplication/Month/", full.names = TRUE),
                                          ~fread(.x))

fwrite(weighted_data_uncapped_cleaned %>% select(-station,-intervention), "D:/Data/Cleaned/Quantity Weighted/full_lmp_uncapped_quantity_weighted_cleaned.csv")
  
#DONE 
#######################################
