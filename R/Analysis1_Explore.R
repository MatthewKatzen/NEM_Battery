#Analysis1_Explore


#load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
library(xtable)
library(scales)
Sys.setenv(TZ='UTC')

#load data
generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE) %>% 
  select(duid, classification, technology_type_primary, station_name, region, dispatch_type) %>% 
  mutate(region = substr(region, 1, nchar(region)-1)) %>% 
  filter(technology_type_primary == "Storage")

full_data <- fread("D:/Data/Cleaned/Battery_2019_data.csv") %>% left_join(generator_details_AEMO %>% select(duid, station_name),
                                                                          by = "duid") %>% 
  mutate(value = ifelse(dispatch_type == "Load" & 
                          (var == "energy5min" | var == "energy30min"),
                        -value,
                        value)) #convert energy load to neg

#charge/discharge time
full_data %>% filter(var == "energy5min") %>% 
  group_by(station_name, settlementdate) %>% 
  summarise(net_mw = sum(value)) %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(settlementdate, 12,20), " UTC"))) %>% 
  group_by(station_name, month = nem_month(settlementdate), time) %>% 
  summarise(mean_mwh = mean(net_mw)/12) %>% 
  ggplot(aes(x = time, y = mean_mwh))+
  geom_line()+
  facet_grid(station_name ~ month, scales = "free_y") +
  scale_x_datetime(labels = date_format("%H"))

full_data %>% filter(var == "energy5min") %>% 
  group_by(station_name, interval = ceiling_date(settlementdate, "30 mins")) %>% 
  summarise(net_mw = sum(value)) %>% 
  mutate(time = ymd_hms(paste0("2019/01/01 ", substr(interval, 12,20), " UTC"))) %>% 
  group_by(station_name, month = nem_month(interval), time) %>% 
  summarise(mean_mwh = mean(net_mw)/2) %>% 
  ggplot(aes(x = time, y = mean_mwh))+
  geom_line()+
  facet_grid(station_name ~ month, scales = "free_y") +
  scale_x_datetime(labels = date_format("%H"))

full_data %>% mutate(rev = value*price/12) %>% 
  filter(var != "energy5min") %>% 
  mutate(var = case_when(dispatch_type == "Load" & var == "energy30min" ~ "energy_load",
                         dispatch_type == "Generator" & var == "energy30min" ~ "energy_gen",
                         TRUE ~ var),
         category = case_when(var %in% c("energy_load", "energy_gen") ~ "energy",
                              TRUE ~ "FCAS"),
         value = case_when(category == "energy" ~ value/12,
                           TRUE ~ value)) %>% #convert energy to MWh
  group_by(station_name, var, category, month = nem_month(settlementdate)) %>% 
  summarise(total_value = sum(value)) %>% 
  ggplot(aes(x = month, y = total_value, fill = var))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(category~station_name, scales = "free_y")

#profit stack
full_data %>% mutate(rev = value*price/12) %>% 
  filter(var != "energy5min") %>% 
  mutate(var = case_when(dispatch_type == "Load" & var == "energy30min" ~ "energy_load",
                     dispatch_type == "Generator" & var == "energy30min" ~ "energy_gen",
                     TRUE ~ var)) %>% 
  group_by(station_name, var, month = nem_month(settlementdate)) %>% 
  summarise(total_rev = sum(rev)) %>% 
  ggplot(aes(x = month, y = total_rev, fill = var))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(~station_name)

full_data %>% mutate(rev = value*price/12) %>% 
  filter(var != "energy5min") %>% 
  mutate(var = case_when(dispatch_type == "Load" & var == "energy30min" ~ "energy_load",
                         dispatch_type == "Generator" & var == "energy30min" ~ "energy_gen",
                         TRUE ~ "FCAS")) %>% 
  group_by(station_name, var, month = nem_month(settlementdate)) %>% 
  summarise(total_rev = sum(rev)) %>% 
  ggplot(aes(x = month, y = total_rev, fill = var))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(~station_name)

full_data %>% mutate(rev = value*price/12) %>% 
  filter(var != "energy5min") %>% 
  mutate(var = case_when(dispatch_type == "Load" & var == "energy30min" ~ "energy_load",
                         dispatch_type == "Generator" & var == "energy30min" ~ "energy_gen",
                         var %in% c("raisereg", "lowerreg") ~ "Frequancy Control",
                         TRUE ~ "Ancillary Services")) %>% 
  group_by(station_name, var, month = nem_month(settlementdate)) %>% 
  summarise(total_rev = sum(rev)) %>% 
  ggplot(aes(x = month, y = total_rev, fill = var))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(~station_name)
