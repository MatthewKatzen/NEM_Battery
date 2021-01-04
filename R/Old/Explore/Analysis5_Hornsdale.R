#Analysis5_Hornsdale

#What is hornsdale strategy?


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
Sys.setenv(TZ='UTC')


### load data

#generator details
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")

battery_duids <- generator_details %>% filter(fuel_type %in% c("Battery (Discharge)", "Battery (Charge)")) %>% .[["duid"]]

#rrp data
f <- function(x, pos) x %>% clean_names() %>% 
  filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything. Just an indicator
  #for intervention dispatch quantity (which is definitely different)
  mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
  mutate(settlementdate = ymd_hms(settlementdate))%>% 
  group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #create RRP30 
  mutate(rrp30 = mean(rrp)) %>% 
  as.data.frame() %>% 
  select(settlementdate, region, rrp30) %>% #remove unnused vars
  filter(year(settlementdate)==2018) %>%   #keep only 2018
  filter(minute(settlementdate) == 0 | minute(settlementdate) == 30) %>%  #only keep half hourly rows
  rename(settlementdate_30min = settlementdate)

rrp_2018_data <- read_csv_chunked("D:/NEM_LMP/Data/Raw/dispatchprice_24-01-2020.csv", DataFrameCallback$new(f), chunk_size = 2000000)

#battery dispatch data
f <- function(x, pos) x %>% clean_names() %>%  #only keep half hourly rows
  filter(initialmw > 0,
         duid %in% battery_duids) %>% #only keep pos initialmw
  group_by(settlementdate, duid) %>% 
  filter(intervention == max(intervention)) %>% #if intervention, keep it and remove int==0)
  ungroup() %>% 
  select(-intervention) %>% #remove intervention var
  mutate(settlementdate_30min = ceiling_date(settlementdate, "30 minutes"))

battery_data <- map_dfr(paste0("D:/NEM_LMP/Data/RAW/INITIALMW/2018-",str_pad(c(1:12), 2, pad = "0"),".csv"),
        ~read_csv_chunked(.,DataFrameCallback$new(f), chunk_size = 2000000))

# what time does it charge? 2018

hornsdale_data <- battery_data %>% filter(duid %in% c("HPRG1", "HPRL1")) %>% 
   group_by(settlementdate_30min, duid) %>% 
   summarise(mwh = sum(initialmw/12)) %>% #convert 5min MW to 30min MWh
   pivot_wider(names_from = duid, values_from = mwh) %>% #make wide
   right_join(rrp_2018_data %>% filter(region == "SA"), by = "settlementdate_30min") %>% #add price
   pivot_longer(cols = c(HPRG1, HPRL1, rrp30), names_to = "var") %>% 
   mutate(value = ifelse(is.na(value), 0, value)) %>% #change NA to 0
   mutate(time = settlementdate_30min %>% strftime(format="%H:%M:%S") %>% 
            paste0("2018-01-01 ",.) %>% ymd_hms()) %>% #create time only variable
  mutate(quarter = quarter(settlementdate_30min)) %>% 
  group_by(time, quarter, var) %>% 
  summarise(value = mean(value))
   
ggplot(hornsdale_data %>% 
         #mutate(value = ifelse(var=="HPRL1",-value, value)) %>% 
         mutate(type = ifelse(var =="rrp30", "price", "gen/load")), 
       aes(x = time, y = value, colour = var)) +
  geom_line()+
  scale_x_datetime(labels = date_format("%H:%M:%S"))+
  facet_wrap(~type, nrow=2, scales = "free")

ggplot(hornsdale_data %>% pivot_wider(names_from = var, values_from = value) %>% 
         mutate(net=HPRG1-HPRL1) %>% 
         pivot_longer(cols = c(net, rrp30), names_to = "var"), 
       aes(x = time, y = value)) +
  geom_area()+
  scale_x_datetime(labels = date_format("%H:%M:%S"))+
  facet_wrap(var~quarter, nrow=2, scales = "free") +
  labs(title = "Hornsdale 2018")

 
# is there load and gen at same time?
# yes, 155 intervals/22k:
battery_data %>% filter(duid %in% c("HPRG1", "HPRL1")) %>% 
  group_by(settlementdate) %>% 
  filter(n()>1)

# what is price dif b/w charge and dicharge?
battery_data %>% filter(duid %in% c("HPRG1", "HPRL1")) %>% 
  group_by(settlementdate_30min, duid) %>% 
  summarise(mwh = sum(initialmw/12)) %>% 
  pivot_wider(names_from = duid, values_from = mwh) %>% #make wide
  right_join(rrp_2018_data %>% filter(region == "SA") %>% select(-region), 
             by = "settlementdate_30min") %>% #add price
  mutate(HPRG1 = ifelse(is.na(HPRG1), 0, HPRG1),
         HPRL1 = ifelse(is.na(HPRL1), 0, HPRL1)) %>% 
  ungroup() %>% 
  summarise(ave_charge = mean(rrp30[HPRL1>0]),
            ave_discharge = mean(rrp30[HPRG1>0]))

battery_data %>% filter(duid %in% c("HPRG1", "HPRL1")) %>% 
  group_by(settlementdate_30min, duid) %>% 
  summarise(mwh = sum(initialmw/12)) %>% 
  pivot_wider(names_from = duid, values_from = mwh) %>% #make wide
  right_join(rrp_2018_data %>% filter(region == "SA") %>% select(-region), 
             by = "settlementdate_30min") %>% #add price
  mutate(HPRG1 = ifelse(is.na(HPRG1), 0, HPRG1),
         HPRL1 = ifelse(is.na(HPRL1), 0, HPRL1)) %>% 
  group(quarter) %>% 
  summarise(ave_charge = mean(rrp30[HPRL1>HPRG1]),
            ave_discharge = mean(rrp30[HPRG1>HPRL1]))

#total energy revenue/exp
battery_data %>% filter(duid %in% c("HPRG1", "HPRL1")) %>% 
  group_by(settlementdate_30min, duid) %>% 
  summarise(mwh = sum(initialmw/12)) %>% #convert 5min MW to 30min MWh
  pivot_wider(names_from = duid, values_from = mwh) %>% #make wide
  right_join(rrp_2018_data %>% filter(region == "SA"), by = "settlementdate_30min") %>% #add price
  pivot_longer(cols = c(HPRG1, HPRL1, rrp30), names_to = "var") %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% #change NA to 0
  pivot_wider(names_from = var, values_from = value) %>% 
  mutate(quarter = lubridate::quarter(settlementdate_30min)) %>% 
  group_by(quarter) %>% 
  summarise(total_gen = sum(HPRG1), total_load = sum(HPRL1),
            total_rev = sum(HPRG1*rrp30), total_exp = sum(HPRL1*rrp30), 
            profit = sum(HPRG1*rrp30) - sum(HPRL1*rrp30))
