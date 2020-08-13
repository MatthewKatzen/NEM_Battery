#Analysis6_All_Batteries

#What are all battery strategies for 2019?

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
  filter(year(settlementdate)==2019) %>%   #keep only 2018
  filter(minute(settlementdate) == 0 | minute(settlementdate) == 30) %>%  #only keep half hourly rows
  rename(settlementdate_30min = settlementdate)

rrp_2019_data <- read_csv_chunked("D:/NEM_LMP/Data/Raw/dispatchprice_24-01-2020.csv", DataFrameCallback$new(f), chunk_size = 2000000)

#battery dispatch data
f <- function(x, pos) x %>% clean_names() %>%  #only keep half hourly rows
  filter(initialmw > 0,
         duid %in% battery_duids) %>% #only keep pos initialmw
  group_by(settlementdate, duid) %>% 
  filter(intervention == max(intervention)) %>% #if intervention, keep it and remove int==0)
  ungroup() %>% 
  select(-intervention) #remove intervention var
  

battery_data_2019 <- map_dfr(paste0("D:/NEM_LMP/Data/RAW/INITIALMW/2019-",str_pad(c(1:12), 2, pad = "0"),".csv"),
                        ~read_csv_chunked(.,DataFrameCallback$new(f), chunk_size = 2000000)) %>% 
  mutate(settlementdate_30min = ceiling_date(settlementdate, "30 minutes"))

#which batteries run all year?
battery_data_2019 %>% group_by(duid) %>% summarise(min(settlementdate), max(settlementdate))
battery_duids <- c("BALBG1", "BALBL1", "DALNTH01", "DALNTHL1", "GANNBG1", "GANNBL1", "HPRG1", "HPRL1")
generator_details %>% filter(duid %in% battery_duids) #which state are each in?
sa_batteries_duids <- c("DALNTH01", "DALNTHL1", "HPRG1", "HPRL1")
sa_batteries_stations <- c("Dalrymple North BESS", "Hornsdale Power Reserve")

vic_batteries_duids <- c("BALBG1", "BALBL1", "GANNBG1", "GANNBL1")
vic_batteries_stations <- c("Ballarat Battery Energy Storage System", "Gannawarra Energy Storage System")

# SA what time do batteries charge in 2019?
battery_average_charge_data_SA <- battery_data_2019 %>% 
  filter(duid %in% sa_batteries_duids) %>% #only batteries which were active for entire year in SA
  group_by(settlementdate_30min, duid) %>% 
  summarise(mwh = sum(initialmw/12)) %>% #convert 5min MW to 30min MWh
  left_join(generator_details %>% select(duid, type, station), by = "duid") %>%  #add duid state
  mutate(mwh = ifelse(type == "Gen", mwh, -mwh)) %>% #make load neg
  group_by(settlementdate_30min, station) %>% 
  summarise(net = sum(mwh)) %>% 
  pivot_wider(names_from = station, values_from = net) %>% #make wide
  right_join(rrp_2019_data %>% filter(region == "SA"), 
             by = c("settlementdate_30min")) %>% #add price
  pivot_longer(cols = c(sa_batteries_stations, rrp30), names_to = "vars") %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% #change NA to 0
  mutate(time = settlementdate_30min %>% strftime(format="%H:%M:%S") %>% 
           paste0("2019-01-01 ",.) %>% ymd_hms()) %>% #create time only variable
  mutate(quarter = quarter(settlementdate_30min)) %>% 
  group_by(time, quarter, vars) %>% 
  summarise(value = mean(value))

ggplot(battery_average_charge_data_SA %>% 
         group_by(time, quarter, vars) %>% 
         summarise(value = mean(value)), 
       aes(x = time, y = value)) +
  geom_area() +
  scale_x_datetime(labels = date_format("%H:%M:%S")) +
  facet_wrap(vars~quarter, nrow=3, scales = "free") +
  labs(title = "SA Batteries Dispatch Average by Time 2019")

# VIC what time do batteries charge in 2019?
battery_average_charge_data_VIC <- battery_data_2019 %>% 
  filter(duid %in% vic_batteries_duids) %>% #only batteries which were active for entire year in vic
  group_by(settlementdate_30min, duid) %>% 
  summarise(mwh = sum(initialmw/12)) %>% #convert 5min MW to 30min MWh
  left_join(generator_details %>% select(duid, type, station), by = "duid") %>%  #add duid state
  mutate(mwh = ifelse(type == "Gen", mwh, -mwh)) %>% #make load neg
  group_by(settlementdate_30min, station) %>% 
  summarise(net = sum(mwh)) %>% 
  pivot_wider(names_from = station, values_from = net) %>% #make wide
  right_join(rrp_2019_data %>% filter(region == "VIC"), 
             by = c("settlementdate_30min")) %>% #add price
  pivot_longer(cols = c(vic_batteries_stations, rrp30), names_to = "vars") %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% #change NA to 0
  mutate(time = settlementdate_30min %>% strftime(format="%H:%M:%S") %>% 
           paste0("2019-01-01 ",.) %>% ymd_hms()) %>% #create time only variable
  mutate(quarter = quarter(settlementdate_30min)) %>% 
  group_by(time, quarter, vars) %>% 
  summarise(value = mean(value))

ggplot(battery_average_charge_data_VIC, 
       aes(x = time, y = value)) +
  geom_area() +
  scale_x_datetime(labels = date_format("%H:%M:%S")) +
  facet_wrap(vars~quarter, nrow=3, scales = "free") +
  labs(title = "VIC Batteries Dispatch Average by Time 2019")


### distribution of daily energy profits?

#SA 

battery_data_SA <- battery_data_2019 %>% 
  filter(duid %in% sa_batteries_duids) %>% #only batteries which were active for entire year in SA
  group_by(settlementdate_30min, duid) %>% 
  summarise(mwh = sum(initialmw/12)) %>% #convert 5min MW to 30min MWh
  left_join(generator_details %>% select(duid, type, station), by = "duid") %>%  #add duid state
  mutate(mwh = ifelse(type == "Gen", mwh, -mwh)) %>% #make load neg
  group_by(settlementdate_30min, station) %>% 
  summarise(net = sum(mwh)) %>% 
  pivot_wider(names_from = station, values_from = net) %>% #make wide
  right_join(rrp_2019_data %>% filter(region == "SA"), 
             by = c("settlementdate_30min")) %>% #add price
  pivot_longer(cols = c(sa_batteries_stations), names_to = "vars") %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% #change NA to 0
  mutate(time = settlementdate_30min %>% strftime(format="%H:%M:%S") %>% 
           paste0("2019-01-01 ",.) %>% ymd_hms()) %>% #create time only variable
  mutate(quarter = quarter(settlementdate_30min),
         rev = rrp30*value) 

#SA boxplot
battery_data_SA %>% 
  group_by(vars,quarter, day = floor_date(settlementdate_30min, "day")) %>% 
  summarise(profit = sum(rev)) %>% 
  ggplot(aes(x = quarter, group = quarter, y = profit))+
  geom_boxplot()+
  facet_wrap(~vars, nrow=2, scales = "free") +
  coord_cartesian(ylim = c(-10000,10000)) +
  labs(title = "Daily Profits SA")

#SA density plot
battery_data_SA %>% 
  group_by(vars,quarter, day = floor_date(settlementdate_30min, "day")) %>% 
  summarise(profit = sum(rev)) %>% 
  ggplot(aes(x = profit, group = as.factor(quarter), fill = as.factor(quarter), alpha=0.5))+
  geom_density()+
  facet_wrap(~vars)+
  coord_cartesian(xlim = c(-1000,10000))

#SA histogram
battery_data_SA %>% 
  group_by(vars,quarter, day = floor_date(settlementdate_30min, "day")) %>% 
  summarise(profit = sum(rev)) %>% 
  ggplot(aes(x = profit))+
  geom_histogram(binwidth = 1000)+
  facet_wrap(vars~quarter, nrow = 2)+
  coord_cartesian(xlim = c(-10000,20000))

#Vic
battery_data_VIC <- battery_data_2019 %>% 
  filter(duid %in% (vic_batteries_duids)) %>% #only batteries which were active for entire year in vic
  group_by(settlementdate_30min, duid) %>% 
  summarise(mwh = sum(initialmw/12)) %>% #convert 5min MW to 30min MWh
  left_join(generator_details %>% select(duid, type, station, region), by = "duid") %>%  #add duid state
  mutate(mwh = ifelse(type == "Gen", mwh, -mwh)) %>% #make load neg
  group_by(settlementdate_30min, station) %>% 
  summarise(net = sum(mwh)) %>% 
  pivot_wider(names_from = station, values_from = net) %>% #make wide
  right_join(rrp_2019_data %>% filter(region == "VIC"), 
             by = c("settlementdate_30min")) %>% #add price
  pivot_longer(cols = c(vic_batteries_stations), names_to = "vars") %>% 
  mutate(value = ifelse(is.na(value), 0, value)) %>% #change NA to 0
  mutate(time = settlementdate_30min %>% strftime(format="%H:%M:%S") %>% 
           paste0("2019-01-01 ",.) %>% ymd_hms()) %>% #create time only variable
  mutate(quarter = quarter(settlementdate_30min),
         rev = rrp30*value) 

#VIC box plot
battery_data_VIC %>% 
  group_by(vars,quarter, day = floor_date(settlementdate_30min, "day")) %>% 
  summarise(profit = sum(rev)) %>% 
  ggplot(aes(x = quarter, group = quarter, y = profit))+
  geom_boxplot()+
  facet_wrap(~vars, nrow=2, scales = "free") +
  coord_cartesian(ylim = c(-10000,10000)) +
  labs(title = "Daily Profits VIC")

#VIC density plot
battery_data_VIC %>% 
  group_by(vars,quarter, day = floor_date(settlementdate_30min, "day")) %>% 
  summarise(profit = sum(rev)) %>% 
  ggplot(aes(x = profit, group = as.factor(quarter), fill = as.factor(quarter), alpha=0.5))+
  geom_density()+
  facet_wrap(~vars)+
  coord_cartesian(xlim = c(-10000,10000))

#VIC histogram
battery_data_VIC %>% 
  group_by(vars,quarter, day = floor_date(settlementdate_30min, "day")) %>% 
  summarise(profit = sum(rev)) %>% 
  ggplot(aes(x = profit))+
  geom_histogram(binwidth = 1000)+
  facet_wrap(vars~quarter, nrow = 2)+
  coord_cartesian(xlim = c(-10000,20000))
