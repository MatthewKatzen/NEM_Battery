#Analysis7_All_Batteries_LMP

# All battery graphs
# What time do they charge/discharge
# How much profit do they earn each day?

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

battery_duids <- c("BALBG1", "BALBL1", "DALNTH01", "DALNTHL1", "GANNBG1", "GANNBL1", "HPRG1", "HPRL1") #run all of 2019
sa_batteries_duids <- c("DALNTH01", "DALNTHL1", "HPRG1", "HPRL1")
sa_batteries_stations <- c("Dalrymple North BESS", "Hornsdale Power Reserve")

vic_batteries_duids <- c("BALBG1", "BALBL1", "GANNBG1", "GANNBL1")
vic_batteries_stations <- c("Ballarat Battery Energy Storage System", "Gannawarra Energy Storage System")

#RRP & LMP
f <- function(x, pos) x %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  filter(duid %in% battery_duids, #only keep batteries
         settlementdate != "2019-01-01 00
         :00:00 UTC") 

lmp_data <- read_csv_chunked("D:/Battery/Data/full_lmp_mc_filtered.csv", DataFrameCallback$new(f), chunk_size = 200000)%>% #remove first interval bc its part of 2018
  group_by(interval = ceiling_date(settlementdate, "30 min"), duid) %>% #create RRP30 
  mutate(rrp30 = mean(rrp)) 

fwrite(lmp_data, "D:/Battery/Data/full_lmp_mc_battery.csv")

#read lmp and rrp battery
f <- function(x, pos) x %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) 

lmp_data <- read_csv_chunked("D:/Battery/Data/full_lmp_mc_battery.csv", DataFrameCallback$new(f), chunk_size = 2000000)

#battery dispatch data
f <- function(x, pos) x %>% clean_names() %>%  #only keep half hourly rows
  filter(initialmw > 0,
         duid %in% battery_duids) %>% #only keep pos initialmw
  group_by(settlementdate, duid) %>% 
  filter(intervention == max(intervention)) %>% #if intervention, keep it and remove int==0)
  ungroup() %>% 
  select(-intervention) #remove intervention var

battery_data_2019 <- map_dfr(paste0("D:/NEM_LMP/Data/RAW/INITIALMW/2019-",str_pad(c(1:12), 2, pad = "0"),".csv"),
                             ~read_csv_chunked(.,DataFrameCallback$new(f), chunk_size = 2000000))

### Battery rev rrp and lmp

battery_and_price_data <- lmp_data  %>%  
  left_join(battery_data_2019, by = c("duid", "settlementdate")) %>% #add dispatch data
  left_join(generator_details %>% select(type, duid, station), by = "duid") %>% #add gen type
  mutate(initialmw = case_when(is.na(initialmw) ~ 0,
                               type == "Load" ~ -initialmw,
                               type == "Gen" ~ initialmw))


#daily under each of lmp, lmpmc, rrp30
battery_profits <- battery_and_price_data %>% 
  mutate(quarter = lubridate::quarter(settlementdate),
         day = floor_date(settlementdate, "day"),
         rev_rrp = initialmw/12 * rrp,
         rev_rrp30 = initialmw/12 * rrp30,
         rev_lmp = initialmw/12 * lmp,
         rev_lmp_mc = initialmw/12 * lmp_mc) %>% 
  group_by(station, day, quarter) %>% 
  summarise(profit_rrp = sum(rev_rrp),
            profit_rrp30 = sum(rev_rrp30),
            profit_lmp = sum(rev_lmp),
            profit_lmp_mc = sum(rev_lmp_mc)) 

# quarterly profits TABLE
battery_profits %>% group_by(quarter, station) %>% 
  summarise(profit_rrp = sum(profit_rrp),
           profit_rrp30 = sum(profit_rrp30),
           profit_lmp = sum(profit_lmp),
           profit_lmp_mc = sum(profit_lmp_mc)) %>% 
  left_join(generator_details %>% select(station, region) %>% unique(), by = "station") %>% 
  fwrite("Output/Real Batteries/2019 profits.csv")


# daily profit boxplot
battery_profits %>% pivot_longer(cols = c(profit_rrp, profit_rrp30, profit_lmp, profit_lmp_mc), names_to = "var") %>% 
  ggplot(aes(x = var, y = value, fill = var))+
  geom_boxplot() + 
  facet_wrap(station~quarter, nrow = 4)

battery_profits %>% pivot_longer(cols = c(profit_rrp, profit_rrp30, profit_lmp, profit_lmp_mc), names_to = "var") %>% 
  ggplot(aes(x = var, y = value, fill = var))+
  geom_boxplot() + 
  facet_wrap(station~quarter, nrow = 4)+
  coord_cartesian(ylim = c(-5000,10000))

battery_profits %>% pivot_longer(cols = c(profit_rrp, profit_rrp30, profit_lmp, profit_lmp_mc), names_to = "var") %>% 
  filter(var %in% c("profit_lmp", "profit_rrp30")) %>% 
  ggplot(aes(x = station, y = value, fill = station))+
  geom_boxplot() + 
  facet_wrap(var~quarter, nrow = 2)+
  coord_cartesian(ylim = c(-5000,10000))+
  labs(title = "LMP & RRP30 daily profits - grouped by quarter")+
  ggsave("Output/Real Batteries/Battery LMP & RRP30 Profit boxplot.png", width = 10, height = 7)

# daily profit histogram
battery_profits %>% 
  ggplot(aes(x = profit_rrp30))+
  geom_histogram(binwidth = 1000) + 
  facet_wrap(station~quarter, nrow = 4)+
  coord_cartesian(xlim = c(-10000,20000))+
  labs(title = "RRP30 daily profits - grouped by quarter")+
  ggsave("Output/Real Batteries/Battery RRP30 daily profit histogram.png", width = 10, height = 7)

battery_profits %>% 
  ggplot(aes(x = profit_lmp))+
  geom_histogram(binwidth = 1000) + 
  facet_wrap(station~quarter, nrow = 4)+
  coord_cartesian(xlim = c(-10000,20000))+
  ggsave("Output/Real Batteries/Battery LMP daily profit histogram.png", width = 10, height = 7)

# Graph of mean output by time
daily_battery_output <- battery_and_price_data %>% 
    group_by(interval, station, rrp30) %>% 
    summarise(netMWh = sum(initialmw/12)) %>% 
    mutate(time = interval %>% strftime(format="%H:%M:%S") %>% 
             paste0("2019-01-01 ",.) %>% ymd_hms(),
           quarter = lubridate::quarter(interval))

daily_battery_output %>% left_join(generator_details %>% select(station, region) %>% unique(), "station") %>% 
  filter(region == "SA") %>% #only SA
  group_by(station, time, quarter) %>% 
  summarise(netMWh = mean(netMWh), rrp30 = mean(rrp30)) %>% 
  pivot_wider(id_cols = c(time, rrp30, quarter), names_from = station, values_from = netMWh) %>% 
  pivot_longer(cols = c(rrp30, sa_batteries_stations), names_to = "var") %>% 
  ggplot(aes(x = time, y = value))+
  geom_area()+
  facet_wrap(var~quarter, nrow=3, scales = "free") +
  scale_x_datetime(labels = date_format("%H:%M:%S"))+
  labs(title = "SA mean battery output - grouped by quarter")
ggsave("Output/Real Batteries/SA mean battery output.png", width = 15, height = 7)

daily_battery_output %>% left_join(generator_details %>% select(station, region) %>% unique(), "station") %>% 
  filter(region == "VIC") %>% #only VIC
  group_by(station, time, quarter) %>% 
  summarise(netMWh = mean(netMWh), rrp30 = mean(rrp30)) %>% 
  pivot_wider(id_cols = c(time, rrp30, quarter), names_from = station, values_from = netMWh) %>% 
  pivot_longer(cols = c(rrp30, vic_batteries_stations), names_to = "var") %>% 
  ggplot(aes(x = time, y = value))+
  geom_area()+
  facet_wrap(var~quarter, nrow=3, scales = "free")+
  scale_x_datetime(labels = date_format("%H:%M:%S"))+
  labs(title = "VIC mean battery output - grouped by quarter")
ggsave("Output/Real Batteries/VIC mean battery output.png", width = 15, height = 7)

# boxplot by half hour
daily_battery_output %>% left_join(generator_details %>% select(station, region) %>% unique(), "station") %>% 
  filter(region == "SA") %>% #only SA
  ggplot(aes(x = time, y = netMWh, group = time))+
  geom_boxplot()+
  facet_wrap(station~quarter, nrow=2, scales = "free")+
  ggsave("Output/Real Batteries/SA battery output boxplot.png", width = 15, height = 7)

daily_battery_output %>% left_join(generator_details %>% select(station, region) %>% unique(), "station") %>% 
  filter(region == "VIC") %>% #only SA
  ggplot(aes(x = time, y = netMWh, group = time))+
  geom_boxplot()+
  facet_wrap(station~quarter, nrow=2, scales = "free")+
  ggsave("Output/Real Batteries/VIC battery output boxplot.png", width = 15, height = 7)

#average charge/discharge price by quarter
daily_battery_output %>% group_by(station, quarter) %>% 
  summarise(charge_ave = mean(rrp30[netMWh>0]),
            discharge_ave = mean(rrp30[netMWh<0]))
