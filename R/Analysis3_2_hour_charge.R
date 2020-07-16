#load packages
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

### Load rrp data

f <- function(x, pos) x %>% clean_names() %>% 
  filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything. Just an indicator
  #for intervention dispatch quantity (which is definitely different)
  mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
  mutate(settlementdate = ymd_hms(settlementdate))%>% 
  group_by(interval = ceiling_date(settlementdate, "30 min"), region) %>% #create RRP30 
  mutate(rrp30 = mean(rrp)) %>% 
  as.data.frame() %>% 
  select(settlementdate, region, rrp30) %>% #remove unnused vars
  filter(year(settlementdate)==2018) %>% #keep only 2018
  filter(minute(settlementdate) == 0 | minute(settlementdate) == 30) #only keep half hourly rows
  
rrp_2018_data <- read_csv_chunked("D:/NEM_LMP/Data/Raw/dispatchprice_24-01-2020.csv", DataFrameCallback$new(f), chunk_size = 2000000)

f <- function(x, pos) x %>% clean_names() %>% 
  filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything. Just an indicator
  #for intervention dispatch quantity (which is definitely different)
  mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
  mutate(settlementdate = ymd_hms(settlementdate))%>% 
  group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #create RRP30 
  mutate(rrp30 = mean(rrp)) %>% 
  as.data.frame() %>% 
  select(settlementdate, region, rrp30) %>% #remove unnused vars
  filter(year(settlementdate)==2019) %>% #keep only 2018
  filter(minute(settlementdate) == 0 | minute(settlementdate) == 30) #only keep half hourly rows

rrp_2019_data <- read_csv_chunked("D:/NEM_LMP/Data/Raw/dispatchprice_24-01-2020.csv", DataFrameCallback$new(f), chunk_size = 2000000)



# Train on 2018 data
# Highest/lowest average seasonal prices


rrp_2018_data <- rrp_2018_data %>% group_by(region) %>% 
  mutate(rrp_2_hour = frollmean(rrp30, n = 4, fill = NA, align = "left"))#mean of 2 hour interval starting at settlementdate

quarterly_means <- rrp_2018_data %>% mutate(time = settlementdate %>% strftime(format="%H:%M:%S") %>% 
                                             paste0("2018-01-01 ",.) %>% ymd_hms()) %>% #add time var
  group_by(region, quarter = lubridate::quarter(settlementdate, fiscal_start = 1), time) %>% 
  summarise(mean_2_hour = mean(rrp_2_hour)) 

charge_intervals <- quarterly_means %>% 
  filter(!is.na(mean_2_hour)) %>% #remove last 1.5 hours of 2018 which don't have a rolling mean bc year cuts off
  group_by(region, quarter) %>% 
  mutate(action = case_when(mean_2_hour == min(mean_2_hour) ~ "charge",
                            mean_2_hour == max(mean_2_hour) ~ "discharge")) %>% 
  filter(mean_2_hour == min(mean_2_hour) | mean_2_hour == max(mean_2_hour))

# Apply algorithm to 2019

charge_intervals_2019 <- rrp_2019_data %>% group_by(region) %>% 
  mutate(rrp_2_hour_2019 = frollmean(rrp30, n = 4, fill = NA, align = "left"),
         quarter = lubridate::quarter(settlementdate, fiscal_start = 1),  #mean of 2 hour interval starting at settlementdate
           time = settlementdate %>% strftime(format="%H:%M:%S") %>% 
           paste0("2018-01-01 ",.) %>% ymd_hms()) %>% #add time var
  right_join(charge_intervals, by= c("time", "quarter", "region")) #join to only keep charge/discharge intervals

#TABLE of profits
charge_intervals_2019 %>%
  mutate(cashflow = case_when(action == "charge"  ~ -rrp_2_hour_2019 * 0.5 * 4, #$/MWh * 0.5MW * 2h (4 x30min intervals) = cost to charge
                           action == "discharge" ~ 0.9* rrp_2_hour_2019 * 0.5 * 4)) %>% 
  group_by(region, quarter, month = month(settlementdate)) %>% 
  summarise(profit = sum(rrp_2_hour_2019), 
            charge = time[action == "charge"][1],
            discharge = time[action == "discharge"][1])

seasonal_profits_2019 %>% group_by(region) %>% 
  summarise(sum = sum(profit)) %>% 
  fwrite("Output/2 Hour Charge/yearly_seasonal_profits_2019.csv")

fwrite(quarterly_profits_2019, "Output/2 Hour Charge/seasonal_profits_2019.csv")


#Why season and quarter dif? (fiscal_year = 12/1) Check Analsysis 4

# distribution of daily profits

#histogram
charge_intervals_2019 %>%
  mutate(cashflow = case_when(action == "charge"  ~ -rrp_2_hour_2019 * 0.5 * 4, #$/MWh * 0.5MW * 2h (4 x30min intervals) = cost to charge
                              action == "discharge" ~ 0.9* rrp_2_hour_2019 * 0.5 * 4)) %>% 
  group_by(day = floor_date(settlementdate, "day"), region, quarter = lubridate::quarter(settlementdate)) %>% 
  summarise(profit = sum(cashflow)) %>% 
  ggplot(aes(x = profit))+
  geom_histogram(binwidth = 10)+
  facet_wrap(region ~ quarter, nrow = 6)+
  coord_cartesian(xlim = c(-100,400))+
  ggsave("Output/2 Hour Charge/daily profit histogram.png", width = 15, height = 7)

#boxplot
charge_intervals_2019 %>%
  mutate(cashflow = case_when(action == "charge"  ~ -rrp_2_hour_2019 * 0.5 * 4, #$/MWh * 0.5MW * 2h (4 x30min intervals) = cost to charge
                              action == "discharge" ~ 0.9* rrp_2_hour_2019 * 0.5 * 4)) %>% 
  group_by(day = floor_date(settlementdate, "day"), region, quarter = lubridate::quarter(settlementdate)) %>% 
  summarise(profit = sum(cashflow)) %>% 
  ggplot(aes(y = profit, x = region, fill = region))+
  geom_boxplot()+
  facet_wrap(~ quarter)+
  coord_cartesian(ylim = c(-100,400))+
  ggsave("Output/2 Hour Charge/daily profit boxplot.png", width = 15, height = 7)
