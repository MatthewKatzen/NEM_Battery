#Analysis8_Cycles

#How many charge/discharge cylces do the gens go through?



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
##########################################

battery_and_price_data <- fread("D:/Battery/Data/battery_and_price_data.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate),
         interval = ymd_hms(interval))

### ANALYSIS
#############################################

# Graph of one day
ballarat <- battery_and_price_data %>% 
  mutate(day = floor_date(settlementdate, "day"),
         month = floor_date(settlementdate, "month")) %>% 
  filter(station == "Ballarat Battery Energy Storage System", month == "2019-01-01 00:00:00 UTC") %>% 
  group_by(settlementdate, station, day) %>% 
  summarise(initialmw = sum(initialmw)) %>% ungroup()

ggplot(ballarat,(aes(x = settlementdate, y = initialmw)))+
  geom_line()

ballarat %>% filter(day == "2019-01-22 00:00:00 UTC") %>% 
  ggplot((aes(x = settlementdate, y = initialmw)))+
  geom_line()

#use capacity

p <- ballarat %>% group_by(day) %>% filter(day > "2019-01-20 00:00:00 UTC") %>%  
  mutate(mwh = cumsum(initialmw/12)) %>% 
  ggplot((aes(x = settlementdate, y = mwh, group = day)))+
  geom_smooth(n = 20)+
  geom_vline(xintercept = min_x)

p
temp <- ggplot_build(p)$data[[1]]$y 

# counts local min and local maxs
min <- c(FALSE,(diff(sign(diff(temp)))==2),FALSE) & (temp<(-10)) #is local min and has val< -10
min_x <- ggplot_build(p)$data[[1]]$x[min]

(diff(sign(diff(temp)))==-2) & (temp<10)[-c(1,length(temp))] #is local min and has val< -10




#use initialmw

p2 <- ballarat %>% filter(day > "2019-01-20 00:00:00 UTC") %>%  
  ggplot((aes(x = settlementdate, y = initialmw, group = day)))+
  geom_smooth(n = 100)+
  geom_line(colour = "red")
  geom_vline(xintercept = min_x)

p2
temp <- ggplot_build(p)$data[[1]]$y 

# counts local min and local maxs
min <- c(FALSE,(diff(sign(diff(temp)))==2),FALSE) & (temp<(-10)) #is local min and has val< -10
min_x <- ggplot_build(p)$data[[1]]$x[min]

(diff(sign(diff(temp)))==-2) & (temp<10)[-c(1,length(temp))] #is local min and has val< -10

# take 2
ballarat %>% filter(day > "2019-01-20 00:00:00 UTC") %>% filter(abs(initialmw)>10) %>% 
  
