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

### Load data
data_location <- "D:/NEM_LMP/Data/Raw"
adjustment <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(-locally_constrained) %>% 
  filter(year(settlementdate)==2019) 

generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")

price <- fread(paste0(data_location, "/dispatchprice_24-01-2020.csv")) %>% clean_names() %>% 
  filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything. Just an indicator
  #for intervention dispatch quantity (which is definitely different)
  mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
  mutate(settlementdate = ymd_hms(settlementdate))%>% 
  group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #add RRP30 
  mutate(rrp30 = mean(rrp)) %>% 
  as.data.frame() %>% 
  select(settlementdate, region, rrp, rrp30) %>% 
  filter(year(settlementdate)==2019)

### Merge

merged_data <- price %>% left_join(generator_details, by = "region") %>% #merge dataframes
  left_join(adjustment, by = c("settlementdate", "duid")) %>% 
  mutate(local_price_adjustment = coalesce(local_price_adjustment, 0)) %>% #convert NA to 0
  mutate(lmp = rrp + local_price_adjustment) %>% 
  mutate(lmp = case_when((lmp < -1000) ~ (-1000), #censoring at price cap and floor
                                  ((lmp > 14700) & 
                                     settlementdate %within% interval(ymd_hms("2019-07-01 00:05:00 UTC"), 
                                                                      ymd_hms("2020-07-01 00:00:00 UTC"))) ~ 
                                    14700,
                                  ((lmp > 14500) & 
                                     settlementdate %within% interval(ymd_hms("2018-07-01 00:05:00 UTC"), 
                                                                      ymd_hms("2019-07-01 00:00:00 UTC"))) ~ 
                                    14500,
                                  ((lmp > 14200) & 
                                     settlementdate %within% interval(ymd_hms("2017-07-01 00:05:00 UTC"), 
                                                                      ymd_hms("2018-07-01 00:00:00 UTC"))) ~ 
                                    14200,
                                  ((lmp > 14000) & 
                                     settlementdate %within% interval(ymd_hms("2016-07-01 00:05:00 UTC"), 
                                                                      ymd_hms("2017-07-01 00:00:00 UTC"))) ~ 
                                    14000,
                                  ((lmp > 13800) & 
                                     settlementdate %within% interval(ymd_hms("2015-07-01 00:05:00 UTC"), 
                                                                      ymd_hms("2016-07-01 00:00:00 UTC"))) ~ 
                                    13800,
                                  ((lmp > 13500) & 
                                     settlementdate %within% interval(ymd_hms("2014-07-01 00:05:00 UTC"), 
                                                                      ymd_hms("2015-07-01 00:00:00 UTC"))) ~ 
                                    13500,
                                  ((lmp > 13100) & 
                                     settlementdate %within% interval(ymd_hms("2013-07-01 00:05:00 UTC"), 
                                                                      ymd_hms("2014-07-01 00:00:00 UTC"))) ~ 
                                    13100,
                                  ((lmp > 12900) & 
                                     settlementdate %within% interval(ymd_hms("2012-07-01 00:05:00 UTC"), 
                                                                      ymd_hms("2013-07-01 00:00:00 UTC"))) ~ 
                                    12900,
                                  TRUE ~ lmp))

fwrite(merged_data, "D:/Battery/Data/full_lmp.csv")

