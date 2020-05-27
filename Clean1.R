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

data_location <- "D:/NEM_LMP/Data/Raw"
Price <- fread(paste0(data_location, "/dispatchprice_24-01-2020.csv")) %>% clean_names() %>% 
  filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything. Just an indicator
  #for intervention dispatch quantity (which is definitely different)
  mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
  mutate(settlementdate = ymd_hms(settlementdate))%>% 
  select(-regionid) %>% 
  group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #add RRP30 
  mutate(rrp30 = mean(rrp)) %>% 
  as.data.frame()

Adjustment <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(-locally_constrained)

Generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv")


LMP <- inner_join(Adjustment, Generator_details, by = "duid") %>% 
  right_join(Price, by = c("settlementdate", "region")) %>% 
  mutate(lmp = rrp + if_else(is.na(local_price_adjustment), 0, local_price_adjustment))

Latlon <- fread("C:/Users/matta/Dropbox/RA/Battery/DUID_latlon.csv") %>% clean_names()

Mean_LMP <- LMP %>% group_by(duid) %>% summarise(mean_lmp = mean(lmp))

Output <- Mean_LMP %>% right_join(Latlon, by = "duid") %>% filter(!is.na(mean_lmp))
write.csv(Output, "Output.csv")
                                              