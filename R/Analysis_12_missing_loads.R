#Missing loads

#do we have load data?
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

#load data
data_location <- "D:/NEM_LMP/Data/Raw"
adjustment <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(-locally_constrained) %>% 
  filter(year(settlementdate)==2019) 

generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")

adjustment[["duid"]] %>% unique()
