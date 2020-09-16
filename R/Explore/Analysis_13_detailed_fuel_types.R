

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

#Compare nemsight to aemo
nemsight_generator_details <- fread("D:/Data/Nemsight/generator_details.csv") %>% clean_names() 

aemo_generator_details <- fread("D:/Data/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)


full <- nemsight_generator_details %>% left_join(aemo_generator_details %>% select(-participant, -region, -station_name), by = "duid") %>% 
  filter(duid %notin% c("LAVNORTH", "CALL_A_2"))#split into 2 duids at some point
  #all missing are decommissioned
