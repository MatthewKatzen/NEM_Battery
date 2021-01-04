#Results_2.2_QW_RRP_zone

### load packages
library(tidyverse)
#library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
#library(readxl)
#library(writexl)
#library(xtable)
#library(nlme)
#library(scales)
#library(zoo)
#library(hexbin)
library(ggpubr)
Sys.setenv(TZ='UTC')

#load data
#########################################

full_data <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw_cleaned.csv") %>% 
  left_join(fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
              distinct(duid, .keep_all = TRUE) %>% select(duid, dispatch_type, technology_type_descriptor), by = "duid") %>% 
  filter(dispatch_type == "Generator") %>% #remove loads
  select(-dispatch_type) %>% 
  mutate(lmp = case_when(lmp<(-1000) ~ (-1000), #cap
                         lmp>15000 ~ 15000,
                         TRUE ~ lmp)) %>%
  #mutate(dif = lmp - rrp) %>% #dif
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  group_by(settlementdate) %>% 
  mutate(qw_rrp = sum(initialmw*rrp)/sum(initialmw)) #add QW_LMP

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

#Regression
#######################

full_data %>% group_by(region) %>% 
  select(settlementdate, region, rrp, qw_rrp) %>% 
  distinct() %>% 
  group_by(region) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_rrp, data = .))) %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1],
         rows = data.frame(data) %>% nrow(),
         total_gen = data.frame(data) %>% .[["initialmw"]] %>% sum()) %>% 
  select(duid, alpha, beta, rows, total_gen) %>% 
  arrange(-beta)
  

# Plots
###########################
full_data %>% group_by(region) %>% 
  select(settlementdate, region, rrp, qw_rrp) %>% 
  distinct() %>% 
  #filter(abs(qw_rrp)<1000, abs(rrp)<1000) %>% 
  ggplot(aes(x = qw_rrp, y = rrp))+
  facet_wrap(~region)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 10000)+
  ggsave("Output/Regressions/New Results/QW_RRP_zone/Zone_heatmap.png", width = 10)
  
full_data %>% group_by(region) %>% 
  select(settlementdate, region, rrp, qw_rrp) %>% 
  distinct() %>% 
  filter(abs(qw_rrp)<1000, abs(rrp)<1000) %>% 
  ggplot(aes(x = qw_rrp, y = rrp))+
  facet_wrap(~region)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 1000)+
  ggsave("Output/Regressions/New Results/QW_RRP_zone/Zone_capped_heatmap.png", width = 10)
