#Results_2.1_QW_RRP

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

# quick look at a few duids data
##########################

#only central cluster
full_data %>% filter(duid %in% c("NBHWF1", "BASTYAN", "GSTONE1", "MCKAY1")) %>% 
  filter(abs(qw_rrp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_rrp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black")+
  stat_regline_equation(label.y = 1000)

# How many not in central cluster?
#############################
full_data %>% group_by(duid) %>% 
  summarise(center_cluster = sum(abs(qw_rrp)<1000 & abs(lmp)<1000)/n()) %>% 
  arrange(center_cluster) #at least 96%

# REGRESSION
##################################
reg_coeffs <- full_data %>% 
  filter(abs(qw_rrp)<1000, abs(lmp)<1000) %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_rrp, data = .))) %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1],
         rows = data.frame(data) %>% nrow(),
         total_gen = data.frame(data) %>% .[["initialmw"]] %>% sum()) %>% 
  select(duid, alpha, beta, rows, total_gen) %>% 
  left_join(generator_details_AEMO, by = "duid") %>% 
  arrange(-beta) #put in order of beta

# Plots
##################################
reg_coeffs %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+
  ggsave("Output/Regressions/New Results/QW_RRP/beta.png", width = 10)

full_data %>% filter(duid %in% c("STWF1", "AGLSOM", "ER01", "REECE1")) %>% 
  filter(abs(qw_rrp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_rrp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 1000)+
  labs(title = "Group 1 gens")+
  ggsave("Output/Regressions/New Results/QW_RRP/Group1_heatmap.png", width = 10)

full_data %>% filter(duid %in% c("MEWF1", "CSPVPS1", "SMCSF1", "CALL_B_1")) %>% 
  filter(abs(qw_rrp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_rrp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 1000)+
  labs(title = "Group 2 gens: QLD")+
  ggsave("Output/Regressions/New Results/QW_RRP/Group2_heatmap.png", width = 10)

full_data %>% filter(duid %in% c("NBHWF1", "BLUFF1", "WGWF1", "HDWF1")) %>% 
  filter(abs(qw_rrp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_rrp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 1000)+
  labs(title = "Group 3 gens: SA Wind")+
  ggsave("Output/Regressions/New Results/QW_RRP/Group3_heatmap.png", width = 10)
