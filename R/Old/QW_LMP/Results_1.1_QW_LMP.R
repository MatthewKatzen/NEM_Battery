#Results_1.1_QW_LMP

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
  mutate(qw_lmp = sum(initialmw*lmp)/sum(initialmw)) #add QW_LMP

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

# quick look at a few duids data
##########################
full_data %>% filter(duid %in% c("NBHWF1", "BASTYAN", "GSTONE1", "MCKAY1")) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 10000)+
  ggsave("Output/Regressions/New Results/QW_LMP/Uncapped_heatmap.png", width = 10)

#only central cluster
full_data %>% filter(duid %in% c("NBHWF1", "BASTYAN", "GSTONE1", "MCKAY1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black")+
  stat_regline_equation(label.y = 1000)+
  ggsave("Output/Regressions/New Results/QW_LMP/Capped_heatmap.png", width = 10)

# How many not in central cluster?
#############################
full_data %>% group_by(duid) %>% 
  summarise(center_cluster = sum(abs(qw_lmp)<1000 & abs(lmp)<1000)/n()) %>% 
  arrange(center_cluster) #at least 96%

# REGRESSION
##################################
reg_coeffs <- full_data %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_lmp, data = .))) %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1],
         rows = data.frame(data) %>% nrow(),
         total_gen = data.frame(data) %>% .[["initialmw"]] %>% sum()) %>% 
  select(duid, alpha, beta, rows, total_gen) %>% 
  left_join(generator_details_AEMO, by = "duid") %>% 
  arrange(-beta) #put in order of beta
  

# PLOTS
#####################

reg_coeffs %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+
  ggsave("Output/Regressions/New Results/QW_LMP/beta.png", width = 10)

#Multiple hexbins 

full_data %>% filter(duid %in% c("STWF1", "AGLSOM", "ER01", "REECE1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 1000)+
  labs(title = "Group 1 gens")+
  ggsave("Output/Regressions/New Results/QW_LMP/Group1_heatmap.png", width = 10)

full_data %>% filter(duid %in% c("MEWF1", "CSPVPS1", "SMCSF1", "CALL_B_1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 1000)+
  labs(title = "Group 2 gens: QLD")+
  ggsave("Output/Regressions/New Results/QW_LMP/Group2_heatmap.png", width = 10)

full_data %>% filter(duid %in% c("NBHWF1", "BLUFF1", "WGWF1", "HDWF1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black", linetype = "dashed")+
  stat_regline_equation(label.y = 1000)+
  labs(title = "Group 3 gens: SA Wind")+
  ggsave("Output/Regressions/New Results/QW_LMP/Group3_heatmap.png", width = 10)
# Remove price floor observations
##########################################
reg_coeffs %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+
  ggsave("Output/Regressions/New Results/QW_LMP/beta.png", width = 10)


reg_coeffs %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+
  ggsave("Output/Regressions/New Results/QW_LMP/beta_LMPneq500.png", width = 10)

# Weird clustering in group 1
##################################

full_data %>% filter(duid %in% c("AGLSOM")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red") + 
  geom_smooth(method='lm', formula= y~x, colour = "black")+
  stat_regline_equation(label.y = 1000)+
  ggsave("Output/Regressions/New Results/QW_LMP/AGLSOM_heatmap.png", width = 10)



#split into cluster
full_data %>% filter(duid %in% c("AGLSOM")) %>%
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  mutate(left_cluster = lmp > 125 + qw_lmp) %>% 
  ggplot(aes(x = qw_lmp, y = lmp, colour = left_cluster))+
  facet_wrap(~duid)+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, aes(colour=NULL))+
  stat_regline_equation(label.y = 700, aes(colour=NULL))+
  geom_smooth(data = . %>% filter(!left_cluster),
              method='lm', formula= y~x)+
  stat_regline_equation(data = . %>% filter(!left_cluster), label.y = 500)+
  geom_smooth(data = . %>% filter(left_cluster),
              method='lm', formula= y~x)+
  stat_regline_equation(data = . %>% filter(left_cluster), label.y = 600)+
  ggsave("Output/Regressions/New Results/QW_LMP/AGLSOM_cluster.png", width = 10)

#how many are in cluster
full_data %>% filter(duid %in% c("AGLSOM")) %>%
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  mutate(left_cluster = lmp > 125 + qw_lmp) %>% ungroup() %>% 
  count(left_cluster) %>% 
  mutate(prop = n/sum(n)) #5% in left cluster

#make dataset of left cluster intervals
cluster <- full_data %>% filter(duid %in% c("AGLSOM")) %>%
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  mutate(left_cluster = lmp > 125 + qw_lmp) %>% ungroup() %>% 
  select(settlementdate, left_cluster)

cluster %>% ungroup() %>% 
  group_by(date = floor_date(settlementdate, "day")) %>% 
  summarise(count = sum(left_cluster)) %>% 
  ggplot(aes(x = date, y = count))+
  geom_line()+
  labs(title = "How many intervals were in left_cluster in each day?")+
  ggsave("Output/Regressions/New Results/QW_LMP/Left_cluster time series.png", width = 10)

#check cluster against other duids
full_data %>% filter(duid %in% c("MEWF1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  left_join(cluster, by = "settlementdate") %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  geom_point(aes(colour = left_cluster))+
  labs(title = "MEWF1: Group 2")+
  ggsave("Output/Regressions/New Results/QW_LMP/Left_cluster onto MEWF1.png", width = 10)

#how much did each group generate?
reg_coeffs %>% mutate(group = case_when(beta>1.5 & region == "SA1" ~ "Group3",
                                        beta>1.5 & region == "QLD1" ~ "Group2",
                                        TRUE ~ "Group1")) %>% 
  select(duid, beta, group, total_gen) %>% 
  group_by(group) %>% 
  summarise(total_gen = sum(total_gen)/12)

# Reg but only right cluster (too computationally intensive)
#####################################
reg_coeffs <- full_data %>% 
  filter(!(settlementdate %in% cluster$settlementdate),
         abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_lmp, data = .))) %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1],
         rows = data.frame(data) %>% nrow(),
         total_gen = data.frame(data) %>% .[["initialmw"]] %>% sum()) %>% 
  select(duid, alpha, beta, rows, total_gen) %>% 
  left_join(generator_details_AEMO, by = "duid") %>% 
  arrange(-beta) #put in order of beta
