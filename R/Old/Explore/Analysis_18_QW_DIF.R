#Analysis_18_DIFF

# capped LMPs and detailed fuel types

# %notin% function in file func.R

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

# DATA
###########################


weighted_data <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw_cleaned.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  group_by(settlementdate) %>% 
  mutate(dif = lmp - rrp,
         qw_dif = sum(initialmw*rrp)/sum(initialmw))

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)
# ANALSYIS
#####################################

#uncapped
reg <- weighted_data %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(dif ~ qw_dif, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details_AEMO, by = "duid")


reg_coeffs %>% filter(beta>(-100)) %>% 
  ggplot(aes(x = beta, fill = technology_type_descriptor))+
  geom_histogram()


reg_coeffs %>% filter(beta>(-100)) %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()

# capped
############################################

weighted_data <- weighted_data %>% 
  mutate(lmp = case_when(lmp<(-1000) ~ (-1000), #cap 
                         lmp>15000 ~ 15000, 
                         TRUE ~ lmp)) %>% 
  group_by(settlementdate) %>% 
  mutate(dif = lmp - rrp,
         qw_dif = sum(initialmw*rrp)/sum(initialmw)) 

reg <- weighted_data %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(dif ~ qw_dif, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details_AEMO, by = "duid")


reg_coeffs %>% filter(beta>(-100)) %>% 
  ggplot(aes(x = beta, fill = technology_type_descriptor))+
  geom_histogram()


reg_coeffs %>% filter(beta>(-100)) %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()
