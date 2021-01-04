#Results_4.2_Filtered

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
#########################################

full_data <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw_cleaned.csv") %>% 
  left_join(fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
              distinct(duid, .keep_all = TRUE) %>% select(duid, dispatch_type, technology_type_descriptor), by = "duid") %>% 
  filter(dispatch_type == "Generator") %>% #remove loads
  select(-dispatch_type) %>% 
  mutate(lmp = case_when(lmp<(-1000) ~ (-1000), #cap
                         lmp>15000 ~ 15000, 
                         TRUE ~ lmp)) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  group_by(settlementdate) %>% 
  mutate(qw_rrp = sum(initialmw*rrp)/sum(initialmw)) #add QW_LMP

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

# REGRESSION
##################################

reg <- full_data %>% filter(qw_rrp<1000) %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_rrp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details_AEMO, by = "duid")

# PLOTS
#####################

reg_coeffs %>% 
  ggplot(aes(x = beta))+
  geom_histogram()+
  ggsave("Output/Regressions/Results/QW_RRP/Filtered/beta.png", width = 10)

reg_coeffs %>% filter(beta <10)  %>% 
  ggplot(aes(x = beta, fill = fuel_source_descriptor))+
  geom_histogram()+
  ggsave("Output/Regressions/Results/QW_RRP/Filtered/beta_fueltype.png", width = 10)

reg_coeffs %>% filter(beta <10)  %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+
  ggsave("Output/Regressions/Results/QW_RRP/Filtered/beta_region.png", width = 10)

# INDIVIDUAL PLOTS
###########################################

top_beta <- reg_coeffs %>% arrange(-beta) %>% distinct(station_name, .keep_all = TRUE) %>% head(n=10)
bottom_beta <-reg_coeffs %>% arrange(beta) %>% distinct(station_name, .keep_all = TRUE) %>% head(n=10)  

full_data %>% filter(duid %in% top_beta$duid, qw_rrp<1000) %>% 
  ggplot(aes(x = qw_rrp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)+
  labs(title = "Top 10 betas") +
  ggsave("Output/Regressions/Results/QW_RRP/Filtered/top_10_betas.png", width = 10)

full_data %>% filter(duid %in% bottom_beta$duid, qw_rrp<1000) %>% 
  ggplot(aes(x = qw_rrp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)+
  labs(title = "Bottom 10 betas")+
  ggsave("Output/Regressions/Results/QW_RRP/Filtered/bottom_10_betas.png", width = 10)


full_data %>% filter(duid %in% bottom_beta$duid[1], qw_rrp<1000) %>% 
  ggplot(aes(x = qw_rrp, y = lmp)) + 
  geom_point() +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradientn(colors = c("#FFEDA0", "#FEB24C", "#F03B20"))

full_data %>% filter(duid %in% bottom_beta$duid[1], qw_rrp<1000) %>% 
  ggplot(aes(x = qw_rrp, y = lmp)) + 
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_gradientn(colors = c("#FFEDA0", "#FEB24C", "#F03B20"))
