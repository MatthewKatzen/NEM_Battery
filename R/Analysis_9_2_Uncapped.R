#Analysis_9_2_Uncapped

# Uncapped LMPs and detailed fuel types

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

# MERGE DATASETS
###########################################

# Uncapped LMPs
RRP <-  fread("D:/Data/Cleaned/RRP/2019_RRP.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate))

generator_details <- fread("D:/Data/RAW/Nemsight/generator_details.csv") %>% clean_names() %>% 
  select(duid, type, station, region, capacity, fuel_type, schedule_type) %>% 
  filter(schedule_type != "Non-scheduled")

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

adjustment <- fread(paste0("D:/Data/Raw/AEMC/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(-locally_constrained) %>% 
  filter(year(settlementdate)==2019) 

merged_data <- RRP %>% 
  left_join(generator_details, by = "region") %>% #merge dataframes
  left_join(adjustment, by = c("settlementdate", "duid")) %>% 
  mutate(local_price_adjustment = coalesce(local_price_adjustment, 0)) %>% #convert NA to 0
  mutate(lmp = case_when(type == "Gen" ~ rrp + local_price_adjustment,
                         type == "Load" ~ rrp - local_price_adjustment))

fwrite(merged_data, "D:/Data/Cleaned/LMP/full_lmp_uncapped.csv")


#Add initialmw

uncapped_lmp <- fread("D:/Data/Cleaned/LMP/full_lmp_uncapped.csv") %>% 
  select(-schedule_type, -type, -station, -fuel_type, -local_price_adjustment) %>% mutate(settlementdate = ymd_hms(settlementdate))
  

map2(paste0("D:/Data/RAW/AEMC/INITIALMW/2019-",str_pad(c(1:12), 2, pad = "0"),".csv"), #data from
     paste0("D:/Data/Cleaned/INITIALMW/Monthly_Uncapped/INITIALMW_LMP_UNCAPPED_2019-",str_pad(c(1:12), 2, pad = "0"),".csv"), #data to
     ~fread(.x) %>% clean_names() %>%  
       mutate(settlementdate = ymd_hms(settlementdate)) %>% 
       filter(intervention == 0) %>% #keep non-intervention
       inner_join(uncapped_lmp, by = c("settlementdate", "duid")) %>% #merge with lmp, only keeps duids which are in both lmp and initialmw
       fwrite(.y))

# merge into yearly file
full_data_uncapped <- map_df(list.files("D:/Data/Cleaned/INITIALMW/Monthly_Uncapped/", full.names = TRUE),
                    ~fread(.x) %>% 
                      mutate(settlementdate = ymd_hms(settlementdate))) 
  

fwrite(full_data_uncapped, "D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw.csv")



#create quantity weighted dataset
full_data_uncapped <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  left_join(generator_details_AEMO %>% select(duid, dispatch_type), by = "duid") %>% 
  filter(dispatch_type == "Generator") %>% select(-dispatch_type) #remove loads
  

weighted_data_uncapped <- full_data_uncapped %>% 
  group_by(settlementdate) %>% 
  mutate(qw_lmp = sum(initialmw*lmp)/sum(initialmw),
         qw_rrp = sum(initialmw*rrp)/sum(initialmw))

fwrite(weighted_data_uncapped, "D:/Data/Cleaned/Quantity Weighted/full_lmp_uncapped_quantity_weighted.csv")


# THEN RUN 16.R


# ANALYSIS
#################################

weighted_data_uncapped <- fread("D:/Data/Cleaned/Quantity Weighted/full_lmp_uncapped_quantity_weighted_cleaned.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) 

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

reg <- weighted_data_uncapped %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_lmp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details_AEMO, by = "duid")



#  COEFF Plots
##################################


reg_coeffs %>% 
  ggplot(aes(x = alpha, y = beta))+
  geom_point()

reg_coeffs %>% 
  ggplot(aes(x = alpha))+
  geom_histogram()

reg_coeffs %>%
  ggplot(aes(x = beta))+
  geom_histogram() +
  ggsave("Output/Regressions/Uncapped Weighted Average/uncapped_weighted_ave_beta.png", width = 10)


reg_coeffs %>% filter(abs(beta)<10) %>% 
  ggplot(aes(x = beta, fill = technology_type_descriptor))+
  geom_histogram()+
  ggsave("Output/Regressions/Uncapped Weighted Average/uncapped_weighted_ave_beta_fueltype.png", width = 10)


# INDIVIDUAL PLOTS
###########################################

top_beta <- reg_coeffs %>% arrange(-beta) %>% distinct(station_name, .keep_all = TRUE) %>% head(n=10)
bottom_beta <-reg_coeffs %>% arrange(beta) %>% distinct(station_name, .keep_all = TRUE) %>% head(n=10)  

weighted_data_uncapped %>% filter(duid %in% top_beta$duid) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)+
  labs(title = "Top 10 betas (beta = 4 to 28)") +
  ggsave("Output/Regressions/Uncapped Weighted Average/top_10_betas.png", width = 10)

weighted_data_uncapped %>% filter(duid %in% bottom_beta$duid) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)+
  labs(title = "Bottom 10 betas (beta = -2.5 to -0.07)")+
  ggsave("Output/Regressions/Uncapped Weighted Average/bottom_10_betas.png", width = 10)

#BASTYAN which has some very large outliers

weighted_data_uncapped %>% filter(duid == "FINLYSF1") %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  geom_point() +  
  geom_smooth(method='lm', formula= y~x)+
  geom_smooth(data = . %>% filter(lmp > (-1000000)), method='lm', formula= y~x)


weighted_data_uncapped %>% filter(duid == "FINLYSF1") %>% 
  lm(lmp ~ qw_lmp, data = .)
            
weighted_data_uncapped %>% filter(duid == "FINLYSF1") %>% filter(lmp > (-1000000)) %>% 
  lm(lmp ~ qw_lmp, data = .)


