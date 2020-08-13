#Analysis_9_Quantity_Weighted_reg

#calculates quantity weighted average for regression
#NOTE: data only contains observations where initialmw>0, when initialmw!=0 it doesnt affact QWA
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

# create data

lmp_data <- fread("D:/Battery/Data/full_lmp_mc_filtered.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) 


map2(paste0("D:/NEM_LMP/Data/RAW/INITIALMW/2019-",str_pad(c(1:12), 2, pad = "0"),".csv"), #data from
     paste0("D:/Battery/Data/Monthly/INITIALMW_LMP_2019-",str_pad(c(1:12), 2, pad = "0"),".csv"), #data to
     ~fread(.x) %>% clean_names() %>%  
      filter(initialmw > 0) %>% #only keep pos initialmw
      mutate(settlementdate = ymd_hms(settlementdate)) %>% 
      group_by(settlementdate, duid) %>% 
      filter(intervention == max(intervention)) %>% #if intervention, keep it and remove int==0)
      ungroup() %>% 
      select(-intervention) %>% 
      left_join(lmp_data, by = c("settlementdate", "duid")) %>% #merge with lmp
      fwrite(.y))

full_data <- map_df(paste0("D:/Battery/Data/Monthly/",list.files("D:/Battery/Data/Monthly/")),
                   ~fread(.x) %>% 
                     mutate(settlementdate = ymd_hms(settlementdate)))

weighted_data <- full_data %>% 
  filter(year(settlementdate)==2019, #remove 2020-01-01
         duid %notin% c("RT_VIC1", "RT_SA1", "RT_VIC2", "RT_VIC3", "DG_SA1" )) %>% #remove weird extra initialmw vals, think its to do w emergency reserves
  mutate(prod_initialmw_lmp = initialmw*lmp,
         prod_initialmw_lmp_mc = initialmw*lmp_mc) %>% 
  group_by(settlementdate) %>% 
  mutate(qw_lmp = sum(prod_initialmw_lmp)/sum(initialmw),
         qw_lmp_mc = sum(prod_initialmw_lmp_mc)/sum(initialmw)) 

fwrite(weighted_data, "D:/Battery/Data/Quantity Weighted/full_lmp_quantity_weighted.csv")

#Load data

weighted_data <- fread("D:/Battery/Data/Quantity Weighted/full_lmp_quantity_weighted.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate))

generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")

# Regression

reg <- weighted_data %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_lmp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details, by = "duid")

fwrite(reg_coeffs, "D:/Battery/Data/Quantity Weighted/reg_coeffs.csv")

reg_coeffs <- fread("D:/Battery/Data/Quantity Weighted/reg_coeffs.csv")


reg_coeffs %>% 
  ggplot(aes(x = beta))+
  geom_histogram()+
  ggsave("Output/Regressions/Weighted Average/weighted_ave_beta.png")

reg_coeffs %>% 
  ggplot(aes(x = alpha))+
  geom_histogram() + 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_alpha.png")


fwrite(reg_coeffs, "Output/Regressions/Weighted Average/weighted_ave_coeffs.csv")

# beta clear split >1
reg_coeffs %>% mutate(group_1 = ifelse(beta<1, TRUE, FALSE)) %>% 
  group_by(group_1) %>% 
  count(fuel_type, region) %>% 
  pivot_wider(id_cols = c(fuel_type, region), names_from = group_1, values_from = n)

reg_coeffs %>% 
  ggplot(aes(x = beta, fill = fuel_type))+
  geom_histogram()

reg_coeffs %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+ 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_beta_region.png")

reg_coeffs %>% 
  ggplot(aes(x = beta, fill = fuel_type))+
  geom_histogram()+ 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_beta_fueltype.png")

reg_coeffs %>% 
  ggplot(aes(x = beta, fill = type))+
  geom_histogram() + 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_beta_genload.png")


#alphas
reg_coeffs %>% filter(alpha<1000) %>% 
  ggplot(aes(x = alpha, fill = fuel_type))+
  geom_histogram()

reg_coeffs %>% filter(alpha<1000) %>% 
  ggplot(aes(x = alpha, fill = region))+
  geom_histogram()+ 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_alpha_region.png")

reg_coeffs %>% filter(alpha<1000) %>% 
  ggplot(aes(x = alpha, fill = fuel_type))+
  geom_histogram()+ 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_alpha_fueltype.png")

reg_coeffs %>% filter(alpha<1000) %>% 
  ggplot(aes(x = alpha, fill = (beta<1)))+
  geom_histogram()+ 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_alpha_by_beta.png")

reg_coeffs %>% filter(alpha<1000) %>% 
  ggplot(aes(x = alpha, fill = type))+
  geom_histogram() + 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_alpha_genload.png")

#LMP_mc

reg_mc <- weighted_data %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp_mc ~ qw_lmp_mc, data = .)))

reg_coeffs_mc <- reg_mc %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details, by = "duid")

reg_coeffs_mc %>% 
  ggplot(aes(x = beta))+
  geom_histogram()+
  ggsave("Output/Regressions/Weighted Average/weighted_ave_beta_mc.png")

reg_coeffs_mc %>% 
  ggplot(aes(x = alpha))+
  geom_histogram() + 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_alpha_mc.png")


reg_coeffs %>% 
  fwrite("Output/Regressions/Weighted Average/weighted_ave_coeffs_mc.csv")

