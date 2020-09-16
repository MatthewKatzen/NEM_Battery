#Analysis_9_Quantity_Weighted_reg

# Run 9_2_UNcapped first
# We will cap from here

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

# CREATE DATA 
################################
generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

full_data_capped <- fread("D:/Data/Cleaned/INITIALMW/full_lmp_uncapped_initialmw.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  left_join(generator_details_AEMO %>% select(duid, dispatch_type), by = "duid") %>% 
  filter(dispatch_type == "Generator") %>% select(-dispatch_type) %>% #remove loads
  mutate(lmp = case_when(lmp < (-1000) ~ (-1000),
                               lmp > 15000 ~ 15000,
                               TRUE ~ lmp))


weighted_data_capped <- full_data_capped %>% 
   group_by(settlementdate) %>% 
  mutate(qw_lmp = sum(initialmw*lmp)/sum(initialmw),
         qw_rrp = sum(initialmw*rrp)/sum(initialmw))

fwrite(weighted_data_capped, "D:/Data/Cleaned/Quantity Weighted/full_lmp_capped_quantity_weighted.csv")

#Load data
#############################

weighted_data_capped <- fread("D:/Battery/Data/Quantity Weighted/full_lmp_quantity_weighted.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate))


# Regression
####################################

reg <- weighted_data_capped %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_lmp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details_AEMO, by = "duid")

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
  ggplot(aes(x = beta, fill = technology_type_descriptor))+
  geom_histogram()+ 
  ggsave("Output/Regressions/Weighted Average/weighted_ave_beta_fueltype.png")


