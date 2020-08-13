#Analysis_9_1_Quantity_Weighted_plots

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

# Load Data

weighted_data <- fread("D:/Battery/Data/Quantity Weighted/full_lmp_quantity_weighted.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate))

reg_coeffs <- fread("Output/Regressions/Weighted Average/weighted_ave_coeffs.csv")

generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")

# Plots
################################

# Top and bottom betas

top_beta <- reg_coeffs %>% arrange(-beta) %>% distinct(station, .keep_all = TRUE) %>% head(n=10) %>% .[["duid"]]
bottom_beta <-reg_coeffs %>% arrange(-beta) %>% distinct(station, .keep_all = TRUE) %>% tail(n=10)  %>% .[["duid"]]

weighted_data %>% filter(duid %in% top_beta) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)+
  labs(title = "Top 10 betas")+
  ggsave("Output/Regressions/Weighted Average/top_10_betas_lmp_v_qw_lmp.png", width = 10)


weighted_data %>% filter(duid %in% bottom_beta) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)+
  labs(title = "Bottom 10 betas")+
  ggsave("Output/Regressions/Weighted Average/bottom_10_betas_lmp_v_qw_lmp.png", width = 10)

# plot of qw_lmps
weighted_data %>% distinct(settlementdate, qw_lmp) %>% 
  ggplot(aes(x = qw_lmp))+
  geom_histogram()

# do reg with only qw_lmp<500
######################################

weighted_data %>% distinct(settlementdate, qw_lmp) %>% count((qw_lmp<500)) #143 out of 104k intervals
weighted_data %>% group_by(duid) %>% summarise(perc = (sum(qw_lmp<500)/n()*100), n=n()) %>% arrange(perc) %>% data.frame() %>% 
  left_join(generator_details, by = "duid")# lowest perc of operating in high qw_lmp is 72%

reg_filtered <- weighted_data %>% filter(qw_lmp<500) %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_lmp, data = .)))

reg_coeffs_filtered <- reg_filtered %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details, by = "duid")

fwrite(reg_coeffs_filtered, "Output/Regressions/Weighted Average/Filtered/weighted_ave_coeffs_filtered.csv")

#graphs

reg_coeffs_filtered %>% filter(beta>(-1)) %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+ 
  ggsave("Output/Regressions/Weighted Average/Filtered/weighted_ave_filtered_beta_region.png")

reg_coeffs_filtered %>% filter(beta>(-1)) %>% 
  ggplot(aes(x = beta, fill = fuel_type))+
  geom_histogram()+ 
  ggsave("Output/Regressions/Weighted Average/Filtered/weighted_ave_filtered_beta_fueltype.png")

reg_coeffs_filtered %>% 
  ggplot(aes(x = alpha, fill = region))+
  geom_histogram()


#coeffs merged
reg_coeffs %>% left_join(reg_coeffs_filtered %>% 
                           rename(alpha_filtered = alpha, beta_filtered = beta) %>% 
                           select(duid, alpha_filtered, beta_filtered), by = "duid") %>% 
  ggplot(aes(x = beta, y = beta_filtered))+
  geom_point()

reg_coeffs %>% left_join(reg_coeffs_filtered %>% 
                           rename(alpha_filtered = alpha, beta_filtered = beta) %>% 
                           select(duid, alpha_filtered, beta_filtered), by = "duid") %>% 
  mutate(dif = beta - beta_filtered) %>% 
  ggplot(aes(x = dif))+
  geom_histogram()
