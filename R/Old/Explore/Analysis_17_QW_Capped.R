#Analysis_17_QW_Capped

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

# LOAD DATA
weighted_data_capped <- fread("D:/Data/Cleaned/Quantity Weighted/full_lmp_uncapped_quantity_weighted_cleaned.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate))  %>% 
  mutate(lmp = case_when(lmp<(-1000) ~ (-1000), #re-cap
                               lmp>15000 ~ 15000, 
                               TRUE ~ lmp)) %>% 
  group_by(settlementdate) %>% 
  mutate(qw_lmp = sum(initialmw*lmp)/sum(initialmw), #recalc qw_lmp
         qw_rrp = sum(initialmw*rrp)/sum(initialmw))


generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)
# ANALSYIS
#####################################

reg <- weighted_data_capped %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_lmp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details_AEMO, by = "duid")


reg_coeffs %>% 
  ggplot(aes(x = beta, fill = technology_type_descriptor))+
  geom_histogram()+
  ggsave("Output/Regressions/Uncapped Weighted Average/Recapped/capped_weighted_ave_beta_fuel.png", width = 10)


reg_coeffs %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+
  ggsave("Output/Regressions/Uncapped Weighted Average/Recapped/capped_weighted_ave_beta_region.png", width = 10)


# INDIVIDUAL PLOTS
###########################################

top_beta <- reg_coeffs %>% arrange(-beta) %>% distinct(station_name, .keep_all = TRUE) %>% head(n=10)
bottom_beta <-reg_coeffs %>% arrange(beta) %>% distinct(station_name, .keep_all = TRUE) %>% head(n=10)  

weighted_data_capped %>% filter(duid %in% top_beta$duid) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)+
  labs(title = "Top 10 betas (beta = 2.84 to 3)") +
  ggsave("Output/Regressions/Uncapped Weighted Average/Recapped/top_10_capped_betas.png", width = 10)

weighted_data_capped %>% filter(duid %in% bottom_beta$duid) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)+
  labs(title = "Bottom 10 betas (beta = 0.002 to 0.08)")+
  ggsave("Output/Regressions/Uncapped Weighted Average/Recapped/bottom_10_capped_betas.png", width = 10)

# OUTLIER EXPLORE
#############################################
weighted_data_capped %>% filter(duid == "AGLHAL") %>% #just need 1 duid
  ggplot(aes(x = qw_lmp))+
  geom_histogram()


qw_lmp_distribution <- cut(weighted_data_capped %>% filter(duid == "AGLHAL") %>% .[["qw_lmp"]], 
                        breaks=c(-1000, -100, 0, 100, 1000, 15000)) %>% table() %>% 
  data.frame() %>% 
  rename("qw_lmp_range" = ".") %>% 
  mutate(Percent = 100*Freq/sum(Freq))

fwrite(qw_lmp_distribution, "Output/Regressions/Uncapped Weighted Average/Recapped/qw_lmp_distribution.csv")

# FILTER QW_LMP<1000
#################################################

reg <- weighted_data_capped %>% filter(qw_lmp<1000) %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_lmp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details_AEMO, by = "duid")


reg_coeffs %>% 
  ggplot(aes(x = beta, fill = technology_type_descriptor))+
  geom_histogram() +
  ggsave("Output/Regressions/Uncapped Weighted Average/Recapped/Filtered/capped_filtered_weighted_ave_beta_fuel.png.png", width = 10)

reg_coeffs %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+
  ggsave("Output/Regressions/Uncapped Weighted Average/Recapped/Filtered/capped_filtered_weighted_ave_beta_region.png.png", width = 10)



