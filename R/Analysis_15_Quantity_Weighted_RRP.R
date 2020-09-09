#Quantity Weighted RRP

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
#################################
# LOAD DATA
weighted_data <- fread("D:/Data/Cleaned/Quantity Weighted/full_lmp_uncapped_quantity_weighted_cleaned.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate))  %>% 
  mutate(lmp = case_when(lmp<(-1000) ~ (-1000), #cap
                        lmp>15000 ~ 15000,
                        TRUE ~ lmp)) %>%
  group_by(settlementdate) %>% 
  mutate(qw_rrp = sum(initialmw*rrp)/sum(initialmw))


generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

# ANALSYIS
#####################################

# LMP ~ QW_RRP
###################################################

cut(weighted_data %>% filter(duid == "AGLHAL") %>% .[["qw_rrp"]], 
    breaks=c(-1000, -100, 0, 100, 1000, 15000)) %>% table() %>% 
  data.frame() %>% 
  rename("qw_rrp_range" = ".") %>% 
  mutate(Percent = 100*Freq/sum(Freq)) #0.12% qw_rrp>1000


reg <- weighted_data %>% filter(qw_rrp<1000) %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ qw_rrp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta) %>% 
  left_join(generator_details_AEMO, by = "duid")

reg_coeffs %>% 
  ggplot(aes(x = beta, fill = technology_type_descriptor))+
  geom_histogram()+
  ggsave("Output/Regressions/Uncapped Weighted Average/RRP/Capped and Filtered/beta_fueltype.png")

reg_coeffs %>% 
  ggplot(aes(x = beta, fill = region))+
  geom_histogram()+
  ggsave("Output/Regressions/Uncapped Weighted Average/RRP/Capped and Filtered/beta_region.png")


#are the low betas the Q_CS gens?

Q_CS <- c("BARCALDN", "LILYSF1", "BARRON-1", "BARRON-2", "CALL_B_1", "CALL_B_2", "CPP_3", "CPP_4", "DAYDSF1", "HAYMSF1", 
          "CLARESF1", "CSPVPS1", "YABULU2", "GSTONE1", "GSTONE2", "GSTONE3", "GSTONE4", "GSTONE5", "GSTONE6", "HAUGHT11", 
          'KAREEYA1', "KAREEYA2", "KAREEYA3", "KAREEYA4", "EMERASF1", "QLIS2M", "CLERMSF1", "MACKAYGT", "RUGBYR1", 
          "MSTUART1", "MSTUART2", "MSTUART3", "KSP1", "RRSF1", "QROW1K", "QROW2K", "HAMISF1",
          "WHITSF1", "STAN-1", "STAN-2", "STAN-3", "STAN-4", "YABULU", 'SMCSF1', "MEWF1")

reg_coeffs %>% filter(beta<0.5) %>% 
  mutate(Q_CS = (duid %in% Q_CS)) %>% select(duid, beta, Q_CS) %>% data.frame()

# RRP ~ QW_RRP
#########################################
reg <- weighted_data %>% select(settlementdate, region, rrp, qw_rrp) %>% distinct() %>% 
  group_by(region) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(rrp ~ qw_rrp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(region, alpha, beta)

weighted_data %>% select(settlementdate, region, rrp, qw_rrp) %>% distinct() %>% 
  ggplot(aes(x = qw_rrp, y = rrp))+
  geom_point() +
  facet_wrap(~region)+
  ggsave("Output/Regressions/Uncapped Weighted Average/RRP/Zone/rrp v qw_rrp.png")



reg <- weighted_data %>% filter(qw_rrp<1000) %>% select(settlementdate, region, rrp, qw_rrp) %>% distinct() %>% 
  group_by(region) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(rrp ~ qw_rrp, data = .)))

reg_coeffs_2 <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(region, alpha, beta)

weighted_data %>% filter(qw_rrp<1000) %>% select(settlementdate, region, rrp, qw_rrp) %>% distinct() %>% 
  ggplot(aes(x = qw_rrp, y = rrp))+
  geom_point() +
  facet_wrap(~region) +
  ggsave("Output/Regressions/Uncapped Weighted Average/RRP/Zone/rrp v qw_rrp filtered.png")

