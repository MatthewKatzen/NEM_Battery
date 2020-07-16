#Analysis1
#buy low sell high
#alpha beta

#load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
library(xtable)
library(nlme)
Sys.setenv(TZ='UTC')

### Load data
full_data <- fread("D:/Battery/Data/full_lmp.csv") %>% mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(settlementdate, duid, lmp, rrp)

generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")

### Summary stats

summary_stats <- full_data %>% group_by(duid, fuel_type) %>% 
  summarise(mean_lmp = mean(lmp), mean_rrp = mean(rrp), mean_rrp_30 = mean(rrp30)) %>% 
  ungroup() %>% 
  mutate(mean_lmp_zscore = (mean_lmp - mean(mean_lmp))/sd(mean_lmp),
         mean_lmp_percentrank = percent_rank(mean_lmp))
  
fwrite(summary_stats, "D:/Battery/Data/summary_stats.csv")


latlon <- read.csv("https://services.aremi.data61.io/aemo/v6/csv/all", stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  select(station_name, region, duid, lat, lon, dispatch_type, classification) %>% 
  #filter(classification %in% c("Scheduled", "Semi-Scheduled")) %>% 
  mutate(duid = ifelse(duid == "ARFW1", "ARWF1", duid)) #duid mislabel

summary_stats_latlon <- summary_stats %>% left_join(latlon, by = "duid")

fwrite(summary_stats_latlon, "D:/Battery/Output/summary_stats_latlon.csv")


### Regressions

reg <- full_data %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ rrp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta)

reg_coeffs %>% pivot_longer(cols = c(alpha, beta)) %>% filter(name == "beta") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(~name) + 
  ggsave("Output/beta.png")

reg_coeffs %>% pivot_longer(cols = c(alpha, beta)) %>% filter(name == "alpha") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(~name) + 
  ggsave("Output/alpha.png")


reg_coeffs %>% left_join(generator_details, by = "duid") %>% 
  fwrite("Output/coeffs.csv")



