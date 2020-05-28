#Analysis 

#load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
library(xtable)
Sys.setenv(TZ='UTC')

# Load data
full_data <- fread("D:/Battery/Data/merged_data.csv") %>% mutate(settlementdate = ymd_hms(settlementdate)) %>% select(-local_price_adjustment)

#Mean LMP

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



