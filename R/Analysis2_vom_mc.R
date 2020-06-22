#Analysis 2, vom_mc
#Same as Analysis.R but using vom_mc data to get LMPmc

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

#Load data
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")

full_vom_mc <- fread("D:/Battery/Data/MC/full_vom_mc.csv") %>% 
  mutate(day = ymd(day)) %>% 
  filter(year(day) == 2019)


# merge in chunks
f <- function(x, pos) x %>% 
  select(settlementdate, duid, lmp, rrp) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  mutate(day = ymd(floor_date(settlementdate, "day"))) %>% 
  left_join(full_vom_mc, by = c("duid", "day")) %>% #merge lmp data with vom_mc
  mutate(lmp_mc = ifelse(lmp>vom_mc, lmp, vom_mc)) %>% #create LMPmc
  select(-day, -vom_mc) #remove vars

lmp_data <- read_csv_chunked("D:/Battery/Data/full_lmp.csv", DataFrameCallback$new(f), chunk_size = 2000000)
 

fwrite(lmp_data, "D:/Battery/Data/full_lmp_mc.csv")

# Read data again if needed
f <- function(x, pos) x %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) 

lmp_data <- read_csv_chunked("D:/Battery/Data/full_lmp_mc.csv", DataFrameCallback$new(f), chunk_size = 2000000)

### Regressions

reg <- lmp_data %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp_mc ~ rrp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha_mc = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta_mc = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha_mc, beta_mc)

reg_coeffs %>% pivot_longer(cols = beta_mc) %>% filter(name == "beta_mc") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(~name) + 
  ggsave("Output/beta_mc.png")

reg_coeffs %>% pivot_longer(cols = alpha_mc) %>% filter(name == "alpha_mc") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(~name) + 
  ggsave("Output/alpha_mc.png")


reg_coeffs %>% left_join(generator_details, by = "duid") %>% 
  fwrite("Output/coeffs.csv")

reg_coeffs <- fread("Output/coeffs.csv")

# buy low, sell high with LMPmc

profit <- lmp_data %>% 
  group_by(duid, date = floor_date(settlementdate, "day")) %>% 
  summarise(#dif_lmp = max(lmp) - min(lmp), #for each day, buy at min price sell at max
            #dif_lmp_mc = max(lmp_mc) - min(lmp_mc),
            #dif_rrp = max(rrp) - min(rrp),
            dif_lmp = 0.9*max(lmp) - min(lmp), #90% efficiency
            dif_lmp_mc = 0.9*max(lmp_mc) - min(lmp_mc),
            dif_rrp = 0.9*max(rrp) - min(rrp)) %>% 
  ungroup() %>% 
  group_by(duid) %>%
  summarise(#profit_lmp = sum(dif_lmp),
            #profit_lmp_mc = sum(dif_lmp_mc),
            #profit_rrp = sum(dif_rrp),
            profit_lmp = sum(dif_lmp),
            profit_lmp_mc = sum(dif_lmp_mc),
            profit_rrp = sum(dif_rrp)) %>% #calculate yearly profit
  left_join(generator_details, by = "duid") #add generator details


fwrite(profit, "Output/profit.csv")

# Latlons

profit <- fread("Output/profit_mc.csv")

latlon <- read.csv("https://services.aremi.data61.io/aemo/v6/csv/all", stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  select(station_name, region, duid, lat, lon, dispatch_type, classification) %>% 
  #filter(classification %in% c("Scheduled", "Semi-Scheduled")) %>% 
  mutate(duid = ifelse(duid == "ARFW1", "ARWF1", duid)) #duid mislabel


output_latlon <- reg_coeffs %>% ungroup() %>% 
  left_join(profit %>% select(duid, profit_lmp, profit_lmp_mc, profit_rrp), by = "duid") %>% 
  left_join(latlon %>% select(duid, lat, lon), by = "duid") %>% 
  mutate_if(is.numeric, funs(`percentile` = ntile(.,100))) %>% 
  select(-lat_percentile, -lon_percentile, -profit_rrp_percentile)

fwrite(output_latlon, "Output/output_latlon.csv")
