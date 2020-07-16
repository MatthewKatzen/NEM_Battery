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
  select(-c(loss_factor, emission_factor, participant))

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
  select(-day, -vom_mc)  #remove vars

lmp_data <- read_csv_chunked("D:/Battery/Data/full_lmp.csv", DataFrameCallback$new(f), chunk_size = 2000000)
 

fwrite(lmp_data, "D:/Battery/Data/full_lmp_mc_filtered.csv")

#remove Gens that aren't actually running (data from nemsight)
monthly_output <- fread("D:/Battery/Data/Nemsight/monthly_station_output2.csv") %>% 
  pivot_longer(-"Monthly") %>% 
  clean_names() %>% 
  rename(station = name) %>% 
  left_join(generator_details, by = "station") %>% #merge to get duids
  filter(schedule_type != "Non-scheduled") #remove non-scheduled gens

some_output_duid <- monthly_output %>% group_by(station) %>% filter(any(value != 0)) %>% .[["duid"]] %>% unique()

lmp_data_filtered <- lmp_data %>% filter(duid %in% some_output_duid) #filter only duids which actually generate

fwrite(lmp_data_filtered, "D:/Battery/Data/full_lmp_mc_filtered.csv")

# Read data again if needed
f <- function(x, pos) x %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) 

lmp_data_allduids <- read_csv_chunked("D:/Battery/Data/full_lmp_mc_filtered.csv", DataFrameCallback$new(f), chunk_size = 2000000)

### Regressions

# LMP ~ RRP
reg <- lmp_data_allduids %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp ~ rrp, data = .)))

reg_coeffs <- reg %>% 
  mutate(alpha = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha, beta)

reg_coeffs %>% pivot_longer(cols = beta) %>% filter(name == "beta") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(~name) + 
  ggsave("Output/Regressions/beta.png")

reg_coeffs %>% pivot_longer(cols = alpha) %>% filter(name == "alpha") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(~name) + 
  ggsave("Output/Regressions/alpha.png")


reg_coeffs %>% left_join(generator_details, by = "duid") %>% 
  fwrite("Output/Regressions/coeffs.csv")


# LMP_mc ~ RRP
reg_mc <- lmp_data_allduids %>% 
  group_by(duid) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(lmp_mc ~ rrp, data = .)))

reg_coeffs_mc <- reg_mc %>% 
  mutate(alpha_mc = model[[n()]] %>% summary() %>% coefficients() %>% .[1,1],
         beta_mc = model[[n()]] %>% summary() %>% coefficients() %>% .[2,1]) %>% 
  select(duid, alpha_mc, beta_mc)

reg_coeffs_mc %>% pivot_longer(cols = beta_mc) %>% filter(name == "beta_mc") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(~name) + 
  ggsave("Output/Regressions/beta_mc.png")

reg_coeffs_mc %>% pivot_longer(cols = alpha_mc) %>% filter(name == "alpha_mc") %>% 
  ggplot(aes(x = value))+
  geom_histogram()+
  facet_wrap(~name) + 
  ggsave("Output/Regressions/alpha_mc.png")


reg_coeffs_mc %>% left_join(generator_details, by = "duid") %>% 
  fwrite("Output/Regressions/coeffs_mc.csv")


## buy low, sell high with LMPmc

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

profit <- fread("Output/profit.csv")

latlon <- read.csv("https://services.aremi.data61.io/aemo/v6/csv/all", stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  select(station_name, region, duid, lat, lon, dispatch_type, classification) %>% 
  #filter(classification %in% c("Scheduled", "Semi-Scheduled")) %>% 
  mutate(duid = ifelse(duid == "ARFW1", "ARWF1", duid)) #duid mislabel


output_latlon <- profit %>% select(duid, station, fuel_type, region, profit_lmp, profit_lmp_mc, profit_rrp) %>% 
  left_join(reg_coeffs %>% select(duid, alpha_mc, beta_mc), by = "duid") %>% 
  left_join(latlon %>% select(duid, lat, lon), by = "duid") %>% 
  mutate_if(is.numeric, funs(`percentile` = ntile(.,100))) %>% 
  select(-lat_percentile, -lon_percentile, -profit_rrp_percentile)


#manually check which gens are commissioned in 2019

commissioned_stations <- c("Barker Inlet Power Station", "Beryl Solar Farm", "Childers Solar Farm", "Clermont Solar Farm", "Coopers Gap Wind Farm", "Finely Solar Farm", "Haughton Solar Farm Stage 1 Units 1-81",
                        "Lake Bonney BESS1", "Lilyvale Solar Farm", "Limondale Solar Farm 2", "Lincoln Gap Wind Farm", "Murra Warra Wind Farm",
                        "NEVERTIRE SOLAR FARM", "Numurkah Solar Farm", "Oakey 1 Solar Farm", "Oakey 2 Solar Farm", "Rugby Run Solar Farm", "Tailem Bend Solar Project 1",
                        "Yendon Wind Farm")

output_latlon <- output_latlon %>% mutate(commission_2019 = (station %in% commissioned_stations))

fwrite(output_latlon, "Output/output_latlon.csv")

output_latlon <- fread("Output/output_latlon.csv")
