#Clean_2_MC
#
#Get vom+mc to calculate lmp_mc
#Get All MC for each duid from 2013-2019
# MC ($/Gj) = FuelPrice ($/GJ) * 3.6 (GJ/MWh) * 1/thermal efficiency (how much input needed for one output)


# Load packages
library(tidyverse)
library(tidyr)
library(lubridate)
library(data.table)
library(janitor)
library(readxl)
library(writexl)
library(padr)
Sys.setenv(TZ='UTC')

### Load Generator details
#############

generator_details <- fread("D:/NEM_LMP/Data/RAW/generator_details.csv") %>% clean_names()

pumped_hydro_stations <- generator_details %>% filter(type == "Load" & fuel_type == "Hydro") %>% select(station) %>% 
  unique() %>% .[['station']] #remove 

generator_details <- fread("D:/NEM_LMP/Data/RAW/generator_details.csv") %>% clean_names() %>% 
  select(duid, type, station, fuel_type, thermal_efficiency, schedule_type, region) %>% 
  mutate(region = case_when(region == "Queensland" ~ "QLD",
                            region == "New South Wales" ~ "NSW",
                            region == "Victoria" ~ "VIC",
                            region == "South Australia" ~ "SA",
                            region == "Tasmania" ~ "TAS")) %>% 
  filter(schedule_type != "Non-scheduled") %>% 
  mutate(fuel_type = case_when(((station %in% pumped_hydro_stations) & type == "Load") ~ 
                                 "Pumped Hydro (Pumping)", #deal with loads
                               ((station %in% pumped_hydro_stations) & type == "Gen") ~ 
                                 "Pumped Hydro (Release)",
                               (!(station %in% pumped_hydro_stations) & type == "Gen" & fuel_type == "Hydro") ~ 
                                 "Hydro",
                               (fuel_type == "Battery" & type == "Load") ~ "Battery (Charge)",
                               (fuel_type == "Battery" & type == "Gen") ~ "Battery (Discharge)",
                               TRUE ~ fuel_type))


#Add missing t.e for BARKIPS1, CALL_A_2	
generator_details %>% filter(thermal_efficiency > 0) %>% 
  group_by(fuel_type) %>% summarise(mean = mean(thermal_efficiency))

generator_details <- generator_details %>% mutate(thermal_efficiency = case_when((duid == "BARKIPS1") ~ 0.311,
                                                                                 (duid == "CALL_A_2") ~ 0.249, 
                                                                                 (duid == "ARWF1") ~ 0, #wind farm
                                                                                 TRUE ~ thermal_efficiency))
                             
fwrite(generator_details, "D:/NEM_LMP/Data/RAW/generator_details_cleaned_2.csv")



#### Black Coal
#### 

#https://www.indexmundi.com/commodities/?commodity=coal-australian&months=120&currency=aud

#create index using Newcastle/Port Kembla black coal
coal_index <- fread("D:/Thesis/Data/External/black_coal_index.csv") %>% clean_names() %>% 
  select(month, coal_price = price) %>% 
  mutate(month = dmy(month)) %>% filter(month >= ymd("2013/01/01")) %>% 
  mutate(change = (coal_price/lag(coal_price))) %>% 
  mutate(base = NA)

for (i in 84:1){
  if (i==84){
    coal_index$base <- 1
  }else{
    coal_index$base[i] <- coal_index$base[i+1]/coal_index$change[i+1]
  }
}

#get 2019 vals from esoo and ba\
ckdate 

black_coal_price <- fread("D:/Thesis/Data/External/black_coal_price.csv") %>% #Coal price from esoo
  mutate(station = ifelse(station %in% c("Callide B", "Callide C"), #rename Callide B/C
                          "Callide",
                          station)) %>% 
  unique() %>% #remove duplicate Callide
  right_join(generator_details %>% filter(fuel_type == "Black Coal"), 
             by = "station") %>% #add duid data
  mutate(black_coal_price = ifelse(is.na(black_coal_price), 
                                   mean(black_coal_price, na.rm = TRUE), 
                                   black_coal_price)) %>%  #if missing from esoo (i.e. decommissioned) then make it mean of others
  mutate(temp_row = 1) #fake column to help with merge


black_coal_mc <- coal_index %>% mutate(temp_row = 1) %>% 
  full_join(black_coal_price, by = "temp_row") %>% 
  select(-temp_row) %>% 
  mutate(black_coal_price_adjusted = black_coal_price * base) %>% 
  mutate(mc =  black_coal_price_adjusted * 3.6 * (1/thermal_efficiency))




#### Gas
#Taken from NEMSIGHT

gas_data <- fread("D:/Thesis/Data/External/gas_price.csv") %>% #taken from nemsight
  clean_names() %>% rename(vic_price = vic_gas_price) %>% 
  mutate(day = dmy(day)) %>% 
  data.frame() %>% fill(colnames(.)) %>%  #some missing days, just fill down
  mutate(temp_row = 1) #fake column to help with merge


gas_mc <- generator_details %>% filter(fuel_type == "Gas") %>% 
  mutate(temp_row = 1) %>% 
  full_join(gas_data, by = "temp_row") %>% 
  select(-temp_row) %>% 
  mutate(mc = case_when(region == "QLD" ~ bri_price/thermal_efficiency * 3.6,
                         region == "NSW" ~ syd_price/thermal_efficiency * 3.6,
                         region == "VIC" ~ vic_price/thermal_efficiency * 3.6,
                         region == "SA" ~ adl_price/thermal_efficiency * 3.6,
                         region == "TAS" ~ vic_price/thermal_efficiency * 3.6))


# Brown Coal
# ESOO 2019/20 = $0.64 /GJ 

gdp_data <- fread("D:/Thesis/Data/External/oecd_gdp.csv") %>% clean_names() %>% as.data.frame()

for (i in 28:1){
  if (i %in% c(27,28)){
    gdp_data$base <- 1 #assume no change in q4 2019
  }else{
    gdp_data$base[i] <- gdp_data$base[i+1]/(1+gdp_data$change[i]/100)
  }
}


brown_coal_data <- gdp_data %>% select(quarter, base) %>% 
  mutate(brown_coal_price = 0.64 * base) %>% 
  mutate(temp_row = 1)

brown_coal_mc <- generator_details %>% filter(fuel_type == "Brown Coal") %>% 
  mutate(temp_row = 1) %>% 
  full_join(brown_coal_data, by = "temp_row") %>% 
  select(-temp_row, -base) %>% 
  mutate(mc =  brown_coal_price * 3.6 * (1/thermal_efficiency)) 
  



# Liquid Fuel
#ESOO 19/20 = $37.71 /GJ 

liquid_fuel_data <- gdp_data %>% select(quarter, base) %>% 
  mutate(liquid_fuel_price = 37.71 * base) %>% 
  mutate(temp_row = 1)

liquid_fuel_mc <- generator_details %>% filter(fuel_type == "Liquid Fuel") %>% 
  mutate(temp_row = 1) %>% 
  full_join(liquid_fuel_data, by = "temp_row") %>% 
  select(-temp_row, -base) %>% 
  mutate(mc =  liquid_fuel_price * 3.6 * (1/thermal_efficiency))


#merge

full_mc <- gas_mc %>% select(duid, day, mc) %>% 
  rbind(black_coal_mc %>% 
          pad(by = "month", interval = "day", start_val = ymd("2013-01-01"), end_val = ymd("2019-12-31"), group = "duid") %>% 
          fill(names(.)) %>% 
          rename(day = month) %>% 
          select(duid, day, mc)) %>% 
  rbind(brown_coal_mc %>% mutate(quarter = dmy(quarter)) %>% 
          pad(by = "quarter", interval = "day", start_val = ymd("2013-01-01"), end_val = ymd("2019-12-31"), group = "duid") %>% 
          fill(names(.)) %>% 
          rename(day = quarter) %>% 
          select(duid, day, mc)) %>% 
  rbind(liquid_fuel_mc %>% mutate(quarter = dmy(quarter)) %>% 
          pad(by = "quarter", interval = "day", start_val = ymd("2013-01-01"), end_val = ymd("2019-12-31"), group = "duid") %>% 
          fill(names(.)) %>% 
          rename(day = quarter) %>% 
          select(duid, day, mc))

fwrite(full_mc,"D:/Battery/Data/MC/full_mc.csv")

### VOM

gdp_data <- fread("D:/Thesis/Data/External/oecd_gdp.csv") %>% clean_names() %>% as.data.frame()

for (i in 28:1){
  if (i %in% c(27,28)){
    gdp_data$base <- 1 #assume no change in q4 2019
  }else{
    gdp_data$base[i] <- gdp_data$base[i+1]/(1+gdp_data$change[i]/100)
  }
}



vom_data <- fread("D:/Thesis/Data/External/vom.csv") %>% 
  filter(!(station %in% c("Callide C", "Murray 2","Yabulu Steam Turbin", "Torrens Island B"))) %>% 
  mutate(station = case_when(station == "Callide B" ~ "Callide",
                             station == "Swanbank E GT" ~ "Swanbank",
                             station == "Barcaldine Power Station" ~ "Barcaldine",
                             station == "Beryl solar farm" ~ "Beryl Solar Farm",
                             station == "Bungala one solar farm"~ "Bungala One Solar Farm",
                             station == "Braemar" ~ "Braemar 1",
                             station == "Braemar 2 Power Station" ~ "Braemar 2",
                             station == "Catagunya / Liapootah / Wayatinah" ~ 
                               "Liapootah Wayatinah Catagunya Aggregate",
                             station == "Laverton North" ~ "Laverton",
                             station == "Lemonthyme / Wilmot" ~ "Lemonthyme",
                             station == "Loy Yang A Power Station" ~ "Loy Yang A",
                             (substr(station, nchar(station) - 1, nchar(station)) %in% c("GT", "Gt")) &
                               (station != "Hunter Valley GT") ~ 
                               substr(station, 1, nchar(station) - 3), #gas suffix
                             station == "Condamine A" ~ "Condamine",
                             station == "Hume Dam NSW" ~ "Hume Hydro",
                             station == "Hume Dam VIC" ~ "Hume",
                             station == "Murray 1" ~ "Murray",
                             station == "Oakey 1 Solar Farm" ~ "Oakey 1 Solar Farm",
                             station == "Oakey Power Station" ~ "Oakey",
                             station == "Port Stanvac 1" ~ "Pt Stanvac",
                             station == "Swanbank E" ~ "Swanbank",
                             station == "Yabulu PS" ~ "Yabulu",
                             station == "Torrens Island A" ~ "Torrens Island",
                             station == "Ballarat Energy Storage System" ~ "Ballarat Battery Energy Storage System",
                             station == "Hornsdale Power Reserve Unit 1" ~ "Hornsdale Power Reserve",
                             station == "Tamar Valley Combined Cycle" ~ "Tamar Valley",
                             station == "Yarwun Cogen" ~ "Yarwun",
                             TRUE ~ station)) %>% 
  right_join(generator_details, by = "station") %>% #want fuel_type for grouping
  group_by(fuel_type) %>% 
  mutate(vom = ifelse(is.na(vom),#remaining plants are just decommissioned and t/f not in esoo. Just fill with mean of fuel type
                      mean(vom, na.rm=TRUE),
                      vom)) %>% ungroup() %>% 
  select(duid, vom, fuel_type) %>% 
  mutate(temp_row = 1)

full_vom <- gdp_data %>% mutate(temp_row = 1) %>% 
  full_join(vom_data, by = "temp_row") %>% 
  select(-temp_row) %>% mutate(quarter = dmy(quarter)) %>% 
  pad(by = "quarter", interval = "day", start_val = ymd("2013-01-01"), end_val = ymd("2019-12-31"), group = "duid") %>% 
  fill(names(.)) %>% 
  rename(day = quarter) %>% 
  mutate(adjusted_vom = vom * base) %>% 
  select(duid, day, adjusted_vom)

fwrite(full_vom,"D:/Battery/Data/MC/full_vom.csv")

### Merge vom and mc

full_vom <- fread("D:/Battery/Data/MC/full_vom.csv")
full_mc <- fread("D:/Battery/Data/MC/full_mc.csv")


full_vom_mc <- full_join(full_vom, full_mc, by = c("duid", "day")) %>% 
  mutate(mc = ifelse(is.na(mc), 0, mc)) %>% 
  mutate(vom_mc = mc + adjusted_vom) %>% 
  select(duid, day, vom_mc)

fwrite(full_vom_mc, "D:/Battery/Data/MC/full_vom_mc.csv")



#plot of vom_mv

full_vom_mc %>% left_join(generator_details, by = "duid") %>%
  filter(day == "2019-12-31") %>% 
  ggplot(aes(x = vom_mc)) + 
  geom_histogram() +
  facet_wrap(~fuel_type)