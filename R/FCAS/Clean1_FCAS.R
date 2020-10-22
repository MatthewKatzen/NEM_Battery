#Analysis_11_FCAS

#Calculate FCAS related reveneues
#note: initialmw rev is at 5 min rrp

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
Sys.setenv(TZ='UTC')

#################################################
### DOWNLOAD DATA


#dispatch
dispatch_fun <- function(yearmonth){
  external_data_location <- "D:/Battery/Data/NEMWEB/DISPATCHLOAD" #for big data
  year <- substr(yearmonth, 1, 4)
  month <- substr(yearmonth, 5, 6)
  url <- 0
  csv_name <- paste0(external_data_location, "/PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV")
  if(!file.exists(csv_name)){
    url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                  year, "_", month, 
                  "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHLOAD_",yearmonth,
                  "010000.zip")
    temp <- tempfile()
    download.file(url, temp, mode="wb", method = "curl")
    unzip(temp, paste0("PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV"),
          exdir = external_data_location)
  }
  if(url != 0){
    unlink(temp) #delete zip
  }    
}

map(paste0(2019,str_pad(c(1:12), 2, pad = "0")), ~dispatch_fun(.))

#price
price_fun <- function(yearmonth){
  external_data_location <- "D:/Battery/Data/NEMWEB/DISPATCHPRICE" #for big data
  year <- substr(yearmonth, 1, 4)
  month <- substr(yearmonth, 5, 6)
  url <- 0
  csv_name <- paste0(external_data_location, "/PUBLIC_DVD_DISPATCHPRICE_", yearmonth, "010000.CSV")
  if(!file.exists(csv_name)){
    url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_", 
                  year, "_", month, 
                  "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHPRICE_",yearmonth,
                  "010000.zip")
    temp <- tempfile()
    download.file(url, temp, mode="wb", method = "curl")
    unzip(temp, paste0("PUBLIC_DVD_DISPATCHPRICE_", yearmonth, "010000.CSV"),
          exdir = external_data_location)
  }
  if(url != 0){
    unlink(temp) #delete zip
  }    
}

map(paste0(2019,str_pad(c(1:12), 2, pad = "0")), ~price_fun(.))

########################################################
### MERGE FILES

#dispatch
f <- function(x, pos) x %>% clean_names() %>%  #only keep half hourly rows
  clean_names %>% 
  select(settlementdate, duid, intervention, 
         lower5min, lower60sec, lower6sec, 
         raise5min, raise60sec, raise6sec,
         lowerreg, raisereg) %>% 
  filter_at(vars(-settlementdate, -duid, -intervention), any_vars(. != 0)) #remove rows of 0 (due to initialmw=0)


dispatch <- map_dfr(list.files("D:/Battery/Data/NEMWEB/DISPATCHLOAD",full.names = TRUE)[1:12],
                    ~read_csv_chunked(.,DataFrameCallback$new(f), chunk_size = 2000000, skip=1)) 

fwrite(dispatch, "D:/Battery/Data/Cleaned/FCAS/DISPATCHLOAD - FCAS/dispatchload_fcas_2019_unfiltered.csv")
dispatch <- fread("D:/Battery/Data/Cleaned/FCAS/DISPATCHLOAD - FCAS/dispatchload_fcas_2019_unfiltered.csv") 

dispatch <- dispatch %>% 
  filter(intervention == 0) %>% #if intervention, keep it and remove int==0)
  select(-intervention) %>% 
  pivot_longer(cols = c(-settlementdate, -duid), #make long form
               names_to = "var") %>% 
  filter(value!=0)

fwrite(dispatch, "D:/Battery/Data/Cleaned/FCAS/DISPATCHLOAD - FCAS/dispatchload_fcas_2019_noint.csv")

#price
f <- function(x, pos) x %>% clean_names() %>%  #only keep half hourly rows
  clean_names %>% 
  select(settlementdate, regionid, intervention,  
         lower5minrrp, lower60secrrp, lower6secrrp, 
         raise5minrrp, raise60secrrp, raise6secrrp,
         lowerregrrp, raiseregrrp) 
  
price <- map_dfr(list.files("D:/Battery/Data/NEMWEB/DISPATCHPRICE",full.names = TRUE)[1:12],
                    ~read_csv_chunked(.,DataFrameCallback$new(f), chunk_size = 2000000, skip=1)) %>% 
  filter(intervention == 0) %>% #if intervention, keep it and remove int==0)
  select(-intervention) %>% 
  rename(region = regionid,
         lower5min = lower5minrrp, lower60sec = lower60secrrp, lower6sec = lower6secrrp, 
         raise5min = raise5minrrp, raise60sec = raise60secrrp, raise6sec = raise6secrrp,
         lowerreg = lowerregrrp, raisereg = raiseregrrp) %>% 
  pivot_longer(cols = c(-settlementdate, -region), #make long form
               names_to = "var", values_to = "price") %>% 
  mutate(region = case_when(region == "NSW1" ~ "NSW",
                            region == "QLD1" ~ "QLD",
                            region == "VIC1" ~ "VIC",
                            region == "SA1" ~ "SA",
                            region == "TAS1" ~ "TAS"))

fwrite(price, "D:/Battery/Data/Cleaned/FCAS/DISPATCHPRICE - FCAS/dispatchprice_fcas_2019_noint.csv") 


### MERGE DATAFRAMES
#########################################
dispatch <- fread("D:/Battery/Data/Cleaned/FCAS/DISPATCHLOAD - FCAS/dispatchload_fcas_2019_noint.csv")
price <- fread("D:/Battery/Data/Cleaned/FCAS/DISPATCHPRICE - FCAS/dispatchprice_fcas_2019_noint.csv") 
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) #keep non-scheduled as some cases(Portland aluminium smelter) can provide fcas


#merge dfs together
merged <- dispatch %>% left_join(generator_details %>% select(duid, station, region, fuel_type), by = "duid") %>% 
  left_join(price, by = c("settlementdate", "var", "region")) %>% 
  mutate(category = case_when(var %in% c("raisereg", "lowerreg") ~ "reg",
                              TRUE ~ "cont"))

fwrite(merged, "D:/Data/Cleaned/FCAS/Merged/fcas_merged_noint.csv")

### ANALYSIS
##########################################################

merged <- fread("D:/Data/Cleaned/FCAS/Merged/fcas_merged_noint.csv")

merged <- merged %>% 
  mutate(rev = value*price/12)

### revenue

#by cat
merged %>% 
  group_by(quarter = lubridate::quarter(settlementdate), category, fuel_type) %>% 
  summarise(total_rev = sum(rev))%>% 
  ggplot(aes(x = quarter, y = total_rev, fill = category))+
  geom_col()+
  facet_wrap(~fuel_type)

#by var
merged %>% 
  group_by(quarter = lubridate::quarter(settlementdate), var, fuel_type) %>% 
  summarise(total_rev = sum(rev)) %>% 
  ggplot(aes(x = quarter, y = total_rev, fill = var))+
  geom_col()+
  facet_wrap(~fuel_type)+
  ggsave("Output/FCAS/Revenue by Var & Fuel Type.png")

#battery only
merged %>% filter(fuel_type %in% c("Battery (Charge)", "Battery (Discharge)")) %>% 
  group_by(quarter = lubridate::quarter(settlementdate), category) %>% 
  summarise(total_rev = sum(rev)) %>% 
  ggplot(aes(x = quarter, y = total_rev, fill = category))+
  geom_col()+
  ggsave("Output/FCAS/Revenue Battery Only by Cat.png")

#histogram of fcas revs 
merged %>%  
  ggplot(aes(y = rev, group = fuel_type))+
  geom_boxplot()




### quantity

#subcat
merged %>% 
  mutate(subcat = case_when(var %in% c("raise6sec", "raise60sec", "raise5min")  ~ "cont_raise",
                                       var %in% c("lower6sec", "lower60sec", "lower5min")  ~ "cont_lower",
                            TRUE ~ "reg")) %>% 
  group_by(settlementdate, subcat) %>% 
  summarise(total_q = sum(value)) %>% 
  group_by(quarter = lubridate::quarter(settlementdate), subcat) %>% 
  summarise(mean_q = mean(total_q))
  
#cat
merged %>% 
  group_by(quarter = lubridate::quarter(settlementdate), category, fuel_type) %>% 
  summarise(total_q = sum(value))%>% 
  ggplot(aes(x = quarter, y = total_q, fill = category))+
  geom_col()+
  facet_wrap(~fuel_type)

#var
merged %>% 
  group_by(quarter = lubridate::quarter(settlementdate), var, fuel_type) %>% 
  summarise(total_q = sum(value))%>% 
  ggplot(aes(x = quarter, y = total_q, fill = var))+
  geom_col()+
  facet_wrap(~fuel_type)+
  ggsave("Output/FCAS/Quantity by Var & Fuel Type.png")


#battery var
merged %>% filter(fuel_type %in% c("Battery (Charge)", "Battery (Discharge)")) %>% 
  group_by(quarter = lubridate::quarter(settlementdate), var) %>% 
  summarise(total_q = sum(value)) %>% 
  ggplot(aes(x = quarter, y = total_q, fill = var))+
  geom_col()+
  ggsave("Output/FCAS/Quantity Battery Only by Var.png")

