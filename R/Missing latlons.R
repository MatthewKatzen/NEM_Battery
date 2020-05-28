Latlon <- read.csv("https://services.aremi.data61.io/aemo/v6/csv/all", stringsAsFactors = FALSE) %>% 
  clean_names() %>% 
  select(station_name, region, duid, lat, lon, dispatch_type, classification, fuel_source_descriptor) %>% 
  #filter(classification %in% c("Scheduled", "Semi-Scheduled")) %>% 
  mutate(duid = ifelse(duid == "ARFW1", "ARWF1", duid)) #duid mislabel

data_location <- "D:/NEM_LMP/Data/Raw"
Adjustment <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(-locally_constrained) %>% 
  filter(year(settlementdate)==2019) #some latlons missing, probably bc they are decomissioned

#some duids in adjustment don't have latlon
Generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv")
missing <- Adjustment$duid %>% unique() %>% .[Adjustment$duid %>% unique() %notin% Latlon$duid]

Generator_details %>% filter(duid %in% missing)

Adjustment %>% filter(duid == missing[1]) %>% head()