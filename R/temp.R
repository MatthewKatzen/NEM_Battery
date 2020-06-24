# When do batteries start?
lmp_data %>% filter(duid %in% c("BALBG1", "BALBL1", "DALNTH01", "DALNTHL1", "GANNBG1", "GANNBL1", "HPRG1", "HPRL1", "LBBG1", "LBBL1"),
                    lmp!=rrp) %>% 
  group_by(duid) %>% summarise(min(settlementdate))

#checked nemsight, Lake Bonney only one commissioned in Oct 2019

#gens which aren't constrained
temp <- lmp_data %>% group_by(duid) %>% filter(all(lmp==rrp)) %>% summarise(n()) %>% 
  left_join(generator_details, by = "duid")

#Gens that are actually running (data from nemsight)
monthly_output <- fread("D:/Battery/Data/Nemsight/monthly_station_output2.csv") %>% 
  pivot_longer(-"Monthly") %>% 
  clean_names() %>% 
  rename(station = name)

some_output_stations <- monthly_output %>% group_by(station) %>% filter(any(value != 0)) %>% .[["station"]] %>% unique()

#which gens started/stopped in 2019?
monthly_output %>% group_by(station) %>% filter(any(value != 0) & any(value == 0)) %>% .[["station"]] %>% unique()

output_latlon_2 <- output_latlon %>% filter(station %in% some_output_stations)
