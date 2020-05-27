data_location <- "D:/NEM_LMP/Data/Raw"
Price <- fread(paste0(data_location, "/dispatchprice_24-01-2020.csv")) %>% clean_names() %>% 
  filter(intervention == 0) %>% #remove all intervention prices, they dont actually change anything. Just an indicator
  #for intervention dispatch quantity (which is definitely different)
  mutate(region = substr(regionid, 1, nchar(regionid)-1))%>% 
  mutate(settlementdate = ymd_hms(settlementdate))%>% 
  select(-regionid) %>% 
  group_by(interval = cut(settlementdate, breaks = "30 min"), region) %>% #add RRP30 
  mutate(rrp30 = mean(rrp)) %>% 
  as.data.frame()

LPA <- fread(paste0(data_location, "/dispatch_local_price_24-01-2020.csv")) %>% clean_names() %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  select(-locally_constrained)

head(LPA)
head(Price)

temp <- inner_join(LPA, Price, by = "settlementdate")
temp2 <- temp %>% mutate(lmp = rrp + local_price_adjustment)
temp2$lmp %>% max()
temp2$lmp %>% min()
temp2 %>% filter(settlementdate == "2019-06-04 08:00:00", duid == "GSTONE4")
