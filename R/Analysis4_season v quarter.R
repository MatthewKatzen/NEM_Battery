# check monthly profits when grouped by quarter and by season
dif_quarterly_seasonal <- seasonal_profits_2019 %>% 
  select(region, month, quarter, profit, charge, discharge) %>% 
  mutate(charge = as.character(charge), discharge = as.character(discharge)) %>% 
  rename(season = quarter, profit_seasonal = profit, charge_seasonal = charge, discharge_seasonal = discharge) %>% 
  left_join(quarterly_profits_2019 %>% 
              mutate(charge = as.character(charge), discharge = as.character(discharge)) %>% 
              rename(profit_quarterly = profit, charge_quarterly = charge, discharge_quarterly = discharge), 
            by = c("region", "month")) %>% 
  mutate(profit_dif = profit_seasonal - profit_quarterly)

fwrite(dif_quarterly_seasonal, "Output/2 Hour Charge/dif_quarterly_seasonal.csv")

#Clearly a few huge differences in SA and VIC
#Check monthly 2 hour means

monthly_means_2019 <- rrp_2019_data %>% group_by(region) %>% 
  mutate(rrp_2_hour = frollmean(rrp30, n = 4, fill = NA, align = "left")) %>% #mean of 2 hour interval starting at settlementdate
  mutate(time = settlementdate %>% strftime(format="%H:%M:%S") %>% 
           paste0("2018-01-01 ",.) %>% ymd_hms()) %>% #add time var
  group_by(region, month = month(settlementdate), time) %>% 
  summarise(mean_2_hour = mean(rrp_2_hour)) %>% 
  mutate(year = "2019")

monthly_means_2018 <- rrp_2018_data %>% group_by(region) %>% 
  mutate(rrp_2_hour = frollmean(rrp30, n = 4, fill = NA, align = "left")) %>% #mean of 2 hour interval starting at settlementdate
  mutate(time = settlementdate %>% strftime(format="%H:%M:%S") %>% 
           paste0("2018-01-01 ",.) %>% ymd_hms()) %>% #add time var
  group_by(region, month = month(settlementdate), time) %>% 
  summarise(mean_2_hour = mean(rrp_2_hour)) %>% 
  mutate(year = "2018")


monthly_mean <- monthly_means_2019 %>% 
  rbind(monthly_means_2018)

ggplot(monthly_mean, aes(x = time, y = mean_2_hour, colour = year))+
  geom_line()+
  facet_grid(region~month)+
  scale_x_datetime(date_labels = "%H") +
  labs(title = "Average Price over 2 consecutive hours")

temp <- monthly_mean %>% filter(region == "SA")

ggplot(temp, aes(x = time, colour = year, y = mean_2_hour))+
  geom_line() +
  facet_grid(~month)+
  scale_x_datetime(date_labels = "%H") +
  ggsave("Output/2 Hour Charge/mean_2_hour.png", width = 12)
  
