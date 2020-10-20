#visualise cluster
full_data %>% filter(duid %in% c("GSTONE1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red")+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.y = 500)+
  geom_abline(intercept = 125, slope = 1, linetype ="dashed", color = "black", size = 1.5)

full_data %>% filter(duid %in% c("STWF1")) %>%
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  mutate(left_cluster = lmp > 125 + qw_lmp) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  geom_point(aes(colour = left_cluster))+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.y = 500)

full_data %>% filter(duid %in% c("AGLSOM")) %>%
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  mutate(left_cluster = lmp > 125 + qw_lmp) %>% 
  ggplot(aes(x = qw_lmp, y = lmp, colour = left_cluster))+
  facet_wrap(~duid)+
  geom_point()+
  geom_smooth(method='lm', formula= y~x, aes(colour=NULL))+
  stat_regline_equation(label.y = 700, aes(colour=NULL))+
  geom_smooth(data = . %>% filter(!left_cluster),
              method='lm', formula= y~x)+
  stat_regline_equation(data = . %>% filter(!left_cluster), label.y = 500)+
  geom_smooth(data = . %>% filter(left_cluster),
              method='lm', formula= y~x)+
  stat_regline_equation(data = . %>% filter(left_cluster), label.y = 600)



 #how many are in cluster?
full_data %>% filter(duid %in% c("BOCORWF1")) %>%
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  mutate(left_cluster = lmp > 125 + qw_lmp) %>% ungroup() %>% 
  count(left_cluster) %>% 
  mutate(prop = n/sum(n))
  #96% in right cluster

#Left Cluster dataset
cluster <- full_data %>% filter(duid %in% c("BOCORWF1")) %>%
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  mutate(left_cluster = lmp > 125 + qw_lmp) %>% ungroup() %>% 
  select(settlementdate, left_cluster)

#check a high beta gen
full_data %>% filter(duid %in% c("TORRA1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red")

full_data %>% filter(duid %in% c("MEWF1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  left_join(cluster, by = "settlementdate") %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  geom_point(aes(colour = left_cluster))

  
# investigate cluster
cluster %>% 
  group_by(day = floor_date(settlementdate, "day")) %>% 
  summarise(daily = sum(left_cluster)) %>% 
  ggplot(aes(x = day, y = daily))+
  geom_line()

# TYPE 2 C2
#############################
full_data %>% filter(duid %in% c("BLUFF1")) %>% 
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  facet_wrap(~duid)+
  stat_binhex(bins = 100) + 
  scale_fill_gradient(name = "count", trans = "log", low = "blue", high = "red")+
  geom_smooth(method='lm', formula= y~x)+
  stat_regline_equation(label.y = 500)+
  geom_abline(intercept = 125, slope = 2, linetype ="dashed", color = "black", size = 1.5)

cluster_2 <- full_data %>% filter(duid %in% c("BLUFF1")) %>%
  filter(abs(qw_lmp)<1000, abs(lmp)<1000) %>% 
  mutate(bottom_cluster = (lmp > 125 + 2*qw_lmp)) %>% ungroup() %>% 
  select(settlementdate, bottom_cluster)


# Plot all duids, but only obs within cluster type1c2
temp <- full_data %>% filter(settlementdate %in% (cluster %>% filter(left_cluster) %>% .[["settlementdate"]]))
  
temp %>% ggplot(aes(x = qw_lmp, y = lmp, colour = duid)) +
  geom_point()+
  theme(legend.position = "none")

temp2 <- temp %>% group_by(duid) %>% summarise(n = sum(lmp<(-500))) %>% arrange(-n) %>% 
  left_join(reg_coeffs, by = "duid")

