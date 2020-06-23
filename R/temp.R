### Dalrymple

lmp_data %>% filter(duid %in% c("DALNTHL1","DALNTH01"), lmp!=rrp) 
#S>NIL_HUWT_STBG2
#coefficients are +-1 opposite
#constraint is stopping all flow along line

#S>NIL_BWMP_HUWT
#2019-02-04 03:55:00
#+-1

lmp_data %>% filter(duid %in% c("HPRG1","HPRL1"), lmp!=rrp) %>% head()

lmp_data %>% filter(duid %in% c("HPRG1","HPRL1")) %>% 
  group_by(settlementdate) %>% filter(any(lmp!=rrp))

lmp_data %>% filter(duid %in% c("HPRG1","HPRL1")) %>% #both gen and load different to rrp
  group_by(settlementdate) %>% filter(all(lmp!=rrp))

#Check Nemsight for quantity of battery
gen_data <- fread("D:/NEM_LMP/Data/Raw/INITIALMW/2019-01.csv")

temp <- gen_data %>% filter(duid %in% c("DALNTHL1","DALNTH01")) 
temp2 <- lmp_data %>% filter(duid %in% c("DALNTHL1","DALNTH01"), lmp!=rrp) 

lmp_data %>% filter(duid %in% c("DALNTHL1","DALNTH01"), lmp!=rrp) %>% 
  group_by(settlementdate) %>% filter(all(lmp>0))

lmp_data %>% filter(duid %in% c("LBBG1","LBBL1"), lmp!=rrp) %>% 
  group_by(settlementdate) %>% filter(all(lmp>0), abs(lmp-rrp)>10)
