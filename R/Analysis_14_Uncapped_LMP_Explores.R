# Uncapped LMP Exploration

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
library(zoo)
library(ggallin)
Sys.setenv(TZ='UTC')

#Load data
generator_details <- fread("D:/NEM_LMP/Data/Raw/generator_details_cleaned.csv") %>% 
  select(-c(loss_factor, emission_factor, participant)) %>% 
  filter(schedule_type != "Non-scheduled")

generator_details_AEMO <- fread("D:/Data/RAW/AEMO/Website/generators_and_loads.csv") %>% clean_names() %>% 
  distinct(duid, .keep_all = TRUE)

weighted_data_uncapped <- fread("D:/Data/Cleaned/Quantity Weighted/full_lmp_uncapped_quantity_weighted_cleaned.csv") %>% 
  mutate(settlementdate = ymd_hms(settlementdate))



# HIGH/LOW LMP INVESTIGATION
###################################
weighted_data_uncapped %>% arrange(-lmp) %>% head(10) %>% 
  left_join(generator_details %>% select(duid, region), by = "duid")

weighted_data_uncapped %>% arrange(-lmp) %>% tail(10) %>% 
  left_join(generator_details %>% select(duid, region), by = "duid")

#### 2019-12-30 15:25:00 POR01

weighted_data_uncapped %>% filter(settlementdate == "2019-12-30 15:25:00 UTC") %>% arrange(-abs(lmp))

weighted_data_uncapped %>% filter(duid == "POR01") %>% 
  ggplot(aes(x = qw_lmp, y = lmp))+
  geom_point() +
  facet_wrap(~duid)

#POR01 have 3 super high abs(LMP) in close proximity
weighted_data_uncapped %>% filter(duid == "POR01", abs(lmp)>1000000)


# 2019-01-24 18:10:00

weighted_data_uncapped %>% filter(settlementdate == "2019-01-24 18:10:00 UTC") %>% arrange(-abs(lmp)) %>% head(10) %>% 
  left_join(generator_details %>% select(duid, region), by = "duid")
  #some units hitting RRP cap but LMP<<0
  #unit snot in RRP cap have LMP>>0



# 2019-11-26 02:50:00

weighted_data_uncapped %>% filter(settlementdate == "2019-11-26 02:50:00 UTC") %>% 
  arrange(-abs(lmp)) %>% head(10) 
  #2 units LMP<<0


# HOW MANY OBS at extreme values?
##################################################

weighted_data_uncapped %>% count(lmp > 15000)
weighted_data_uncapped %>% count(lmp < (-1000))

weighted_data_uncapped %>% summarise(prop_below_neg10k = sum(lmp < (-10000))/n(),
                                     prop_below_neg1k = sum(lmp < (-1000))/n(),
                                     prop_above_15k = sum(lmp > (15000))/n())


lmp_distribution <- cut(weighted_data_uncapped$lmp, 
    breaks=c(-Inf, -100000, -10000, -1000, -100, 0, 100, 1000, 10000, 100000, Inf)) %>% table() %>% 
  data.frame() %>% 
  rename("lmp_range" = ".") %>% 
  mutate(Percent = 100*Freq/sum(Freq))

fwrite(lmp_distribution, "Output/Regressions/Uncapped Weighted Average/lmp_distribution.csv")

lmp_distribution_cap <- cut(weighted_data_uncapped$lmp, 
                        breaks=c(-Inf, -1000, 15000, Inf)) %>% table() %>% 
  data.frame() %>% 
  rename("lmp_range" = ".") %>% 
  mutate(Percent = 100*Freq/sum(Freq))

fwrite(lmp_distribution_cap, "Output/Regressions/Uncapped Weighted Average/lmp_distribution_cap.csv")

# WHat is distribution of QW_LMP. Need to only keep 1 duid as just repeats for each
weighted_data_uncapped %>% filter(duid == "AGLHAL") %>% count(qw_lmp > 15000)
weighted_data_uncapped %>% filter(duid == "AGLHAL") %>% count(qw_lmp < (-1000))

weighted_data_uncapped %>% filter(duid == "AGLHAL") %>% count(qw_lmp < (-100000))

weighted_data_uncapped %>% filter(duid == "AGLHAL") %>% 
  ggplot(aes(x = qw_lmp)) +
  geom_histogram()

cut(weighted_data_uncapped %>% filter(duid == "AGLHAL") %>% .[["qw_lmp"]], 
    breaks=c(-Inf, -100000, -10000, -1000, 0, 100, 1000, 10000, 100000, Inf)) %>% table()


