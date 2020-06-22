f <- function(x, pos) filter(x, gear == 3)
 
read_csv_chunked(readr_example("mtcars.csv"), DataFrameCallback$new(f), chunk_size = 5)

read.csv(readr_example("mtcars.csv")) %>% filter()


#
lmp_data <- fread("D:/Battery/Data/full_lmp.csv", select = c("settlementdate", "duid", "lmp", "rrp"), nrows = 100)

f <- function(x, pos) x %>% select(settlementdate, duid, lmp, rrp) %>% 
  mutate(settlementdate = ymd_hms(settlementdate)) %>% 
  mutate(day = ymd(floor_date(settlementdate, "day")))
lmp_data <- read_csv_chunked("D:/Battery/Data/full_lmp.csv", DataFrameCallback$new(f), chunk_size = 1000000)
