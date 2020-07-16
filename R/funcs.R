
#not in 
`%notin%` = function(x,y) !(x %in% y)

#season
season <- function(datetime){
  case_when((month(datetime) %in% c(9:11)) ~ "Spring",
            (month(datetime) %in% c(12,1,2)) ~ "Summer",
            (month(datetime) %in% c(3:5)) ~ "Autumn",
            (month(datetime) %in% c(6:8)) ~ "Winter")
}

