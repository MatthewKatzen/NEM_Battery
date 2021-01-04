
#not in 
`%notin%` = function(x,y) !(x %in% y)

#season
season <- function(datetime){
  case_when((month(datetime) %in% c(9:11)) ~ "Spring",
            (month(datetime) %in% c(12,1,2)) ~ "Summer",
            (month(datetime) %in% c(3:5)) ~ "Autumn",
            (month(datetime) %in% c(6:8)) ~ "Winter")
}

nem_year <- function(datetime){
  year(datetime - minutes(5))
}
nem_month <- function(datetime){
  month(datetime - minutes(5))
}

#downlaod data 
#########################

download_scada <- function(yearmonth){
  external.data.location <- "D:/Data/RAW/AEMO/NEMWEB/UNIT_SCADA" 
  year <- substr(yearmonth, 1, 4)
  month <- substr(yearmonth, 5, 6)
  url <- 0 #initialise
  #check if already downloaded
  csv.name <- paste0(external.data.location,"/PUBLIC_DVD_DISPATCH_UNIT_SCADA_", yearmonth, 
                     "010000.CSV")
  if(!file.exists(csv.name)){
    url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                  year, "_",month, 
                  "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCH_UNIT_SCADA_",
                  yearmonth, "010000.zip")
    temp <- tempfile()
    download.file(url, temp, mode="wb", method = "curl") #download zip
    unzip(temp, paste0("PUBLIC_DVD_DISPATCH_UNIT_SCADA_", yearmonth, "010000.CSV"), 
          exdir = external.data.location) #unzip file and save csv to external storage
  }
  if(url != 0){
    unlink(temp) #delete zip
  }    
}

download_dispatchload <- function(yearmonth){
  external.data.location <- "D:/Data/RAW/AEMO/NEMWEB/DISPATCHLOAD" 
  year <- substr(yearmonth, 1, 4)
  month <- substr(yearmonth, 5, 6)
  url <- 0 #initialise
  #check if already downloaded
  csv.name <- paste0(external.data.location,"/PUBLIC_DVD_DISPATCHLOAD_", yearmonth, 
                     "010000.CSV")
  if(!file.exists(csv.name)){
    url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                  year, "_",month, 
                  "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHLOAD_",
                  yearmonth, "010000.zip")
    temp <- tempfile()
    download.file(url, temp, mode="wb", method = "curl") #download zip
    unzip(temp, paste0("PUBLIC_DVD_DISPATCHLOAD_", yearmonth, "010000.CSV"), 
          exdir = external.data.location) #unzip file and save csv to external storage
  }
  if(url != 0){
    unlink(temp) #delete zip
  }    
}

download_dispatchprice <- function(yearmonth){
  external.data.location <- "D:/Data/RAW/AEMO/NEMWEB/DISPATCHPRICE" 
  year <- substr(yearmonth, 1, 4)
  month <- substr(yearmonth, 5, 6)
  url <- 0 #initialise
  #check if already downloaded
  csv.name <- paste0(external.data.location,"/PUBLIC_DVD_DISPATCHPRICE_", yearmonth, 
                     "010000.CSV")
  if(!file.exists(csv.name)){
    url <- paste0("http://nemweb.com.au/Data_Archive/Wholesale_Electricity/MMSDM/", year,"/MMSDM_",
                  year, "_",month, 
                  "/MMSDM_Historical_Data_SQLLoader/DATA/PUBLIC_DVD_DISPATCHPRICE_",
                  yearmonth, "010000.zip")
    temp <- tempfile()
    download.file(url, temp, mode="wb", method = "curl") #download zip
    unzip(temp, paste0("PUBLIC_DVD_DISPATCHPRICE_", yearmonth, "010000.CSV"), 
          exdir = external.data.location) #unzip file and save csv to external storage
  }
  if(url != 0){
    unlink(temp) #delete zip
  }    
}


