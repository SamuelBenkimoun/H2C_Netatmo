#Radiative night selections

library(dplyr)
library(readr)
library(lubridate)

#Importing and binding the data
files_list <- list.files("../H2C/Meteo_France/", pattern = "\\.csv$", full.names = TRUE)
mfdata <- lapply(X = files_list, 
                 FUN = read_delim, delim = ";", escape_double = FALSE, trim_ws = TRUE)
mfdata <- do.call(rbind, mfdata)
head(mfdata)
table(mfdata$NOM_USUEL)
mfdata <- subset(mfdata, NOM_USUEL %in% c("ORLY", "TRAPPES", "ROISSY"))

# Adding date/time to later subset summer nights
mfdata$datetime <- lubridate::ymd_h(mfdata$AAAAMMJJHH)
mfdata$datetime_paris <- lubridate::with_tz(mfdata$datetime, tzone = "Europe/Paris")
mfdata$date <- as.Date(mfdata$datetime_paris)
mfdata$hour <- lubridate::hour(mfdata$datetime_paris)

# Keeping only the summer nights
nights <- mfdata %>%
  filter(datetime_paris >= as.Date("2022-06-01") & date <= as.Date("2024-08-31"))
# Midnight to 5:OO AM to qualify the night weather conditions without too much influence from the sunset/sunlight
nights <- subset(nights, hour >= 0 & hour <=5) 

nights <- nights %>%
  filter(
    lubridate::month(datetime_paris) >= 6,
    lubridate::month(datetime_paris) <= 8
  )

nights <- dplyr::select(nights, c("NOM_USUEL", "AAAAMMJJHH", "datetime_paris", "hour", "FF", "RR1", "N", "T")) #keeping only the indicators needed for the selection
nights <- nights %>%
  arrange(NOM_USUEL, AAAAMMJJHH) #Sorting by station and date/time
head(nights)

# Aggregating the values by night for: wind, cloud coverage and rain

nights$date <- lubridate::date(nights$datetime_paris) #Extracting the date of the night

nights_agg_wind <- aggregate(FF ~ NOM_USUEL + date, data = nights, FUN = function (x) round(mean(x), digits = 2)) %>% ## average of wind speed
  rename(FF_avg = FF)
nights_agg_wind_NA <- aggregate(FF ~ NOM_USUEL + date, data = nights, FUN = function (x) sum(is.na(x)), 
                                  na.action = na.pass) %>% ## important !! to maintain the NA in the sample
  rename(miss_FF = FF)

nights_agg_clouds <- aggregate(N ~ NOM_USUEL + date, data = nights, FUN = function (x) sum(x <= 2)) %>% #<=2 or <2 ?
  rename(low_N = N)
nights_agg_clouds_NA <- aggregate(N ~ NOM_USUEL + date, data = nights, FUN = function (x) sum(is.na(x)), 
          na.action = na.pass) %>%
  rename(miss_N = N)

nights_agg_rain <- aggregate(RR1 ~ NOM_USUEL + date, data = nights, FUN = function (x) sum(x == 0)) %>%
  rename(no_RR1 = RR1)
nights_agg_rain_NA <- aggregate(RR1 ~ NOM_USUEL + date, data = nights, FUN = function (x) sum(is.na(x)), 
                                  na.action = na.pass) %>%
  rename(miss_RR1 = RR1)

# Merging all the intermediary results

nights_merged <- merge(nights_agg_wind, nights_agg_wind_NA) %>%
  merge(nights_agg_rain, all = TRUE) %>%
  merge(nights_agg_rain_NA, all = TRUE) %>%
  merge(nights_agg_clouds, all = TRUE) %>%
  merge(nights_agg_clouds_NA, all = TRUE) 
nights_merged  
## thresholds 4/6 at least than 2 octats, and none above 5 ? => count (N>2) => Nmax => FFmean, RR = 0
## mean wind <= 2m/s
FF_avg <= 2 #2.5 ?
no_RR1 = 6
low_N = 4 #3low + 3NA ?

nights_merged$radiative <- ifelse(nights_merged$FF_avg < 2.5 
                                  & nights_merged$no_RR1 == 6
                                  & nights_merged$low_N >=4, 
                                  yes=TRUE, no=FALSE)
nights_merged$radiative <- ifelse(nights_merged$FF_avg < 2.5 
                                  & nights_merged$no_RR1 == 6 
                                  &nights_merged$low_N ==3 & nights_merged$miss_N ==3, 
                                  yes=TRUE, no= nights_merged$radiative)
table(nights_merged$radiative)
nights_merged %>% 
  dplyr::filter(radiative == TRUE) %>% 
  arrange(date)
