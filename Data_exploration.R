library(arrow)
library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)

# Loading the files -
##netatmo22 <- open_dataset("./H2C/Data/Netatmo_ini/JJA22.parquet") #To avoid loading the whole file
netatmo22 <- read_parquet("./H2C/Data/Netatmo_ini/JJA22.parquet")
netatmo23 <- read_parquet("./H2C/Data/Netatmo_ini/JJA23.parquet")
netatmo24 <- read_parquet("./H2C/Data/Netatmo_ini/JJA24.parquet")

# Creating a function to format time columns of each dataset.
time_format <- function(df, datetime_column) {
  # Convert the datetime column to POSIXct (if not already)
  df[[datetime_column]] <- lubridate::ymd_hms(df[[datetime_column]])
  # Create the 'datetimeUTC_rounded' column (rounded to the nearest hour)
  df$datetimeUTC_rounded <- lubridate::round_date(df[[datetime_column]], unit = "hours")
  # Create the 'datetimeUTC_rounded_min' column (rounded to the nearest minute)
  df$datetimeUTC_rounded_min <- lubridate::round_date(df[[datetime_column]], unit = "minutes")
  # Return the modified dataframe
  return(df)
}
netatmo22 <- time_format(netatmo22, datetime_column = "datetimeUTC")
netatmo23 <- time_format(netatmo23, datetime_column = "datetimeUTC")
netatmo24 <- time_format(netatmo24, datetime_column = "datetimeUTC")

## YEARLY PLOTS FOR NUMBER OF MEASURES ##

# Count of measurements for each hour *(year to be updated)*
count_data <- netatmo24 %>%
  group_by(datetimeUTC_rounded) %>%
  summarise(count = n())

# Create a plot of the counts *(colour + title to be updated)*
ggplot(count_data, aes(x = datetimeUTC_rounded, y = count)) +
  geom_line(color="seagreen") + 
  labs(title = "Count of hourly measurements - 2024",
       x = "Rounded Date",
       y = "Count") +
  scale_x_datetime(breaks = "2 days") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", family = "Montserrat", size = 18))

## MEASUREMENTS PER MINUTES ##

# Subsetting the dataset for several hours, to observe the tendency of capture at minute level
date = as.POSIXct("2022-07-15 05:00:00", format = "%Y-%m-%d %H:%M:%S", tz= "Europe/Paris")
dates_seq <- seq(from = date, by = "hour", length.out = 24)
netatmo_extract <- netatmo22 %>% 
  subset(datetimeUTC_rounded %in% dates_seq)
#  subset(datetimeUTC_rounded %in% c(date, date+3600, date+7200, date+10800, date+14400, date+18000, date+21600, date+25200))

# Counting the number of stations per rounded minutes for each hour
count_data <- netatmo_extract %>%
  group_by(datetimeUTC_rounded, datetimeUTC_rounded_min) %>%
  summarise(count = n_distinct(id)) %>%
  ungroup()

# Extracting the minutes, and adding 60 to those round from the following hour (ex: 4h54=54, 5h03=63) for plotting purpose
count_data$minutes = count_data$datetimeUTC_rounded_min %>% minute()
count_data$minutes = ifelse(count_data$minutes < 30,
                            count_data$minutes + 60,
                            count_data$minutes)

# Plotting, to observe the dynamic of the measurements capture minute by minute 
ggplot(count_data, aes(x = minutes, y = count, color = as.factor(datetimeUTC_rounded))) +
  geom_line() +
  labs(title = "Count of measures captured for each hour",
       x = "Minutes",
       y = "Count") +
  geom_vline(xintercept = 60, linetype = "dotted", color = "red", linewidth = 1.5) + # Line to mark the hour on the plot
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold", family = "Montserrat", size = 16))

find_extremes <- function(df, temp_col = "temperature", humidity_col = "humidity", id_col = "id") {
    hottest <- df %>% 
      arrange(desc(.data[[temp_col]])) %>% 
      distinct(.data[[id_col]], .keep_all = TRUE) %>% 
      slice_head(n = 3) %>%
      pull(id)
    hot <- subset(df, id %in% hottest) %>%
      mutate(cat = "hottest")
  coldest <- df %>% 
    arrange(desc(.data[[temp_col]])) %>% 
    distinct(.data[[id_col]], .keep_all = TRUE) %>% 
    slice_tail(n = 3) %>% 
    pull(id)
  cold <- subset(df, id %in% coldest) %>%
    mutate(cat = "coldest")
  moistest <- df %>% 
    arrange(desc(.data[[humidity_col]])) %>% 
    distinct(.data[[id_col]], .keep_all = TRUE) %>% 
    slice_head(n = 3) %>% 
    pull(id)
  moist <- subset(df, id %in% moistest) %>%
    mutate(cat = "moistest")
  dryest <- df %>% 
    arrange(desc(.data[[humidity_col]])) %>% 
    distinct(.data[[id_col]], .keep_all = TRUE) %>% 
    slice_tail(n = 3) %>% 
    pull(id)
  dry <- subset(df, id %in% dryest) %>%
    mutate(cat = "dryest")
  extremes <- rbind(hot, cold, moist, dry)
  return(extremes)
}
extremes <- find_extremes(netatmo22)

# Plotting the "extreme stations" // facet_wrap ?
ggplot(extremes, aes(x = datetimeUTC_rounded, y = temperature, color = id)) +
  geom_line() +  # Plot the lines
  labs(title = "Hourly Temperature For Extreme Stations",
       x = "Date/Time",
       y = "Temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "bottom")+
  facet_wrap(~ cat, scales = "free_y") 

palette_cat <- leaflet::colorFactor(
  palette = c("red", "darkblue", "seagreen", "yellow"),
  domain = c("hottest", "coldest", "moistest", "dryest"),
  ordered = TRUE
)

# Create the leaflet map with the extreme stations
leaflet(extremes) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(
    lng = ~lng_moy, 
    lat = ~lat_moy,  
    color = "white",
    weight = 0.2,
    fillColor = ~palette_cat(cat),  
    radius = 6,  
    popup = ~paste("ID:", id, "<br>Category:", cat),  
    fillOpacity = 0.8
  ) %>%
  addLegend(
    position = "topright",  
    pal = palette_cat,  
    values = c("hottest", "coldest", "moistest", "dryest"),  
    title = "Category",  
    opacity = 1  
  ) %>%
  addScaleBar(position = "bottomright")


## MAPPING THE WHOLE DATASET ##

selected_ts <- netatmo22 %>% subset(
  datetimeUTC_rounded == "2022-07-15 03:00:00 CEST"
)

# Function to filter the data by removing outliers beyond the bounds
filtering_outliers <- function(df, field) {
  mean_value = mean(df[[field]], na.rm = TRUE)
  sd_value = sd(df[[field]], na.rm = TRUE)
  upper = mean_value + 3 * sd_value
  lower = mean_value - 3 * sd_value
  df_filtered <- df[df[[field]] > lower & df[[field]] < upper, ]
  return(df_filtered)
}
selected_ts <- filtering_outliers(selected_ts, "temperature") 

# Mapping the selected date
leaflet(selected_ts) %>%
  addTiles() %>%  
  addCircleMarkers(
    lng = ~lng_moy,  
    lat = ~lat_moy, 
    radius = 5, 
    color = ~colorFactor("YlOrRd", temperature)(temperature),  # Color by temperature
    #color = ~colorFactor("YlOrRd", round(temperature * 2) / 2)(round(temperature * 2) / 2),  #rounding to 0.5
    stroke = FALSE,  
    fillOpacity = 0.7,
    popup = ~paste("ID: ", id, "<br>Temperature: ", temperature, "°C", "<br>Humidity: ", humidity, "%") 
  ) %>%
  addLegend(
    position = "bottomright", 
    #pal = colorFactor("YlOrRd", selected_ts$temperature), 
    #values = selected_ts$temperature, 
    pal = colorFactor("YlOrRd", round(selected_ts$temperature * 2) / 2), 
    values = round(selected_ts$temperature * 2) / 2, 
    title = "Temperature (°C)", 
    opacity = 1
  )
