library(FedData)
FedData::pkg_test("sp")
FedData::pkg_test("raster")
FedData::pkg_test("dplyr")
FedData::pkg_test("readxl")
FedData::pkg_test("ggplot2")

# Suppress scientific notation
options(scipen=999)

# Load other useful functions
for(f in list.files("./src", pattern = ".R", full.names = T)){
  source(f)
}

# Create a local directory for the raw temperature data
unlink("../DATA/WEATHER_STATIONS", recursive = T, force = T)
dir.create("../DATA/WEATHER_STATIONS", showWarnings = F, recursive = T)

# Copy and load the temperature pendant data collected by Grant Coffey and the
# UNT team.
file.copy(from = list.files("/Volumes/users/Pueblo\ Farming\ Project/Temperature_Precipitation_GDDs/CSV_WEATHER_STATIONS_AND_TEMPERATURE_PENDANTS/", pattern = ".csv", full.names = T),
          to = normalizePath("../DATA/WEATHER_STATIONS/"),
          overwrite = T)

weather_stations <- list.files("../DATA/WEATHER_STATIONS/", pattern = ".csv", full.names = T) %>%
  lapply(FUN = function(f){
    out <- readr::read_csv(f) # Read in the data
    names(out) <- gsub(" c:1","",names(out))
    names(out) <- gsub(" c:2","",names(out))
    names(out) <- make.unique(names(out))
    out$Location <- paste0(strsplit(basename(f),"_")[[1]][1:2], collapse = "_") # Add location information
    names(out)[1] <- "Date_Time"
    # names(out) <- c("Date_Time", "Temp (°C)", "Intensity (Lux)", "Location") # Rename variables
    return(out)
  }) %>%
  dplyr::bind_rows() %>% # Merge all together
  dplyr::mutate(Date_Time = lubridate::mdy_hms(Date_Time, tz = "GMT")) %>% # Dates are GMT
  dplyr::mutate(Date_Time = lubridate::with_tz(Date_Time, tzone = "MST")) %>% # Set to MST
  dplyr::mutate(Date_Time = lubridate::round_date(Date_Time, unit = "day")) %>% # Round date to day
  dplyr::distinct() %>% # Get only unique rows
  dplyr::filter(lubridate::year(Date_Time) %in% 2009:2015) %>% # Only keep years 2009:2015
  dplyr::select(-`Intensity (Lux)`, -`Batt (V)`) %>% # Drop intensity and battery data
  dplyr::group_by(Location,Date_Time) %>% # Group by location and date
  dplyr::summarise(TMIN = min(`Temp (°C)`, na.rm = T),
                   TMAX = max(`Temp (°C)`, na.rm = T),
                   MEAN_Wind_Speed = mean(`Wind Speed (m/s)`),
                   MAX_Gust_Speed = max(`Gust Speed (m/s)`),
                   PRCP_IN = sum(`Rain (mm)`)
                   ) %>% # Calculate daily summaries
  dplyr::ungroup() %>%
  dplyr::mutate(FGDD = calc_gdd(tmin = TMIN, tmax = TMAX, t.base=10, t.cap=30, to_fahrenheit=T),
                TMAX = ((TMAX)*1.8 + 32),
                TMIN = ((TMIN)*1.8 + 32),
                PRCP_IN = ifelse(PRCP_IN<0,NA,PRCP_IN)*0.00393701)
  
weather_station_IDs <- readr::read_csv("/Volumes/users/Pueblo\ Farming\ Project/Temperature_Precipitation_GDDs/PFP_weather_stations.csv")
weather_station_IDs <- sp::SpatialPointsDataFrame(coords = weather_station_IDs[,c("Easting","Northing")], data = as.data.frame(weather_station_IDs[,c("ID","Abbreviation","Description")]), proj4string = CRS("+proj=utm +datum=NAD27 +zone=12"))
weather_station_IDs <- sp::spTransform(weather_station_IDs,CRS("+proj=longlat"))

# Only keep weather stations pertinent to the PFP
weather_stations <- weather_stations %>%
  dplyr::filter(Location %in% weather_station_IDs$ID)

# Fill missing dates
all_dates <- dplyr::data_frame(Date_Time = seq(min(weather_stations$Date_Time),max(weather_stations$Date_Time),by='days'))
weather_stations <- weather_stations %>%
  dplyr::group_by(Location) %>%
  dplyr::do(
    out <- dplyr::full_join(x = ., y = all_dates, by = "Date_Time") %>%
      dplyr::mutate(Location = unique(Location)[!is.na(unique(Location))])
    ) %>%
  dplyr::ungroup()

# Write Weather stations to a new CSV file
readr::write_csv(weather_stations, "./data/weather_stations.csv")

# Write station location data to a shapefile
unlink('./data/weather_stations')
rgdal::writeOGR(weather_station_IDs,
                dsn='./data/weather_stations',
                layer="weather_stations",
                driver="GeoJSON",
                overwrite=T)
file.rename('./data/weather_stations','./data/weather_stations.geojson')

weather_stations %>%
  dplyr::filter(Location == "CCAC_11") %>%
  ggplot2::ggplot(aes(x = Date_Time, y = TMIN)) +
  ggplot2::geom_line()

