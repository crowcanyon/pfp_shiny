library(FedData)
FedData::pkg_test("sp")
FedData::pkg_test("raster")
FedData::pkg_test("dplyr")
FedData::pkg_test("magrittr")
FedData::pkg_test("readxl")
FedData::pkg_test("ggplot2")

# Suppress scientific notation
options(scipen=999)

# Load other useful functions
for(f in list.files("./src", pattern = ".R", full.names = T)){
  source(f)
}

# Create a local directory for the raw temperature data
# unlink("../DATA/WEATHER_STATIONS", recursive = T, force = T)
# dir.create("../DATA/WEATHER_STATIONS", showWarnings = F, recursive = T)

# Copy and load the temperature pendant data collected by Grant Coffey and the
# UNT team.
file.copy(from = list.files("/Volumes/crow-dfs/Pueblo\ Farming\ Project/WEATHER/CSV_WEATHER_STATIONS_AND_TEMPERATURE_PENDANTS/", pattern = ".csv", full.names = T),
          to = normalizePath("../DATA/WEATHER_STATIONS/"),
          overwrite = T)
file.copy(from = "/Volumes/crow-dfs/Pueblo\ Farming\ Project/WEATHER/PFP_weather_stations.csv",
          to = normalizePath("../DATA/"),
          overwrite = T)

weather_stations <- list.files("../DATA/WEATHER_STATIONS/", pattern = ".csv", full.names = T) %>%
  lapply(FUN = function(f){
    out <- readr::read_csv(f) # Read in the data
    names(out)[1] <- "DATE"
    names(out) <- gsub(" c:1","",names(out))
    names(out) <- gsub(" c:2","",names(out))
    names(out) <- gsub("_1","",names(out))
    
    test <- c(names(out)[-1] != names(out)[-length(names(out))], TRUE) # identify duplicates (these are soil monitors)
    test <- c(test[1],sapply(2:length(test), function(i){
      if(!test[i] | !test[i-1]) return(FALSE)
      return(TRUE)
    }))
    names(out)[!test] <- paste0("Soil ",names(out)[!test])
    
    names(out) <- make.unique(names(out))
    out$Location <- paste0(strsplit(basename(f),"_")[[1]][1:2], collapse = "_") # Add location information

    # names(out) <- c("Date_Time", "Temp (°C)", "Intensity (Lux)", "Location") # Rename variables
    return(out)
  }) %>%
  dplyr::bind_rows() %>% # Merge all together
  dplyr::mutate(DATE = lubridate::mdy_hms(DATE, tz = "GMT")) %>% # Dates are GMT
  dplyr::mutate(DATE = lubridate::with_tz(DATE, tzone = "MST")) %>% # Set to MST
  dplyr::distinct() %>% # Get only unique rows
  dplyr::mutate(DATE = lubridate::round_date(DATE, unit = "day")) %>% # Round date to day
  dplyr::filter(lubridate::year(DATE) %in% seasons) %>% # Only keep years 2009:2015
  dplyr::group_by(Location,DATE) %>% # Group by location and date
  dplyr::filter((n() %in% c(24,48))) %>%
  dplyr::summarise(TMIN_F = min(`Temp (°C)`, na.rm = T),
                   TMAX_F = max(`Temp (°C)`, na.rm = T),
                   PRCP_IN = sum(`Rain (mm)`)
                   ) %>% # Calculate daily summaries
  dplyr::ungroup() %>%
  dplyr::mutate(FGDD = calc_gdd(tmin = TMIN_F, tmax = TMAX_F, t.base=10, t.cap=30, to_fahrenheit=T),
                TMAX_F = ((TMAX_F)*1.8 + 32),
                TMIN_F = ((TMIN_F)*1.8 + 32),
                PRCP_IN = ifelse(PRCP_IN<0,NA,PRCP_IN)*0.00393701)

weather_station_IDs <- readr::read_csv("../DATA/PFP_weather_stations.csv")
weather_station_IDs <- sp::SpatialPointsDataFrame(coords = weather_station_IDs[,c("Easting","Northing")], data = as.data.frame(weather_station_IDs[,c("ID","Abbreviation","Description")]), proj4string = CRS("+proj=utm +datum=NAD27 +zone=12"))
weather_station_IDs <- sp::spTransform(weather_station_IDs,CRS("+proj=longlat"))

# Only keep weather stations pertinent to the PFP
weather_stations <- weather_stations %>%
  dplyr::filter(Location %in% weather_station_IDs$ID)
locs <- weather_station_IDs$Abbreviation[match(weather_stations$Location, weather_station_IDs$ID)]
weather_stations %<>% dplyr::mutate(Location = locs)

## (Down)load the weather station data for Cortez
cortez_weather <- c(FedData::get_ghcn_daily_station(ID="USC00051886", elements = c("TMIN","TMAX"), standardize = T, raw.dir = "../DATA/GHCN"),FedData::get_ghcn_daily_station(ID="USC00051886", elements = c("PRCP"), raw.dir = "../DATA/GHCN")) %>%
  FedData::station_to_data_frame() %>%
  dplyr::as_data_frame() %>%
  dplyr::filter(year(DATE) %in% seasons) %>%
  dplyr::mutate(DATE = lubridate::ymd(DATE),
                TMIN = zoo::na.approx(TMIN/10, na.rm = F),
                TMAX = zoo::na.approx(TMAX/10, na.rm = F),
                FGDD = calc_gdd(tmin = TMIN, tmax = TMAX, t.base=10, t.cap=30, to_fahrenheit=T),
                TMAX = ((TMAX)*1.8 + 32),
                TMIN = ((TMIN)*1.8 + 32),
                PRCP = PRCP*0.00393701) %>%
  dplyr::rename(TMAX_F = TMAX, TMIN_F = TMIN, PRCP_IN = PRCP)

readr::write_csv(cortez_weather,"./data/cortez_weather.csv")

cortez_weather %<>%
  dplyr::mutate(Location = "Cortez")

weather_stations %<>%
  dplyr::mutate(DATE = as_date(DATE)) %>%
  dplyr::bind_rows(cortez_weather)

# Fill missing dates
all_dates <- dplyr::data_frame(DATE = seq(min(weather_stations$DATE),max(weather_stations$DATE),by='days'))
weather_stations <- weather_stations %>%
  dplyr::group_by(Location) %>%
  dplyr::do(
    out <- dplyr::full_join(x = ., y = all_dates, by = "DATE") %>%
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
  dplyr::filter(Location == "CDG") %>%
  ggplot2::ggplot(aes(x = DATE, y = TMIN_F)) +
  ggplot2::geom_line() +
  ggplot2::ylim(-30,80)

