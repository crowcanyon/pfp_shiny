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

# Copy and load the soil moisture data collected by Grant Coffey and the
# UNT team.
unlink("../DATA/UNT_MOISTURE_MONITORS/", recursive = T, force = T)
file.copy(from = normalizePath("/Volumes/crow-dfs/Pueblo\ Farming\ Project/WEATHER/UNT_MOISTURE_MONITORS/"),
          to = normalizePath("../DATA/"),
          overwrite = T,
          recursive = T)

# Read in soil moisture data for each garden
locs <- c("NW","NE","SE")
cdg <- lapply(locs, function(loc){
  lapply(
    grep(loc,
         list.files("../DATA/UNT_MOISTURE_MONITORS/Check_Dam_Garden",
                                               recursive = T,
                                               pattern = "xls",
                                               full.names = T),
         value = T),
    readxl::read_excel,
    col_names = c("Time","VWC_15","Temp_15","VWC_30","Temp_30","VWC_45","Temp_45","none"),
    col_types = c("date",rep("numeric",7)),
    skip = 3) %>%
    dplyr::bind_rows() %>%
    dplyr::select(-none) %>%
    dplyr::mutate(Location = loc, Garden = "CDG")
}) %>%
  dplyr::bind_rows()

mcg <- lapply(
  list.files("../DATA/UNT\ Moisture\ Monitors/Coffey_Garden",
             recursive = T,
             pattern = "xls",
             full.names = T),
  readxl::read_excel,
  col_names = c("Time","VWC_15","Temp_15","VWC_30","Temp_30","VWC_45","Temp_45","none"),
  col_types = c("date",rep("numeric",7)),
  skip = 3) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-none) %>%
  dplyr::mutate(Location = NA, Garden = "MCG")

plc <- lapply(
  list.files("../DATA/UNT\ Moisture\ Monitors/PLC_Garden",
             recursive = T,
             pattern = "xls",
             full.names = T),
  readxl::read_excel,
  col_names = c("Time","VWC_15","Temp_15","VWC_30","Temp_30","VWC_45","Temp_45","none"),
  col_types = c("date",rep("numeric",7)),
  skip = 3) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-none) %>%
  dplyr::mutate(Location = NA, Garden = "PLC")

# Combine all gardens, and find average of each day.
soil_moisture <- dplyr::bind_rows(cdg,mcg,plc) %>%
  dplyr::mutate(Time = lubridate::round_date(Time, unit = "day")) %>%
  # dplyr::filter(lubridate::year(Time) %in% 2009:2015) %>% # Only keep years 2009:2015
  dplyr::group_by(Garden,Location,Time) %>%
  dplyr::summarise_each("mean") %>%
  dplyr::mutate(Temp_15 = ((Temp_15)*1.8 + 32), Temp_30 = ((Temp_30)*1.8 + 32), Temp_45 = ((Temp_45)*1.8 + 32))

readr::write_csv(soil_moisture,"./data/soil_moisture.csv")

# soil_moisture %>%
#   dplyr::filter(Garden == "CDG") %>%
#   ggplot2::ggplot(aes(x = Time, y = 0)) +
#   ggplot2::geom_line()