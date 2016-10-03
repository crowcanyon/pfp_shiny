## This is the script for calculating the retrodicted maize paleoproductivity dataplanes.
## Previously, in PRISM.R, historical climate data (precipitation and mean temperature) 
## were downloaded from the PRISM database, and cropped and resampled to the study area.

## Here, those data are combined with the lower 54-inch soil moisture layer, PDSI 
## series are calculated for each pixel in the raster, and PDSI values are stored 
## as a new raster brick, "PDSI.brick".

## Author: R. Kyle Bocinsky
## Date: 3/8/2012

library(FedData)
FedData::pkg_test("Hmisc")
FedData::pkg_test("ggplot2")
FedData::pkg_test("lubridate")
FedData::pkg_test("readr")
FedData::pkg_test("zoo")
FedData::pkg_test("scales")
FedData::pkg_test("bocinsky/paleocar")
FedData::pkg_test("dplyr")
FedData::pkg_test("stringr")
FedData::pkg_test("sp")

# Suppress scientific notation
options(scipen=999)

# When to start the burn-in
burnin_start <- 1991

# What years to calculate PDSI norms over
pdsi_norm_years <- 1981:2010

# Load other useful functions
for(f in list.files("./src", pattern = ".R", full.names = T)){
  source(f)
}

## Download GHCN data
cortez_weather <- c(FedData::get_ghcn_daily_station(ID="USC00051886", elements = c("TMIN","TMAX"), standardize = T, raw.dir = "../DATA/GHCN"),FedData::get_ghcn_daily_station(ID="USC00051886", elements = c("PRCP"), raw.dir = "../DATA/GHCN"))

cortez_weather_monthly <- cortez_weather %>%
  station_to_data_frame() %>%
  dplyr::as_data_frame() %>%
  dplyr::filter(lubridate::year(DATE) %in% burnin_start:max(seasons), DATE < lubridate::floor_date(Sys.Date(), unit = "month")) %>%
  dplyr::mutate(DATE = lubridate::ymd(DATE), TMIN = zoo::na.approx(TMIN/10, na.rm = F), TMAX = zoo::na.approx(TMAX/10, na.rm = F), TMAX = ((TMAX)*1.8 + 32), TMIN = ((TMIN)*1.8 + 32), TAVG = (TMAX+TMIN)/2, PRCP = PRCP*0.00393701) %>%
  dplyr::rename(TAVG_F = TAVG, PRCP_IN = PRCP) %>%
  dplyr::mutate(MONTH = lubridate::month(DATE), YEAR = lubridate::year(DATE)) %>%
  dplyr::select(YEAR,MONTH,TAVG_F,PRCP_IN) %>%
  dplyr::group_by(YEAR,MONTH) %>%
  dplyr::summarise(TAVG_F = mean(TAVG_F), PRCP_IN = sum(PRCP_IN, na.rm = T))

# Calculate norms over 1981 -- 2010
cortez_weather_monthly_norms <- cortez_weather %>%
  station_to_data_frame() %>%
  dplyr::as_data_frame() %>%
  dplyr::filter(lubridate::year(DATE) %in% pdsi_norm_years) %>%
  dplyr::mutate(DATE = lubridate::ymd(DATE), TMIN = zoo::na.approx(TMIN/10), TMAX = zoo::na.approx(TMAX/10), TMAX = ((TMAX)*1.8 + 32), TMIN = ((TMIN)*1.8 + 32), TAVG = (TMAX+TMIN)/2, PRCP = PRCP*0.00393701) %>%
  dplyr::rename(TAVG_F = TAVG, PRCP_IN = PRCP) %>%
  dplyr::mutate(MONTH = lubridate::month(DATE), YEAR = lubridate::year(DATE)) %>%
  dplyr::select(YEAR,MONTH,TAVG_F,PRCP_IN) %>%
  dplyr::group_by(YEAR,MONTH) %>%
  dplyr::summarise(TAVG_F = mean(TAVG_F), PRCP_IN = sum(PRCP_IN, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::select(MONTH,TAVG_F,PRCP_IN) %>%
  dplyr::group_by(MONTH) %>%
  dplyr::summarise(TAVG_F = mean(TAVG_F), PRCP_IN = mean(PRCP_IN))

# Fill missing months with norms
cortez_weather_monthly <- cortez_weather_monthly %>% 
  dplyr::bind_rows(anti_join(cortez_weather_monthly_norms %>%
                               dplyr::full_join(expand.grid(YEAR = burnin_start:max(seasons), MONTH = 1:12) %>%
                                                  filter(lubridate::ymd(paste0(sprintf("%04d", YEAR),sprintf("%02d", MONTH),"01")) < lubridate::floor_date(Sys.Date(), unit = "month")),
                                                by = c("MONTH")),
                             cortez_weather_monthly, 
                             by=c("YEAR","MONTH"))) %>%
  dplyr::arrange(YEAR,MONTH) %>%
  dplyr::full_join(expand.grid(YEAR = burnin_start:max(seasons), MONTH = 1:12), by=c("YEAR","MONTH"))

## Read in the soils data
soils <- rgdal::readOGR(dsn = "./data/soils.geojson", "OGRGeoJSON", verbose = FALSE)
# Calculate the centroid of each soil and add to the data table
soils_data <- cbind(soils@data, as.data.frame(rgeos::gCentroid(soils, byid=T)))

## STEP 3: PRODUCE PDSI ESTIMATES FOR EACH SOIL
# Create an output directory for PDSI data
dir.create("../DATA/PDSI/", showWarnings = FALSE, recursive = TRUE)
output.dir <- "../DATA/PDSI/"
scPDSI.path <- "./src/scpdsi"

# Create a "blank" PDSI brick
# Calculate June Monthly PDSI for each soil
monthly_T <- data.frame(year=unique(cortez_weather_monthly$YEAR),matrix(paste0(" ",gsub("0000NA","",sprintf("%06.3f",cortez_weather_monthly$TAVG_F))),ncol=12,byrow=T), stringsAsFactors=F)
monthly_P <- data.frame(year=unique(cortez_weather_monthly$YEAR),matrix(paste0(" ",gsub("0000NA","",sprintf("%06.3f",cortez_weather_monthly$PRCP_IN))),ncol=12,byrow=T), stringsAsFactors=F)
mon_T_normal <- data.frame(matrix(paste0(" ",as.character(formatC(cortez_weather_monthly_norms$TAVG_F, format = 'f', digits = 3, width=6))),ncol=12,byrow=T), stringsAsFactors=F)
mon_T_normal[1,1] <- paste0(" ",mon_T_normal[1,1])
Monthly_PDSI <- lapply(1:nrow(soils_data),FUN = function(x){
  return(rPDSI(output.dir = output.dir, monthly_T = monthly_T, monthly_P = monthly_P, mon_T_normal = mon_T_normal, awc = soils_data[x,'lower_awc'], lat = soils_data[x,'y'], scPDSI.path = scPDSI.path))
}) %>%
  lapply(FUN = function(x){
    out <- dplyr::bind_cols(cortez_weather_monthly %>% dplyr::select(YEAR,MONTH),x) %>%
      dplyr::filter(YEAR %in% seasons, MONTH == 6) %>%
      dplyr::select(YEAR,PDSI) %>%
      collect %>%
      .[["PDSI"]]
    names(out) <- seasons
    return(out)
    })


## Reconstruction
## There are 5 components to the reconstruction back in time: 
# The measured historical maize yields collected by Burns
# The calculated June PDSI values for "Bean Soils"
# The MV Douglas Fir tree-ring chronology (proxying PDSI)
# The 1st Principle Component of the combined San Francisco Peaks and Almagre chronologies (proxying September mean temperature)

## Here, we use the calculated PDSI directly.
Burns.maize.bu.ac <- c(9, 12, 12,  4, 10,  9, 12, 12,  9, 12, 20, 15, 16, 12, 16, 18, 20, 13, 19, 16, 15, 15, 14, 14, 14, 18, 19, 16, 15, 16) # From Burns (1983: 310)
VEPI.bean.soils.PDSI <- c(0.751,3.963,-0.645,-3.020,1.884,-1.081,1.294,1.998,0.548,0.041,4.497,2.499,0.59,0.661,0.357,-2.357,1.020,2.021,3.567,-0.896,-3.765,1.890,-1.946,-1.564,-0.174,-1.54,1.819,1.884,-4.057,1.318) # From Kohler (2012: Table 6.5)
VEPI.calibration.data <- data.frame(YEAR = 1931:1960, PDSI = VEPI.bean.soils.PDSI, MAIZE = Burns.maize.bu.ac)

# Calculate the linear models
VEPI_LM <- lm(MAIZE ~ scale(YEAR) + PDSI, data=VEPI.calibration.data)
pdsi.coef <- coefficients(VEPI_LM)['PDSI']
intercept <- coefficients(VEPI_LM)['(Intercept)']

## ESTIMATE POTENTIAL MAIZE PRODUCTION FOR EACH YEAR, PER SOIL
# Perform the prediction
# Retrodiction equation (per cell): prediction ~ pdsi.coef*modern.PDSI + intercept
predictions <- lapply(Monthly_PDSI,function(x){
  (pdsi.coef*x + intercept) * 62.77 # Convert to kg/ha
})
predictions <- do.call(rbind,predictions)

## REWEIGHT PRODUCTION APPROPRIATELY FOR EACH SOIL CLUSTER
# Read in the NRCS normal year dry-weight soil productivity (NPP) & Bean Soils

# Mean Bean Soil NPP for VEP I soils
NPP.bean.mean <- 1093 * 1.12085 # Convert to kg/ha
NPP.reweight <- soils_data$NPP/NPP.bean.mean
predictions <- sweep(predictions,MARGIN=1,NPP.reweight,`*`)

## We depart a bit from the methods described in the VEPI FR. Before, the retrodicted values
## would be multiplied by the NPP.renorm values, then loaded into the simulation, where they 
## would be renormed to prehispanic maize varieties and further reduced by a cold correction 
## and a hand-planting reduction (steps 9, 10, and 11 in Chapter 6 of the VEPI final report).
## We perform all of those steps "up front" here.

## RENORM MAIZE PRODUCTION FOR PREHISPANIC VARIETIES AND CULTIVATION PRACTICES
# Set a renorm factor using the logic found in Chapter 6 of the VEPI FR.
renorm.factor <- 0.68 # From Kohler (2012: 101)
predictions <- predictions * 0.68

## MAKE HAND-PLANTING ADJUSTMENT
# Read in hand-planting factor raster produced in NRCS.R
predictions <- sweep(predictions,MARGIN=1,soils_data$SCM_RED,`*`)


## MAKE COLD CORRECTION
# We don't make the cold correction here, because CCAC PFP gardens are below 7,000 feet.

# Write yield info to soils shapefile
soils@data <- cbind(soils@data,predictions)
# 
# dir.create("./")
# plots <- apply(predictions,1,function(x){
#   out <- data_frame(`Simulated yield` = x,Year = as.integer(colnames(predictions))) %>%
#     plotly::plot_ly(
#       x = Year,
#       y = `Simulated yield`,
#       hoverinfo = "text",
#       text = paste(Year,"Yield:", round(`Simulated yield`),"(kg/ha)"),
#       width = 500, 
#       height = 300)
#   out <- plotly::as.widget(out)
#   return(htmltools::tagList(out))
# })
# 
# head <- "<head><meta charset=\"utf-8\"/><script src=\"scripts/htmlwidgets-0.6/htmlwidgets.js\"></script><script src=\"scripts/plotlyjs-1.6.1/plotly-latest.min.js\"></script><script src=\"scripts/plotly-binding-3.2.1/plotly.js\"></script></head>"    
# 
# soils@data$Popup <- paste0(head,soils@data$Popup,plots)

unlink('./data/soils_VEPII_yields')
rgdal::writeOGR(soils,dsn='./data/soils_VEPII_yields',layer="soils_VEPII_yields",driver="GeoJSON",overwrite=T)
file.rename('./data/soils_VEPII_yields','./data/soils_VEPII_yields.geojson')
