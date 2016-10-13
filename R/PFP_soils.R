library(FedData)
FedData::pkg_test("sp")
FedData::pkg_test("raster")
FedData::pkg_test("dplyr")

# Suppress scientific notation
options(scipen=999)

# Load other useful functions
for(f in list.files("./src", pattern = ".R", full.names = T)){
  source(f)
}

## A study area in the CCAC/BCP/ICR
# ICR <- sp::spTransform(rgdal::readOGR("/Volumes/crow-dfs/Research/GIS/Basketmaker\ Communities\ Project/Basketmaker\ Communities\ CCAC.gdb","A_ICR_Outline"),CRS("+proj=longlat"))
# rgdal::writeOGR(ICR,dsn='../DATA/',layer="ICR",driver="ESRI Shapefile")
# CCAC <- rgdal::readOGR("/Volumes/users/Research/GIS/MontezumaCounty/parcels/","NewMontezumaParcels")
# CCAC <- CCAC[,"PID"]
# rgdal::writeOGR(CCAC,dsn='../DATA/',layer="MontezumaParcels",driver="ESRI Shapefile")
# CCAC <- rgdal::readOGR('../DATA/',"MontezumaParcels")
# 
# CCAC <- CCAC[CCAC$PID %in% c(561128100001,
#                              561128200042,
#                              561128200041,
#                              561128200047,
#                              561128200048,
#                              561121300038,
#                              561121300001,
#                              561121400001),]
# CCAC <- CCAC %>%
#   sp::spTransform(CRS("+proj=utm +zone=12 +datum=NAD83")) %>%
#   sp::spTransform(CRS("+proj=longlat"))
# rgdal::writeOGR(CCAC,dsn='../DATA/',layer="CCAC",driver="ESRI Shapefile")

ICR <- rgdal::readOGR('../DATA/',"ICR")
CCAC <- rgdal::readOGR('../DATA/',"CCAC")
CCAC_poly <- rgeos::gUnion(ICR,spTransform(CCAC,CRS(raster::projection(ICR))))
CCAC_poly <- rgeos::gUnaryUnion(CCAC_poly)
CCAC_poly <- SpatialPolygonsDataFrame(CCAC_poly, data=data.frame(ID="CCAC and ICR"))

polys <- !sapply(CCAC_poly@polygons[[1]]@Polygons,slot,name="hole")
CCAC_poly@polygons[[1]]@Polygons <- CCAC_poly@polygons[[1]]@Polygons[polys]
CCAC_poly@polygons[[1]]@plotOrder <- as.integer(1)
comment(slot(CCAC_poly, "polygons")[[1]]) <- "0"
CCAC_poly@polygons[[1]]@Polygons[[1]]@coords <- CCAC_poly@polygons[[1]]@Polygons[[1]]@coords[c(-1,-2, -3,-25,-28,-29,-30),]

unlink('./data/CCAC_poly')
rgdal::writeOGR(CCAC_poly,dsn='./data/CCAC_poly',layer="CCAC_poly",driver="GeoJSON")
file.rename('./data/CCAC_poly','./data/CCAC_poly.geojson')

CCAC_poly <- rgdal::readOGR(dsn = "./data/CCAC_poly.geojson", "OGRGeoJSON", verbose = FALSE)

## NRCS Soils in CCAC area ##
CCAC_SSURGO <- FedData::get_ssurgo(template=CCAC_poly, label="CCAC", raw.dir="../DATA/SSURGO/RAW/", extraction.dir="../DATA/SSURGO/EXTRACTIONS/", force.redo=F)

## Convert all tables to dplyr data_tables
CCAC_SSURGO$tabular <- lapply(CCAC_SSURGO$tabular,dplyr::as_data_frame)

CCAC_SSURGO <- CCAC_SSURGO %>%
  calculate_mukey_annual_npp() %>% # Calculate annual NPP
  calculate_mukey_bean_productivity() %>% # Calculate bean productivity
  calculate_mukey_awc() %>% # Calculate and output AWC
  calculate_mukey_hand_planting_restriction() # Calculate hand-planting restriction

soils <- CCAC_SSURGO$spatial
soils@data <- dplyr::left_join(soils@data,CCAC_SSURGO$tabular$mapunit %>%
                                 dplyr::select(mukey,muname) %>%
                                 dplyr::mutate(mukey = as.factor(mukey)),
                               by = c("MUKEY" = "mukey"))

soils$area <- rgeos::gArea(spTransform(soils,CRS("+proj=utm +datum=NAD83 +zone=12")),byid=T)

soils$Popup <- paste0("<b>",soils$muname,"</b><br/>",
                      "<p>Mapunit area: ",round(soils$area) * 0.0001," ha<br/>",
                      "Average annual NPP: ",round(soils$NPP * 1.12085)," kg/ha<br/>",
                      "Average bean yield: ",round(soils$bean_yield * 1.12085)," kg/ha<br/>",
                      "AWC, 0–6 inches: ",round(soils$upper_awc, digits=2)," inches<br/>",
                      "AWC, 6–60 inches: ",round(soils$lower_awc, digits=2)," inches<br/>",
                      "Hand-planting multiplier: ",round(soils$SCM_RED, digits=2),
                      "</p>")

soils@data <- soils@data %>%
  dplyr::select(-muname,-area,-AREASYMBOL,-SPATIALVER, -MUSYM) %>%
  dplyr::as_data_frame() %>%
  as.data.frame()

unlink('./data/soils')
rgdal::writeOGR(soils,dsn='./data/soils',layer="soils",driver="GeoJSON",overwrite=T)
file.rename('./data/soils','./data/soils.geojson')
