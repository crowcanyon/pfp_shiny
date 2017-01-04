library(FedData)
FedData::pkg_test("Hmisc")
FedData::pkg_test("ggplot2")
FedData::pkg_test("dtplyr")
FedData::pkg_test("lubridate")
FedData::pkg_test("readr")
FedData::pkg_test("sp")
FedData::pkg_test("zoo")
FedData::pkg_test("scales")
FedData::pkg_test("bocinsky/paleocar")

# Suppress scientific notation
options(scipen=999)

## Data munging part of the script. Run once to output clean CSVs

## Read in the PFP data directly from the PFP results database
unlink("../DATA/Pueblo Farmers Project database.mdb", recursive = TRUE, force = TRUE)
file.copy(from = "/Volumes/crow-dfs/Pueblo Farming Project/DATA/Pueblo Farmers Project database.mdb",
          to="../DATA/Pueblo Farmers Project database.mdb",
          overwrite = TRUE)
# file.copy(from = "/Volumes/Crow-DFS/Pueblo Farming Project/Documentation Forms and Data/Pueblo Farmers Project database.mdb",
#           to="../DATA/Pueblo Farmers Project database.mdb",
#           overwrite=T)
PFP_data <- Hmisc::mdb.get("../DATA/Pueblo\\ Farmers\\ Project\\ database.mdb") %>%
  lapply(FUN=dplyr::as_data_frame)

# Read in the garden table, and export a csv
gardens <- PFP_data$`tbl garden` %>%
  dplyr::select(Season,Garden,Variety,Clumps,PlantingDate,HarvestDate,UTMEast,UTMNorth, Spacing, Area, Comments) %>%
  dplyr::mutate(PlantingDate = lubridate::mdy(PlantingDate)) %>%
  dplyr::arrange(Garden,Season) %>%
  dplyr::filter(Season %in% seasons) # Only keep the right seasons

readr::write_csv(gardens,"./data/gardens.csv")

# Read in the garden data
gardens <- readr::read_csv("./data/gardens.csv")

# Read in the growth table
PFP_data$`tbl growth`$Date <- PFP_data$`tbl growth`$Date %>%
  mdy_hms

# A function to make a logical column monotonic
make_mono <- function(x){
  return(as.logical(stats::filter(x,filter=1, method="recursive")))
}

# Fill in the missing clump observations with NAs 
for(g in unique(PFP_data$`tbl growth`$Garden)){
  PFP_data_growth <- PFP_data$`tbl growth` %>%
    dplyr::select(Date,Garden,Clump) %>%
    dplyr::arrange(Date) %>%
    dplyr::filter(Garden == g) %>%
    dplyr::mutate(Season = year(Date))
  
  for(y in unique(PFP_data_growth$Season)){
    if(!(y %in% seasons)) next
    
    sub <- PFP_data_growth %>%
      dplyr::filter(Season == y) %>%
      dplyr::select(-Season)
    
    this.gardens <- gardens %>%
      dplyr::filter(Garden == g, Season == y)
    
    vals <- expand.grid(Date = sort(c((this.gardens %>%
                                         dplyr::select(PlantingDate))[[1]],
                                      lubridate::as_date(unique(sub$Date)))),
                        Garden = g,
                        Clump = 1:(this.gardens %>%
                               dplyr::select(Clumps) %>%
                               unlist() %>%
                               as.numeric())
    )
    
    PFP_data$`tbl growth` <- merge(PFP_data$`tbl growth`, vals, all=T)
  }
}

PFP_data$`tbl growth` <- dplyr::tbl_df(PFP_data$`tbl growth`)

# Recode variables and make them monotonic within groups
growth <- PFP_data$`tbl growth` %>%
  dplyr::filter(Removed != 1) %>%
  dplyr::rename(`Early Tassel Development` = ETD,
                `Tassel Development` = TD,
                `Tasseling` = `T`,
                `Silk Development` = SD,
                `Silking` = S,
                `Ear Development` = ED) %>%
  # dplyr::select(Date,Garden,Clump,`Early Tassel Development`,`Tassel Development`,`Tasseling`,`Silk Development`,`Silking`,`Ear Development`) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(`Early Tassel Development` = as.logical(`Early Tassel Development`),
                `Tassel Development` = as.logical(`Tassel Development`),
                `Tasseling` = as.logical(`Tasseling`),
                `Silk Development` = as.logical(`Silk Development`),
                `Silking` = as.logical(`Silking`),
                `Ear Development` = as.logical(`Ear Development`)) %>%
  dplyr::mutate(`Early Tassel Development` = ifelse(is.na(`Early Tassel Development`),FALSE,`Early Tassel Development`),
                `Tassel Development` = ifelse(is.na(`Tassel Development`),FALSE,`Tassel Development`),
                `Tasseling` = ifelse(is.na(`Tasseling`),FALSE,`Tasseling`),
                `Silk Development` = ifelse(is.na(`Silk Development`),FALSE,`Silk Development`),
                `Silking` = ifelse(is.na(`Silking`),FALSE,`Silking`),
                `Ear Development` = ifelse(is.na(`Ear Development`),FALSE,`Ear Development`)) %>%
  dplyr::mutate(`Silking` = ifelse(`Ear Development`,TRUE,`Silking`),
                `Silk Development` = ifelse(`Silking`,TRUE,`Silk Development`),
                `Tasseling`=ifelse(`Silk Development`,TRUE,`Tasseling`),
                `Tassel Development` = ifelse(`Tasseling`,TRUE,`Tassel Development`),
                `Early Tassel Development` = ifelse(`Tassel Development`,TRUE,`Early Tassel Development`)) %>%
  dplyr::mutate(Season = year(Date)) %>%
  dplyr::filter(Season %in% seasons) %>% # Only keep years 2009:2015
  dplyr::group_by(Season, Garden, Clump) %>% 
  dplyr::mutate(`Early Tassel Development` = make_mono(`Early Tassel Development`),
                `Tassel Development` = make_mono(`Tassel Development`),
                `Tasseling` = make_mono(`Tasseling`),
                `Silk Development` = make_mono(`Silk Development`),
                `Silking` = make_mono(`Silking`),
                `Ear Development` = make_mono(`Ear Development`)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(y = (gardens %>% dplyr::select(Season,Garden,Variety)), by = c("Season","Garden"))

# Write a csv of the growth table
readr::write_csv(growth,"./data/growth.csv")

# Summarize growth data into proportions of clumps to reach developmental stages
growth_summaries <- growth %>%
  dplyr::group_by(Date, Garden) %>%
  dplyr::summarise(`Early Tassel Development` = mean(`Early Tassel Development`),
                   `Tassel Development` = mean(`Tassel Development`),
                   `Tasseling` = mean(`Tasseling`),
                   `Silk Development` = mean(`Silk Development`),
                   `Silking` = mean(`Silking`),
                   `Ear Development` = mean(`Ear Development`)) %>%
  dplyr::mutate(Season = year(Date)) %>%
  dplyr::filter(Season %in% seasons) %>% # Only keep years 2009:2015
  dplyr::left_join(y = (gardens %>% dplyr::select(Season,Garden,Variety)), by = c("Season","Garden")) %>%
  dplyr::mutate(Variety = as.factor(Variety))

garden_years <- growth_summaries %>%
  dplyr::ungroup() %>%
  dplyr::select(Date,Garden) %>%
  dplyr::mutate(Date = year(Date)) %>%
  dplyr::filter(Date %in% seasons) %>% # Only keep years 2009:2015
  unique()


ranges <- function(x){
  x <- sort(x)
  breaks <- which(diff(x) > 1)
  starts <- c(1,breaks+1)
  ends <- c(breaks,length(x))
  singles <- base::intersect(starts,ends)
  starts <- base::setdiff(starts,singles)
  ends <- base::setdiff(ends,singles)
  paste(sort(c(as.character(x[singles]),paste(x[starts],x[ends],sep='–'))), collapse=', ')
}

garden_locations <- lapply(list.files("../DATA/Gardens/"), function(f){
  out <- rgdal::readOGR(dsn=paste0("../DATA/Gardens/",f),layer=f)
  out <- spChFIDs(out, as.character(out$Name))
}) %>%
  do.call(what=rbind)
garden_locations <- garden_locations[,c("Name")]
garden_locations$Abbreviation <- garden_locations$Name %>%
  gsub(pattern = "(.+) \\(",replacement = "") %>%
  gsub(pattern = "\\)",replacement = "") 
garden_locations$Area <- rgeos::gArea(spTransform(garden_locations,CRS("+proj=utm +datum=NAD83 +zone=12")),byid=T)
garden_locations$Years_planted <- sapply(split(garden_years$Date,garden_years$Garden)[garden_locations$Abbreviation],ranges)
garden_locations$Popup <- paste0("<b>",garden_locations$Name,"</b><br/>",
                                 "<p>Years planted: ",garden_locations$Years_planted,"<br/>",
                                 "Garden area: ",round(garden_locations$Area)," m<sup>2</sup></p>")
unlink('./data/gardens')
rgdal::writeOGR(garden_locations,dsn='./data/gardens',layer="gardens",driver="GeoJSON",overwrite=T)
file.rename('./data/gardens','./data/gardens.geojson')

gardens <- readr::read_csv("./data/gardens.csv")

weather_stations <- readr::read_csv("./data/weather_stations.csv") %>%
  dplyr::mutate(DATE = lubridate::round_date(DATE,"day"))
weather_station_IDs <- rgdal::readOGR(dsn = "./data/weather_stations.geojson", "OGRGeoJSON", verbose = FALSE, stringsAsFactors = F)@data %>%
  as_data_frame()

growth_summaries$Acc_FGDD <- NA

for(g in unique(growth_summaries$Garden)){
  
  # if(!(g %in% weather_station_IDs$Abbreviation)) next
  
  PFP_data_growth <- growth_summaries %>%
    dplyr::select(Date,Garden) %>%
    dplyr::arrange(Date) %>%
    dplyr::filter(Garden == g)
  
  for(y in unique(year(PFP_data_growth$Date))){
    gardens_year <- gardens %>%
      dplyr::filter(Garden == g, Season == y)
    PFP_data_growth_year <- PFP_data_growth %>%
      dplyr::filter(year(Date) == y)
    PFP_data_growth_year$Acc_FGDD <- weather_stations %>%
      dplyr::filter(year(DATE) == y, 
                    DATE >= gardens_year$PlantingDate,
                    Location == "Cortez") %>%
      #       dplyr::filter(Location == (weather_station_IDs %>% dplyr::filter(Abbreviation == g))$ID, 
      #                     year(Date_Time) == y, 
      #                     Date_Time >= gardens_year$PlantingDate) %>%
      dplyr::mutate(Acc_FGDD = cumsum(FGDD)) %>%
      dplyr::filter(DATE %in% lubridate::as_date(PFP_data_growth_year$Date)) %>%
      dplyr::select(Acc_FGDD) %>%
      unlist() %>%
      as.numeric()
    
    growth_summaries[growth_summaries$Garden == g & year(growth_summaries$Date) == y,"Acc_FGDD"] <- PFP_data_growth_year$Acc_FGDD
  }
}
readr::write_csv(growth_summaries,"./data/growth_summaries.csv")

growth_summaries %>%
  filter(Garden == "MCG")

# weather_stations %>%
#   dplyr::filter(Location == (weather_station_IDs %>% dplyr::filter(Abbreviation == g))$ID) %>%
#   ggplot2::ggplot(aes(x = Date_Time, y = 0)) +
#   ggplot2::geom_point()

ears <- PFP_data$`tbl ears` %>%
  dplyr::filter(Ear > 0, !is.na(EarWt)) %>%
  dplyr::select(Season, Garden, EarWt, CobWt, KernelWt, Rows, Condition, Clump) %>%
  dplyr::rename(`Ear weight` = EarWt, `Cob weight` = CobWt, `Kernel weight` = KernelWt) %>%
  dplyr::mutate(`Kernel weight` = `Ear weight`-`Cob weight`) %>%
  dplyr::filter(Season %in% seasons) %>%
  dplyr::left_join(y = (gardens %>% dplyr::select(Season,Garden,Variety)), by = c("Season","Garden")) %>%
  dplyr::mutate(Variety = as.factor(Variety))

# Estimate Kernel and Cob weight for ears withheld whole from analysis (e.g., POG, 2009, Clump 24)
ears <- ears %>%
  dplyr::group_by(Season,Garden,Condition) %>%
  dplyr::summarise(Kernel_r = mean(`Kernel weight`/`Ear weight`, na.rm = T)) %>%
  dplyr::mutate(Cob_r = 1 - Kernel_r) %>%
  dplyr::full_join(ears, by = c("Season","Garden","Condition")) %>%
  dplyr::mutate(`Kernel weight` = ifelse(is.na(`Kernel weight`),`Ear weight` * Kernel_r,`Kernel weight`), `Cob weight` = ifelse(is.na(`Cob weight`),`Ear weight` * Cob_r,`Cob weight`)) %>%
  dplyr::select(-Kernel_r, -Cob_r) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(Season, Garden, Clump)

readr::write_csv(ears,"./data/ears.csv")

yields <- ears %>%
  dplyr::group_by(Season, Garden) %>%
  dplyr::summarise(`Ear yield` = sum(`Ear weight`),
                   `Cob yield` = sum(`Cob weight`),
                   `Kernel yield` = sum(`Kernel weight`)) %>%
  dplyr::left_join(y = gardens, by = c("Season","Garden")) %>%
  dplyr::select(Season:`Kernel yield`,Spacing,Area) %>%
  dplyr::mutate(`Ear yield` = (`Ear yield`/1000)/(Area*0.0001),
                `Cob yield` = (`Cob yield`/1000)/(Area*0.0001),
                `Kernel yield` = (`Kernel yield`/1000)/(Area*0.0001)) %>%
  dplyr::left_join(y = (gardens %>% dplyr::select(Season,Garden,Variety)), by = c("Season","Garden")) %>%
  dplyr::mutate(Variety = as.factor(Variety))
readr::write_csv(yields,"./data/yields.csv")


# as.data.frame(ears %>%
#   filter(Garden == "POG", Season == 2009))
