# GetDataSEPA_Rain ---------------------------------------------------

#' Get Scottish Environment Protection Agency (SEPA) hourly rainfall data.
#'
#'@description Extract hourly rainfall data from SEPA's API.
#'@details If the "From" date used is significantly before (approx. 30 years) the start of the available data, the function will fail with an error message saying the connection cannot be opened. You can download data using the gauge name and you can find gauges within a given range using the latitude and longitude.
#'@param Lat Latitude of the point of interest. Provided when the user wants information about available local rain gauges
#'@param Lon Longitude of the point of interest. Provided when the user wants information about available local rain gauges
#'@param Range The radius (km) from the point of interest (Lat, Lon) for which the user wants a list of rain gauges (default is 30).
#'@param StationName The name of the station for which you want rainfall. If you type something other than one of the available stations, the list of stations will be returned.
#'@param From A start date for the data in the form of "YYYY-MM-DD".
#'@param To An end date for the data in the form of "YYYY-MM-DD". The default is the most recent date available.
#'@examples
#'#Get the list of available stations
#'\dontrun{ GetDataSEPA_Rain("AnythingButAStationName") }
#'#Now we'll get rain from the Bannockburn station
#'\dontrun{ Bannockburn <- GetDataSEPA_Rain("Bannockburn", From = "1998-10-01", To = "1998-10-31") }
#'#Now we'll have a look at the top of the data and plot it
#'\dontrun{ head(Bannockburn) }
#'\dontrun{ plot(Bannockburn, type = "h", ylab = "Rainfall (mm)") }
#'@return A data.frame with POSIXct in the first column, and rainfall in the second column. Unless the StationName provided is not in the available list, then the available list is returned.
#'@author Anthony Hammond

GetDataSEPA_Rain <- function(Lat = NULL, Lon = NULL, Range = 30, StationName, From = "2022-10-01", To = "ToDate") {
  Stations <- read.csv("https://www2.sepa.org.uk/rainfall/api/Stations?csv=true")
  if(is.null(Lat) == FALSE & is.null(Lon) == FALSE) {
    LatLonDist <- function(lat1, lon1, lat2, lon2) {acos( sin(lat1*pi/180)*sin(lat2*pi/180) + cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180) ) * 6371000}
    Distance <- NULL
    for(i in 1:nrow(Stations)) {Distance[i] <- LatLonDist(lat1 = Lat, lon1 = Lon, lat2 = Stations$station_latitude[i], lon2 = Stations$station_longitude[i])}
    Distance <- signif(Distance/1000, 4)
    StationsDist <- data.frame(ID = Stations$station_id, Name = Stations$station_name, Distance, Lat = Stations$station_latitude, Lon = Stations$station_longitude)
    StationsDist <- StationsDist[order(Distance),]
    rownames(StationsDist) <- seq(1,nrow(StationsDist))
    if(min(StationsDist$Distance) > Range) warning("No gauges within range. Try increasing your range")
    StationsDist <- subset(StationsDist, Distance <= Range)
    return(StationsDist)
  }
  Ind <- which(Stations$station_name == as.character(StationName))
  if(length(Ind) < 1) {
    Names <- read.csv('https://www2.sepa.org.uk/rainfall/api/Stations?csv=true')[,1]
    print(Names)}
  if(length(Ind) < 1) stop("Station name not found, have a look to see if your chosen station is in the available list which should now be in your console")
  if(To == "ToDate") {EndDate <- as.Date(Stations$itemDate[Ind])} else {EndDate <- as.Date(To)}
  LengthOut <- EndDate - as.Date(From)
  if(LengthOut < 1) print(paste("available data ends", as.character(EndDate), sep = " "))
  if(LengthOut < 1) stop("The 'To' date is before the 'From' date. If you used the default To, it means the 'From' date is after the end of the available data")
  tsID <- Stations$ts_id[Ind]
  Path1 <- "https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesValues&ts_id="
  Path3 <- "&from="
  Path5 <- "T09:00:00&Period=P"
  Path6 <- "D&returnfields=Timestamp,%20Value,%20Quality%20Code"
  FullPath <- paste(Path1, tsID, Path3, From, Path5, LengthOut, Path6, sep = "")
  TEXT <- suppressWarnings(readLines(FullPath))
  Body <- TEXT[6]
  SplitTxt <- strsplit(Body, ">")[[1]]
  LenText <- length(SplitTxt)
  if(LenText == 32) stop("There is no data available for the period selected")
  ValEnd <- LenText - 6
  Values <- SplitTxt[seq(34, ValEnd, by = 8)]
  DateTime <- SplitTxt[seq(32, ValEnd-2, by = 8)]
  Split1 <- strsplit(DateTime[1], split = "T")[[1]][1]
  if(Split1 != From) warning("There is data within the time period chosen but it starts after your chosen 'from' date")
  ValSplit <- strsplit(Values, "<")
  LVal <- (length(ValSplit)*2) - 1
  Vals <- as.numeric(unlist(ValSplit)[seq(1, LVal, by = 2)])
  Vals <- diff(Vals)
  Vals[Vals < 0] <- 0
  SplitT <- strsplit(DateTime, split = "T")
  SplitZ <- NULL
  for(i in 1:length(SplitT)) {SplitZ[i] <- strsplit(SplitT[[i]][2], split = ":")}
  Date <- NULL
  for(i in 1:length(SplitT)) {Date[i] <- SplitT[[i]][1]}
  Hour <- NULL
  for(i in 1:length(SplitZ)) {Hour[i] <- SplitZ[[i]][1]}
  Minute <- NULL
  for(i in 1:length(SplitZ)) {Minute[i] <- SplitZ[[i]][2]}
  Seconds <- rep("00", length(SplitZ))
  DateTimeDF <- data.frame(Date, Hour, Minute, Seconds)
  Time <- paste(DateTimeDF[,2],DateTimeDF[,3],DateTimeDF[,4], sep = ":")
  DateTime <- paste(Date, Time, sep = " ")
  DateTime <- as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")
  Result <- data.frame(DateTime = DateTime[-1], P = Vals)
  return(Result)
}



# GetDataEA_Rain ---------------------------------------------------

#' Get Environment Agency rainfall data (England).
#'
#'@description Extract rainfall data from the Environment Agency's Hydrology Data Explorer.
#'@details The function provides one of two outputs. Either information about available local rain gauges, or the data from a specified gauge (specified by WISKI ID). The process is to find the local information (including WISKI ID) by using the latitude and longitude and range (You can convert BNG to Lat and Lon using the GridRefConvert function). Then use the WISKI ID to get the data. If data requested is not available, for example - outside the date range or not available at the requested sampling rate, an error message is returned stating "no lines available in input". Currently there is a 2 million line limit for each use. That is over 5000 years of daily data or 57 years of 15 minute data. However, the 15 minute data can take quite a while and if it takes over 60 seconds a timeout message is returned. When testing the function I could get 20 years at a time but not 30. Presumably it depends on the connection?
#'@param Lat Latitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Lon Longitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Range The radius (km) from the point of interest (Lat, Lon) for which the user wants rain gauge information (currently it only seems to work to just over 20km).
#'@param WISKI_ID The WISKI identification (as "character") for the rain gauge of interest
#'@param Period The sampling rate of the rainfall in hours. Either "Daily", "15Mins", "Hourly".
#'@param From The start date of the data extraction in the form of "YYYY-MM-DD". To get data from the first date available put in an arbitrarily early date such as "1901-01-01".
#'@param To The end date of the data extraction in the form of "YYYY-MM-DD". If you put in a future date, data up to the most recent available will be returned.
#'@examples
#'#Get information about available rain gauges.
#'#within a 10km radius of Lat = 54.5, Lon = -3.2
#'\dontrun{ GetDataEA_Rain(Lat = 54.5, Lon = -3.2) }
#'#Now we'll use the WISKI reference for the Honister rain gauge
#'# to get some hourly rain data for the Dec 2015 (default dates)
#'\dontrun{ HonisterDec2015 <- GetDataEA_Rain(WISKI_ID = "592463", Period = "Hourly") }
#'#Now we'll have a look at the top of the data and plot it
#'\dontrun{ head(HonisterDec2015) }
#'\dontrun{ plot(HonisterDec2015, type = "h", ylab = "Rainfall (mm)") }
#'@return If searching for rain gauge details with the Latitude and Longitude a dataframe of gauges is returned. If extracting rainfall using the WISKI_ID, a dataframe is returned with Date or POSIXct in the first columns and rainfall in the second.
#'@author Anthony Hammond
GetDataEA_Rain <- function(Lat = 54, Lon = -2, Range = 10, WISKI_ID = NULL, Period = "Daily", From = "2015-12-01", To = "2015-12-31") {
  if(as.Date(From) > as.Date(To)) stop("The From date is after the To date")
  if(is.null(WISKI_ID)) {
    WISKI_ID <- as.character(WISKI_ID)
    if(Range > 20) warning("You have put a range greater than 20km. Not sure why at the moment but it seems to be capped at just over 20km. Hopefully this will be fixed soon.")
    range <- as.character(Range)
    RainStations <- read.csv(paste("https://environment.data.gov.uk/hydrology/id/stations.csv?lat=", Lat, "&long=", Lon, "&dist=", range, sep = ""))
    LatLonDist <- function(lat1, lon1, lat2, lon2) {acos( sin(lat1*pi/180)*sin(lat2*pi/180) + cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180) ) * 6371000}
    Distance <- NULL
    for(i in 1:nrow(RainStations)) {Distance[i] <- signif(LatLonDist(Lat, Lon, RainStations$lat[i], RainStations$long[i])/1000, 4)}
    Type <- RainStations$observedProperty
    SplitText <- strsplit(Type, "/")
    Type <- NULL
    for(i in 1:length(SplitText)) {Type[i] <- SplitText[[i]][length(SplitText[[i]])]}
    wiskiID <- RainStations$wiskiID
    Station <- RainStations$label
    Easting <- RainStations$easting
    Northing <- RainStations$northing
    Latitude <- RainStations$lat
    Longitude <- RainStations$long
    DateOpened <- RainStations$dateOpened
    DateClosed <- RainStations$dateClosed
    Stations <- data.frame(wiskiID, Station, Easting, Northing, Type, DateOpened, DateClosed, Distance)
    Stations <- Stations[which(Stations$Type == "rainfall"),]
    return(Stations)
  }
  if(is.null(WISKI_ID) == FALSE) {
    PeriodOptions <- c("Daily", "15Mins", "Hourly")
    PeriodCheck <- match(Period, PeriodOptions)
    if(is.na(PeriodCheck)) stop("Period must be one of Daily, 15Mins, or Hourly")
    WID <- as.character(WISKI_ID)
    Path <- paste("https://environment.data.gov.uk/hydrology/id/measures.csv?station.wiskiID=", WID, "&observedProperty=rainfall", sep = "")
    StationInfo <- read.csv(Path)
    Periods <- StationInfo$periodName
    if(Period == "Daily") {
      PeriodIndex <- which(Periods == "daily")
    }
    if(Period == "15Mins" | Period == "Hourly") {
      PeriodIndex <- which(Periods == "15min")
      if(as.numeric(as.Date(To) - as.Date(From)) > 3650) {print("While this is running, please note that The download times out if it takes over 60 seconds. You may find this happens if you try to extract significantly over 10 years of 15 minute data. Note that the hourly option first extracts 15 minute data and aggregates it.") }
    }

    IDPath <- StationInfo$X.id
    #Data <- read.csv(paste(IDPath[PeriodIndex], "/readings.csv?mineq-date=", From, "&max-date=", To, sep = ""))
    Data <- read.csv(paste(IDPath[PeriodIndex], "/readings.csv?", "_limit=2000000&","mineq-date=", From, "&max-date=", To, sep = ""))
    DateTime <- as.POSIXct(Data$dateTime, format = "%Y-%m-%dT%H:%M:%S")
    Result <- data.frame(DateTime, P = Data$value)
    if(Period == "Hourly") {Result <- AggDayHour(Result, sum, "Hour")}
    return(Result)
  }
}



# GetDataMetOffice ---------------------------------------------------

#' Get regional Met Office average temperature or rainfall series (monthly, seasonal, and annual).
#'
#'@description Extracts regional mean temperature or rainfall from the met office UK & regional series. The total duration of bright sunshine is also available.
#'@details The function returns time series data from the 19th century through to the present month.
#'@param Variable Either Tmean, Rainfall, or Sunshine
#'@param Region One of "UK", "England", "Wales", "Scotland", "Northern_Ireland", "England_and_Wales",
#'"England_N", "England_S", "Scotland_N", "Scotland_E", "Scotland_W", "England_E_and_NE", "England_NW_and_N_Wales",
#'"Midlands", "East_Anglia", "England_SW_and_S_Wales", "England_SE_and_Central_S".
#'@examples
#'#Get the Rainfall series for the UK
#'\dontrun{UKRain <-  GetDataMetOffice(Variable = "Rainfall", Region = "UK") }
#'#Now we'll get mean temperature data for East Anglia
#'\dontrun{TempEastAnglia <- GetDataMetOffice(Variable = "Tmean", Region = "East_Anglia") }
#'@return A data.frame with 18 columns; year, months, seasons, and annual. Rows then represent each year of the timeseries.
#'@author Anthony Hammond

GetDataMetOffice <- function(Variable, Region) {
  Variable <- as.character(Variable)
  Region <- as.character(Region)
  MetRegions <- c("UK", "England", "Wales", "Scotland", "Northern_Ireland", "England_and_Wales",
                  "England_N", "England_S", "Scotland_N", "Scotland_E", "Scotland_W", "England_E_and_NE", "England_NW_and_N_Wales",
                  "Midlands", "East_Anglia", "England_SW_and_S_Wales", "England_SE_and_Central_S")
  MatchRegion <- match(Region, MetRegions)
  if(is.na(MatchRegion)) {
    print("Region must be one of the following:")
    print(MetRegions)
    stop()
    }
  if(Variable != "Tmean" & Variable != "Rainfall" & Variable != "Sunshine") stop("Variable must equal Tmean, Rainfall, or Sunshine")
  Path <- paste("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/datasets/", Variable, "/date/", Region, ".txt", sep = "")
  Data <- read.table(Path, header = TRUE, skip = 5, fill = TRUE)
  Data[1,14] <- "NA"
  Data[nrow(Data), (as.POSIXlt(Sys.Date())$mon+2) : ncol(Data)] <- NA
  Data[,14] <-suppressWarnings(as.numeric(Data[,14]))
  return(Data)
}




# GetDataNRFA ---------------------------------------------------

#' Get National River Flow Archive data using gauge ID.
#'
#'@description Extracts NRFA data using the API.
#'@details The function can be used to get daily catchment rainfall or mean flow, or both together (concurrent). It can also be used to get gaugings, AMAX, and POT data. Note that some sites have rejected peak flow years. In which case, if Type = AMAX or POT, the function returns a list, the first element of which is the rejected years, the second is the full AMAX or POT. Lastly if Type = "Catalogue" it will return a dataframe of all the NRFA gauges, associated details, comments, and descriptors.
#'@param ID ID number of the gauge of interest.
#'@param Type Type of data required. One of "Q", "P", "PQ", "Gaugings", "AMAX", "POT", or "Catalogue".
#'@examples
#'#Get the concurrent rainfall and mean flow series for the Tay at Ballathie (site 15006).
#'\dontrun{BallathiePQ <-  GetDataNRFA(15006, "PQ")}
#'#Now we'll get the gaugings
#'\dontrun{BallathieGaugings <- GetDataNRFA(15006, "Gaugings") }
#'@return A data.frame with date in the first columns and variable/s of interest in the remaining column/s.
#'@author Anthony Hammond

GetDataNRFA <- function(ID, Type = "Q") {
  Types <- c("Q", "P", "PQ", "Gaugings", "AMAX", "POT", "Catalogue")
  MatchType <- match(Type, Types)
  if(is.na(MatchType)) stop("Type must be one of Q, P, PQ, Gaugings, AMAX, POT, or Catalogue")

  PQnrfa <- function(ID, type = "Both") {
  CDRfunc <- function(ID) {
    ID <- as.character(ID)
    Path <- paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=cdr&station=", ID, sep = "")
    Data <- read.csv(Path)
    Data <- Data[20:nrow(Data),]
    Date <- as.Date(Data$file)
    P <- as.numeric(Data$timestamp)
    PData <- data.frame(Date, P)
    return(PData)
  }

  GDFfunc <- function(ID) {
    ID <- as.character(ID)
    Path <- paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=gdf&station=", ID, sep = "")
    Data <- read.csv(Path)
    Data <- Data[20:nrow(Data),]
    Date <- as.Date(Data$file)
    Q <- as.numeric(Data$timestamp)
    QData <- data.frame(Date, Q)
    return(QData)
  }

  P <- CDRfunc(ID)
  Q <- GDFfunc(ID)

  Concurrents <- function(x,y)
  {
    Index1 <- x$Date %in% y$Date
    Index2 <- y$Date %in% x$Date
    x.Var <- x[Index1, ]
    y.Var <- y[Index2,2]
    Comb <- data.frame(x.Var, y.Var)
    colnames(Comb) <- c("Date", "x", "y")
    return(Comb)
  }

  if(type == "Both") {
    Res <- Concurrents(P, Q)
    colnames(Res) <- c("Date", "P", "Q")}
  if(type == "CDR" | type == "cdr"){
    Res <- P}
  if(type == "GDF" | type == "gdf"){
    Res <- Q}
  return(Res)
}

NRFAGaugings <- function(ID) {
  ID <- as.character(ID)
  Q <- read.csv(paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=gauging-flow&station=", ID, sep = ""), skip = 19)[,1:2]
  H <- read.csv(paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=gauging-stage&station=", ID, sep = ""), skip = 19)[,1:2]
  colnames(H) <- c("DateTime", "h")
  H$DateTime <- as.POSIXct(H$DateTime, format = "%Y-%m-%dT%H:%M:%S")
  colnames(Q) <- c("DateTime", "Q")
  Q$DateTime <- as.POSIXct(Q$DateTime, format = "%Y-%m-%dT%H:%M:%S")
  ResDF <- merge(Q, H, by = 1)
  return(ResDF)
}

AMAXfunc <- function(ID) {
  AllCat <- GetDataNRFA(Type = "Catalogue")
  Index <- which(AllCat$id == ID)
  if(AllCat$nrfa.peak.flow[Index] == "false") stop("No available peak flows at this site")
  rejYears <- AllCat$peak.flow.rejected.amax.years[Index]
  if(rejYears == "") {rejYears <- NA}
  ID <- as.character(ID)
  Path <- paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=amax-flow&station=", ID, sep = "")
  Data <- read.csv(Path)
  Data <- Data[21:nrow(Data),]
  Date <- as.Date(Data$file)
  Q <- as.numeric(Data$timestamp)
  QData <- data.frame(Date, Q)
  if(is.na(rejYears)) {
    return(QData)
  }
  if(is.na(rejYears) == FALSE){
    ResultList <- list(rejYears, QData)
    names(ResultList) <- c("Rejected hydrological years", "AMAX")
    return(ResultList)}
}

POTfunc <- function(ID) {
  ID <- as.character(ID)
  AllCat <- GetDataNRFA(Type = "Catalogue")
  Index <- which(AllCat$id == ID)
  if(AllCat$nrfa.peak.flow[Index] == "false") stop("No available peak flows at this site")
  rejYears <- AllCat$peak.flow.rejected.amax.years[Index]
  if(rejYears == "") {rejYears <- NA}
  Path <- paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=pot-flow&station=", ID, sep = "")
  Data <- read.csv(Path)
  Data <- Data[21:nrow(Data),]
  Date <- as.Date(Data$file)
  Q <- as.numeric(Data$timestamp)
  QData <- data.frame(Date, Q)
  if(is.na(rejYears)) {
    return(QData)
  }
  if(is.na(rejYears) == FALSE){
    ResultList <- list(rejYears, QData)
    names(ResultList) <- c("Rejected hydrological years", "POT")
    return(ResultList)}
}

if(Type == "Q") {Result <- PQnrfa(ID, type = "gdf")}
if(Type == "P") {Result <- PQnrfa(ID, type = "cdr")}
if(Type == "PQ") {Result <- PQnrfa(ID, type = "Both")}
if(Type == "Gaugings") {Result <- NRFAGaugings(ID)}
if(Type == "Catalogue") {Result <- read.csv("https://nrfaapps.ceh.ac.uk/nrfa/ws/station-info?station=*&format=csv&fields=all")}
if(Type == "AMAX") {Result <- AMAXfunc(ID)}
if(Type == "POT") {Result <- POTfunc(ID)}
return(Result)
}


# GetDataEA_QH ---------------------------------------------------

#' Get flow or level data from the Environment Agency's Hydrology Data Explorer
#'
#'@description Function to extract flow or level data from the Environment Agency's Hydrology Data Explorer.
#'@details To find gauges you can input either a river name or a latitude and longitude. You can convert BNG to Lat and Lon using the ConvertGridRef function. The lat and lon option will provide all flow and level gauges within a specified range (default of 10km). This provides gauged details including the WISKI ID. You can get data from specific gauges using the WISKI_ID. Note that many flow gauges also have level data available. Do ensure you get the start date you can put in a year that is many years prior to the start of the gauge. Lastly, I have noticed that WISKI IDs can be returned without a preceding 0 which might be necessary for the data extraction (oddly, most do have the necessary 0). If it does not work try adding a 0 to the beginning of the WISKI ID.
#'@param Lat Latitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Lon Longitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Range Radius of search when using latitude and longitude inputs (km).
#'@param RiverName Name of the river along which you want to search for gauges. Character string.
#'@param WISKI_ID The WISKI ID for the gauge from which you want to obtain data (character string)
#'@param From Date for start of data extraction in the format of "2015-12-02"
#'@param To Date for the end of data extraction in the format of "2015-12-02"
#'@param Type The variable to extract, either "flow" or "level"
#'@param Period The sampling rate of the data you want. Either "DailyMax", "DailyMean", "Hourly", "15Mins".
#'@examples
#'#Find gauges on the river Tame
#'\dontrun{GetDataEA_QH(RiverName = "Tame")}
#'#Find gauges within 10km of a latlon grid reference somewhere near the centre of Dartmoor
#'\dontrun{GetDataEA_QH(lat = 50.6, lon = -3.9, Range = 10)}
#'#Get daily maximum flow data from the Bellever gauge on the
#'#East Dart River for the default From - To date range
#'\dontrun{BelleverMax <- GetDataEA_QH(WISKI_ID = "SX67F051")}
#'@return If searching for gauge details with lat and lon or river name, then a list is returned. The first element is a dataframe with flow gauge details and the second is a dataframe of level gauge details.
#'When extracting flow or level data with a WISKI ID then a dataframe with two columns is returned. The first being a Date or POSIXct column/vector and the second is the timeseries of interest.
#'@author Anthony Hammond

GetDataEA_QH <- function(Lat = 54, Lon = -2.25, Range = 20, RiverName = NULL, WISKI_ID = NULL, From = "2015-10-01", To = "2016-09-30", Type = "flow", Period = "DailyMean") {
  #ListRiverNames
  #read.csv("https://environment.data.gov.uk/hydrology/id/stations.csv?observedProperty=waterFlow&riverName=River+Exe")
  if(is.null(WISKI_ID)) {
    if(is.null(RiverName)) {
      if(Lat > 55.9 | Lat < 49.94) stop("Grid reference appears to be outside England")
      if(Lon > 1.87 | Lon < -5.77) stop("Grid reference appears to be outside England")
      RangePath <- paste("https://environment.data.gov.uk/hydrology/id/stations.csv?lat=", Lat, "&long=", Lon, "&dist=", Range, sep = "")
      StationsWithinRange <- read.csv(RangePath)
      Observed_Property <- strsplit(StationsWithinRange$observedProperty, "/")
      FlowGrepFunc <- function(x, Var = "waterFlow") {
        Result <- grep(Var, x)
        if(length(Result) >= 1) {Result <- 1} else {Result <- 0}
        return(Result)
      }
      FlowsGrep <- NULL
      for(i in 1:length(Observed_Property)) {FlowsGrep[i] <- FlowGrepFunc(Observed_Property[[i]])}
      LevelGrep <- NULL
      for(i in 1:length(Observed_Property)) {LevelGrep[i] <- FlowGrepFunc(Var = "waterLevel", Observed_Property[[i]])}
      GWCheck <- NULL
      for(i in 1:length(Observed_Property)) {GWCheck[i] <- FlowGrepFunc(Var = "groundwaterLevel", Observed_Property[[i]])}
      GWIndex <- which(GWCheck == 1)
      LevelGrep[GWIndex] <- 0

      #StationsWithinRange1 <- StationsWithinRange[,c(3, 5, 6,10,14)]
      FlowIndex <- which(FlowsGrep == 1)
      LevelIndex <- which(LevelGrep == 1)
      FlowStations <- StationsWithinRange[FlowIndex, c(3, 14, 5, 6,10)]
      LevelStations <- StationsWithinRange[LevelIndex, c(3, 14, 5, 6,10)]
      #LevelStations <- LevelStations[LevelIndex, ]
      #ResultList <- list(FlowStations, LevelStations)
      MatchFlow <- match(FlowStations$wiskiID, LevelStations$wiskiID)
      MatchFlow <- MatchFlow[!is.na(MatchFlow)]
      ResultList <- list(FlowStations, LevelStations[-MatchFlow,])
      rownames(ResultList[[1]]) <- seq(1, nrow(ResultList[[1]]))
      rownames(ResultList[[2]]) <- seq(1, nrow(ResultList[[2]]))
      names(ResultList) <- c("Flow", "Water level")
      RowTestQ <- nrow(ResultList[[1]])
      RowTestH <- nrow(ResultList[[2]])
      if(RowTestQ ==0) print("It appears no flow gauges are available within the search radius")
      if(RowTestH ==0) print("It appears no 'level only' gauges are available within the search radius")
      return(ResultList)
    }
    if(is.null(RiverName) == FALSE) {
      FlowPath <- paste("https://environment.data.gov.uk/hydrology/id/stations.csv?observedProperty=waterFlow&riverName=River+", RiverName, sep = "")
      StagePath <- paste("https://environment.data.gov.uk/hydrology/id/stations.csv?observedProperty=waterLevel&riverName=River+", RiverName, sep = "")
      FlowStations <- read.csv(FlowPath)
      LevelStations <- read.csv(StagePath)
      FlowStations <- FlowStations[,c(3, 14, 5, 6,10)]
      LevelStations <- LevelStations[,c(3, 14, 5, 6,10)]
      MatchFlow <- match(FlowStations$wiskiID, LevelStations$wiskiID)
      MatchFlow <- MatchFlow[!is.na(MatchFlow)]
      ResultList <- list(FlowStations, LevelStations[-MatchFlow,])
      rownames(ResultList[[1]]) <- seq(1, nrow(ResultList[[1]]))
      rownames(ResultList[[2]]) <- seq(1, nrow(ResultList[[2]]))
      names(ResultList) <- c("Flow", "Water level")
      RowTestQ <- nrow(ResultList[[1]])
      RowTestH <- nrow(ResultList[[2]])
      if(RowTestQ ==0) print("It appears no flow gauges are available on the river you searched")
      if(RowTestH ==0) print("It appears no level-only gauges are available on the river you searched")
      return(ResultList)
    }
  }

  if(is.null(WISKI_ID) == FALSE) {
    WISKI_ID <- as.character(WISKI_ID)
    Periods <- c("DailyMax", "DailyMean", "15Mins", "Hourly")
    if(is.na(match(Period, Periods)) ) stop("Period must be one of DailyMax, DailyMean, 15Mins, or Hourly")
    if(Period == "15Mins" | Period == "Hourly") {
      if(as.numeric(as.Date(To) - as.Date(From)) > 3650) {print("While this is running, please note that The download times out if it takes over 60 seconds. You may find this happens if you try to extract significantly over 10 years of 15 minute data. Note that the hourly option first extracts 15 minute data and aggregates it.") }

    }
    if(Type != "flow" & Type != "level") stop("Type must equal level or flow")

    if(Type == "flow") {Unit <- "-m3s"}
    if(Type == "level") {Unit <- "-m"}
    if(Period == "DailyMax") {PeriodPath <- "max-86400"}
    if(Period == "DailyMean") {PeriodPath <- "m-86400"}
    if(Period == "15Mins" | Period == "Hourly") {PeriodPath <- "i-900"}

    IDPath <- read.csv(paste("https://environment.data.gov.uk/hydrology/id/stations.csv?wiskiID=", WISKI_ID ,sep = "") )
    TypeCheck <- grep(Type, IDPath$measures.parameter)
    if(length(TypeCheck) == 0) stop("Your Type is not available for this WISKI_ID")
    ID <- IDPath$notation
    Data <- read.csv(paste("https://environment.data.gov.uk/hydrology/id/measures/", ID, "-", Type, "-", PeriodPath, Unit, "-qualified/readings.csv?_limit=2000000&mineq-date=", From, "&maxeq-date=", To, sep = ""))
    if(PeriodPath == "max-86400" | PeriodPath == "m-86400"){
      Data <- Data[,3:4]
      Data$date <- as.Date(Data$date)}
    if(PeriodPath == "i-900") {
      Data <- Data[,c(2,4)]
      Data$dateTime <- as.POSIXct(Data$dateTime, format = "%Y-%m-%dT%H:%M:%S")
      if(Period == "Hourly") {
        Data <- AggDayHour(Data, func = mean, Freq = "Hour")
        colnames(Data) <- c("dateTime", "value")}
    }
    return(Data) }
}
