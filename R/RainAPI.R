# RainSEPA ---------------------------------------------------

#' Get Scottish Environment Protection Agency (SEPA) hourly rainfall data.
#'
#'@description Extract hourly rainfall data from SEPA's API.
#'@details If the "From" date used is significantly before (approx. 30 years) the start of the available data, the function will fail with an error message saying the connection cannot be opened.
#'@param StationName The name of the station for which you want rainfall. If you type something other than one of the available stations, the list of stations will be returned.
#'@param From A start date for the data in the form of "YYYY-MM-DD".
#'@param To An end date for the data in the form of "YYYY-MM-DD". The default is the most recent date available.
#'@examples
#'#Get the list of available stations
#'\dontrun{ RainSEPA("AnythingButAStationName") }
#'#Now we'll get rain from the Bannockburn station
#'\dontrun{ Bannockburn <- RainSEPA("Bannockburn", From = "1998-10-01", To = "1998-10-31") }
#'#Now we'll have a look at the top of the data and plot it
#'\dontrun{ head(Bannockburn) }
#'\dontrun{ plot(Bannockburn, type = "h", ylab = "Rainfall (mm)") }
#'@return A data.frame with POSIXct in the first column, and rainfall in the second column. Unless the StationName provided is not in the available list, then the available list is returned.
#'@author Anthony Hammond

RainSEPA <- function(StationName, From = "2022-10-01", To = "ToDate") {
Stations <- read.csv("https://www2.sepa.org.uk/rainfall/api/Stations?csv=true")
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
Path5 <- "T09:00:00&period=P"
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



# RainEA ---------------------------------------------------

#' Get Environment Agency rainfall data (England).
#'
#'@description Extract rainfall data from the Environment Agency's API.
#'@details The function provides one of two outputs. Either information about available local rain gauges, or the data from a specified gauge (specified by WISKI ID). The process is to find the local information (including WISKI ID) by using the latitude and longitude and range. Then use the WISKI ID to get the data. If data requested is not available, for example - outside the date range or not available at the requested sampling rate, an error message is returned stating "no lines available in input". There is currently a limit to how many lines of data can be extracted (100,000) for each use. That is approximately 274 years of daily rainfall, but only 2.85 years of 15 minute data.
#'@param Lat Latitude of the point of interest. Provided when the user wants information about available local rain gauges
#'@param Lon Longitude of the point of interest. Provided when the user wants information about available local rain gauges
#'@param Range The radius (km) from the point of interest (Lat, Lon) for which the user wants rain gauge information.
#'@param WISKI_ID The WISKI identification (as "character") for the rain gauge of interest
#'@param Period The sampling rate of the rainfall in hours (generally 0.25 or 24. Default is 0.25 (15 minutes).
#'@param DateSt The start date of the data extraction in the form of "YYYY-MM-DD".
#'@param DateEnd The end date of the data extraction in the form of "YYYY-MM-DD".
#'@examples
#'#Get information about available rain gauges.
#'#within a 10km radius of Lat = 54.5, Lon = -3.2
#'\dontrun{ RainEA(Lat = 54.5, Lon = -3.2) }
#'#Now we'll use the WISKI reference for the Honister rain gauge
#'# to get some data for the Dec 2015 (default dates)
#'\dontrun{ Honister15 <- RainEA(WISKI_ID = "592463") }
#'#Now we'll have a look at the top of the data and plot it
#'\dontrun{ head(Honister15) }
#'\dontrun{ plot(Honister15, type = "h", ylab = "Rainfall (mm)") }
#'#Now we'll get the daily rain for the same period and plot it
#'\dontrun{ Honister24 <- RainEA(WISKI_ID = "592463", Period = 24) }
#'\dontrun{ plot(Honister24, type = "h", ylab = "Rainfall (mm)") }
#'@return A data.frame with POSIXct (DateTime) in the first column, and rainfall (mm) in the second column. Unless the WISKI_ID is null, then information about rain gauges approximately within a radius (Range) of the Lat and Lon is povided.
#'@author Anthony Hammond
RainEA <- function(Lat = 54, Lon = -2, Range = 10, WISKI_ID = NULL, Period = 0.25, DateSt = "2015-12-01", DateEnd = "2015-12-31") {
  if(is.null(WISKI_ID)) {
    #WISKI_ID <- as.character(WISKI_ID)
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
    WID <- as.character(WISKI_ID)
    Path <- paste("https://environment.data.gov.uk/hydrology/id/measures.csv?station.wiskiID=", WID, "&observedProperty=rainfall", sep = "")
    StationInfo <- read.csv(Path)
    Periods <- StationInfo$periodName
    if(Period == 24) {
      PeriodIndex <- which(Periods == "daily")
    }
    if(Period == 0.25) {
      PeriodIndex <- which(Periods == "15min")
    }
    IDPath <- StationInfo$X.id
    Data <- read.csv(paste(IDPath[PeriodIndex], "/readings.csv?mineq-date=", DateSt, "&max-date=", DateEnd, sep = ""))
    #Data <- read.csv(paste(IDPath[PeriodIndex], "_limit=2000000", "/readings.csv?mineq-date=", DateSt, "&max-date=", DateEnd, sep = ""))
    DateTime <- as.POSIXct(Data$dateTime, format = "%Y-%m-%dT%H:%M:%S")
    Result <- data.frame(DateTime, P = Data$value)
    return(Result)
  }
}
