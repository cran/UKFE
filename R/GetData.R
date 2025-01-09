# GetDataSEPA_Rain ---------------------------------------------------

#' Get Scottish Environment Protection Agency (SEPA) hourly rainfall data.
#'
#'@description Extract hourly rainfall data from SEPA's API.
#'@details You can download data using the gauge name and you can find gauges within a given range using the latitude and longitude. If the 'From' date is left as null, the earliest date of available data will be used. If the 'To' date is left as null, the most recent date of available data will be used.
#'@param Lat Latitude of the point of interest. Provided when the user wants information about available local rain gauges
#'@param Lon Longitude of the point of interest. Provided when the user wants information about available local rain gauges
#'@param Range The radius (km) from the point of interest (Lat, Lon) for which the user wants a list of rain gauges (default is 30).
#'@param StationName The name of the station for which you want rainfall. If you type something other than one of the available stations, the list of stations will be returned.
#'@param From A start date for the data in the form of "YYYY-MM-DD". Default of NULL means the earliest available date is used
#'@param To An end date for the data in the form of "YYYY-MM-DD". The default is the most recent date available.
#'@examples
#'#Get the list of available stations
#'\dontrun{ GetDataSEPA_Rain(StationName = "AnythingButAStationName") }
#'#Now we'll get rain from the Bannockburn station
#'\dontrun{ Bannockburn <- GetDataSEPA_Rain(StationName = "Bannockburn",
#'From = "1998-10-01", To = "1998-10-31") }
#'#Now we'll have a look at the top of the data and plot it
#'\dontrun{ head(Bannockburn) }
#'\dontrun{ plot(Bannockburn, type = "h", ylab = "Rainfall (mm)") }
#'@return A data.frame with POSIXct in the first column, and rainfall in the second column. Unless the StationName provided is not in the available list, then the available list is returned.
#'@author Anthony Hammond

GetDataSEPA_Rain <- function(Lat = NULL, Lon = NULL, Range = 30, StationName, From = NULL, To = NULL) {
  Stations <- read.csv("https://www2.sepa.org.uk/rainfall/api/Stations?csv=true")
  if(is.null(Lat) == FALSE & is.null(Lon) == FALSE) {
    LatLonDist <- function(lat1, lon1, lat2, lon2) {acos( sin(lat1*pi/180)*sin(lat2*pi/180) + cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180) ) * 6371000}
    Distance <- NULL
    for(i in 1:nrow(Stations)) {Distance[i] <- LatLonDist(lat1 = Lat, lon1 = Lon, lat2 = Stations$station_latitude[i], lon2 = Stations$station_longitude[i])}
    Distance <- signif(Distance/1000, 4)
    StationsDist <- data.frame(ID = Stations$station_id, Name = Stations$station_name, Distance, Lat = Stations$station_latitude, Lon = Stations$station_longitude)
    StationsDist <- StationsDist[order(Distance),]
    rownames(StationsDist) <- seq(1,nrow(StationsDist))
    if(min(StationsDist$Distance) > Range) stop("No gauges within range. Try increasing your range")
    StationsDist <- subset(StationsDist, Distance <= Range)
    return(StationsDist)
  }
  Ind <- which(Stations$station_name == as.character(StationName))
  if(length(Ind) < 1) {
    Names <- read.csv('https://www2.sepa.org.uk/rainfall/api/Stations?csv=true')[,1]
    print(Names)}
  if(length(Ind) < 1) stop("Station name not found, have a look to see if your chosen station is in the available list which should now be in your console")

  StationID <- Stations$station_id[which(Stations$station_name == StationName)]
  PathDates <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesList&station_id=", StationID, "&stationparameter_name=Rain&returnfields=coverage&dateformat=yyyy-MM-dd", "&format=csv", sep = "")
  StationDates <- suppressWarnings(read.csv(PathDates, sep = ";"))
  if(is.null(From)) {From <- min(StationDates$from)} else {From <- From}
  if(is.null(To)) {EndDate <- Sys.Date()} else {EndDate <- as.Date(To)}
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
  #if(Split1 != From) warning("There is data within the time period chosen but it starts after your chosen 'from' date")
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
#'@details The function provides one of two outputs. Either information about available local rain gauges, or the data from a specified gauge (specified by WISKI ID). The process is to find the local information (including WISKI ID) by using the latitude and longitude and range (You can convert BNG to Lat and Lon using the GridRefConvert function). Then use the WISKI ID to get the data. If data requested is not available, for example - outside the date range or not available at the requested sampling rate, an error message is returned stating "no lines available in input". To extract all the available data leave the From and To arguments as Null.
#'@param Lat Latitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Lon Longitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Range The radius (km) from the point of interest (Lat, Lon) for which the user wants rain gauge information (currently it only seems to work to just over 20km).
#'@param WISKI_ID The WISKI identification (as "character") for the rain gauge of interest
#'@param Period The sampling rate of the rainfall in hours. Either "Daily", "15Mins", "Hourly".
#'@param From The start date of the data extraction in the form of "YYYY-MM-DD". To get data from the first date available leave as NULL.
#'@param To The end date of the data extraction in the form of "YYYY-MM-DD". To get data from the first date available leave as NULL.
#'@examples
#'#Get information about available rain gauges.
#'#within a 10km radius of Lat = 54.5, Lon = -3.2
#'\dontrun{ GetDataEA_Rain(Lat = 54.5, Lon = -3.2) }
#'#Now we'll use the WISKI reference for the Honister rain gauge
#'# to get some hourly rain data for the Dec 2015
#'\dontrun{ HonisterDec2015 <- GetDataEA_Rain(WISKI_ID = "592463",
#'Period = "Hourly", From = "2015-12-01", To = "2015-12-31") }
#'#Now we'll have a look at the top of the data and plot it
#'\dontrun{ head(HonisterDec2015) }
#'\dontrun{ plot(HonisterDec2015, type = "h", ylab = "Rainfall (mm)") }
#'@return If searching for rain gauge details with the Latitude and Longitude a dataframe of gauges is returned. If extracting rainfall using the WISKI_ID, a dataframe is returned with Date / POSIXct in the first columns and rainfall in the second.
#'@author Anthony Hammond
GetDataEA_Rain <- function(Lat = 54, Lon = -2, Range = 10, WISKI_ID = NULL, Period = "Daily", From = NULL, To = NULL) {
  if(is.null(From)) {From <- "1901-10-01"}
  if(is.null(To)) {To <- Sys.Date()}
  if(as.Date(To) > Sys.Date()) stop("To is in the future. This tool is for extracting observed data, not predicting it :)")
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
    if(nrow(Stations) == 0) stop("There are no gauges within the specified range")
    return(Stations)
  }
  if(is.null(WISKI_ID) == FALSE) {
    PeriodOptions <- c("Daily", "15Mins", "Hourly")
    PeriodCheck <- match(Period, PeriodOptions)
    if(is.na(PeriodCheck)) stop("Period must be one of Daily, 15Mins, or Hourly")



    #Here is a separate Get P function. Then we need a loop one for all the 15 minute data

    GetP <- function(WISKI_ID, From = NULL, To = NULL, Period = "Daily")  {
      #WISKI_ID <- as.character(WISKI_ID)
      Periods <- c("Daily", "15Mins", "Hourly")
      if(is.na(match(Period, Periods)) ) stop("Period must be one of Daily, 15Mins, or Hourly")

      WID <- as.character(WISKI_ID)
      Path <- paste("https://environment.data.gov.uk/hydrology/id/measures.csv?station.wiskiID=", WID, "&observedProperty=rainfall", sep = "")
      StationInfo <- read.csv(Path)
      Periods <- StationInfo$periodName
      if(Period == "Daily") {
        PeriodIndex <- which(Periods == "daily")
      }
      if(Period == "15Mins" | Period == "Hourly") {
        PeriodIndex <- which(Periods == "15min")
      }

      IDPath <- StationInfo$X.id
      #Data <- read.csv(paste(IDPath[PeriodIndex], "/readings.csv?mineq-date=", From, "&max-date=", To, sep = ""))
      Data <- read.csv(paste(IDPath[PeriodIndex], "/readings.csv?", "_limit=2000000&","mineq-date=", From, "&max-date=", To, sep = ""))
      DateTime <- as.POSIXct(Data$dateTime, format = "%Y-%m-%dT%H:%M:%S")
      Result <- data.frame(DateTime, P = Data$value)
      return(Result)
    }


    if(Period == "15Mins" | Period == "Hourly") {
      if(as.numeric(as.Date(To) - as.Date(From)) > 1825) {
        print("This may take some time (up to 60 seconds per ten years of data), please be patient")
        Froms <- seq(as.Date(From), as.Date(To), by = "year")
        Froms[length(Froms)] <- To
        #print("This may take a few minutes, please be patient")
        xList <- list()
        for(i in 1:(length(Froms)-1)) {xList[[i]] <- try(GetP(WISKI_ID = WISKI_ID, From = Froms[i], To = Froms[i+1], Period = "15Mins"), silent = TRUE) }
        IndexNoData <- NULL
        for(i in 1:length(xList)) {IndexNoData[i] <- class(xList[[i]])}
        IndexData <- which(IndexNoData != "try-error")
        if(length(IndexData) == 0) stop("No data available for the WISKI ID used")
        xListResults <- list()
        for(i in 1:length(IndexData)) {xListResults[[i]] <- xList[[IndexData[i]]]}
        Result <- do.call(rbind,xListResults)
      }
      if(as.numeric(as.Date(To) - as.Date(From)) <= 1825) {
        Result <- GetP(WISKI_ID = WISKI_ID, From = From, To = To, Period = "15Mins")
      }
      if(Period == "Hourly") {Result <- AggDayHour(Result, func = sum, Freq = "Hour")}
    }

    if(Period == "Daily") {
      Result <- GetP(WISKI_ID = WISKI_ID, From =From, To = To)
    }

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
  if(as.POSIXlt(Sys.Date())$mon > 0) {Data[nrow(Data), (as.POSIXlt(Sys.Date())$mon+2) : ncol(Data)] <- NA}
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
#'Except for the following circumstances: When Type = "Catalogue", then a large dataframe is returned with all the NRFA gauge metadata.
#'When Type = "AMAX" or "POT" and there are rejected years a list is returned. Where the first element is the dataframe of data and the second is rejected year/s (character string).
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
    ResultList <- list(QData, rejYears)
    names(ResultList) <- c("AMAX", "Rejected hydrological years")
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
    ResultList <- list(QData, rejYears)
    names(ResultList) <- c("POT", "Rejected hydrological years")
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
#'@details To find gauges you can input either a river name or a latitude and longitude. You can convert BNG to Lat and Lon using the ConvertGridRef function (you can also get lat and lon by left clicking on google maps). The lat and lon option will provide all flow and level gauges within a specified range (default of 10km). This provides gauged details including the WISKI ID. You can get data from specific gauges using the WISKI_ID. Note that flow gauges also have level data available. You can get data from a date range using the From and To arguments or you can return all data by leaving the From and To as the default (NULL). Lastly, WISKI IDs are sometimes returned without a preceding 0 which might be necessary for the data extraction (oddly, most do have the necessary 0). If data extraction fails try adding a 0 to the beginning of the WISKI ID.
#'@param Lat Latitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Lon Longitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Range Radius of search when using latitude and longitude inputs (km).
#'@param RiverName Name of the river along which you want to search for gauges. Character string.
#'@param WISKI_ID The WISKI ID for the gauge from which you want to obtain data (character string). Note that sometimes a preceding zero, which is not returned via the API, is needed. If the data extraction fails, this may be the cause and you can resolve it by including the preceding zero in the WISKI_ID.
#'@param From Date for start of data extraction in the format of "2015-12-02". If NULL the start date of the available data is used.
#'@param To Date for the end of data extraction in the format of "2015-12-02". If NULL end date of the available data is used.
#'@param Type The variable to extract, either "flow" or "level"
#'@param Period The sampling rate of the data you want. Either "DailyMax", "DailyMean", "Hourly", "15Mins".
#'@examples
#'#Find gauges on the river Tame
#'\dontrun{GetDataEA_QH(RiverName = "Tame")}
#'#Find gauges within 10km of a latlon grid reference somewhere near the
#'#centre of Dartmoor
#'\dontrun{GetDataEA_QH(Lat = 50.6, Lon = -3.9, Range = 10)}
#'#Get all available daily maximum flow data from the Bellever gauge on the
#'#East Dart River.
#'\dontrun{BelleverMax <- GetDataEA_QH(WISKI_ID = "SX67F051")}
#'#Get 15-minute data from the Bellever for the Novermeber 2024 event
#'\dontrun{BelleverNov2024 <- GetDataEA_QH(WISKI_ID = "SX67F051",
#'From = "2024-11-23", To = "2024-11-25", Period = "15Mins")}
#'@return If searching for gauge details with lat and lon or river name, then a list is returned. The first element is a dataframe with flow gauge details and the second is a dataframe of level gauge details.
#'When extracting flow or level data with a WISKI ID then a dataframe with two columns is returned. The first being a Date or POSIXct column/vector and the second is the timeseries of interest.
#'@author Anthony Hammond

GetDataEA_QH <- function(Lat = 54, Lon = -2.25, Range = 20, RiverName = NULL, WISKI_ID = NULL, From = NULL, To = NULL, Type = "flow", Period = "DailyMean") {
  if(is.null(From)) {From <- "1901-10-01"}
  if(is.null(To)) {To <- Sys.Date()}
  if(as.Date(To) > Sys.Date()) stop("To is in the future. This tool is for extracting observed data, not predicting it :)")
  if(as.Date(From) > as.Date(To)) stop("The From date is after the To date")

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

      FlowIndex <- which(FlowsGrep == 1)
      LevelIndex <- which(LevelGrep == 1)
      FlowStations <- StationsWithinRange[FlowIndex, c(3, 14, 5, 6,10)]
      LevelStations <- StationsWithinRange[LevelIndex, c(3, 14, 5, 6,10)]

      MatchFlow <- match(FlowStations$wiskiID, LevelStations$wiskiID)
      MatchFlow <- MatchFlow[!is.na(MatchFlow)]
      ResultList <- list(FlowStations, LevelStations[-MatchFlow,])
      RowTestQ <- nrow(ResultList[[1]])
      RowTestH <- nrow(ResultList[[2]])
      if(RowTestQ == 0 & RowTestH == 0) stop("There are no flow gauges or level only gauges within the specified range")
      if(RowTestQ ==0) print("It appears no flow gauges are available within the search radius")
      if(RowTestH ==0) print("It appears no 'level only' gauges are available within the search radius")
      if(nrow(ResultList[[2]]) == 0) {ResultList[[2]] <- data.frame(label = NA, wiskiID = NA, easting = NA, Northing = NA, riverName = NA)
      print("There are no level only gauges within the range. However, Water level data can be obtained from the flow gauges returned.")
      }
      rownames(ResultList[[1]]) <- seq(1, nrow(ResultList[[1]]))
      rownames(ResultList[[2]]) <- seq(1, nrow(ResultList[[2]]))
      names(ResultList) <- c("Flow", "Water level")

      print("In some cases the WISKI_ID requires a preceding '0' which is not returned by the API. If the WISKI_ID fails, try it with a 0 in front")
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
      if(nrow(ResultList[[2]]) == 0) {ResultList[[2]] <- data.frame(label = NA, wiskiID = NA, easting = NA, Northing = NA, riverName = NA)
      print("There are no level only gauges within the range. However, Water level data can be obtained from the flow gauges returned.")
      }
      rownames(ResultList[[1]]) <- seq(1, nrow(ResultList[[1]]))
      rownames(ResultList[[2]]) <- seq(1, nrow(ResultList[[2]]))
      names(ResultList) <- c("Flow", "Water level")
      RowTestQ <- nrow(ResultList[[1]])
      RowTestH <- nrow(ResultList[[2]])
      if(RowTestQ ==0) print("It appears no flow gauges are available on the river you searched")
      if(RowTestH ==0) print("It appears no level-only gauges are available on the river you searched")
      print("In some cases the WISKI_ID requires a preceding '0' which is not returned by the API. If the WISKI_ID fails try it with a 0 in front")
      return(ResultList)
    }
  }

  if(is.null(WISKI_ID) == FALSE) {

    #Here is a separate Get q or H function. Then we need a loop one for all the 15 minute data

    GetQH <- function(WISKI_ID, From = NULL, To = NULL, Period = "DailyMax", Type = "flow")  {
      WISKI_ID <- as.character(WISKI_ID)
      Periods <- c("DailyMax", "DailyMean", "15Mins", "Hourly")
      if(is.na(match(Period, Periods)) ) stop("Period must be one of DailyMax, DailyMean, 15Mins, or Hourly")
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
      return(Data)
    }

    if(Period == "15Mins" | Period == "Hourly") {

      if(Period == "15Mins" | Period == "Hourly") {
        if(as.numeric(as.Date(To) - as.Date(From)) > 1825) {
          print("This may take some time (up to 60 seconds per ten years of data), please be patient")
          Froms <- seq(as.Date(From), as.Date(To), by = "year")
          Froms[length(Froms)] <- To
          #print("This may take a few minutes, please be patient")
          xList <- list()
          for(i in 1:(length(Froms)-1)) {xList[[i]] <- try(GetQH(WISKI_ID = WISKI_ID, From = Froms[i], To = Froms[i+1], Period = "15Mins", Type = Type), silent = TRUE) }
          IndexNoData <- NULL
          for(i in 1:length(xList)) {IndexNoData[i] <- class(xList[[i]])}
          IndexData <- which(IndexNoData != "try-error")
          if(length(IndexData) == 0) stop("No data available for the WISKI ID used")
          xListResults <- list()
          for(i in 1:length(IndexData)) {xListResults[[i]] <- xList[[IndexData[i]]]}
          Result <- do.call(rbind,xListResults)
        }
        if(as.numeric(as.Date(To) - as.Date(From)) <= 1825) {
          Result <- GetQH(WISKI_ID = WISKI_ID, From = From, To = To, Period = "15Mins")
        }
      }

    }
    if(Period == "DailyMax" | Period == "DailyMean") {
      Result <- GetQH(WISKI_ID = WISKI_ID, From =From, To = To, Period = Period, Type = Type)
    }
    return(Result)
  }
}




# GetDataSEPA_QH ---------------------------------------------------

#' Get flow or level data from the Scottish Environmental Protection Agency.
#'
#'@description Function to extract flow or level data from SEPA.
#'@details To find gauges you can input either a river name or a latitude and longitude. You can convert BNG to Lat and Lon using the ConvertGridRef function. The lat and lon option will provide all flow or level gauges within a specified range (default of 50km). This provides gauged details including the StationID. You can get data from specific gauges using the StationID. Note that flow gauges also have level data available. You can get data from a date range using the From and To arguments. If the From and To arguments are left as NULL the full range of available data are returned.
#'@param Lat Latitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Lon Longitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#'@param Range Radius of search when using latitude and longitude inputs (km).
#'@param RiverName Name of the river along which you want to search for gauges. Character string.
#'@param StationID The ID for the gauge from which you want to obtain data (character string)
#'@param From Date for start of data extraction in the format of "2015-12-02". If NULL the first date of the available data is used.
#'@param To Date for the end of data extraction in the format of "2015-12-02". If NULL the present date is used (and most recent available data is returned).
#'@param Type The variable to extract, either "flow" or "level"
#'@param Period The sampling rate of the data you want. Either "Daily", "Hourly", or "15Mins".
#'@examples
#'#Find gauges on the river Spey
#'\dontrun{GetDataSEPA_QH(RiverName = "Spey")}
#'#Find gauges within 20km of a latlon grid reference somewhere near the centre of Scotland
#'\dontrun{GetDataSEPA_QH(lat = 56, lon = -4, Range = 20)}
#'#Get all available daily mean flow data from the Boat o Brig gauge on the Spey
#'\dontrun{SpeyDaily <- GetDataSEPA_QH(StationID = "37174")}
#'#Get 15-minute data from the Boat o Brig for the highest recorded peak
#'\dontrun{BoatOBrigAug1970 <- GetDataSEPA_QH(StationID = "37174",
#'From = "1970-08-16", To = "1970-08-19", Period = "15Mins")}
#'@return If searching for gauge details with lat and lon or river name, then a dataframe is returned with necessary information to obtain flow or level data.
#'When extracting flow or level data with a station ID then a dataframe with two columns is returned. The first being a Date or POSIXct column/vector and the second is the timeseries of interest.
#'@author Anthony Hammond

GetDataSEPA_QH <- function(Lat = NULL, Lon = NULL, RiverName = NULL, Type = "Flow", StationID = NULL, Range = 20, From = NULL, To = NULL, Period = "Daily") {
  #Types <- c("Flow", "Rain", "Level")
  Types <- c("Flow", "Level")
  TypeCheck <- match(Type, Types)
  if(is.na(TypeCheck)) stop("Type must be Flow or Level")
  if(is.null(RiverName) == FALSE) {
    Stations <- read.csv("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getStationList&returnfields=station_id,%20station_name,catchment_name,station_latitude,station_longitude,stationparameter_name,%20stationparameter_no&object_type=General&format=csv", sep = ";")
    LonList <- strsplit(Stations$station_longitude, split = "'")
    station_longitude <- NULL
    for(i in 1:length(LonList)) {station_longitude[i] <- as.numeric(LonList[[i]])[2]}
    Stations$station_longitude <- station_longitude
    #Stations <- subset(Stations, stationparameter_name == Type)
    Stations <- Stations[Stations$stationparameter_name == Type, ]
    #  DatesPath <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesList&station_id=", StationID, "&stationparameter_name=", type, "&returnfields=coverage&dateformat=yyyy-MM-dd", "&format=csv", sep = "")
    Match <- grep(RiverName, Stations$catchment_name)
    if(length(Match) < 1) stop("The river name is not recognised")
    Result <- Stations[Match,1:6]
    rownames(Result) <- seq(1, nrow(Result))
    return(Result)
  }

  if(is.null(Lat) == FALSE & is.null(Lon) == FALSE & is.null(RiverName) == TRUE) {
    if(Lat > 61.3 | Lat < 54.4) stop("Latitude is not in Scotland")
    if(Lon > 0 | Lon < -8) stop("Longitude is not in Scotland")
    Stations <- read.csv("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getStationList&returnfields=station_id,%20station_name,catchment_name,station_latitude,station_longitude,stationparameter_name,%20stationparameter_no&object_type=General&format=csv", sep = ";")
    #  Stations <- read.csv("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getStationList&returnfields=station_id,%20station_name,catchment_name,station_latitude,station_longitude,stationparameter_name,%20stationparameter_no&object_type=General,coverage&dateformat=yyyy-MM-dd&format=csv", sep = ";")
    LonList <- strsplit(Stations$station_longitude, split = "'")
    station_longitude <- NULL
    for(i in 1:length(LonList)) {station_longitude[i] <- as.numeric(LonList[[i]])[2]}
    Stations$station_longitude <- station_longitude
    LatLonDist <- function(lat1, lon1, lat2, lon2) {acos( sin(lat1*pi/180)*sin(lat2*pi/180) + cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180) ) * 6371000}
    Dists <- NULL
    for(i in 1:nrow(Stations)) {Dists[i] <- LatLonDist(lat1 = Lat, lon1 = Lon, lat2 = Stations$station_latitude[i], lon2 = Stations$station_longitude[i])}
    DF <- data.frame(Stations, Distance = Dists/1000)
    DF <- DF[DF$stationparameter_name == Type, ]
    DF <- DF[DF$Distance <= Range, ]
    #DF <- subset(DF, Distance <= Range)
    if(nrow(DF) == 0) stop("No gauges within range. Try increasing range")
    #DF <- subset(DF, stationparameter_name == Type)
    DF <- DF[order(DF$Distance),]
    rownames(DF) <- seq(1, nrow(DF))
    DF <- DF[,-which(colnames(DF) == "stationparameter_no")]
    return(DF)
  }


  if(is.null(StationID) == FALSE) {
    Periods <- c("15Mins", "Hourly", "Daily")
    TestPeriod <- match(Period, Periods)
    if(is.na(TestPeriod)) stop("Period must equal 15Mins, Hourly, or Daily")

    if(Type == "Flow" | Type == "Level") {
      Stations <- read.csv("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getStationList&returnfields=station_id,%20station_name,catchment_name,station_latitude,station_longitude,stationparameter_name,%20stationparameter_no&object_type=General&format=csv", sep = ";")
      Index <- which(Stations$station_id == StationID)
      if(length(Index) < 1) stop("Station ID does not appear to be in the available list of river gauges")
      #StationName <- gsub(" ", "", StationName)
      Path <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesList&station_id=", StationID, "&stationparameter_name=", Type, "&format=csv", sep = "")
      PathDates <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesList&station_id=", StationID, "&stationparameter_name=", Type, "&returnfields=coverage&dateformat=yyyy-MM-dd", "&format=csv", sep = "")
      StationDetails <- suppressWarnings(read.csv(Path, sep = ";"))
      StationDates <- suppressWarnings(read.csv(PathDates, sep = ";"))
      StationDetails <- cbind(StationDetails, StationDates)
      if(Period == "15Mins" | Period == "Hourly"){
        if(is.null(To)) {To <- StationDetails$to[which(StationDetails$ts_name == "15minute")]}
        if(is.null(From)) {From <- StationDetails$from[which(StationDetails$ts_name == "15minute")]}
        ts_id <- StationDetails$ts_id[which(StationDetails$ts_name == "15minute")]}
      if(Period == "Daily"){
        if(is.null(To)) {To <- StationDetails$to[which(StationDetails$ts_name == "Day.Mean")]}
        if(is.null(From)) {From <- StationDetails$from[which(StationDetails$ts_name == "Day.Mean")]}
        ts_id <- StationDetails$ts_id[which(StationDetails$ts_name == "Day.Mean")]}


      if(Period == "Daily") {

        Days <- as.numeric(as.Date(To) - as.Date(From))
        Path <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesValues&ts_id=", ts_id, "&from=", From,"T09:00:00&period=", "P", Days, "D", "&returnfields=Timestamp,Value&format=csv", sep = "")
        Result <- read.csv(Path, skip = 2, sep = ";")
        DateTime <- as.POSIXct(Result$X.Timestamp, format = "%Y-%m-%dT%H:%M:%S")
        Result <- data.frame(DateTime, Q = Result$Value)
      }

      #lots of data--------
      if(Period == "15Mins" | Period == "Hourly") {
        Days <- as.numeric(as.Date(To) - as.Date(From))
        if(Days > 1825) {
          GetDataFunc <- function(From, Days, ts_id) {
            Path <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesValues&ts_id=", ts_id, "&from=", From,"T09:00:00&period=", "P", Days, "D", "&returnfields=Timestamp,Value&format=csv", sep = "")
            Result <- read.csv(Path, skip = 2, sep = ";")
            DateTime <- as.POSIXct(Result$X.Timestamp, format = "%Y-%m-%dT%H:%M:%S")
            Result <- data.frame(DateTime, Q = Result$Value)
            return(Result)
          }
          Froms <- seq(as.Date(From), as.Date(To), by = "year")
          Froms[length(Froms)] <- Sys.Date()
          Days2 <- NULL
          for(i in 2:length(Froms)) {Days2[i] <- as.numeric(Froms[i] - Froms[i - 1])}
          Days2 <- Days2[-1]
          print("This may take a few minutes, please be patient")
          TestList <- list()
          for(i in 1:length(Days2)) {TestList[[i]] <- GetDataFunc(From = Froms[i], Days = Days2[i], ts_id = ts_id) }
          Result <- do.call(rbind, TestList)

        }
        #lots of data------
        if(Days <= 1825) {
          Days <- as.numeric(as.Date(To) - as.Date(From))
          Path <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesValues&ts_id=", ts_id, "&from=", From,"T09:00:00&period=", "P", Days, "D", "&returnfields=Timestamp,Value&format=csv", sep = "")
          Result <- read.csv(Path, skip = 2, sep = ";")
          DateTime <- as.POSIXct(Result$X.Timestamp, format = "%Y-%m-%dT%H:%M:%S")
          Result <- data.frame(DateTime, Q = Result$Value)

        }


        if(Period == "Hourly"){
          Result <- AggDayHour(Result, func = mean, "Hour")
        }
      }
      return(Result)
    }


  }
}


