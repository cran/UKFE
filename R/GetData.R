# GetDataSEPA---------------------------------------------------

#' Get Scottish Environment Protection Agency (SEPA) Flow, Level, or Rainfall data.
#'
#' @description Extract hydrometric data from SEPA's API.
#' @details You can download data using the gauge station ID and you can find gauges within a given range using the latitude and longitude, or a river name. If the 'From' date is left as null, the earliest date of available data will be used. If the 'To' date is left as null, the most recent date of available data will be used.
#' @param Lat Latitude of the point of interest. Provided when the user wants information about available local gauges (by specified 'Type')
#' @param Lon Longitude of the point of interest. Provided when the user wants information about available local gauges (by Specified 'Type')
#' @param Range The radius (km) from the point of interest (Lat, Lon) for which the user wants a list of gauges by specified 'Type' (default is 20).
#' @param RiverName A river name for searching gauges by river catchment. Starting with a capital letter.
#' @param Type The type of hydrometric data required. Either "Flow", "Level", or "Rain".
#' @param StationID The ID name of the station for which you want data.
#' @param From A start date for the data in the form of "YYYY-MM-DD". Default of NULL means the earliest available date is used
#' @param To An end date for the data in the form of "YYYY-MM-DD". The default is the most recent date available.
#' @param Period This argument specifies the required timestep of the data. Either "15Mins", "Hourly", or "Daily".
#' @examples
#' # search Rain gauges by Lat and Lon
#' \dontrun{
#' GetDataSEPA(Lat = 56, Lon = -4, Type = "Rain")
#' }
#'
#' # Get daily rain from the Bannockburn station between two dates
#' \dontrun{
#' bannockburn <- GetDataSEPA(
#'   StationID = "36494",
#'   From = "1998-10-01", To = "1998-10-31"
#' )
#' }
#'
#' # Inspect the first few rows and plot the data
#' \dontrun{
#' head(bannockburn)
#' plot(bannockburn, type = "h", ylab = "Rainfall (mm)")
#' }
#'
#' @return A data.frame with POSIXct in the first column, and rainfall in the second column. Unless the StationName provided is not in the available list, then the available list is returned.
#' @author Anthony Hammond

GetDataSEPA <- function(Lat = NULL, Lon = NULL, RiverName = NULL, Type = "Flow", StationID = NULL, Range = 20, From = NULL, To = NULL, Period = "Daily") {
  stationparameter_name <- NULL
  Types <- c("Flow", "Rain", "Level")
  #Types <- c("Flow", "Level")
  TypeCheck <- match(Type, Types)
  if(is.na(TypeCheck)) stop("Type must be Flow, Level, or Rain")
  if(is.null(RiverName) == FALSE) {
    Stations <- read.csv("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getStationList&returnfields=station_id,%20station_name,catchment_name,station_latitude,station_longitude,stationparameter_name,%20stationparameter_no&object_type=General&format=csv", sep = ";")
    #Stations <- subset(Stations, stationparameter_name == Type)
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
    Stations <- subset(Stations, stationparameter_name == Type)
    #  Stations <- read.csv("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getStationList&returnfields=station_id,%20station_name,catchment_name,station_latitude,station_longitude,stationparameter_name,%20stationparameter_no&object_type=General,coverage&dateformat=yyyy-MM-dd&format=csv", sep = ";")
    LonList <- strsplit(Stations$station_longitude, split = "'")
    station_longitude <- NULL
    for(i in 1:length(LonList)) {station_longitude[i] <- as.numeric(LonList[[i]])[2]}
    Stations$station_longitude <- as.numeric(station_longitude)
    Stations$station_latitude <- as.numeric(Stations$station_latitude)
    LatLonDist <- function(lat1, lon1, lat2, lon2) {acos( sin(lat1*pi/180)*sin(lat2*pi/180) + cos(lat1*pi/180)*cos(lat2*pi/180)*cos(lon2*pi/180-lon1*pi/180) ) * 6371000}
    Dists <- NULL
    for(i in 1:nrow(Stations)) {Dists[i] <- LatLonDist(lat1 = Lat, lon1 = Lon, lat2 = Stations$station_latitude[i], lon2 = Stations$station_longitude[i])}
    DF <- data.frame(Stations, Distance = Dists/1000)
    #DF <- DF[DF$stationparameter_name == Type, ]
    DF <- DF[DF$Distance <= Range, ]
    #DF <- subset(DF, Distance <= Range)
    if(nrow(DF) == 0) stop("No gauges within range. Try increasing range")
    DF <- subset(DF, stationparameter_name == Type)
    DF <- DF[order(DF$Distance),]
    rownames(DF) <- seq(1, nrow(DF))
    DF <- DF[,-which(colnames(DF) == "stationparameter_no")]
    return(DF)
  }


  if(is.null(StationID) == FALSE) {
    Periods <- c("15Mins", "Hourly", "Daily")
    TestPeriod <- match(Period, Periods)
    if(is.na(TestPeriod)) stop("Period must equal 15Mins, Hourly, or Daily")

    if(Type == "Flow" | Type == "Level" | Type == "Rain") {
      Stations <- read.csv("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getStationList&returnfields=station_id,%20station_name,catchment_name,station_latitude,station_longitude,stationparameter_name,%20stationparameter_no&object_type=General&format=csv", sep = ";")
      Stations <- subset(Stations, stationparameter_name == Type)
      Index <- which(Stations$station_id == StationID)
      if(length(Index) < 1) stop("Station ID does not appear to be in the available list of gauges. Make sure you have the correct 'Type' of gauge specified and that there is no mistake in the ID")
      #StationName <- gsub(" ", "", StationName)
      Path <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesList&station_id=", StationID, "&stationparameter_name=", Type, "&format=csv", sep = "")
      PathDates <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesList&station_id=", StationID, "&stationparameter_name=", Type, "&returnfields=coverage&dateformat=yyyy-MM-dd", "&format=csv", sep = "")
      StationDetails <- suppressWarnings(read.csv(Path, sep = ";"))
      StationDates <- suppressWarnings(read.csv(PathDates, sep = ";"))
      StationDetails <- cbind(StationDetails, StationDates)
      if(Period == "15Mins" | Period == "Hourly"){
        if(is.null(To)) {To <- StationDetails$to[grep("15minute", StationDetails$ts_name)]}
        if(is.null(From)) {From <- StationDetails$from[grep("15minute", StationDetails$ts_name)]}
        ts_id <- StationDetails$ts_id[grep("15minute", StationDetails$ts_name)]}
      if(Period == "Daily"){
        if(is.null(To)) {To <- StationDetails$to[grep("Day", StationDetails$ts_name)]}
        if(is.null(From)) {From <- StationDetails$from[grep("Day", StationDetails$ts_name)]}
        ts_id <- StationDetails$ts_id[grep("Day",StationDetails$ts_name)]}


      if(Period == "Daily") {

        Days <- as.numeric(as.Date(To) - as.Date(From))
        Path <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesValues&ts_id=", ts_id, "&from=", From,"T09:00:00&period=", "P", Days, "D", "&returnfields=Timestamp,Value&format=csv", sep = "")
        Result <- read.csv(Path, skip = 2, sep = ";")
        DateTime <- as.POSIXct(Result$X.Timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
        Result <- data.frame(DateTime, Q = Result$Value)
      }

      #lots of data--------
      if(Period == "15Mins" | Period == "Hourly") {
        Days <- as.numeric(as.Date(To) - as.Date(From))
        if(Days > 1825) {
          GetDataFunc <- function(From, Days, ts_id) {
            Path <- paste("https://timeseries.sepa.org.uk/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=0&request=getTimeseriesValues&ts_id=", ts_id, "&from=", From,"T09:00:00&period=", "P", Days, "D", "&returnfields=Timestamp,Value&format=csv", sep = "")
            Result <- read.csv(Path, skip = 2, sep = ";")
            DateTime <- as.POSIXct(Result$X.Timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
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
          DateTime <- as.POSIXct(Result$X.Timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
          Result <- data.frame(DateTime, Q = Result$Value)

        }


        if(Period == "Hourly"){
          if(Type == "Rain") {Result <- AggDayHour(Result, func = sum, "Hour")} else {
            Result <- AggDayHour(Result, func = mean, "Hour")}
        }
      }
      if(Type == "Level") {colnames(Result)[2] <- "Stage"}
      if(Type == "Rain") {colnames(Result)[2] <- "P"}
      return(Result)
    }


  }
}



# GetDataEA_Rain ---------------------------------------------------

#' Get Environment Agency rainfall data (England).
#'
#' @description Extract rainfall data from the Environment Agency's Hydrology Data Explorer.
#' @details The function provides one of two outputs. Either information about available local rain gauges, or the data from a specified gauge (specified by WISKI ID). The process is to find the local information (including WISKI ID) by using the latitude and longitude and range (You can convert BNG to Lat and Lon using the GridRefConvert function). Then use the WISKI ID to get the data. If data requested is not available, for example - outside the date range or not available at the requested sampling rate, an error message is returned stating "no lines available in input". To extract all the available data leave the From and To arguments as Null.
#' @param Lat Latitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#' @param Lon Longitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#' @param Range The radius (km) from the point of interest (Lat, Lon) for which the user wants rain gauge information (currently works to a maximum of just over 20km).
#' @param WISKI_ID The WISKI identification (as "character") for the rain gauge of interest
#' @param Period The sampling rate of the rainfall in hours. Either "Daily", "15Mins", "Hourly".
#' @param From The start date of the data extraction in the form of "YYYY-MM-DD". To get data from the first date available leave as NULL.
#' @param To The end date of the data extraction in the form of "YYYY-MM-DD". To get data from the first date available leave as NULL.
#' @examples
#' # Get information about available rain gauges
#' # within a 10km radius of latitude 54.5 and longitude -3.2
#' \dontrun{
#' GetDataEA_Rain(Lat = 54.5, Lon = -3.2)
#' }
#'
#' # Use the WISKI reference ID for the Honister rain gauge
#' # to get some hourly rain data for December 2015
#' \dontrun{
#' honister_dec_2015 <- GetDataEA_Rain(
#'   WISKI_ID = "592463",
#'   Period = "Hourly", From = "2015-12-01", To = "2015-12-31"
#' )
#' }
#'
#' # Inspect the first few rows and plot the data
#' \dontrun{
#' head(honister_dec_2015)
#' plot(honister_dec_2015, type = "h", ylab = "Rainfall (mm)")
#' }
#'
#' @return If searching for rain gauge details with the Latitude and Longitude a dataframe of gauges is returned. If extracting rainfall using the WISKI_ID, a dataframe is returned with Date / POSIXct in the first columns and rainfall in the second.
#' @author Anthony Hammond
GetDataEA_Rain <- function(Lat = 54, Lon = -2, Range = 10, WISKI_ID = NULL, Period = "Daily", From = NULL, To = NULL) {
  if (is.null(From)) {
    From <- "1901-10-01"
  }
  if (is.null(To)) {
    To <- Sys.Date()
  }
  if (as.Date(To) > Sys.Date()) stop("To is in the future. This tool is for extracting observed data, not predicting it :)")
  if (as.Date(From) > as.Date(To)) stop("The From date is after the To date")
  if (is.null(WISKI_ID)) {
    if (Range > 20) warning("You have put a range greater than 20km. Not sure why at the moment but it seems to be capped at just over 20km. Hopefully this will be fixed soon.")
    range <- as.character(Range)
    RainStations <- read.csv(paste("https://environment.data.gov.uk/hydrology/id/stations.csv?lat=", Lat, "&long=", Lon, "&dist=", range, sep = ""))
    LatLonDist <- function(lat1, lon1, lat2, lon2) {
      acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos(lon2 * pi / 180 - lon1 * pi / 180)) * 6371000
    }
    Distance <- NULL
    for (i in 1:nrow(RainStations)) {
      Distance[i] <- signif(LatLonDist(Lat, Lon, RainStations$lat[i], RainStations$long[i]) / 1000, 4)
    }
    Type <- RainStations$observedProperty
    SplitText <- strsplit(Type, "/")
    Type <- NULL
    for (i in 1:length(SplitText)) {
      Type[i] <- SplitText[[i]][length(SplitText[[i]])]
    }
    wiskiID <- RainStations$wiskiID
    Station <- RainStations$label
    Easting <- RainStations$easting
    Northing <- RainStations$northing
    Latitude <- RainStations$lat
    Longitude <- RainStations$long
    DateOpened <- RainStations$dateOpened
    DateClosed <- RainStations$dateClosed
    Stations <- data.frame(wiskiID, Station, Easting, Northing, Type, DateOpened, DateClosed, Distance)
    Stations <- Stations[which(Stations$Type == "rainfall"), ]
    if (nrow(Stations) == 0) stop("There are no gauges within the specified range")
    return(Stations)
  }
  if (is.null(WISKI_ID) == FALSE) {
    WISKI_ID <- as.character(WISKI_ID)
    PeriodOptions <- c("Daily", "15Mins", "Hourly")
    PeriodCheck <- match(Period, PeriodOptions)
    if (is.na(PeriodCheck)) stop("Period must be one of Daily, 15Mins, or Hourly")



    # Here is a separate Get P function. Then we need a loop one for all the 15 minute data

    GetP <- function(WISKI_ID, From = NULL, To = NULL, Period = "Daily") {
      # WISKI_ID <- as.character(WISKI_ID)
      Periods <- c("Daily", "15Mins", "Hourly")
      if (is.na(match(Period, Periods))) stop("Period must be one of Daily, 15Mins, or Hourly")

      WID <- as.character(WISKI_ID)
      Path <- paste("https://environment.data.gov.uk/hydrology/id/stations.csv?wiskiID=", WID, "&observedProperty=rainfall", sep = "")

      StationInfo <- try(suppressWarnings(suppressMessages(read.csv(Path))), silent = TRUE)

      if (inherits(StationInfo, "try-error")) {
        WID = paste("0", WID, sep = "")
        Path <- paste("https://environment.data.gov.uk/hydrology/id/stations.csv?wiskiID=", WID, "&observedProperty=rainfall", sep = "")
        StationInfo <- read.csv(Path)
      }


      if (Period == "Daily") {
        PeriodPath <- "86400"
      }
      if (Period == "15Mins" | Period == "Hourly") {
        PeriodPath <- "900"
      }

      IDPath <- StationInfo$stationGuid
      Data <- read.csv(paste("https://environment.data.gov.uk/hydrology/id/measures/", IDPath, "-rainfall-t-", PeriodPath, "-mm-qualified/readings.csv?_limit=2000000&mineq-date=", From, "&maxeq-date=", To, sep = ""))
      DateTime <- as.POSIXct(Data$dateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
      Result <- data.frame(DateTime, P = Data$value)
      return(Result)
    }


    if (Period == "15Mins" | Period == "Hourly") {
      if (as.numeric(as.Date(To) - as.Date(From)) > 1825) {
        print("This may take some time (up to 60 seconds per ten years of data), please be patient")
        Froms <- seq(as.Date(From), as.Date(To), by = "year")
        Froms[length(Froms)] <- To
        # print("This may take a few minutes, please be patient")
        xList <- list()
        for (i in 1:(length(Froms) - 1)) {
          xList[[i]] <- try(GetP(WISKI_ID = WISKI_ID, From = Froms[i], To = Froms[i + 1], Period = "15Mins"), silent = TRUE)
        }
        IndexNoData <- NULL
        for (i in 1:length(xList)) {
          IndexNoData[i] <- class(xList[[i]])
        }
        IndexData <- which(IndexNoData != "try-error")
        if (length(IndexData) == 0) stop("No data available for the WISKI ID used")
        xListResults <- list()
        for (i in 1:length(IndexData)) {
          xListResults[[i]] <- xList[[IndexData[i]]]
        }
        Result <- do.call(rbind, xListResults)
      }
      if (as.numeric(as.Date(To) - as.Date(From)) <= 1825) {
        Result <- GetP(WISKI_ID = WISKI_ID, From = From, To = To, Period = "15Mins")
      }
      if (Period == "Hourly") {
        Result <- AggDayHour(Result, func = sum, Freq = "Hour")
      }
    }

    if (Period == "Daily") {
      Result <- GetP(WISKI_ID = WISKI_ID, From = From, To = To)
    }

    return(Result)
  }
}



# GetDataMetOffice ---------------------------------------------------

#' Get regional Met Office average temperature or rainfall series (monthly, seasonal, and annual).
#'
#' @description Extracts regional mean temperature or rainfall from the Met Office UK & regional series. The total duration of bright sunshine is also available.
#' @details The function returns time series data from the 19th century through to the present month.
#' @param Variable Either Tmean, Rainfall, or Sunshine
#' @param Region One of "UK", "England", "Wales", "Scotland", "Northern_Ireland", "England_and_Wales",
#' "England_N", "England_S", "Scotland_N", "Scotland_E", "Scotland_W", "England_E_and_NE", "England_NW_and_N_Wales",
#' "Midlands", "East_Anglia", "England_SW_and_S_Wales", "England_SE_and_Central_S".
#' @examples
#' # Get the rainfall time series for the UK
#' \dontrun{
#' uk_rain <- GetDataMetOffice(Variable = "Rainfall", Region = "UK")
#' }
#'
#' # Get the mean temperature data for East Anglia
#' \dontrun{
#' temp_east_anglia <- GetDataMetOffice(Variable = "Tmean", Region = "East_Anglia")
#' }
#'
#' @return A data.frame with 18 columns; year, months, seasons, and annual. Rows then represent each year of the timeseries.
#' @author Anthony Hammond

GetDataMetOffice <- function(Variable, Region) {
  Variable <- as.character(Variable)
  Region <- as.character(Region)
  MetRegions <- c(
    "UK", "England", "Wales", "Scotland", "Northern_Ireland", "England_and_Wales",
    "England_N", "England_S", "Scotland_N", "Scotland_E", "Scotland_W", "England_E_and_NE", "England_NW_and_N_Wales",
    "Midlands", "East_Anglia", "England_SW_and_S_Wales", "England_SE_and_Central_S"
  )
  MatchRegion <- match(Region, MetRegions)
  if (is.na(MatchRegion)) {
    print("Region must be one of the following:")
    print(MetRegions)
    stop()
  }
  if (Variable != "Tmean" & Variable != "Rainfall" & Variable != "Sunshine") stop("Variable must equal Tmean, Rainfall, or Sunshine")
  Path <- paste("https://www.metoffice.gov.uk/pub/data/weather/uk/climate/datasets/", Variable, "/date/", Region, ".txt", sep = "")
  Data <- read.table(Path, header = TRUE, skip = 5, fill = TRUE)
  Data[1, 14] <- "NA"
  if (as.POSIXlt(Sys.Date())$mon > 0) {
    Data[nrow(Data), (as.POSIXlt(Sys.Date())$mon + 2):ncol(Data)] <- NA
  }
  Data[, 14] <- suppressWarnings(as.numeric(Data[, 14]))
  return(Data)
}




# GetDataNRFA ---------------------------------------------------

#' Get National River Flow Archive data using gauge ID.
#'
#' @description Extracts NRFA data using the API.
#' @details The function can be used to get daily catchment rainfall or mean flow, or both together (concurrent). It can also be used to get gaugings, AMAX, and POT data. Note that some sites have rejected peak flow years. In which case, if Type = AMAX or POT, the function returns a list, the first element of which is the rejected years, the second is the full AMAX or POT. Lastly if Type = "Catalogue" and ID  is NULL, it will return a dataframe of all the NRFA gauges, associated details, comments, and descriptors. If Type equals "Catalogue" and a valid ID is used, then all these gauge details are provided for that gauge.
#' @param ID ID number of the gauge of interest.
#' @param Type Type of data required. One of "Q", "P", "PQ", "Gaugings", "AMAX", "POT", or "Catalogue".
#' @examples
#' # Get the concurrent rainfall (P) and mean flow (Q) series for the Tay at Ballathie (site 15006)
#' \dontrun{
#' ballathie_pq <- GetDataNRFA(15006, Type = "PQ")
#' }
#'
#' # Get the gaugings
#' \dontrun{
#' ballathie_gaugings <- GetDataNRFA(15006, Type = "Gaugings")
#' }
#'
#' @return A data.frame with date in the first columns and variable/s of interest in the remaining column/s.
#' Except for the following circumstances: When Type = "Catalogue", then a large dataframe is returned with all the NRFA gauge metadata.
#' When Type = "AMAX" or "POT" and there are rejected years a list is returned, where the first element is the dataframe of data and the second is rejected year/s (character string).
#' @author Anthony Hammond

GetDataNRFA <- function(ID = NULL, Type = "Q") {
  Types <- c("Q", "P", "PQ", "Gaugings", "AMAX", "POT", "Catalogue")
  MatchType <- match(Type, Types)
  if (is.na(MatchType)) stop("Type must be one of Q, P, PQ, Gaugings, AMAX, POT, or Catalogue")

  PQnrfa <- function(ID, type = "Both") {
    CDRfunc <- function(ID) {
      ID <- as.character(ID)
      Path <- paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=cdr&station=", ID, sep = "")
      Data <- read.csv(Path)
      Data <- Data[20:nrow(Data), ]
      Date <- as.Date(Data$file)
      P <- as.numeric(Data$timestamp)
      PData <- data.frame(Date, P)
      return(PData)
    }

    GDFfunc <- function(ID) {
      ID <- as.character(ID)
      Path <- paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=gdf&station=", ID, sep = "")
      Data <- read.csv(Path)
      Data <- Data[20:nrow(Data), ]
      Date <- as.Date(Data$file)
      Q <- as.numeric(Data$timestamp)
      QData <- data.frame(Date, Q)
      return(QData)
    }

    P <- CDRfunc(ID)
    Q <- GDFfunc(ID)

    Concurrents <- function(x, y) {
      Index1 <- x$Date %in% y$Date
      Index2 <- y$Date %in% x$Date
      x.Var <- x[Index1, ]
      y.Var <- y[Index2, 2]
      Comb <- data.frame(x.Var, y.Var)
      colnames(Comb) <- c("Date", "x", "y")
      return(Comb)
    }

    if (type == "Both") {
      Res <- Concurrents(P, Q)
      colnames(Res) <- c("Date", "P", "Q")
    }
    if (type == "CDR" | type == "cdr") {
      Res <- P
    }
    if (type == "GDF" | type == "gdf") {
      Res <- Q
    }
    return(Res)
  }

  NRFAGaugings <- function(ID) {
    ID <- as.character(ID)
    Q <- read.csv(paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=gauging-flow&station=", ID, sep = ""), skip = 19)[, 1:2]
    H <- read.csv(paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=gauging-stage&station=", ID, sep = ""), skip = 19)[, 1:2]
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
    if (AllCat$nrfa.peak.flow[Index] == "false") stop("No available peak flows at this site")
    rejYears <- AllCat$peak.flow.rejected.amax.years[Index]
    if (rejYears == "") {
      rejYears <- NA
    }
    ID <- as.character(ID)
    Path <- paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=amax-flow&station=", ID, sep = "")
    Data <- read.csv(Path)
    Data <- Data[21:nrow(Data), ]
    Date <- as.Date(Data$file)
    Q <- as.numeric(Data$timestamp)
    QData <- data.frame(Date, Q)
    if (is.na(rejYears)) {
      return(QData)
    }
    if (is.na(rejYears) == FALSE) {
      ResultList <- list(QData, rejYears)
      names(ResultList) <- c("AMAX", "Rejected hydrological years")
      return(ResultList)
    }
  }

  POTfunc <- function(ID) {
    ID <- as.character(ID)
    AllCat <- GetDataNRFA(Type = "Catalogue")
    Index <- which(AllCat$id == ID)
    if (AllCat$nrfa.peak.flow[Index] == "false") stop("No available peak flows at this site")
    rejYears <- AllCat$peak.flow.rejected.amax.years[Index]
    if (rejYears == "") {
      rejYears <- NA
    }
    Path <- paste("https://nrfaapps.ceh.ac.uk/nrfa/ws/time-series?format=nrfa-csv&data-type=pot-flow&station=", ID, sep = "")
    Data <- read.csv(Path)
    Data <- Data[21:nrow(Data), ]
    Date <- as.Date(Data$file)
    Q <- as.numeric(Data$timestamp)
    QData <- data.frame(Date, Q)
    if (is.na(rejYears)) {
      return(QData)
    }
    if (is.na(rejYears) == FALSE) {
      ResultList <- list(QData, rejYears)
      names(ResultList) <- c("POT", "Rejected hydrological years")
      return(ResultList)
    }
  }

  if (Type == "Q") {
    Result <- PQnrfa(ID, type = "gdf")
  }
  if (Type == "P") {
    Result <- PQnrfa(ID, type = "cdr")
  }
  if (Type == "PQ") {
    Result <- PQnrfa(ID, type = "Both")
  }
  if (Type == "Gaugings") {
    Result <- NRFAGaugings(ID)
  }
  if (Type == "Catalogue") {
    Result <- read.csv("https://nrfaapps.ceh.ac.uk/nrfa/ws/station-info?station=*&format=csv&fields=all")
  if(is.null(ID) == FALSE) {
    Result <- Result[Result$id == ID,]
    row.names(Result) <- 1
    }
    }
  if (Type == "AMAX") {
    Result <- AMAXfunc(ID)
  }
  if (Type == "POT") {
    Result <- POTfunc(ID)
  }
  return(Result)
}


# GetDataEA_QH ---------------------------------------------------

#' Get flow or level data from the Environment Agency's Hydrology Data Explorer
#'
#' @description Function to extract flow or level data from the Environment Agency's Hydrology Data Explorer.
#' @details To find gauges you can input either a river name or a latitude and longitude. You can convert BNG to Lat and Lon using the ConvertGridRef function (you can also get lat and lon by left clicking on Google maps). The lat and lon option will provide all flow and level gauges within a specified range (default of 10km). This provides gauged details including the WISKI ID. You can get data from specific gauges using the WISKI_ID. Note that flow gauges also have level data available. You can get data from a date range using the From and To arguments or you can return all data by leaving the From and To as the default (NULL). Lastly, WISKI IDs are sometimes returned without a preceding 0 which might be necessary for the data extraction (oddly, most do have the necessary 0). If data extraction fails try adding a 0 to the beginning of the WISKI ID.
#' @param Lat Latitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#' @param Lon Longitude (as a decimal) for the centre of the search for gauges. You can convert BNG to Lat and Lon using the GridRefConvert function.
#' @param Range Radius of search when using latitude and longitude inputs (km).
#' @param RiverName Name of the river along which you want to search for gauges. Character string.
#' @param WISKI_ID The WISKI ID for the gauge from which you want to obtain data (character string). Note that sometimes a preceding zero, which is not returned via the API, is needed. If the data extraction fails, this may be the cause and you can resolve it by including the preceding zero in the WISKI_ID.
#' @param From Date for start of data extraction in the format of "2015-12-02". If NULL the start date of the available data is used.
#' @param To Date for the end of data extraction in the format of "2015-12-02". If NULL end date of the available data is used.
#' @param Type The variable to extract, either "flow" or "level"
#' @param Period The sampling rate of the data you want. Either "DailyMax", "DailyMean", "Hourly", "15Mins".
#' @examples
#' # Find gauges on the River Tame
#' \dontrun{
#' GetDataEA_QH(RiverName = "Tame")
#' }
#'
#' # Find gauges within 10km of a latitude/longitude coordinate somewhere near the
#' # centre of Dartmoor
#' \dontrun{
#' GetDataEA_QH(Lat = 50.6, Lon = -3.9, Range = 10)
#' }
#'
#' # Get all available daily maximum flow data from the Bellever gauge on the
#' # East Dart River
#' \dontrun{
#' bellever_max <- GetDataEA_QH(WISKI_ID = "SX67F051", Period = "DailyMax")
#' }
#'
#' # Get 15-minute data from the Bellever for the November 2024 event
#' \dontrun{
#' bellever_nov_2024 <- GetDataEA_QH(
#'   WISKI_ID = "SX67F051",
#'   From = "2024-11-23", To = "2024-11-25", Period = "15Mins"
#' )
#' }
#'
#' @return If searching for gauge details with lat and lon or river name, then a list is returned. The first element is a dataframe with flow gauge details and the second is a dataframe of level gauge details.
#' When extracting flow or level data with a WISKI ID then a dataframe with two columns is returned. The first being a Date or POSIXct column/vector and the second is the timeseries of interest.
#' @author Anthony Hammond

GetDataEA_QH <- function(Lat = 54, Lon = -2.25, Range = 20, RiverName = NULL, WISKI_ID = NULL, From = NULL, To = NULL, Type = "flow", Period = "DailyMean") {
  if (is.null(From)) {
    From <- "1901-10-01"
  }
  if (is.null(To)) {
    To <- Sys.Date()
  }
  if (as.Date(To) > Sys.Date()) stop("To is in the future. This tool is for extracting observed data, not predicting it :)")
  if (as.Date(From) > as.Date(To)) stop("The From date is after the To date")

  if (is.null(WISKI_ID)) {
    if (is.null(RiverName)) {
      if (Lat > 55.9 | Lat < 49.94) stop("Grid reference appears to be outside England")
      if (Lon > 1.87 | Lon < -5.77) stop("Grid reference appears to be outside England")
      RangePath <- paste("https://environment.data.gov.uk/hydrology/id/stations.csv?lat=", Lat, "&long=", Lon, "&dist=", Range, sep = "")
      StationsWithinRange <- read.csv(RangePath)
      Observed_Property <- strsplit(StationsWithinRange$observedProperty, "/")
      FlowGrepFunc <- function(x, Var = "waterFlow") {
        Result <- grep(Var, x)
        if (length(Result) >= 1) {
          Result <- 1
        } else {
          Result <- 0
        }
        return(Result)
      }
      FlowsGrep <- NULL
      for (i in 1:length(Observed_Property)) {
        FlowsGrep[i] <- FlowGrepFunc(Observed_Property[[i]])
      }
      LevelGrep <- NULL
      for (i in 1:length(Observed_Property)) {
        LevelGrep[i] <- FlowGrepFunc(Var = "waterLevel", Observed_Property[[i]])
      }
      GWCheck <- NULL
      for (i in 1:length(Observed_Property)) {
        GWCheck[i] <- FlowGrepFunc(Var = "groundwaterLevel", Observed_Property[[i]])
      }
      GWIndex <- which(GWCheck == 1)
      LevelGrep[GWIndex] <- 0

      FlowIndex <- which(FlowsGrep == 1)
      LevelIndex <- which(LevelGrep == 1)
      FlowStations <- StationsWithinRange[FlowIndex, c(3, 14, 5, 6, 10)]
      LevelStations <- StationsWithinRange[LevelIndex, c(3, 14, 5, 6, 10)]

      MatchFlow <- match(FlowStations$wiskiID, LevelStations$wiskiID)
      MatchFlow <- MatchFlow[!is.na(MatchFlow)]
      ResultList <- list(FlowStations, LevelStations[-MatchFlow, ])
      RowTestQ <- nrow(ResultList[[1]])
      RowTestH <- nrow(ResultList[[2]])
      if (RowTestQ == 0 & RowTestH == 0) stop("There are no flow gauges or level only gauges within the specified range")
      if (RowTestQ == 0) print("It appears no flow gauges are available within the search radius")
      if (RowTestH == 0) print("It appears no 'level only' gauges are available within the search radius")
      if (nrow(ResultList[[2]]) == 0) {
        ResultList[[2]] <- data.frame(label = NA, wiskiID = NA, easting = NA, Northing = NA, riverName = NA)
        print("There are no level only gauges within the range. However, Water level data can be obtained from the flow gauges returned.")
      }
      rownames(ResultList[[1]]) <- seq(1, nrow(ResultList[[1]]))
      rownames(ResultList[[2]]) <- seq(1, nrow(ResultList[[2]]))
      names(ResultList) <- c("Flow", "Water level")

      print("In some cases the WISKI_ID requires a preceding '0' which is not returned by the API. If the WISKI_ID fails, try it with a 0 in front")
      return(ResultList)
    }
    if (is.null(RiverName) == FALSE) {
      FlowPath <- paste("https://environment.data.gov.uk/hydrology/id/stations.csv?observedProperty=waterFlow&riverName=River+", RiverName, sep = "")
      StagePath <- paste("https://environment.data.gov.uk/hydrology/id/stations.csv?observedProperty=waterLevel&riverName=River+", RiverName, sep = "")
      FlowStations <- read.csv(FlowPath)
      LevelStations <- read.csv(StagePath)
      FlowStations <- FlowStations[, c(3, 14, 5, 6, 10)]
      LevelStations <- LevelStations[, c(3, 14, 5, 6, 10)]
      MatchFlow <- match(FlowStations$wiskiID, LevelStations$wiskiID)
      MatchFlow <- MatchFlow[!is.na(MatchFlow)]
      ResultList <- list(FlowStations, LevelStations[-MatchFlow, ])
      if (nrow(ResultList[[2]]) == 0) {
        ResultList[[2]] <- data.frame(label = NA, wiskiID = NA, easting = NA, Northing = NA, riverName = NA)
        print("There are no level only gauges within the range. However, Water level data can be obtained from the flow gauges returned.")
      }
      rownames(ResultList[[1]]) <- seq(1, nrow(ResultList[[1]]))
      rownames(ResultList[[2]]) <- seq(1, nrow(ResultList[[2]]))
      names(ResultList) <- c("Flow", "Water level")
      RowTestQ <- nrow(ResultList[[1]])
      RowTestH <- nrow(ResultList[[2]])
      if (RowTestQ == 0) print("It appears no flow gauges are available on the river you searched")
      if (RowTestH == 0) print("It appears no level-only gauges are available on the river you searched")
      print("In some cases the WISKI_ID requires a preceding '0' which is not returned by the API. If the WISKI_ID fails try it with a 0 in front")
      return(ResultList)
    }
  }

  if (is.null(WISKI_ID) == FALSE) {
    # Here is a separate Get q or H function. Then we need a loop one for all the 15 minute data
    if(Type == "level" & Period == "DailyMean") warning("DailyMean is not generally available for the level data.")
    GetQH <- function(WISKI_ID, From = NULL, To = NULL, Period = "DailyMax", Type = "flow") {
      WISKI_ID <- as.character(WISKI_ID)
      Periods <- c("DailyMax", "DailyMean", "15Mins", "Hourly")
      if (is.na(match(Period, Periods))) stop("Period must be one of DailyMax, DailyMean, 15Mins, or Hourly. Note that level gauges don't tend to have a DailyMean option")
      if (Type != "flow" & Type != "level") stop("Type must equal level or flow")
      if (Type == "flow") {
        Unit <- "-m3s"
      }
      if (Type == "level") {
        Unit <- "-m"
      }
      if (Period == "DailyMax") {
        PeriodPath <- "max-86400"
      }
      if (Period == "DailyMean") {
        PeriodPath <- "m-86400"
      }
      if (Period == "15Mins" | Period == "Hourly") {
        PeriodPath <- "i-900"
      }

      IDPath <- read.csv(paste("https://environment.data.gov.uk/hydrology/id/stations.csv?wiskiID=", WISKI_ID, sep = ""))
      TypeCheck <- grep(Type, IDPath$measures.parameter)
      if (length(TypeCheck) == 0) stop("Your Type is not available for the specified WISKI_ID and Period")
      ID <- IDPath$notation
      Data <- read.csv(paste("https://environment.data.gov.uk/hydrology/id/measures/", ID, "-", Type, "-", PeriodPath, Unit, "-qualified/readings.csv?_limit=2000000&mineq-date=", From, "&maxeq-date=", To, sep = ""))
      if (PeriodPath == "max-86400" | PeriodPath == "m-86400") {
        Data <- Data[, 3:4]
        Data$date <- as.Date(Data$date)
      }
      if (PeriodPath == "i-900") {
        Data <- Data[, c(2, 4)]
        Data$dateTime <- as.POSIXct(Data$dateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
        if (Period == "Hourly") {
          Data <- AggDayHour(Data, func = mean, Freq = "Hour")
          colnames(Data) <- c("dateTime", "value")
        }
      }
      return(Data)
    }

    if (Period == "15Mins" | Period == "Hourly") {
      if (Period == "15Mins" | Period == "Hourly") {
        if (as.numeric(as.Date(To) - as.Date(From)) > 1825) {
          print("This may take some time (up to 60 seconds per ten years of data), please be patient")
          Froms <- seq(as.Date(From), as.Date(To), by = "year")
          Froms[length(Froms)] <- To
          # print("This may take a few minutes, please be patient")
          xList <- list()
          for (i in 1:(length(Froms) - 1)) {
            xList[[i]] <- try(GetQH(WISKI_ID = WISKI_ID, From = Froms[i], To = Froms[i + 1], Period = "15Mins", Type = Type), silent = TRUE)
          }
          IndexNoData <- NULL
          for (i in 1:length(xList)) {
            IndexNoData[i] <- class(xList[[i]])
          }
          IndexData <- which(IndexNoData != "try-error")
          if (length(IndexData) == 0) stop("No data available for the WISKI ID used. Check you have the correct Type; flow or level")
          xListResults <- list()
          for (i in 1:length(IndexData)) {
            xListResults[[i]] <- xList[[IndexData[i]]]
          }
          Result <- do.call(rbind, xListResults)
        }
        if (as.numeric(as.Date(To) - as.Date(From)) <= 1825) {
          Result <- GetQH(WISKI_ID = WISKI_ID, From = From, To = To, Period = "15Mins", Type = Type)
        }
      }
    }
    if (Period == "DailyMax" | Period == "DailyMean") {
      Result <- GetQH(WISKI_ID = WISKI_ID, From = From, To = To, Period = Period, Type = Type)
    }
    if (Period == "Hourly") {
      Result <- AggDayHour(Result, func = mean, Freq = "Hour")
    }
    return(Result)
  }
}
