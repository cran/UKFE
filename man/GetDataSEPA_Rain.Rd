% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetData.R
\name{GetDataSEPA_Rain}
\alias{GetDataSEPA_Rain}
\title{Get Scottish Environment Protection Agency (SEPA) hourly rainfall data.}
\usage{
GetDataSEPA_Rain(
  Lat = NULL,
  Lon = NULL,
  Range = 30,
  StationName,
  From = "2022-10-01",
  To = "ToDate"
)
}
\arguments{
\item{Lat}{Latitude of the point of interest. Provided when the user wants information about available local rain gauges}

\item{Lon}{Longitude of the point of interest. Provided when the user wants information about available local rain gauges}

\item{Range}{The radius (km) from the point of interest (Lat, Lon) for which the user wants a list of rain gauges (default is 30).}

\item{StationName}{The name of the station for which you want rainfall. If you type something other than one of the available stations, the list of stations will be returned.}

\item{From}{A start date for the data in the form of "YYYY-MM-DD".}

\item{To}{An end date for the data in the form of "YYYY-MM-DD". The default is the most recent date available.}
}
\value{
A data.frame with POSIXct in the first column, and rainfall in the second column. Unless the StationName provided is not in the available list, then the available list is returned.
}
\description{
Extract hourly rainfall data from SEPA's API.
}
\details{
If the "From" date used is significantly before (approx. 30 years) the start of the available data, the function will fail with an error message saying the connection cannot be opened. You can download data using the gauge name and you can find gauges within a given range using the latitude and longitude.
}
\examples{
#Get the list of available stations
\dontrun{ GetDataSEPA_Rain("AnythingButAStationName") }
#Now we'll get rain from the Bannockburn station
\dontrun{ Bannockburn <- GetDataSEPA_Rain("Bannockburn", From = "1998-10-01", To = "1998-10-31") }
#Now we'll have a look at the top of the data and plot it
\dontrun{ head(Bannockburn) }
\dontrun{ plot(Bannockburn, type = "h", ylab = "Rainfall (mm)") }
}
\author{
Anthony Hammond
}
