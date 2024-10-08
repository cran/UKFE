% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RainAPI.R
\name{GetRainEA}
\alias{GetRainEA}
\title{Get Environment Agency rainfall data (England).}
\usage{
GetRainEA(
  Lat = 54,
  Lon = -2,
  Range = 10,
  WISKI_ID = NULL,
  Period = "Daily",
  From = "2015-12-01",
  To = "2015-12-31"
)
}
\arguments{
\item{Lat}{Latitude of the point of interest. Provided when the user wants information about available local rain gauges}

\item{Lon}{Longitude of the point of interest. Provided when the user wants information about available local rain gauges}

\item{Range}{The radius (km) from the point of interest (Lat, Lon) for which the user wants rain gauge information (currently it only seems to work to just over 20km).}

\item{WISKI_ID}{The WISKI identification (as "character") for the rain gauge of interest}

\item{Period}{The sampling rate of the rainfall in hours. Either "Daily", "15Mins", "Hourly".}

\item{From}{The start date of the data extraction in the form of "YYYY-MM-DD".}

\item{To}{The end date of the data extraction in the form of "YYYY-MM-DD".}
}
\value{
If searching for rain gauge details with the Latitude and Longitude a dataframe of gauges is returned. If extracting rainfall using the WISKI_ID, a dataframe is returned with Date or POSIXct in the first columns and rainfall in the second.
}
\description{
Extract rainfall data from the Environment Agency's Hydrology Data Explorer.
}
\details{
The function provides one of two outputs. Either information about available local rain gauges, or the data from a specified gauge (specified by WISKI ID). The process is to find the local information (including WISKI ID) by using the latitude and longitude and range. Then use the WISKI ID to get the data. If data requested is not available, for example - outside the date range or not available at the requested sampling rate, an error message is returned stating "no lines available in input". Currently there is a 2 million line limit for each use. That is over 5000 years of daily data or 57 years of 15 minute data. However, the 15 minute data can take quite a while and if it takes over 60 seconds a timeout message is returned. hen testing the function I could get 20 years at a time but not 30. Presumably it depends on the connection?
}
\examples{
#Get information about available rain gauges.
#within a 10km radius of Lat = 54.5, Lon = -3.2
\dontrun{ GetRainEA(Lat = 54.5, Lon = -3.2) }
#Now we'll use the WISKI reference for the Honister rain gauge
# to get some hourly rain data for the Dec 2015 (default dates)
\dontrun{ HonisterDec2015 <- GetRainEA(WISKI_ID = "592463", Period = "Hourly") }
#Now we'll have a look at the top of the data and plot it
\dontrun{ head(HonisterDec2015) }
\dontrun{ plot(HonisterDec2015, type = "h", ylab = "Rainfall (mm)") }
}
\author{
Anthony Hammond
}
