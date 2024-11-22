% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetData.R
\name{GetDataMetOffice}
\alias{GetDataMetOffice}
\title{Get regional Met Office average temperature or rainfall series (monthly, seasonal, and annual).}
\usage{
GetDataMetOffice(Variable, Region)
}
\arguments{
\item{Variable}{Either Tmean, Rainfall, or Sunshine}

\item{Region}{One of "UK", "England", "Wales", "Scotland", "Northern_Ireland", "England_and_Wales",
"England_N", "England_S", "Scotland_N", "Scotland_E", "Scotland_W", "England_E_and_NE", "England_NW_and_N_Wales",
"Midlands", "East_Anglia", "England_SW_and_S_Wales", "England_SE_and_Central_S".}
}
\value{
A data.frame with 18 columns; year, months, seasons, and annual. Rows then represent each year of the timeseries.
}
\description{
Extracts regional mean temperature or rainfall from the met office UK & regional series. The total duration of bright sunshine is also available.
}
\details{
The function returns time series data from the 19th century through to the present month.
}
\examples{
#Get the Rainfall series for the UK
\dontrun{UKRain <-  GetDataMetOffice(Variable = "Rainfall", Region = "UK") }
#Now we'll get mean temperature data for East Anglia
\dontrun{TempEastAnglia <- GetDataMetOffice(Variable = "Tmean", Region = "East_Anglia") }
}
\author{
Anthony Hammond
}
