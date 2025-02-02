% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetData.R
\name{GetDataNRFA}
\alias{GetDataNRFA}
\title{Get National River Flow Archive data using gauge ID.}
\usage{
GetDataNRFA(ID, Type = "Q")
}
\arguments{
\item{ID}{ID number of the gauge of interest.}

\item{Type}{Type of data required. One of "Q", "P", "PQ", "Gaugings", "AMAX", "POT", or "Catalogue".}
}
\value{
A data.frame with date in the first columns and variable/s of interest in the remaining column/s.
Except for the following circumstances: When Type = "Catalogue", then a large dataframe is returned with all the NRFA gauge metadata.
When Type = "AMAX" or "POT" and there are rejected years a list is returned. Where the first element is the dataframe of data and the second is rejected year/s (character string).
}
\description{
Extracts NRFA data using the API.
}
\details{
The function can be used to get daily catchment rainfall or mean flow, or both together (concurrent). It can also be used to get gaugings, AMAX, and POT data. Note that some sites have rejected peak flow years. In which case, if Type = AMAX or POT, the function returns a list, the first element of which is the rejected years, the second is the full AMAX or POT. Lastly if Type = "Catalogue" it will return a dataframe of all the NRFA gauges, associated details, comments, and descriptors.
}
\examples{
#Get the concurrent rainfall and mean flow series for the Tay at Ballathie (site 15006).
\dontrun{BallathiePQ <-  GetDataNRFA(15006, "PQ")}
#Now we'll get the gaugings
\dontrun{BallathieGaugings <- GetDataNRFA(15006, "Gaugings") }
}
\author{
Anthony Hammond
}
