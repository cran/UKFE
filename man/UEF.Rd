% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{UEF}
\alias{UEF}
\title{Urban expansion factor}
\usage{
UEF(Year)
}
\arguments{
\item{Year}{The year for consideration. Numeric}
}
\value{
A numeric urban expansion factor.
}
\description{
This function provides a coefficient to multiply by URBEXT2000 to adjust it to a given year
}
\details{
The urban expansion factor is detailed in Bayliss, A. Black, K. Fava-Verde, A. Kjeldsen, T. (2006). URBEXT2000 - A new FEH catchment descriptor: Calculation, dissemination and application. R&D Technical Report FD1919/TR, DEFRA, CEH Wallingford
}
\examples{
# Get an expansion factor for the year 2023
UEF(2023)
}
\author{
Anthony Hammond
}
