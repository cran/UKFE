% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{LSkew}
\alias{LSkew}
\title{Linear Skewness (LSkew)}
\usage{
LSkew(x)
}
\arguments{
\item{x}{a numeric vector. The sample of interest}
}
\value{
Numeric. The LSkew of a sample.
}
\description{
Calculates the LSkew from a sample of data
}
\details{
LSkew calculated according to methods outlined by Hosking & Wallis (1997): Regional Frequency Analysis and approach based on LMoments. Also in the Flood Estimation Handbook (1999), volume 3.
}
\examples{
#Get an AMAX sample and calculate the Lmoments
AM.27051 <- GetAM(27051)
LSkew(AM.27051$Flow)
}
\author{
Anthony Hammond
}
