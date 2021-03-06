% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{WGaugLcv}
\alias{WGaugLcv}
\title{Gauged pool weighted linear coefficient of variation (Lcv)}
\usage{
WGaugLcv(x)
}
\arguments{
\item{x}{pooling group derived with the Pool() function}
}
\value{
the gauged weighted Lcv from a pooling group
}
\description{
Calculates the gauged weighted Lcv from a pooling group (enhanced single site)
}
\details{
Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
}
\examples{
#Get some CDs, form a gauged pooling group, and estimate gauged Lcv
CDs.96001 <- GetCDs(96001)
Pool.96001 <- Pool(CDs.96001)
WGaugLcv(Pool.96001)
}
\author{
Anthony Hammond
}
