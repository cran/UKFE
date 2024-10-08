% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{WungLcv}
\alias{WungLcv}
\title{Ungauged pool weighted linear coefficient of variation (Lcv)}
\usage{
WungLcv(x)
}
\arguments{
\item{x}{pooling group derived with the Pool() function}
}
\value{
the ungauged weighted Lcv from a pooling group
}
\description{
Calculates the ungauged weighted Lcv from a pooling group
}
\details{
Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
}
\examples{
#Get some CDs, form an ungauged pooling group, and estimate ungauged Lcv
CDs.27051 <- GetCDs(27051)
Pool.27051 <- Pool(CDs.27051, exclude = 27051)
WungLcv(Pool.27051)
}
\author{
Anthony Hammond
}
