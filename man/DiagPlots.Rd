% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{DiagPlots}
\alias{DiagPlots}
\title{Diagnostic plots for pooling groups}
\usage{
DiagPlots(x, gauged = FALSE)
}
\arguments{
\item{x}{pooling group derived from the Pool() function}

\item{gauged}{logical argument with a default of FALSE. TRUE adds the top site in the pooling group to the plots in a different colour}
}
\value{
ten diagnostic plots for pooling groups
}
\description{
Provides 10 plots to compare the sites in the pooling group
}
\examples{
#Form a gauged pooling group and plot the diagnostics with gauged = TRUE
Pool.96001 <- Pool(GetCDs(96001))
DiagPlots(Pool.96001, gauged = TRUE)
#Form an ugauged pooling group and plot the diagnostics
Pool.96001 <- Pool(GetCDs(96001), exclude = 96001)
DiagPlots(Pool.96001)
}
\author{
Anthony Hammond
}
