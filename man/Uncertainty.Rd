% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{Uncertainty}
\alias{Uncertainty}
\title{Uncertainty quantification for gauged and ungauged pooled estimates}
\usage{
Uncertainty(
  x,
  gauged = FALSE,
  RP = 100,
  dist = "GenLog",
  qmed = NULL,
  no.Donors = 2,
  UrbAdj = FALSE,
  CDs = NULL,
  conf = 0.95
)
}
\arguments{
\item{x}{the pooled group derived from the Pool() function}

\item{gauged}{a logical argument with a default of FALSE. If FALSE the uncertainty intervals are calculated for the ungauged case. If TRUE they are calculated for the gauged case}

\item{RP}{the return period of interest. Default is 100}

\item{dist}{a choice of distribution to use for the estimates. Choices are "GEV or "GenLog". The default is "GenLog"}

\item{qmed}{the QMED estimate for the ungauged case. Or for the gauged if the user wishes to override the median from the NRFA data}

\item{no.Donors}{number of donors used for estimation of QMED in the ungauged case}

\item{UrbAdj}{applies an urban adjustment to the growth curves}

\item{CDs}{catchment descriptors derived from either GetCDs or ImportCDs. Necessary if a UrbAdj is TRUE}

\item{conf}{the confidence level of the intervals for the gauged case. Default is 0.95. Must be between 0 and 1}
}
\value{
For the ungauged case a data.frame of four values relating to the lower 68 and upper 68 percent interval and the lower 95 and upper 95 percent intervals. These are headed by the associated percentiles. For the gauged case a numeric vector of two values is provided with the lower and upper intervals of the chosen conf level. The uncertainty function doesn't have a trend option; if trend is used in the pooled estimate this would need to be considered and intervals adjused accordingly. However a greater uncertainty should be considered.
}
\description{
Quantification of aleatoric uncertainty for pooling results for the gauged and ungauged case
}
\details{
Uncertainty in the ungauged case is calulated as equations 2.11 to 2.13 in Science Report – SC130009/R: 'Making better use of local data in flood frequency estimation'. The 68 percent and 95 percent intervals are returned. For the gauged case the pooled group is bootstrapped 500 times and the enhanced single site weighted linear skewness (LSkew) and linear coefficient of variation (Lcv) are calculated 500 times accordingly and 500 associated growth factors are calculated. Each  growth factor (GF) is multiplied by a randomly selected median annual maximum flow (QMED) from the uncertainty distribution of median estimates for the gauged subject site. The distribution of medians is derived from bootstrapping the gauged site 500 times. The intervals are then the upper and lower quantiles (depending on the conf input) of the distribution of median * GFs. For the gauged case the user can choose the level for the intervals. The default is 0.95. Occasionally the single site estimate will be outside the uncertainty intervals. In these cases the intervals are widened to incorporate it. i.e. if above the intervals, the upper interval is increased to the single site estimate and vice versa if below. This occurs regardless of the confidence setting. For details about the calcuations of weighted growth curves & urban adjustment see the PoolEst() function details. A trend option is not included within the Uncertainty function and would need be considered separately if used in PoolEst. An indication of the uncertainty for trend applied in PoolEst is provided in the PoolEst function details. The method and considerations of covariance between the index flood and the Lmoment ratios, as well covariance of the sites within the pooling group, are detailed in a paper soon to be submitted for publication. The reference of which will be included here once published.
}
\examples{
#Get CDs, form an ungauged pooling group and quantify the uncertainty of the
#50-year pooled estimate when using a CDs estimate of QMED with no donors
CDs.203018 <- GetCDs(203018)
Pool.203018 <- Pool(CDs.203018, exclude = 203018)
Uncertainty(Pool.203018, qmed  = QMED(CDs.203018), no.Donors = 0, RP = 50)
#Form pooling group with subject site included. Quantify the uncertainty of the
#50-year pooled estimate at the 99\% level.
 Pool.203018 <- Pool(CDs.203018)
 Uncertainty(Pool.203018, gauged = TRUE, RP = 50, conf = 0.99)
}
\author{
Anthony Hammond
}
