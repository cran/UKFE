% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{LRatioChange}
\alias{LRatioChange}
\title{Adjust L-Ratios in a pooling group}
\usage{
LRatioChange(x, SiteID, lcv, lskew)
}
\arguments{
\item{x}{pooling group derived with the Pool function}

\item{SiteID}{the identification number of the site in the pooling group that is to be changed (character or integer)}

\item{lcv}{The user supplied Lcv. numeric}

\item{lskew}{The user supplied LSkew. numeric}
}
\value{
A new pooling group, the same as x except for the user adjusted Lcv and Lskew for the user selected site.
}
\description{
Adjusts the linear coefficient of variation (Lcv) and the linear skewness (LSkew) for a chosen site in a pooling group
}
\details{
Pooling groups are formed from the NRFAData data.frame and all the Lcv and LSkew values are precalculated using the National River Flow Archive Peak flow dataset noted in the description file. The resulting pooled growth curve is calculated using the Lcv and Lskew in the pooled group. The user may have further data and be able to add further peak flows to the annual maximum samples within a pooling group. If that is the case a new Lcv and Lskew can be determined using the Lmoms function. These new values can be added to the pooling group with this LRatioChange function. Also the permeable adjustment function may have been applied to a site, which provides a new Lcv and LSkew. In which case, the LRatioChange function can be applied. The function creates a new pooling group object and x will still exist in it's original state after the function is applied.
}
\examples{
# Get some catchment descriptors and create a pooling group.
CDs.39001 <- GetCDs(39001)
Pool.39001 <- Pool(CDs.39001, iug = TRUE)
# apply the function to create a new adjusted pooling group,
#changing the subject site lcv and lskew to 0.187 and 0.164, respectively
Pool.39001Adj <- LRatioChange(Pool.39001, SiteID = 39001, lcv = 0.187, lskew = 0.164)
}
\author{
Anthony Hammond
}
