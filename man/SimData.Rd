% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{SimData}
\alias{SimData}
\title{Data simulator}
\usage{
SimData(n, pars = NULL, dist = "GenLog", GF = NULL)
}
\arguments{
\item{n}{sample size to be simulated}

\item{pars}{vector of parameters in the order of location, scale, shape (only location and shape for Gumbel)}

\item{dist}{choice of distribution. Either "GEV", "GenLog", "Gumbel", "Kappa3", or "GenPareto"}

\item{GF}{vector of GF inputs in the order of Lcv, LSkew, QMED (only Lcv and QMED if dist = "Gumbel")}
}
\value{
A random sample of size n for the chosen distribution.
}
\description{
Simulation of a random sample from the generalised extreme value, generalised logistic, Gumbel, Kappa3, or generalised Pareto distributions
}
\details{
The simulated sample can be generated using distribution parameters, or the growth factor (GF) inputs; linear coefficient of variationn (Lcv), linear skewness (LSkew) & the median annual maximum (QMED).
}
\examples{
#Simulate a sample of size 30 using parameters GenLog and parameters 299, 51, -0.042
SimData(30, pars = c(299, 51, -0.042), dist = "GenLog")
#Now simulate using the Lcv, Lskew, and median (0.17, 0.04, 310)
SimData(30, GF = c(0.17, 0.04, 310), dist = "GenLog")
}
\author{
Anthony Hammond
}
