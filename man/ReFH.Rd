% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{ReFH}
\alias{ReFH}
\title{Revitalised Flood Hydrograph Model (ReFH)}
\usage{
ReFH(
  CDs = NULL,
  Depth = NULL,
  duration = NULL,
  timestep = NULL,
  scaled = NULL,
  PlotTitle = NULL,
  RPa = NULL,
  alpha = TRUE,
  season = NULL,
  AREA = NULL,
  TP = NULL,
  BR = NULL,
  BL = NULL,
  Cmax = NULL,
  Cini = NULL,
  BFini = NULL,
  Rain = NULL
)
}
\arguments{
\item{CDs}{catchment descriptors derived from either GetCDs or ImportCD}

\item{Depth}{a numeric value. The depth of rainfall used as input in the estimation of a design hydrograph. The default, when Depth = NULL, is a two year rainfall.}

\item{duration}{a numeric value. A duration (hrs) for the design rainfall}

\item{timestep}{a numeric value. A user defined data interval. The default changes depending on the estimated time to peak to formulate a sensible looking result}

\item{scaled}{a numeric value of peak flow in m3/s}

\item{PlotTitle}{a character string. A user defined title for the ReFH plot}

\item{RPa}{return period for alpha adjustment. This is only for the purposes of the alpha adjustment, it doesn't change the rainfall input}

\item{alpha}{a logical argument with default TRUE. If TRUE the alpha adjustment is applied based on RPa. If FALSE, no alpha adjustment is made}

\item{season}{a choice of "summer" or "winter". The default is "summer" in urban catchments (URBEXT2000 > 0.03) and "winter" in rural catchments}

\item{AREA}{numeric. Catchment area in km2.}

\item{TP}{numeric. Time to peak parameter (hours)}

\item{BR}{numeric. Baseflow recharge parameter}

\item{BL}{numeric. Baseflow lag parameter (hours)}

\item{Cmax}{numeric. Maximum soil moisture capacity parameter (mm)}

\item{Cini}{numeric. Initial soil moisture content (mm)}

\item{BFini}{numeric. Initial baseflow (m3/s)}

\item{Rain}{numeric. User input rainfall (hourly). A numeric vector}
}
\value{
A list with two elements, and a plot. First element of the list is a data.frame of  parameters, initial conditions and the catchment area. The second is a data.frame with columns Rain, NetRain, Runoff, Baseflow, and TotalFlow. If the scale argument is used a numeric vector containing the scaled hydrograph is returned instead of the results dataframe. The plot is of the ReFH output, with rainfall, net-rainfall, baseflow, runoff and total flow. If the scaled argument is used, a scaled hydrograph is plotted.
}
\description{
Provides outputs of the ReFH model from catchment descriptors or user defined inputs
}
\details{
The ReFH is described in the Flood Estimation Handbook Supplementary Report No.1 (2007). The method to derive design rainfall profiles is described in the Flood Estimation Handbook (1999), volume 2. Users can also input their own rainfall with the 'Rain' argument. As a default, when catchment descriptors (CDs) are provided the ReFH function uses catchment descriptors to estimate the parameters of the ReFH model and the two year rainfall for the critical duration. The latter is based on a quadratic interpolation of the catchment descriptors RMED1H, RMED1D, and RMED2D (then a seasonal correction factor is applied). Parameters and initial conditions can also be individually input by the user. If a parameter argument is used for one or more of the parameters, then these overwrite the CD derived parameters. If a value for the scaled argument is provided (m3/s), a scaled hydrograph is returned. The RPa argument doesn't change the rainfall input and is only needed for the alpha adjustment (see the FEH supplement report no.1).
}
\examples{
#Get CDs and apply the ReFH function
CDs.203018 <- GetCDs(203018)
ReFH(CDs.203018)
#Apply the ReFH function, scale to a 100-year flow estimate and change the plot title accordingly
ReFH(CDs.203018, scaled = 182, PlotTitle = "100-Year Design Hydrograph - Site 203018")
#Apply the ReFH function with a user defined initial baseflow
ReFH(CDs.203018, BFini = 6)
}
\author{
Anthony Hammond
}
