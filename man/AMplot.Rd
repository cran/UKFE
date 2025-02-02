% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/All.R
\name{AMplot}
\alias{AMplot}
\title{Plot of the annual maximum sample}
\usage{
AMplot(x, ylab = "Discharge (m3/s)", xlab = "Hydrological year", main = NULL)
}
\arguments{
\item{x}{A data.frame with at least two columns. The first a date column and the second the annual maximum (AM) sequence. A third column with the station id can be applied which is then used for the default plot title.}

\item{ylab}{Label for the y axis (character string).}

\item{xlab}{Label for the x axis (character string).}

\item{main}{Title for the plot (character string). The default is 'Annual maximum sample:', where : is followed by an ID number if this is included in a third column of the dataframe x.}
}
\value{
A barplot of the annual maximum sample
}
\description{
Provides a barplot for an annual maximum sample
}
\details{
When used with a GetAM object or any data.frame with dates/POSIXct in the first column, the date-times are daily or sub-daily. Therefore, although it's an annual maximum (AM) sequence, some bars may be closer together depending on the number of days between them.
}
\examples{
#Get an AMAX sample and plot
AMplot(GetAM(58002))
}
\author{
Anthony Hammond
}
