



# FlowDurationCurve ---------------------------------------------------

#' Flow duration curve
#'
#'@description A function to plot flow duration curves for a single flow series or flow duration curves from multiple flow series.
#'@details The user can input a dataframe of dates and flow to return a plot of the flow duration curve for annual, winter and summer. Or a list of flow series' (vectors) can be applied for a plot comparing the individual flow duration curves
#'@param x a dataframe with date in the first column and numeric (flow) in the second.
#'@param main A title for the plot. The default is Flow duration curve.
#'@param CompareCurves A user supplied list where each element is a numeric vector (each a flow series). This is useful for when you want to compare curves from multiple flow series'.
#'@param LegNames User supplied names for the legend. This only works when the CompareCurves argument is used. The default is Curve1, Curve2...CurveN.
#'@param Cols User supplied vector of colours. This only works when the CompareCurves argument is used. The default is the Zissou 1 palette.
#'
#'@examples
#'# Plot a flow duration curve for the Thames at Kingston Oct 2000 - Sep 2015.
#' FlowDurationCurve(ThamesPQ[,c(1,3)])
#' #Compare flows from the rather wet 2013 water year (rows 4749 and 5114) with the rest of the flow
#' FlowDurationCurve(CompareCurves = list(ThamesPQ$Q[-seq(4749,5114)],
#' ThamesPQ$Q[4749:5114]), LegNames = c("All but 2013","water year 2013"))
#'@return If a dataframe of date in the first column and flow in the second is applied with the x argument a plot of a the flow duration curves for winter, summer and annual is returned.
#' If a list of flow series is applied with the CompareCurves argument the associated flow duration curves are all plotted together.
#'@author Anthony Hammond

FlowDurationCurve <- function(x = NULL, main = "Flow duration curve", CompareCurves = NULL, LegNames = NULL, Cols = NULL) {

  if(is.null(CompareCurves) == FALSE) {
    CombineQs <- unlist(CompareCurves)
    ProbsInd <- c(0.999,0.99, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01, 0.001)
    QNorm <- qnorm(ProbsInd)
    Min <- log10(quantile(CombineQs, 0.0003, na.rm = TRUE))
    Max <- log10(quantile(CombineQs, 0.9997, na.rm = TRUE))
    plot(qnorm(ProbsInd), xaxt = "n", yaxt = "n", log10(quantile(CombineQs, sort(ProbsInd), na.rm = TRUE)), type = "l", xlim = c(-3.5, 3.5), ylim = c(Min, Max), xlab = "Percentage of time flow exceeded", ylab = "Discharge (m3/s)", lwd = 2, col = "transparent", main = main)
    axis(side = 1, at = qnorm(ProbsInd), tick = TRUE, col = "transparent", labels = c(99.9, 99, 95, 90, 80, 70, 60, 50, 40, 30, 20, 10, 5, 1, 0.1))
    abline(v = qnorm(ProbsInd), lty = 3)
    QOrdinate <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100, 500, 1000, 5000)
    #QOrdinate <- as.numeric(quantile(c(Summer[,2], Winter[,2]), c(1, 0.999,0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.01, 0.001, 0)))
    QOrdinatePos <- log10(QOrdinate)
    axis(side = 2, at = QOrdinatePos, labels = as.character(signif(QOrdinate, 2)), tick = FALSE)
    abline(h = QOrdinatePos, lty = 3)

    if(is.null(Cols)) {
    Cols <- hcl.colors(length(CompareCurves), palette = "Zissou 1")} else {Cols <- Cols}
    for(i in 1:length(CompareCurves)) {
      points(qnorm(ProbsInd), log10(quantile(CompareCurves[[i]], sort(ProbsInd), na.rm = TRUE)), type = "l", col = Cols[i], lwd = 2)
      }

    if(is.null(LegNames)) {LegNames <- paste("Curve", seq(1,length(CompareCurves)), sep = "")}
    legend("bottomleft", legend = LegNames, lwd = 2, col = Cols)

  }

  if(is.null(x) == FALSE) {
  LegNames <- c("Annual", "Winter", "Summer")
  x <- x[complete.cases(x), ]
  SeasonInd <- function(x) {
    POSlt <- as.POSIXlt(x)
    Mons <- (POSlt$mon)+1
    WinInd <- which(Mons == 12 | Mons == 1 | Mons == 2)
    SpriInd <- which(Mons == 3 | Mons == 4 | Mons == 5)
    SummInd <- which(Mons == 6 | Mons == 7 | Mons == 8)
    AutuInd <- which(Mons == 9 | Mons == 10 | Mons == 11)
    SeasInd <- list(WinInd, SpriInd, SummInd, AutuInd)
    names(SeasInd) <- c("Winter", "Spring", "Summer", "Autumn")
    return(SeasInd)
  }
  GetInds <- SeasonInd(x[,1])
  Summer <- x[GetInds$Summer,]
  Winter <- x[GetInds$Winter,]
  Seasons <- list(x, Winter, Summer)

  All <- x[,2]
  ProbsInd <- c(0.999,0.99, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01, 0.001)
  QNorm <- qnorm(ProbsInd)
  Min <- min(log10(quantile(Summer[,2], 0.001)),  log10(quantile(Winter[,2], 0.001)) )
  Max <- max(log10(quantile(Winter[,2], 0.999)),  log10(quantile(Summer[,2], 0.999)) )
  plot(qnorm(ProbsInd), xaxt = "n", yaxt = "n", log10(quantile(All, sort(ProbsInd))), type = "l", xlim = c(-3.5, 3.5), ylim = c(Min, Max), xlab = "Percentage of time flow exceeded", ylab = "Discharge (m3/s)", lwd = 2)
  axis(side = 1, at = qnorm(ProbsInd), tick = TRUE, col = "transparent", labels = c(99.9, 99, 95, 90, 80, 70, 60, 50, 40, 30, 20, 10, 5, 1, 0.1))
  abline(v = qnorm(ProbsInd), lty = 3)
  QOrdinate <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100, 500, 1000, 5000)
  #QOrdinate <- as.numeric(quantile(c(Summer[,2], Winter[,2]), c(1, 0.999,0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.01, 0.001, 0)))
  QOrdinatePos <- log10(QOrdinate)
  axis(side = 2, at = QOrdinatePos, labels = as.character(signif(QOrdinate, 2)), tick = FALSE)
  abline(h = QOrdinatePos, lty = 3)

  points(qnorm(ProbsInd), log10(quantile(Winter[,2], sort(ProbsInd))), type = "l", col = rgb(0,0.3,0.7), lwd = 2)
  points(qnorm(ProbsInd), log10(quantile(Summer[,2], sort(ProbsInd))), type = "l", col = rgb(0,0.7,0.2), lwd = 2)
  legend("bottomleft", legend = LegNames, lwd = 2, col = c("black", rgb(0,0.3,0.7), rgb(0,0.7,0.3)))
  }
}
