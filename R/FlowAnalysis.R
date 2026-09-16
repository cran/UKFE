# FlowDurationCurve ---------------------------------------------------

#' Flow duration curve
#'
#' @description A function to plot flow duration curves for a single flow series or flow duration curves from multiple flow series.
#' @details The user can input a dataframe of dates (or POSIXct) and flow to return a plot of the flow duration curve for annual, winter and summer periods. Alternatively a list of flow series' (vectors) can be applied for a plot comparing the individual flow duration curves.
#' @param x a dataframe with date in the first column and numeric (flow) in the second.
#' @param main A title for the plot. The default is 'Flow duration curve'.
#' @param CompareCurves A user supplied list where each element is a numeric vector (each a flow series). This is useful for when you want to compare curves from multiple flow series'.
#' @param LegNames User supplied names for the legend. This only works when the CompareCurves argument is used. The default is Curve1, Curve2...CurveN.
#' @param Cols User supplied vector of colours. This only works when the CompareCurves argument is used. The default is the Zissou 1 palette.
#' @param AddQs Adds additional flows and associated horizontal plot lines to the plot. It should be a single numeric value or a vector, for example c(25, 75, 100).
#' @param ReturnData Logical argument with a default of FALSE. When TRUE, a dataframe is returned with the data from the plot.
#'
#' @examples
#' # Plot a flow duration curve for the Thames at Kingston October 2000 to September 2015
#' FlowDurationCurve(ThamesPQ[, c(1, 3)])
#'
#' # Add two additional flow lines for the plot
#' FlowDurationCurve(ThamesPQ[, c(1, 3)], AddQs = c(25, 200))
#'
#' # Compare flows from the rather wet 2013 water year (rows 4749 and 5114) with the rest of the flow
#' FlowDurationCurve(
#'   CompareCurves = list(
#'     ThamesPQ$Q[-seq(4749, 5114)],
#'     ThamesPQ$Q[4749:5114]
#'   ),
#'   LegNames = c("All but 2013", "Water year 2013")
#' )
#'
#' @return If a dataframe of date in the first column and flow in the second is applied with the x argument a plot of the flow duration curves for the winter, summer and annual periods is returned.
#' If a list of flow series is applied with the CompareCurves argument the associated flow duration curves are all plotted together. If ReturnData is TRUE, the plotted data is also returned.
#' @author Anthony Hammond

FlowDurationCurve <- function(x = NULL, main = "Flow duration curve", CompareCurves = NULL, LegNames = NULL, Cols = NULL, AddQs = NULL, ReturnData = FALSE) {
  Log10Func <- function(x) {
    Result <- log10(x)
    InfTest <- which(is.infinite(Result) == TRUE)
    if(length(InfTest) > 0 ) {Result[InfTest] <- -3}
    return(Result)
  }
  if(is.null(CompareCurves) == FALSE){
    if(class(CompareCurves) != class(list(c(1,2,3,4), c(2,4,6,8)))) stop("CompareCurves must be a list object")
    if(length(CompareCurves) < 2) stop("CompareCurves must be a list with at least two elements")
    CombineQs <- unlist(CompareCurves)
    ZeroCheck <- min(CombineQs,na.rm = TRUE)
    if(ZeroCheck <= 0) warning("The discharge drops to zero, suggesting this is an ephemeral stream")
    ProbsInd <- c(0.999,0.99, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01, 0.001)
    QNorm <- qnorm(ProbsInd)
    Min <- Log10Func(quantile(CombineQs, 0.0003, na.rm = TRUE))
    if(is.infinite(Min)) {Min <- -3}
    Max <- Log10Func(quantile(CombineQs, 0.9997, na.rm = TRUE))
    plot(qnorm(ProbsInd), xaxt = "n", yaxt = "n", Log10Func(quantile(CombineQs, sort(ProbsInd), na.rm = TRUE)), type = "l", xlim = c(-3.5, 3.5), ylim = c(Min, Max), xlab = "Percentage of time flow exceeded", ylab = "Discharge (m3/s)", lwd = 2, col = "transparent", main = main)
    axis(side = 1, at = qnorm(ProbsInd), tick = TRUE, col = "transparent", labels = c(99.9, 99, 95, 90, 80, 70, 60, 50, 40, 30, 20, 10, 5, 1, 0.1))
    abline(v = qnorm(ProbsInd), lty = 3)
    QOrdinate <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100, 500, 1000, 5000)
    #QOrdinate <- as.numeric(quantile(c(Summer[,2], Winter[,2]), c(1, 0.999,0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.01, 0.001, 0)))
    QOrdinatePos <- Log10Func(QOrdinate)
    axis(side = 2, at = QOrdinatePos, labels = as.character(signif(QOrdinate, 2)), tick = FALSE)
    abline(h = QOrdinatePos, lty = 3)

    if(is.null(Cols)) {
      Cols <- hcl.colors(length(CompareCurves), palette = "Zissou 1")} else {Cols <- Cols}
    for(i in 1:length(CompareCurves)) {
      points(qnorm(ProbsInd), Log10Func(quantile(CompareCurves[[i]], sort(ProbsInd), na.rm = TRUE)), type = "l", col = Cols[i], lwd = 2)
    }

    if(is.null(LegNames)) {LegNames <- paste("Curve", seq(1,length(CompareCurves)), sep = "")}
    if(is.null(AddQs) == FALSE) {
      axis(2, at = Log10Func(AddQs), labels = AddQs)
      abline(h = Log10Func(AddQs), lty = 3)
    }
    legend("bottomleft", legend = LegNames, lwd = 2, col = Cols)
    if(ReturnData == TRUE) {
      ResDF <- data.frame(PercentExceeded = (1-ProbsInd)*100, quantile(CompareCurves[[1]], ProbsInd, na.rm = TRUE),
                          quantile(CompareCurves[[2]], ProbsInd, na.rm = TRUE))
      if(length(CompareCurves) > 2)
        for(i in 3:length(CompareCurves)) {ResDF <- cbind(ResDF, quantile(CompareCurves[[i]], ProbsInd, na.rm = TRUE))}
      ResDF[,2:ncol(ResDF)] <- signif(ResDF[,2:ncol(ResDF)], 3)
      colnames(ResDF)[1] <- "PercentExceeded"
      colnames(ResDF) [2:ncol(ResDF)] <- paste("v", seq(1, (ncol(ResDF)-1)), sep = "")
      rownames(ResDF) <- seq(1, nrow(ResDF))
      return(ResDF)
    }


  }

  if(is.null(x) == FALSE) {
    if(class(x) != class(data.frame(c(1,2,3)))) stop("x must be a dataframe with two columns, Date or POSIXct in the first and numeric in the second.")
    if(ncol(x) != 2) stop("x must be a dataframe with two columns, date or POSIXct in the first and numeric in the second.")
    if(class(x[,1])[1] != class(as.POSIXct("1981-10-15"))[1] & class(x[,1])[1] != class(as.Date("1981-10-15"))[1]) stop("x must be a dataframe with two columns, POSIXct or Date in the first and numeric in the second.")
    if(class(x[,2])[1] != class(runif(10))[1]) stop("x must be a dataframe with two columns, POSIXct in the first and numeric in the second.")

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
    ZeroCheck <- min(All, na.rm = TRUE)
    if(ZeroCheck <= 0) warning("The discharge drops to zero, suggesting this is an ephemeral stream")
    ProbsInd <- c(0.999,0.99, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.01, 0.001)
    QNorm <- qnorm(ProbsInd)
    Min <- min(Log10Func(quantile(Summer[,2], 0.001)),  Log10Func(quantile(Winter[,2], 0.001)) )
    if(is.infinite(Min)) {Min <- -3}
    Max <- max(Log10Func(quantile(Winter[,2], 0.999)),  Log10Func(quantile(Summer[,2], 0.999)) )
    plot(qnorm(ProbsInd), xaxt = "n", yaxt = "n", Log10Func(quantile(All, sort(ProbsInd))), type = "l", xlim = c(-3.5, 3.5), ylim = c(Min, Max), xlab = "Percentage of time flow exceeded", ylab = "Discharge (m3/s)", lwd = 2, main = main)
    axis(side = 1, at = qnorm(ProbsInd), tick = TRUE, col = "transparent", labels = c(99.9, 99, 95, 90, 80, 70, 60, 50, 40, 30, 20, 10, 5, 1, 0.1))
    abline(v = qnorm(ProbsInd), lty = 3)
    QOrdinate <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50, 100, 500, 1000, 5000)
    #QOrdinate <- as.numeric(quantile(c(Summer[,2], Winter[,2]), c(1, 0.999,0.99, 0.9, 0.8, 0.5, 0.2, 0.1, 0.01, 0.001, 0)))
    QOrdinatePos <- Log10Func(QOrdinate)
    axis(side = 2, at = QOrdinatePos, labels = as.character(signif(QOrdinate, 2)), tick = FALSE)
    abline(h = QOrdinatePos, lty = 3)

    points(qnorm(ProbsInd), Log10Func(quantile(Winter[,2], sort(ProbsInd))), type = "l", col = rgb(0,0.3,0.7), lwd = 2)
    points(qnorm(ProbsInd), Log10Func(quantile(Summer[,2], sort(ProbsInd))), type = "l", col = rgb(0,0.7,0.2), lwd = 2)
    if(is.null(AddQs) == FALSE) {
      axis(2, at = Log10Func(AddQs), labels = AddQs)
      abline(h = Log10Func(AddQs), lty = 3)
    }

    legend("bottomleft", legend = LegNames, lwd = 2, col = c("black", rgb(0,0.3,0.7), rgb(0,0.7,0.3)))
    if(ReturnData == TRUE) {
      ResDF <- data.frame(PercentExceeded = ProbsInd*100, Annual = signif(quantile(All, sort(ProbsInd)),2), Winter = signif(quantile(Winter[,2], sort(ProbsInd)),3),
                          Summer = signif(quantile(Summer[,2], sort(ProbsInd)), 3))
      ResDF <- ResDF[order(ResDF$PercentExceeded), ]
      rownames(ResDF) <- seq(1, nrow(ResDF))
      return(ResDF)

    }
  }
  }




# FlowSplit ---------------------------------------------------

#' Flow splitter
#'
#' @description A function to separate baseflow from runoff.
#' @details The function is intended for the event scale as opposed to long term flow series. It works by linearly joining all the low points in the hydrograph - also the beginning and end points. Where a low point is any point with two higher points either side. Then any values above the hydrograph (xi) are set as xi.
#' The baseflow point on the falling limb of the hydrograph/s can be raised using the AdjUp argument. The function works for any sampling frequency and arbitrary hydrograph length (although more suited for event scale and sub-annual events  in general). This function is not design for deriving long term baseflow index. It could be used for such a purpose but careful consideration would be required for the BaseQUpper argument especially for comparison across river locations. If baseflow index is required the BFI function (with daily mean flow) is more suitable.
#' @param x A numeric vector (your flow series / hydrograph).
#' @param BaseQUpper Numeric value which is an upper level of baseflow (i.e. the baseflow will not extend above this level). The default is the mean of x. It can be set arbitrarily high so that the baseflow joins all low points/troughs in the hydrograph.
#' @param AdjUp A numeric value between 0 and 0.5. This allows the user to adjust the baseflow up the falling limb/s of the hydrogaph. With 0.05 being a small upward adjustment and 0.49 being a large upward adjustment.
#' @param ylab Label for the y-axis (character string). The default is "value",
#' @param xlab Label for the x-axis (character string). The default is "Time index".
#'
#' @examples
#' # We'll extract a wet six month period on the Thames during the 2006-2007 hydrological year
#' thames_q <- subset(ThamesPQ[, c(1, 3)], Date >= "2006-11-04" & Date <= "2007-05-06")
#'
#' # Then apply the flow split with default settings
#' q_split <- FlowSplit(thames_q$Q)
#'
#' # Now do it with an upper baseflow level of 100 m^3/s
#' q_split <- FlowSplit(thames_q$Q, BaseQUpper = 100)
#'
#'
#' # Next we will get a single peaked "idealised" hydrograph using the ReFH function
#' q_refh <- ReFH(GetCDs(15006))
#' q_refh <- q_refh[[2]]$TotalFlow
#'
#' # Now use the function with and without an upward adjustment of the baseflow on the falling limb
#' q_flow_split <- FlowSplit(q_refh)
#' q_flow_split <- FlowSplit(q_refh, AdjUp = 0.15)
#'
#' @return A dataframe with the original flow (x) in the first column and the baseflow in the second. A plot of the original flow and the baseflow is also returned.
#' @author Anthony Hammond

FlowSplit <- function(x, BaseQUpper = NULL, AdjUp = NULL, ylab = "Value", xlab = "Time index") {
  if (class(x) != class(runif(10))) stop("x must be a numeric vector")
  if(is.na(x[1]) | is.na(x[length(x)])) stop("The first or last value of x is na, make they are both a number")
  Low.Func <- function(TS) {
    L <- length(TS) - 2
    L1 <- length(TS) - 1
    L2 <- length(TS)
    Vec1 <- TS[1:L]
    Vec2 <- TS[2:L1]
    Vec3 <- TS[3:L2]
    P1 <- ifelse(Vec2 <= Vec1 & Vec2 <= Vec3 & Vec1 != Vec2, Vec2, NA)
    #x1 <- TS[1]
    #xEnd <- TS[length(TS)]
    #P1 <- c(x1, P1, xEnd)
    return(P1)
  }
  QOff <- BaseQUpper
  if (is.null(QOff)) {
    MeanFlow <- mean(x, na.rm = TRUE)
  }
  if (is.null(QOff) == FALSE) {
    MeanFlow <- QOff
  }
  if (x[1] > MeanFlow | x[length(x)] > MeanFlow) stop("BaseQUpper must be above the first and last values  of x. Raise BaseQupper or extend x to lower values")

  Lows <- Low.Func(x)
  Lows <- c(x[1], Lows, x[length(x)])
  IndexNA <- which(Lows > MeanFlow)
  Lows[IndexNA] <- NA
  #
  if (is.null(AdjUp) == FALSE) {
    NotNA <- which(is.na(Lows) == FALSE)
    LowsDiff <- diff(NotNA)
    if (AdjUp <= 0 | AdjUp >= 0.5) stop("If AdjUp is used it must be a value greater than zero and less than 0.5")
    AdjUp <- 1 / AdjUp
    Low2Adj <- round(LowsDiff / AdjUp)
    Lows2Ind <- NotNA[2:length(NotNA)] - Low2Adj
    Lows2 <- x[Lows2Ind]
    Lows[Lows2Ind] <- x[Lows2Ind]
  }
  #
  interpolate_na <- function(vec) {
    na_index <- which(is.na(vec))
    if (length(na_index) == 0) {
      return(vec)
    }
    vec[na_index] <- approx(seq_along(vec), vec, xout = na_index)$y
    return(vec)
  }

  baseflow <- interpolate_na(Lows)
  IndHigh <- which((x - baseflow) < 0)
  baseflow[IndHigh] <- x[IndHigh]
  ResDF <- data.frame(Q = x, baseflow)
  plot(ResDF$Q, type = "l", ylab = ylab, xlab = xlab)
  points(ResDF$baseflow, type = "l", col = "red")
  return(ResDF)
}



#' Design hydrograph extraction
#'
#' Extracts a mean hydrograph from a flow series
#'
#' All the peaks over the threshold (default 0.975th) are identified and separated by a user defined value 'EventSep', which is a number of timesteps (peaks are separated by EventSep * 3). The top N peaks are selected and the hydrographs are then extracted. The hydrograph start is the time of peak minus EventSep. The End of the hydrograph is time of peak plus EventSep times 1.5. All events are scaled to have a peak flow of one, and the mean of these is taken as the scaled design hydrograph.
#' @param x a dataframe with Date or POSIXct in the first column and the numeric vector of discharge in the second
#' @param Threshold The threshold above which the peaks of the hydrograph are first identified. The default is 0.975.
#' @param EventSep Number of timesteps to determine individual peak events during the extraction process. For the comparison and averaging process the start and end point of the hydrograph is Peak - EventSep and Peak + EventSep * 1.5.
#' @param N number of event hydrographs from which to derive the mean hydrograph. Default is 10. Depending on the length of x, there may be fewer than 10
#' @param Exclude An index (single integer or vector of integers up to N) for which hydrographs to exclude if you so wish. This may require some trial and error. You may want to increase N for every excluded hydrograph.
#' @param Plot logical argument with a default of TRUE. If TRUE, all the hydrographs from which the mean is derived are plotted along with the mean hydrograph.
#' @param main Title for the plot
#' @param ylab Y label
#' @examples
#' # Extract a design hydrograph from the Thames daily mean flow and print the resulting hydrograph
#' thames_des_hydro <- DesHydro(ThamesPQ[, c(1, 3)], EventSep = 10, N = 10)
#'
#' @return a list of length three. The first element is a dataframe of the peaks of the hydrographs and the associated dates. The second element is a dataframe with all the scaled hydrographs, each column being a hydrograph. The third element is the averaged hydrograph
#' @author Anthony Hammond

DesHydro <- function(x, Threshold = 0.975, EventSep, N = 10, Exclude = NULL, Plot = TRUE, main = "Design Hydrograph", ylab = "Scaled Discharge") {
  if (class(x) != class(data.frame(seq(1, 3)))) stop("x must be a datafrane with Date or POSIXct in the first column and numeric in the second")
  if (class(x[1, 1])[1] != class(as.Date("1990-01-01"))[1] & class(x[1, 1])[1] != class(as.POSIXct("1990-01-01 09:00:00"))[1]) stop("The first column of x must be Date or POSIXct")
  POTx <- suppressWarnings(POTt(x, Plot = FALSE, div = EventSep * 3, threshold = Threshold))
  DateIndex <- match(POTx[, 1], x[, 1])
  UpperIndex <- nrow(x) - ceiling(EventSep * 1.5)
  LowerIndex <- ceiling(EventSep * 1.5)
  MatchUpLow <- which(DateIndex > UpperIndex | DateIndex < LowerIndex)
  if (length(MatchUpLow) > 0) {
    DateIndex <- DateIndex[-MatchUpLow]
    POTx <- POTx[-MatchUpLow, ]
  }
  POTx <- POTx[order(POTx[, 2], decreasing = TRUE), ]
  rownames(POTx) <- seq(1, nrow(POTx))
  if (nrow(POTx) < 3) stop("There are fewer than three peaks identified with the current threshold and EventSep settings. Ideally you should use a longer time series but it might work if you change the settings")
  if (nrow(POTx) < N) warning("There are fewer events (based on the current setting) than N. Use a longer timeseries or change the settings")
  if (nrow(POTx) < N) {
    POTx <- POTx
  } else {
    POTx <- POTx[1:N, ]
  }
  if (is.null(Exclude) == FALSE) {
    POTx <- POTx[-Exclude, ]
  }
  DateIndex <- match(POTx[, 1], x[, 1])
  Hydros <- list()
  for (i in 1:nrow(POTx)) {
    Hydros[[i]] <- x[(DateIndex[i] - EventSep):(DateIndex[i] + EventSep * 1.5), 2]
  }
  ScaleHydros <- list()
  for (i in 1:length(Hydros)) {
    ScaleHydros[[i]] <- Hydros[[i]] / max(Hydros[[i]])
  }
  ScaleHydrosDF <- data.frame(ScaleHydros[[1]], ScaleHydros[[2]])
  for (i in 3:length(ScaleHydros)) {
    ScaleHydrosDF <- cbind(ScaleHydrosDF, ScaleHydros[[i]])
  }
  colnames(ScaleHydrosDF) <- paste("hydro", seq(1, ncol(ScaleHydrosDF)), sep = "")
  Average <- as.numeric(apply(ScaleHydrosDF, 1, mean, na.rm = TRUE))
  if (Plot == TRUE) {
    matplot(ScaleHydrosDF, type = "l", col = hcl.colors(ncol(ScaleHydrosDF)), ylab = ylab, xlab = "Time index", main = main)
    points(Average, lwd = 2, col = "black", type = "l")
  }
  Results <- list(Average, POTx, ScaleHydrosDF)
  names(Results) <- c("DesignHydrograph", "Peaks", "AllScaledHydrographs")
  rownames(Results$Peaks) <- seq(1, nrow(Results$Peaks))
  return(Results)
}



#' Low Flows
#'
#' A function to estimate lower flow quantiles in ungauged catchments.
#'
#' This function provides estimates of the mean flow, Q95, Q70, Q50, Q10, and Q5. The function works by calculating scaled flows for the catchment of interest from the catchments in the NRFA data set. The scaled flow is calculated as Qsi = Qdi * (Area_s / Area_di) * (SAAR_s / SAAR_di), where the subscripts s and d denote the subject site and the donor site, respectively.
#' A Euclidean distance measure is then used to find the most similar sites using catchment area, SAAR, and BFIHOST. The distance measure is weighted by how correlated each variable is to the scaling factor: Scale_f = Q_d / (Area_d * SAAR_d), and the variables are normalised by the standard deviation (log variables are used).
#' A weighted average of the scaled flow is taken from the most similar N sites, and the weighting is based on the reciprocal of the similarity.
#' The function was subjected to a leave one out cross validation (LOOCV). The results across the estimated flows show no systematic biases and the errors show no correlation with the descriptors used in the estimation process. The factorial standard error (FSE) was calculated from the LOOCV process for each of the six estimated flows and these are used to calculate 90 percent confidence intervals for the estimates. The FSEs are 1.25, 1.74, 1.44, 1.34, 1.29, and 1.27, for the mean flow, Q95, Q70, Q50, Q10, Q5, respectively.
#' @param CDs Catchment descriptors derived from the GetCDs or CDsXML function.
#' @param AREA Catchment area (km2) - for when CDs is not applied
#' @param SAAR Average annual rainfall (mm) - for when CDs is not applied
#' @param BFIHOST An estimate of baseflow index - for when CDs is not applied
#' @param Exclude A site reference. This is to exclude sites that you do not want used in the estimate. For example, if you're seeing how the function performs on a gauged site, you may want to exclude it from the analysis.
#' @param FARLRange A vector of length 2. For example c(0.9,1). This determines a FARL range for the catchments you wish to be included in the analysis. Primarily this is to exclude sites which have significant reservoir or lake influence when the site of interest does not. If it is NULL (default) all NRFA sites are included.
#' @param N Number of donor gauges to use for the assessment. The default is 10.
#' @examples
#' # Get some catchment descriptors, then estimate the flows
#' \dontrun{
#' CDs_27083 <- GetCDs(27083)
#' LowFlows(CDs_27083)
#' }
#' # Now estimate again but remove gauge 27083 from the analysis
#' \dontrun{
#' LowFlows(CDs_27083, Exclude = 27083)
#' }
#' @return A list. The first element of which is a data.frame with three columns. The first is the flow estimates, the second and third are the 90% interval for the estimate. The row names denote the name of each estimate. The second element is the dataframe of catchments used in the analysis, with the relevant descriptors and weighting.
#' @author Anthony Hammond

LowFlows <- function(CDs = NULL, AREA = NULL, SAAR = NULL, BFIHOST = NULL, Exclude = NULL, FARLRange = NULL, N = 10) {

  if(is.null(CDs) == FALSE) {

    if(class(CDs) != class(data.frame(c(1,2,3)))) stop("CDs must be a CDs dataframe object which can be derived using the GetCDs or CDsXML function")
    CDsTest <- GetCDs(rownames(PeakFlowData)[1])
    if(!identical(CDs[,1], CDsTest[,1])) stop("CDs must be a CDs dataframe object which can be derived using the GetCDs or CDsXML function")


    Area <- CDs[grep("AREA", CDs$Descriptor)[1] ,2]
    SAAR <- CDs[grep("SAAR", CDs$Descriptor)[1] ,2]
    BFIHOST <- CDs[grep("BFIHOST", CDs$Descriptor)[1],2]
  }
  NRFAAllData <- read.csv("https://nrfaapps.ceh.ac.uk/nrfa/ws/station-info?station=*&format=csv&fields=all")
  if(is.null(Exclude) == FALSE) {
    IndExc <- match(Exclude, NRFAAllData$id)
    NRFAAllData <- NRFAAllData[-IndExc,]
  }
  if(is.null(FARLRange)) {NRFAAllData <- NRFAAllData}
  if(is.null(FARLRange) == FALSE) {
    if(length(FARLRange) != 2) stop("FARLRange must be NULL or a vector of length 2")
    if(FARLRange[1] >= FARLRange[2]) stop("The first FARLRange must be lower than the second")

    KeepIndex <- which(NRFAAllData$farl.2015 >= FARLRange[1] & NRFAAllData$farl.2015 <= FARLRange[2])
    if(length(KeepIndex) < 5) stop("Your FARLRange has resulted in less than five available sites")
    NRFAAllData <- NRFAAllData[KeepIndex,]
  }

  if(is.null(CDs)) {
    Area <- AREA
    SAAR <- SAAR
    BFIHOST <- BFIHOST
  }

  QNames <- c("gdf.mean.flow", "gdf.q95.flow",  "gdf.q70.flow",  "gdf.q50.flow", "gdf.q10.flow",  "gdf.q05.flow")
  ColnamesNRFA <- colnames(NRFAAllData)
  MatchCols <- match(QNames, ColnamesNRFA)
  QScale <- NRFAAllData[,MatchCols] *  (Area / NRFAAllData$catchment.area) * (SAAR / NRFAAllData$saar.1991.2020)
  xData <- data.frame(AREA = NRFAAllData$ihdtm.catchment.area, SAAR = NRFAAllData$saar.1991.2020,
                      BFIHOST = NRFAAllData$bfihost19.scaled, QScale, id = NRFAAllData$id)
  xData <- xData[complete.cases(xData),]
  EuclidDist <- function(x, y, z) {
    SDx <- sd(log(xData$SAAR))
    SDy <- sd(log(xData$BFIHOST))
    SDz <- sd(log(xData$AREA))
    Res <- sqrt( 0.618*((x[1]-x[2])/SDx)^2 + 0.369*((y[1]-y[2])/SDy)^2  +  0.013*((z[1]-z[2])/SDz)^2 )
    return(Res)
  }
  DiffArea <- abs(Area - xData$AREA)
  DiffSAAR <- abs(SAAR - xData$SAAR)
  DiffBFI <- abs(BFIHOST - xData$BFIHOST)
  DiffCDs <- data.frame(DiffArea, DiffSAAR, DiffBFI)
  Mins <- apply(DiffCDs[,2:3], 1, sum)
  Min <- min(Mins)

  if(any(Min == 0)) {
    warning("One of the NRFA sites has exactly the same SAAR and BFIHOST as the user input. Is the site already gauged? If you are testing a gauged site as if ungauged, use the Exclude argument")
  }

  Dists <- NULL
  for(i in 1:nrow(xData)) {Dists[i] <- EuclidDist(c( log(SAAR), log(xData$SAAR[i])), c( log(BFIHOST), log(xData$BFIHOST[i])), c( log(Area), log(xData$AREA[i])))}
  xData <- data.frame(xData, Dists)
  xData <- xData[order(xData$Dists),]

  xData <- xData[1:N,]
  DistsRecip <- 1/xData$Dists
  if(DistsRecip[1] == Inf) {DistsRecip[1] <- 100000}
  Weights <- DistsRecip / sum(DistsRecip)
  QScaleArea <- xData[,4:9] * Weights

  FSEs <- c(1.25, 1.74, 1.44, 1.34, 1.29, 1.27)
  Result <- as.numeric(apply(QScaleArea, 2, sum))
  Lower <- signif(Result / (FSEs^1.64), 3)
  Upper <- signif(Result * (FSEs^1.64), 3)
  Result <- data.frame(Q = signif(Result, 3), row.names = c("mean", "Q95", "Q70", "Q50", "Q10", "Q05"))
  Result <- data.frame(Result, Lower, Upper)
  QNames <- c("id", "name", "saar.1991.2020", "bfihost19.scaled",  "catchment.area", "farl.2015")
  Sites <- NRFAAllData[match(xData$id, NRFAAllData$id),QNames]
  Sites <- data.frame(Sites, Weight = Weights)
  Sites <- Sites[order(Sites$Weight, decreasing = TRUE), ]
  rownames(Sites) <- seq(1, nrow(Sites))
  ResultList <- list(Result, Sites)
  return(ResultList)
}
