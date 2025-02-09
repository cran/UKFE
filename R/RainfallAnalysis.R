
# DDFExtract ---------------------------------------------------

#' Derive and plot rainfall Depth Duration Frequency curves.
#'
#'@description Derive and plot rainfall Depth Duration Frequency curves from an input of rainfall data.
#'@details The function works by extracting the annual maximum sample (by hydrological year - starting Oct 1st) of rainfall for a range of sliding durations (1 hour to 96 hours). It then calculates the median annual maximum rainfall depth (RMED) and a GEV growth curve for each duration.
#'To ensure RMED increases with duration a power curve is fit as a function of duration to provide the final RMED estimates. Then the average growth factor for each return period (across the durations) is assumed.
#'@param x A data.frame with POSIXct in the first column and rainfall in the second. The data must have an hourly or sub-hourly sampling rate.
#'@param Plot Logical argument with a default of TRUE. If TRUE, the DDF curves are plotted.
#'@param main Title for the plot (character string). The default is no title.
#'@param Truncate Logical argument with a default of TRUE. If TRUE the extraction of annual maximum process truncates the data to incorporate only full hydrological years. If there is significant rainfall within a partial year it will not be included unless Truncate = FALSE. If Truncate = FALSE, ensure that there are at least 92 hours of data available in the partial years or the function will fail.
#'
#'@examples
#'# We'll extract all available 15 minute rainfall from the St Ives (Cambridgeshire)
#'#rain gauge (WISKI ID = 179365).
#'\dontrun{StIves <- GetDataEA_Rain(WISKI_ID = "179365", Period = "15Mins")}
#'#Then apply the DDF function.
#'\dontrun{DDFExtract(StIves)}
#'@return A dataframe with hours (1 to 96) in the first column then depths associated with a range of return periods (2 to 1000) in the remaining nine columns. If Plot = TRUE, a plot of the DDF curves is also returned.
#'@author Anthony Hammond


DDFExtract <- function(x, Plot = TRUE, main = NULL, Truncate = TRUE) {
if(class(x) != class(data.frame(c(1,2,3)))) stop("x must be a dataframe with two columns, POSIXct in the first and numeric in the second.")
  if(ncol(x) != 2) stop("x must be a dataframe with two columns, POSIXct in the first and numeric in the second.")
  if(class(x[,1])[1] != class(as.POSIXct("1981-10-15"))[1]) stop("x must be a dataframe with two columns, POSIXct in the first and numeric in the second.")
  if(class(x[,2])[1] != class(runif(10))[1]) stop("x must be a dataframe with two columns, POSIXct in the first and numeric in the second.")
  if(diff(x[1:2,1]) > as.POSIXct("2021-10-01 09:15:00") - as.POSIXct("2021-10-01 08:15:00")) stop("the first column must be of class POSIXct and must have an hourly resolution or less")

  x <-  suppressWarnings(AggDayHour(x, func = sum, Freq = "Hour"))

  PowerCurve <- function(x, Hrs) {
    colnames(x) <- c("Hrs", "RMED")
    min.SLS <- function(data, par) {
      with(data, sum(((par[1] * (Hrs + par[2])^par[3]) - RMED)^2))
    }
    result <- optim(par = c(1, 0, 1), fn = min.SLS, data = x)
    Params <- c(result$par[1], result$par[2], result$par[3])
    Params[1] * (Hrs + Params[2])^Params[3]
  }
  Hours <- c(1,3,6,12,24,36,48,60,72,84,96)
  AMList <- list()
  for(i in 1:11) {AMList[[i]] <- suppressWarnings(AnnualStat(x, sum, Sliding = TRUE, Truncate = FALSE,N = Hours[i], na.rm = TRUE))[,2]}
  RMED <- NULL
  for(i in 1:length(AMList)) {RMED[i] <- median(AMList[[i]])}
  #Hrs <- c(0.25, 1, 6, 12, 24, 36, 48, 60, 72, 84, 96)
  Hrs <- c(1, 3, 6, 12, 24, 36, 48, 60, 72, 84, 96)
  DFRain <- data.frame(Hrs, RMED)
  ModRMED <- PowerCurve(DFRain, Hrs = Hrs)
  GEVGF2 <- function(x, RP) {GEVGF(Lcv(x), LSkew(x), RP)}
  GF5 <- NULL
  for(i in 1:length(Hrs)) {GF5[i] <- GEVGF2(AMList[[i]], RP = 5)}
  GF10 <- NULL
  for(i in 1:length(Hrs)) {GF10[i] <- GEVGF2(AMList[[i]], RP = 10)}
  GF20 <- NULL
  for(i in 1:length(Hrs)) {GF20[i] <- GEVGF2(AMList[[i]], RP = 20)}
  GF50 <- NULL
  for(i in 1:length(Hrs)) {GF50[i] <- GEVGF2(AMList[[i]], RP = 50)}
  GF100 <- NULL
  for(i in 1:length(Hrs)) {GF100[i] <- GEVGF2(AMList[[i]], RP = 100)}
  GF200 <- NULL
  for(i in 1:length(Hrs)) {GF200[i] <- GEVGF2(AMList[[i]], RP = 200)}
  GF500 <- NULL
  for(i in 1:length(Hrs)) {GF500[i] <- GEVGF2(AMList[[i]], RP = 500)}
  GF1000 <- NULL
  for(i in 1:length(Hrs)) {GF1000[i] <- GEVGF2(AMList[[i]], RP = 1000)}

  Mod5 <- mean(GF5)*ModRMED
  Mod10 <- mean(GF10)*ModRMED
  Mod20 <- mean(GF20)*ModRMED
  Mod50 <- mean(GF50)*ModRMED
  Mod100 <- mean(GF100)*ModRMED
  Mod200 <- mean(GF200)*ModRMED
  Mod500 <- mean(GF500)*ModRMED
  Mod1000 <- mean(GF1000)*ModRMED
  ResDF <- data.frame(ModRMED, Mod5, Mod10, Mod20, Mod50, Mod100,
                      Mod200, Mod500, Mod1000)
  RP <- c(2, 5, 10, 20, 50, 100, 200, 500, 1000)
  colnames(ResDF) <- as.character(RP)
  if(Plot == TRUE){
    if(is.null(main)) {main = ""} else {main = main}
    MyColours <- hcl.colors(9, palette = "Temps")
    matplot(Hrs, ResDF, type = "l", lty = 1, ylab = "Rainfall (mm)",
            xlab = "Hours", col = MyColours, lwd = 2, main = main)
    abline(v = seq(0, 100, by = 5), lty = 3)
    abline(h = seq(0, 1000, by = 20), lty = 3)
    legend("topleft", legend = as.character(RP), lty = 1, lwd = 2, col = MyColours, y.intersp = 0.7, cex = 0.7, x.intersp = 0.7)}
  ResDF <- round(ResDF, 1)
  ResDF <- data.frame(Hrs, ResDF)
  return(ResDF)
}
