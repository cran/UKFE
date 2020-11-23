
#' Kingston upon Thames daily flow and catchment precipitation 2000-10-01 to 2015-09-30
#'
#' A data.frame of three columns; Date, Precipitation (P), & daily mean flow (Q)
#'
#' @format A data frame with 5478 rows and 3 columns:
#' \describe{
#'   \item{Date}{Date}
#'   \item{P}{Precipitation, in mm}
#'   \item{Q}{Daily mean discharge, in m3/s}
#' }
#' @source \url{https://nrfa.ceh.ac.uk/data/station/meanflow/39001}
"ThamesPQ"


#' National River Flow Archive descriptors and calculated statistics for sites suitable for pooling
#'
#' A data.frame of catchment descriptors, Lmoments, Lmoment ratios, sample size and median annual maximum flow (QMED). NRFA Peak Flow Dataset - Version 9.
#' @details The functions for pooling group formation and estimation rely on this dataframe. However, the data frame is open for manipulation in case the user wishes to add sites that aren't included, or change parts where local knowledge has improved on the data. Although, usually, in the latter case, such changes will be more appropriately applied to the formed pooling group. If changes are made, they will only remain within the workspace. If a new workspace is opened and the UKFE package is loaded, the data frame will have returned to it's original state.
#'
#' @format A data frame with 545 rows and 30 variables
#' @source \url{https://nrfa.ceh.ac.uk/peak-flow-dataset}
"NRFAData"


#' National River Flow Archive (NRFA) annual maximum data for sites suitable for pooling
#'
#' A data.frame with three columns; Date, Flow, id. NRFA Peak Flow Dataset - Version 9
#'
#'
#' @format A data frame with 24638 rows and 3 columns
#' \describe{
#'   \item{Date}{Date}
#'   \item{Flow}{Annual maximum peak flow, m3/s}
#'   \item{id}{Station identification number}
#' }
#' @source \url{https://nrfa.ceh.ac.uk/peak-flow-dataset}
"AMSP"


#' National River Flow Archive descriptors and calculated statistics for sites suitable for QMED & pooling
#'
#' A data.frame of catchment & data descriptors relating to the median annual maximum flow (QMED). NRFA Peak Flow Dataset - Version 9
#'
#' @details The functions for QMED estimation and retreieval of catchment descriptors rely on this dataframe. However, the data frame is open for manipulation in case the user wishes to add sites that aren't included, or change parts where local knowledge has improved on the data. If changes are made, they will only remain within the workspace. If a new workspace is opened and the UKFE package is loaded, the data frame will have returned to it's original state.
#'
#' @format A data frame with 882 rows and 24 variables
#' @source \url{https://nrfa.ceh.ac.uk/peak-flow-dataset}
"QMEDData"


#' UK outline
#'
#' Easting and northing national grid reference points around the coast of the UK
#'
#' @format A data frame with 3867 rows and 2 variables
#' \describe{
#'   \item{X_BNG}{Easting, British national grid reference}
#'   \item{Y_BNG}{Northing, British national grid reference}
#' }
#' @source \url{https://environment.data.gov.uk/}
"UKOutline"


globalVariables(c("ThamesPQ", "NRFAData", "QMEDData", "UKOutline", "AMSP", "id", "URBEXT2000"))


# QuickResults ------------------------------------------------------------

#' Quick pooled results
#'
#' Provides pooled gauged, ungauged, or fake ungauged results, directly from the catchment descriptors
#'
#' The quick results function provides results with a default pooling group. If gauged = FALSE the median annual maximum flood (QMED) is estimated from catchment descriptors using the QMED equation and then adjusted with two of the closest un-urban gauged sites (can be changed to 0 or 1 donors). If the site is urban, an urban adjustment is made to the QMED and to the pooled growth curve. If gauged = TRUE QMED is the median of the gauged annual maxima and the growth curve is formed with the gauged weighting procedure (often known as enhanced single site). If the gauged catchment is urban, it's included in the pooling group and deurbanised before an urban adjustment is made to the final growth curve. If FUngauged = TRUE, the top site in the pooling group is excluded and the estimate is performed henceforth in the manner of gauged = FALSE. If the CDs are from a gauged site that is not in the list of sites that are considered suitable for pooling, it won't be included in the pooling group. In which case, if gauged = TRUE, the result, will be erroneous. For more details of the trend argument see the details for the PoolEst function.
#'@param CDs catchment descriptors derived from either GetCDs or ImportCDs
#'@param gauged logical argument with a default of FALSE. TRUE for gauged results and FALSE for ungauged
#'@param dons number of donors required with a choice of 0, 1, or 2
#'@param Qmed user supplied QMED which overrides the default QMED estimate
#'@param trend logical argument with a default of FALSE. TRUE adjusts the stationary QMED estimate to a non-stationary estimate
#'@param FUngauged logical argument with a default of FALSE. TRUE provides an ungauged estimate whilst excluding the gauged site (the site with the most similar CDs)
#'@param plot logical argument with a default of TRUE. TRUE provides an extreme value plot. FALSE prevents the plot
#'@param dist a choice of distribution for the estimates. The choices are "GenLog" or "GEV" the generalised logistic and the generalised extreme value distribution, respectively. The default is "GenLog"
#'@examples
#'#Get some catchment descriptors
#'CDs.73005 <- GetCDs(73005)
#'#Get default ungauged results
#'QuickResults(CDs.73005)
#'#Get gauged results with a GEV distribution
#'QuickResults(CDs.73005, gauged = TRUE, dist = "GEV")
#'#Get fake ungauged results with one donor
#'QuickResults(CDs.73005, FUngauged = TRUE, dons = 1)
#'
#'
#'@return A list of length two. Element one is a data frame with columns; return period (RP), peak flow estimates (Q) and growth factor estimates (GF). The second element is the estimated Lcv and Lskew (linear coefficient of variation and skewness). By default an extreme value plot is also returned
#'@author Anthony Hammond
QuickResults <- function(CDs, gauged = FALSE, dons = 2, Qmed = NULL, trend = FALSE, FUngauged = FALSE, plot = TRUE, dist = "GenLog") {
  Donor1 <- function(CDs, DonSite){
    QMED.cd <- 8.3062*CDs[1,2]^0.8510*0.1536^(1000/CDs[15,2])*CDs[8,2]^3.4451*0.0460^(CDs[5,2]^2)
    Site <- DonSite
    Donors <- DonAdj(CDs = CDs, rows = 500)
    Rw <- which(rownames(Donors) == DonSite)
    Result <- Donors[Rw, 27]
    return(Result)
  }
  Donor2 <- function(CDs, Sites) {
    rij <- function(d) {0.4598*exp(-0.0200*d)+(1-0.4598)*exp(-0.4785*d)}
    NGRDist <- function(i, j) {sqrt((i[1]-j[1])^2+(i[2]-j[2])^2)/1000}
    Site1 <- Sites[1]
    Site2 <- Sites[2]
    CDs.Site1 <- GetCDs(Site1)
    CDs.Site2 <- GetCDs(Site2)
    Dist1 <- NGRDist(c(CDs[19,2], CDs[20,2]), c(CDs.Site1[19,2], CDs.Site1[20,2]))
    Dist2 <- NGRDist(c(CDs[19,2], CDs[20,2]), c(CDs.Site2[19,2], CDs.Site2[20,2]))
    Dist12 <- NGRDist(c(CDs.Site1[19,2], CDs.Site1[20,2]), c(CDs.Site2[19,2], CDs.Site2[20,2]))
    ps1 <- rij(Dist1)
    p12 <- rij(Dist12)
    ps2 <- rij(Dist2)
    a1 <- (ps1-p12*ps2)/(1-p12^2)
    a2 <- (ps2-p12*ps1)/(1-p12^2)
    QMEDscd <- 8.3062*CDs[1,2]^0.8510*0.1536^(1000/CDs[15,2])*CDs[8,2]^3.4451*0.0460^(CDs[5,2]^2)
    QMED1cd <- 8.3062*CDs.Site1[1,2]^0.8510*0.1536^(1000/CDs.Site1[15,2])*CDs.Site1[8,2]^3.4451*0.0460^(CDs.Site1[5,2]^2)
    QMED2cd <- 8.3062*CDs.Site2[1,2]^0.8510*0.1536^(1000/CDs.Site2[15,2])*CDs.Site2[8,2]^3.4451*0.0460^(CDs.Site2[5,2]^2)
    QMED1obs <- QMEDData$QMED[which(rownames(QMEDData) == Site1)]
    QMED2obs <- QMEDData$QMED[which(rownames(QMEDData) == Site2)]
    QMEDs.adj <- QMEDscd*(QMED1obs/QMED1cd)^a1 * (QMED2obs/QMED2cd)^a2
    return(QMEDs.adj)
  }
  if(gauged == TRUE & FUngauged == TRUE) {print("Warning: Gauged & FUngauged are both TRUE. Gauged results provided")}
  if(gauged == FALSE) {
    PoolGroup <- Pool(CDs = CDs)
    if(CDs[18,2] > 0.03) {Ptemp <- Pool(CDs = CDs, iug = TRUE)} else {Ptemp <- Pool(CDs = CDs)}
    Ex <- as.numeric(rownames(Ptemp)[1])
    PoolGroupFun <- Pool(CDs = CDs, exclude = Ex)
    if(is.null(Qmed) == TRUE) { qmed <- 8.3062*CDs[1,2]^0.8510*0.1536^(1000/CDs[15,2])*CDs[8,2]^3.4451*0.0460^(CDs[5,2]^2)
    DonQMED <- DonAdj(CDs = CDs, rows = 500)
    if(FUngauged == TRUE) {DonQMED <- DonQMED[-1,]}
    UrbInd <- which(DonQMED$URBEXT2000 >0.03)
    if(length(UrbInd) < 1) {DonQMED <- DonQMED} else {DonQMED <- DonQMED[-UrbInd,]}
    D2Result <- Donor2(CDs = CDs, Sites = rownames(DonQMED)[1:2])
    D1Result <- Donor1(CDs = CDs, DonSite = rownames(DonQMED)[1])
    if(CDs[18,2] <= 0.03) {
      if(dons == 0) {qmed <- qmed}
      if(dons == 1) {qmed <- D1Result}
      if(dons == 2) {qmed <- D2Result} } else {
        if(dons == 0) {qmed <- QMED(CDs = CDs, UrbAdj = TRUE)[[1]]}
        if(dons == 1) {qmed <- QMED(CDs = CDs, Don1 = rownames(DonQMED)[1], UrbAdj = TRUE)[[1]]}
        if(dons == 2) {qmed <- QMED(CDs = CDs, Don2 = rownames(DonQMED)[1:2], UrbAdj = TRUE)[[1]]}
      }} else {qmed = Qmed}
    if(FUngauged == FALSE) {
      if(CDs[18,2] <= 0.03)  {Est <- PoolEst(PoolGroup, QMED = qmed, trend = trend, dist = dist)} else {Est <- PoolEst(PoolGroup, QMED = qmed, UrbAdj = TRUE, CDs = CDs, trend = trend, dist = dist)}
    } else {if(CDs[18,2] <= 0.03)  {Est <- PoolEst(PoolGroupFun, QMED = qmed, trend = trend, dist = dist)} else {Est <- PoolEst(PoolGroupFun, QMED = qmed, UrbAdj = TRUE, CDs = CDs, trend = trend, dist = dist)}}
  }

  if(gauged == TRUE) {
    if(CDs[18,2] > 0.03) {PoolGroup <- Pool(CDs = CDs, iug = TRUE, DeUrb = TRUE)} else {PoolGroup <- Pool(CDs = CDs)}
    Site <- rownames(PoolGroup)[1]
    AMAX <- GetAM(Site)
    if(is.null(Qmed) == TRUE) {qmed <- median(AMAX$Flow)} else {qmed <- Qmed}
    if(CDs[18,2] <= 0.03){Est <- PoolEst(PoolGroup, gauged = TRUE, QMED = qmed, trend = trend, dist = dist)} else {Est <- PoolEst(PoolGroup, gauged = TRUE, QMED = qmed, UrbAdj = TRUE, CDs = CDs, trend = trend, dist = dist)}
  }
  if(plot == TRUE) {
    if(CDs[18,2] <= 0.03){
      if(gauged == FALSE) {EVPool(PoolGroup, dist = dist)}
      if(gauged == TRUE) {EVPool(PoolGroup, gauged = TRUE, dist = dist)}} else {if(gauged == FALSE) {EVPool(PoolGroup, UrbAdj = TRUE, CDs = CDs, dist = dist)}
        if(gauged == TRUE) {EVPool(PoolGroup, gauged = TRUE, UrbAdj = TRUE, CDs = CDs, dist = dist)}}
  }
  return(Est)
}




# Pool --------------------------------------------------------------------

#' Create pooling group
#'
#' Function to develop a pooling group based on catchment descriptors
#'
#' A pooling group is created from a CDs object, derived from GetCDs or ImportCDs, or specifically with the catchment descriptors AREA, SAAR, FARL, & FPEXT (see arguments). To change the default pooling group one or more sites can be excluded using the 'exclude' option, which requires either a site reference or multiple site references in a vector. If this is done, the site with the next lowest similarity distance measure is added to the group (until the total number of years is at least N). Sites with URBEXT2000 (urban extent) > 0.03 are excluded by default. If a gauged assessment is required and the site of interest is urban it can be included by setting iug = TRUE. In which case the user may wish to de-urbanise the subject site's Lcv and Lskew (L-moment ratios) by setting DeUrb = TRUE. If the user has more data available for a parcticular site within the pooling group, the Lcv and Lskew for the site can be updated after the group has been finalised. An example of doing so is provided below. The pooling method is outlined in Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation.
#'@param CDs catchment descriptors derived from either GetCDs or ImportCDs
#'@param AREA catchment area in km2
#'@param SAAR catchment standard average annual rainfall (1961-1990) in mm
#'@param FARL catchment flood attenuation from reservoirs & lakes
#'@param FPEXT catchment floodplain extent. The proportion of the catchment that is estimated to be inundated by a 100-year flood
#'@param N minimum Number of total gauged record years for the pooling group
#'@param exclude sites to exclude from the pooling group. Either a single site reference or a vector of site references (numeric)
#'@param iug iug stands for 'include urban gauge'. It's a logical argument with default of FALSE. TRUE will over-ride the default and add the closest site in catchment descriptor space to the CDs provided if it has URBEXT2000 >= 0.03
#'@param DeUrb logical argument with a default of FALSE. If true, the Lcv and LSkew of the top site in the pooling group will be de-urbanised
#'@examples
#'#Get some catchment descriptors
#'CDs.73005 <- GetCDs(73005)
#'#Set up a pooling group object called Pool.73005 excluding sites 79005 & 71011.
#'#Then print the group to the console
#'Pool.73005 <- Pool(CDs.73005, exclude = c(79005, 71011))
#'Pool.73005
#'#Form a pooling group, called PoolGroup, with the catchment descriptors specifically
#'PoolGroup <- Pool(AREA = 1000, SAAR = 800, FARL = 1, FPEXT = 0.01)
#'#Form a pooling group using an urban catchment which is intended for enhanced
#'#single site estimation - by including it in the group.
#'CDs.39001 <- GetCDs(39001)
#'Pool.39001 <- Pool(CDs.39001, iug = TRUE, DeUrb = TRUE)
#'#Change the Lcv and LSkew of the top site in the pooling group to 0.19 & 0.18,
#'#respectively. Lcv and Lskew are in columns 16 & 17.
#'Pool.39001[1,c(16, 17)] <- c(0.19, 0.18)
#'@return A data.frame of the pooling group with site refence row names and 24 columns each providing catchment & gauge details for the sites in the pooling group.
#'@author Anthony Hammond
Pool <- function(CDs = NULL, AREA, SAAR, FARL, FPEXT, N = 500, exclude = NULL, iug = FALSE, DeUrb = FALSE){
  suppressWarnings(if(is.null(CDs) == TRUE){

    SDMs <- function(x, AREA, SAAR, FARL, FPEXT)
    {
      sqrt(
        (3.2*((log(AREA) - log(x[,1]))/1.28)^2)
        + (0.5*((log(SAAR) - log(x[,15]))/0.37)^2)
        + (0.1*((FARL - x[,8])/0.05)^2)
        + (0.2*((FPEXT - x[,9])/0.04)^2))
    }
    suppressWarnings(if (is.null(exclude) == TRUE) {NRFAData <- NRFAData} else
    {
      index <- NULL
      for (i in 1:length(exclude)) {index[i] <- which(row.names(NRFAData) == exclude[i])}
      NRFAData <- NRFAData[-index,]})
    Site <- SDMs(NRFAData, AREA, SAAR, FARL, FPEXT)
    Refs <- data.frame(row.names(NRFAData), Site)
    colnames(Refs) <- c("Site", "SDM")
    Refs.Order <- Refs[order(Refs$SDM),]
    Char.Sites <- Refs.Order$Site
    Char.Sites <- as.character(Char.Sites)
    Site.NRFA <- NRFAData[Char.Sites, ]
    UrbInd <- Char.Sites[1]
    ug.index <- which(row.names(NRFAData) == UrbInd)
    UrbUrbInd <- NRFAData[ug.index,22]
    Site.NRFA <- subset(Site.NRFA, URBEXT2000 <= 0.03)
    if(iug == FALSE) {Site.NRFA <- Site.NRFA}
    if(iug == TRUE & UrbUrbInd > 0.03) {Site.NRFA <- rbind(NRFAData[ug.index,], Site.NRFA)} else {Site.NRFA <- Site.NRFA}
    if(iug == TRUE & UrbUrbInd <= 0.03) {print("Warning: iug = TRUE and the closest site in catchment descriptor space has URBEXT2000 <0.03. Group formed as if iug = FALSE")}
    SDM <- SDMs(Site.NRFA,AREA, SAAR, FARL, FPEXT)
    Site.NRFA <- cbind(Site.NRFA, SDM)
    Cum.N <- NULL
    for (i in 1:length(Site.NRFA$N)) {Cum.N[i] <- sum(Site.NRFA$N[1:i])}
    min(which(Cum.N >= 500))
    Site.NRFA <- Site.NRFA[1:min(which(Cum.N >= N)),]
    Ds <-  function(x)
    {
      u.hat <- apply(tf, 2, mean)
      Res <- numeric(1)
      for (i in 1:length(Site.NRFA$N)) {Res <- Res+as.numeric(tf[i,]-u.hat)%*%t(as.numeric((tf[i,]-u.hat)))}
      D <- NULL
      for (i in 1:length(Site.NRFA$N)) {D[i] <- ((1/3)*length(Site.NRFA$N))*as.numeric(tf[i,]-u.hat)%*%solve(Res)%*%(as.numeric((tf[i,]-u.hat)))}
      return(D)
    }
    tf <- data.frame(Site.NRFA$Lcv, Site.NRFA$LSkew, Site.NRFA$LKurt)
    Discordancy <- Ds(tf)
    crit.vs <- c(1.333, 1.648, 1.917, 2.140, 2.329, 2.491, 2.632, 2.757, 2.869, 2.971, 3)
    xd <- seq(5,15)
    Crit.frame <- data.frame(xd, crit.vs)
    C.V <- Crit.frame[min(which(Crit.frame$xd >= length(Site.NRFA$N))),2]
    Discordant <- NULL
    for (i in 1:length(Discordancy)) {Discordant[i] <- isTRUE(Discordancy[i] > C.V)}
    Site.NRFA <- cbind(Site.NRFA, Discordancy, Discordant)
    Site.NRFA <- Site.NRFA[,-c(12,13,14,16,18,19,20,21,23)]
    colnames(Site.NRFA)[24] <- "Discordant?"
    if(DeUrb == FALSE) {Site.NRFA <- Site.NRFA} else
    { LcvCol <- which(colnames(Site.NRFA) == "Lcv")
    LskewCol <- which(colnames(Site.NRFA) == "LSkew")
    UrbCol <- which(colnames(Site.NRFA) == "URBEXT2000")
    DeUrbesLCV <- LcvUrb(Site.NRFA[1, LcvCol], Site.NRFA[1, UrbCol], DeUrb = TRUE)
    DeUrbesLSKEW <- LSkewUrb(Site.NRFA[1, LskewCol], Site.NRFA[1, UrbCol], DeUrb = TRUE)
    Site.NRFA[1,LcvCol] <- DeUrbesLCV
    Site.NRFA[1,LskewCol] <- DeUrbesLSKEW
    if(Site.NRFA[1,14] < 0.03) {print("Warning: DeUrb = TRUE and the top site in the group has URBEXT2000 < 0.03. The sites' Lcv & lskew have been adjusted as if it were urban" )}
    }
    return(Site.NRFA)
  } else {
    SDMs <- function(x, AREA, SAAR, FARL, FPEXT)
    {
      sqrt(
        (3.2*((log(AREA) - log(x[,1]))/1.28)^2)
        + (0.5*((log(SAAR) - log(x[,15]))/0.37)^2)
        + (0.1*((FARL - x[,8])/0.05)^2)
        + (0.2*((FPEXT - x[,9])/0.04)^2))
    }
    suppressWarnings(if (is.null(exclude) == TRUE) {NRFAData <- NRFAData} else
    {
      index <- NULL
      for (i in 1:length(exclude)) {index[i] <- which(row.names(NRFAData) == exclude[i])}
      NRFAData <- NRFAData[-index,]})
    Site <- SDMs(NRFAData, CDs[1,2], CDs[15,2], CDs[8,2], CDs[9,2])
    Refs <- data.frame(row.names(NRFAData), Site)
    colnames(Refs) <- c("Site", "SDM")
    Refs.Order <- Refs[order(Refs$SDM),]
    Char.Sites <- Refs.Order$Site
    Char.Sites <- as.character(Char.Sites)
    Site.NRFA <- NRFAData[Char.Sites, ]
    UrbInd <- Char.Sites[1]
    ug.index <- which(row.names(NRFAData) == UrbInd)
    UrbUrbInd <- NRFAData[ug.index,22]
    Site.NRFA <- subset(Site.NRFA, URBEXT2000 <= 0.03)
    if(iug == FALSE) {Site.NRFA <- Site.NRFA}
    if(iug == TRUE & UrbUrbInd > 0.03) {Site.NRFA <- rbind(NRFAData[ug.index,], Site.NRFA)} else {Site.NRFA <- Site.NRFA}
    if(iug == TRUE & UrbUrbInd <= 0.03) {print("Warning: iug = TRUE and the closest site in catchment descriptor space has URBEXT2000 <0.03. Group formed as if iug = FALSE")}
    SDM <- SDMs(Site.NRFA,CDs[1,2], CDs[15,2], CDs[8,2], CDs[9,2])
    Site.NRFA <- cbind(Site.NRFA, SDM)
    Cum.N <- NULL
    for (i in 1:length(Site.NRFA$N)) {Cum.N[i] <- sum(Site.NRFA$N[1:i])}
    min(which(Cum.N >= 500))
    Site.NRFA <- Site.NRFA[1:min(which(Cum.N >= N)),]
    Ds <-  function(x)
    {
      u.hat <- apply(tf, 2, mean)
      Res <- numeric(1)
      for (i in 1:length(Site.NRFA$N)) {Res <- Res+as.numeric(tf[i,]-u.hat)%*%t(as.numeric((tf[i,]-u.hat)))}
      D <- NULL
      for (i in 1:length(Site.NRFA$N)) {D[i] <- ((1/3)*length(Site.NRFA$N))*as.numeric(tf[i,]-u.hat)%*%solve(Res)%*%(as.numeric((tf[i,]-u.hat)))}
      return(D)
    }
    tf <- data.frame(Site.NRFA$Lcv, Site.NRFA$LSkew, Site.NRFA$LKurt)
    Discordancy <- Ds(tf)
    crit.vs <- c(1.333, 1.648, 1.917, 2.140, 2.329, 2.491, 2.632, 2.757, 2.869, 2.971, 3)
    xd <- seq(5,15)
    Crit.frame <- data.frame(xd, crit.vs)
    C.V <- Crit.frame[min(which(Crit.frame$xd >= length(Site.NRFA$N))),2]
    Discordant <- NULL
    for (i in 1:length(Discordancy)) {Discordant[i] <- isTRUE(Discordancy[i] > C.V)}
    Site.NRFA <- cbind(Site.NRFA, Discordancy, Discordant)
    Site.NRFA <- Site.NRFA[,-c(12,13,14,16,18,19,20,21,23)]
    colnames(Site.NRFA)[24] <- "Discordant?"
    if(DeUrb == FALSE) {Site.NRFA <- Site.NRFA} else
    { LcvCol <- which(colnames(Site.NRFA) == "Lcv")
    LskewCol <- which(colnames(Site.NRFA) == "LSkew")
    UrbCol <- which(colnames(Site.NRFA) == "URBEXT2000")
    DeUrbesLCV <- LcvUrb(Site.NRFA[1, LcvCol], Site.NRFA[1, UrbCol], DeUrb = TRUE)
    DeUrbesLSKEW <- LSkewUrb(Site.NRFA[1, LskewCol], Site.NRFA[1, UrbCol], DeUrb = TRUE)
    Site.NRFA[1,LcvCol] <- DeUrbesLCV
    Site.NRFA[1,LskewCol] <- DeUrbesLSKEW
    if(Site.NRFA[1,14] < 0.03) {print("Warning: DeUrb = TRUE and the top site in the group has URBEXT2000 < 0.03")}
    }
    return(Site.NRFA)
  } )
  }


# PoolEst -----------------------------------------------------------------

#' Pooled flood estimates
#'
#' Provides pooled results from a pooling group - gauged, ungauged and with urban adjustment if necessary.
#'
#' PoolEst is a function to provide results from a pooling group derived using the Pool function. QMED (median annual maximum flow) needs to be supplied and can be derived from the QMED function for ungauged estimates or the annual maximum sample for gauged estimates. If the catchment of interest is urban, the UrbAdj argument can be set to TRUE. If this is done, either URBEXT (urban extent) needs to be provided or the catchment descriptors, derived from ImportCDs or GetCDs. The methods for estimating pooled growth curves are according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation. The methods for estimating the L-moments and growth factors are outlined in the Flood Estimation Handbook (1999), volume 3. When UrbAdj = TRUE, urban adjustment is applied to the QMED estimate according to the method outlined in the guidance by Hydro Wallingford solutions: 'WINFAP 4 Urban Adjustment Procedures'. If trend = TRUE & gauged = FALSE, the QMED is multiplied by a trend coefficient. The coefficient was derived by calculating a weighted (by sample size) mean proportional change in the 2-year flow, from the UK National River Flow Archive benchmark sites considered suitable for pooling. The weighted per year change was first calculated and then multiplied by half the mean sample size of sites suitable for QMED. If trend = TRUE and gauged = TRUE, the weighted per year change is multiplied by half the sample size of the first site in the pooling group. This approach attempts to include a generic non-stationarity in the pooled estimates by adjusting the location parameter. Amongst other assumptions (such as the change being linear and only to the location of the distribution), it makes the assumption that apparent trends at individual sites are unreliable due to short record lengths or site specific due to human influence, but across many sites the mean increase in median peak discharge is representative of a non-stationary process across the UK. The trend is applied to whatever is in the QMED argument. Therefore, trend should equal FALSE if the QMED estimate has been user adjusted for trend already. If Lcv & Lskew have also been user adjusted for a gauged site, these can be changed in the pooling group (see Pool function details). The per year, weighted mean QMED trend, was estimated to be 0.12 percent +/- 0.05 (95 percent uncertainty - calculated by weighted resampling).
#'
#'
#'@param x pooling group derived from the Pool function
#'@param gauged logical argument with a default of FALSE. TRUE for gauged results and FALSE for ungauged
#'@param QMED estimate of the median annual maximum flow
#'@param dist a choice of distribution for the estimates. The choices are "GenLog" or "GEV"; the generalised logistic and the generalised extreme value distribution, respectively. The default is "GenLog"
#'@param RP return period of interest. By default the following RPs are provided: 2, 5, 10, 20, 50, 75, 100, 200, 500, 1000
#'@param UrbAdj logical argument with a default of FALSE. When TRUE, an urban adjustment is applied to the pooled Lcv and LSkew
#'@param CDs catchment descriptors derived from either GetCDs or ImportCDs
#'@param URBEXT the catchment URBEXT2000, to be supplied if UrbAdj is TRUE and if CDs have not been
#'@param trend logical argument with a default of FALSE. TRUE adjusts the stationary QMED estimate to a non-stationary estimate
#'@examples
#'#Get some catchment descriptors and form a pooling group. It's urban and
#'#therefore the site of interest is not included.
#'CDs.27083 <- GetCDs(27083)
#'Pool.27083 <- Pool(CDs.27083)
#'#Get results for the ungauged case, with urban adjustment
#'PoolEst(Pool.27083, QMED = 11.941, UrbAdj = TRUE, CDs = CDs.27083)
#'#Form the group again with the urban gauge included & undertake a gauged estimate
#'#with urban adjustment. QMED in this example is estimated as the median of the annual
#'#maximum series for site 27083.
#'PoolG.27083 <- PoolG.27083 <- Pool(CDs.27083, iug = TRUE, DeUrb = TRUE)
#'PoolEst(PoolG.27083, QMED = 12.5, UrbAdj = TRUE, CDs = CDs.27083)
#'
#'@return A list of length two. Element one is a data frame with columns; return period (RP), peak flow estimates (Q) and growth factor estimates (GF). The second element is the estimated Lcv and Lskew.
#'@author Anthony Hammond
PoolEst <- function(x, gauged = FALSE, QMED, dist = "GenLog", RP = c(2,5,10,20,50,75,100,200,500,1000), UrbAdj = FALSE, CDs = NULL, URBEXT = NULL, trend = FALSE) {
  if(ncol(x) != 24) stop ("x must be a pooled group. Pooled groups can be created with the Pool() function")
  if(UrbAdj == TRUE) {
  if(is.null(URBEXT) == TRUE & is.null(CDs) == TRUE) stop("if Urbadj = TRUE, URBEXT or CDs must be provided")
  if(is.null(URBEXT) == TRUE) {URBEXT2000 <- CDs[18,2]} else {URBEXT2000 <- URBEXT}}
  if(dist == "GenLog") {func <- GenLogGF} else {func <- GEVGF}
  if(gauged == FALSE) {lcv <- WungLcv(x)} else {lcv <- WGaugLcv(x)}
  if(gauged == FALSE) {lskew <- WungLSkew(x)} else {lskew <- WGaugLSkew(x)}
  if(UrbAdj == TRUE) {lcv <- lcv*0.68654^(1.567*URBEXT2000)} else {lcv <- lcv}
  if(UrbAdj == TRUE) {lskew <- ((lskew+1)*1.096017^(1.567*URBEXT2000))-1} else {lskew <- lskew}
  Zt <- func(lcv, lskew, RP = RP)
  GF <- as.numeric(format(round(Zt, 3), nsmall = 3))
  if(trend == TRUE & gauged == FALSE) {QMED <- QMED*1.023}
  if(trend == TRUE & gauged == TRUE) {QMED <- QMED+(QMED*0.001048557*(x$N[1]/2))}
  Qt <- Zt*QMED
  Q <- as.numeric(format(round(Qt, 3), nsmall = 3))
  PooledLcv <- lcv
  PooledLSkew <- lskew
  res <- data.frame(RP, Q, GF)
  Pars <- cbind(PooledLcv, PooledLSkew)
  return(list(res, Pars))
}


# OptimPars ---------------------------------------------------------------

#' Optimise distribution parameters
#'
#' Estimates the parameters of the generalised extreme value or generalised logistic distribution from known return period estimates
#'
#' Given a dataframe with return periods (RPs) in the first column and associated estimates in the second column, this function provides an estimate of the distribution parameters. Ideally the first RP should be 2. Extrapolation outside the RPs used for calibration comes with greater uncertainty.
#'@param x a data.frame with RPs in the first column and associated estimates in the second column
#'@param dist a choice of distribution for the estimates. The choices are "GenLog" or "GEV" the generalised logistic and the generalised extreme value distribution, respectively. The default is "GenLog"
#'@examples
#'#Get some catchment descriptors and some quick results. Then estmate the GenLog parameters
#'Results <- QuickResults(GetCDs(96001))[[1]]
#'OptimPars(Results[,1:2])
#'
#'@return The location, scale and shape parameters for the generalised logistic or Generalised extreme value distribution.
#' @author Anthony Hammond
OptimPars <- function(x, dist = "GenLog") {
  res <- x
  RP <- x[,1]
  Q <- x[,2]
  if(dist == "GenLog") {
    min.GL <- function(data, par) {
      with(data, sum(((par[1]+par[2]/par[3]*(1-(RP-1)^-par[3])-Q)^2)))}
    result <- optim(par = c(x[1,1], mean((x[1,1]/4),(x[1,1]/3)), 0.01), fn = min.GL, data = res)
  } else {
    min.GEV <- function(data, par) {
      with(data, sum(((par[1]+par[2]/par[3]*(1-(-log(1-1/RP))^par[3])-Q)^2)))}
    result <- optim(par = c(x[1,1], mean((x[1,1]/4),(x[1,1]/3)), 0.01), fn = min.GEV, data = res)}
  Pars <- result$par
  Pars <- data.frame(Pars[1], Pars[2], Pars[3])
  colnames(Pars) <- c("loc", "scale", "shape")
  return(Pars)
}




# DistFuncs ---------------------------------------------------------------

#'Generalised logistic distribution growth factors
#'
#'Estimated growth factors as a function of return period, with inputs of Lcv & LSkew (linear coefficient of variation & linear skewness)
#'
#'@details Growth factors are calculated by the method outlined in the Flood Estimation Handbook, volume 3, 1999.
#'
#' @param lcv linear coefficient of variation
#' @param lskew linear skewness
#' @param RP return period
#' @examples
#' #Estimate the 50-year growth factors from an Lcv and Lskew of 0.17 and 0.04, respectively.
#' GenLogGF(0.1704826, 0.04211332, RP = 50)
#' @return Generalised logistic estimated growth factor
#' @author Anthony Hammond

GenLogGF <- function(lcv, lskew, RP) {
  k <- -lskew
  B <- lcv*k*sin((pi)*k)/(k*pi*(k+lcv)-lcv*sin((pi)*k))
  zt <- 1+(B/k)*(1-(RP-1)^lskew)
  return(zt)
}

#'Generalised extreme value distribution growth factors
#'
#'Estimated growth factors as a function of return period, with inputs of Lcv & LSkew (linear coefficient of variation & linear skewness)
#'
#'@details Growth factors are calculated by the method outlined in the Flood Estimation Handbook, volume 3, 1999.
#' @param lcv linear coefficient of variation
#' @param lskew linear skewness
#' @param RP return period
#' @examples
#' #Estimate the 50-year growth factors from Lcv = 0.1704 and Lskew = 0.0421
#' GEVGF(0.1704826, 0.04211332, RP = 50)
#' @return Generalised extreme value estimated growth factor
#' @author Anthony Hammond
GEVGF <- function(lcv,lskew, RP) {
  C <- (2/(3+lskew)) - (log(2)/log(3))
  kgev <- 7.859*C+2.95548*C^2
  Bgev <- (kgev*lcv)/(lcv*(gamma(1+kgev)-(log(2))^kgev)+gamma(1+kgev)*(1-2^-kgev))
  zt <- 1+(Bgev/kgev)*(log(2)^kgev - (log(RP/(RP-1)))^kgev)
  return(zt)
}

#'Generalised Pareto distribution growth factors
#'
#'Estimated growth factors as a function of return period, with inputs of Lcv & LSkew (linear coefficient of variation & linear skewness)
#'
#'@details Growth factors (GF) are calculated by the method outlined in the Flood Estimation Handbook, volume 3, 1999. The average number of peaks per year argument (ppy) is for the function to convert from the peaks over threshold (POT) scale to the annual scale. For example, if there are 3 peaks per year, the probability associated with the 100-yr return period estimate would be 0.01/3 (i.e. an RP of 300 on the POT scale) rather than 0.01.
#' @param lcv linear coefficient of variation
#' @param lskew linear skewness
#' @param RP return period
#' @param ppy peaks per year
#' @examples
#' #Get POT flow data from the Thames at Kingston (noting the no. peaks per year).
#' #Then estimate the 100-year growth factor with lcv and lskew estimates
#' TPOT <- POTextract(ThamesPQ[,c(1,3)], thresh = 0.90)
#' GenParetoGF(Lcv(TPOT$peak), LSkew(TPOT$peak), RP = 100, ppy = 1.867)
#' #multiply by the median of the POT data for an estimate of the 100-yr flood
#' GenParetoGF(Lcv(TPOT$peak), LSkew(TPOT$peak), RP = 100, ppy = 1.867)*median(TPOT$peak)
#' @return Generalised Pareto estimated growth factor
#' @author Anthony Hammond
GenParetoGF <- function(lcv, lskew, RP, ppy = 1) {
  k <- (1-3*lskew)/(1+lskew)
  Bgp <- (lcv*k*(1+k)*(2+k))/(k-lcv*(2+k)*(2^-k*(1+k)-1))
  RPppy <- 1/((1/RP)/ppy)
  Zt <- 1 + (Bgp/k) *((2^-k)-(1-(1-(1/RPppy)))^k)
  return(Zt)
}


#'Generalised logistic distribution - estimates directly from sample
#'
#'Estimated quantiles as a function of return period (RP) and vice versa, directly from the data
#'
#'If the argument q is used, it overrides RP and provides RP as a function of q (magnitude of variable) as opposed to q as a function of RP. The parameters are estimated by the method of L-moments, as detailed in 'Hosking J. Wallis J. 1997 Regional Frequency Analysis: An Approach Based on L-moments. Cambridge University Press, New York'. The trend argument allows the location parameter to move in line with the observed linear trend of the sample.
#' @param x numeric vector (block maxima sample)
#' @param RP return period (default = 100)
#' @param q quantile (magnitude of variable)
#' @param trend logical argument with default of FALSE. If TRUE, a linear adjustment to the location parameter is made to account for non-stationarity
#' @examples
#' #Get an annual maximum sample and estimate the 50-year RP
#' AM.27090 <- GetAM(27090)
#' GenLogAM(AM.27090$Flow, RP = 50)
#' #Estimate the RP for a 600m3/s discharge
#' GenLogAM(AM.27090$Flow, q = 600)
#' #Estimate the 50-year RP allowing for non-stationarity in the location parameter
#' GenLogAM(AM.27090$Flow, RP = 50, trend = TRUE)
#' @return quantile as a function of RP or vice versa, with the option of accounting for the linear trend in the sample
#' @author Anthony Hammond
GenLogAM <- function(x, RP = 100, q = NULL, trend = FALSE)
{
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x, na.rm = TRUE)
  b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  L3 <- 6*b2-6*b1+b0
  L4 <- 20*b3-30*b2+12*b1-b0
  Lcv <- L2/L1
  LSkew <- L3/L2
  k <- -LSkew
  a <- (L2*sin(k*pi))/(k*pi)
  loc <- b0-a*((1/k)-(pi/sin(k*pi)))
  if(is.null(q) == TRUE) {res <- loc+ a/k * (1-(RP-1) ^-k)}
  else {
    y <- -k^(-1) * log(1 - k * (q - loc)/a)
    P <- 1-(1/(1+exp(-y)))
    res <- 1/P
        }
  m <- function(i, j) {sum((i-mean(i))*(j-mean(j)))/sum((i-mean(i))^2)}
  M <- m(i = seq(1, length(x)), j = x)
  b <- mean(x)-M*mean(seq(1,length(x)))
  LM <- (M*(length(x))+b)-(M*median(seq(1,length(x)))+b)
  LocTrend <- loc+LM
  if (is.null(q) == TRUE) {resTrend <- LocTrend+ a/k * (1-(RP-1) ^-k)}
  else {
  yTrend <- -k^(-1) * log(1 - k * (q - LocTrend)/a)
  Ptrend <- P <- 1-(1/(1+exp(-yTrend)))
  resTrend <- 1/Ptrend}
  if(trend == FALSE) {return(res)} else {return(resTrend)}
}



#'Generalised extreme value distribution - estimates directly from sample
#'
#'Estimated quantiles as function of return period (RP) and vice versa, directly from the data
#'
#'If the argument q is used, it overrides RP and provides RP as a function of q (magnitude of variable) as opposed to q as a function of RP. The parameters are estimated by the method of L-moments, as detailed in 'Hosking J. Wallis J. 1997 Regional Frequency Analysis: An Approach Based on L-moments. Cambridge University Press, New York'. The trend argument allows the location parameter to move in line with the observed linear trend of the sample
#' @param x numeric vector (block maxima sample)
#' @param RP return period (default = 100)
#' @param q quantile (magnitude of variable)
#' @param trend logical argument with default of FALSE. If TRUE, a linear adjustment to the location parameter is made to account for non-stationarity
#' @examples
#' #Get an annual maximum sample and estimate the 50-year RP
#' AM.27090 <- GetAM(27090)
#' GEVAM(AM.27090$Flow, RP = 50)
#' #Estimate the RP for a 600m3/s discharge
#' GEVAM(AM.27090$Flow, q = 600)
#' #Estimate the 50-year RP allowing for non-stationarity in the location parameter
#' GEVAM(AM.27090$Flow, RP = 50, trend = TRUE)
#' @return quantile as a function of RP or vice versa, with the option of accounting for the linear trend in the sample
#' @author Anthony Hammond
GEVAM <- function(x, RP = 100, q = NULL, trend = FALSE)
{
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x, na.rm = TRUE)
  b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  L3 <- 6*b2-6*b1+b0
  L4 <- 20*b3-30*b2+12*b1-b0
  Lcv <- L2/L1
  LSkew <- L3/L2
  C <- (2/(3+LSkew)) - (log(2)/log(3))
  k <- 7.859*C+2.95548*C^2
  a <- (L2*k)/((1-2^-k)*gamma(1+k))
  loc <- b0-a*((1-gamma(1+k))/k)
  if(is.null(q) == TRUE) {res <- loc+a/k*(1-(-log(1-1/RP))^k)} else {
  y <- -k^(-1) * log(1 - k * (q - loc)/a)
  P <- 1-(exp(-exp(-y)))
  res <- 1/P}
  m <- function(i, j) {sum((i-mean(i))*(j-mean(j)))/sum((i-mean(i))^2)}
  M <- m(i = seq(1, length(x)), j = x)
  b <- mean(x)-M*mean(seq(1,length(x)))
  LM <- (M*(length(x))+b)-(M*median(seq(1,length(x)))+b)
  LocTrend <- loc+LM
  if (is.null(q) == TRUE) {resTrend <- LocTrend+a/k*(1-(-log(1-1/RP))^k)}
  else {
    yTrend <- -k^(-1) * log(1 - k * (q - LocTrend)/a)
    Ptrend <- 1-(exp(-exp(-yTrend)))
    resTrend <- 1/Ptrend}
  if(trend == FALSE) {return(res)} else {return(resTrend)}
}



#'Generalised Pareto distribution - estimates directly from sample
#'
#'Estimated quantiles as function of return period (RP) and vice versa, directly from the data
#'
#'If the argument q is used, it overrides RP and provides RP as a function of q (magnitude of variable) as opposed to q as a function of RP. The average number of peaks per year argument (ppy) is for the function to convert from the peaks over threshold (POT) scale to the annual scale. For example, if there are 3 peaks per year, the probability associated with the 100-yr return period estimate would be 0.01/3 (i.e. an RP of 300 on the POT scale) rather than 0.01. The parameters are estimated by the method of L-moments, as detailed in 'Hosking J. Wallis J. 1997 Regional Frequency Analysis: An Approach Based on L-moments. Cambridge University Press, New York'.
#' @param x numeric vector (block maxima sample)
#' @param ppy peaks per year
#' @param RP return period (default = 100)
#' @param q quantile (magnitude of variable)
#' @examples
#' #Get a POT series and estimate the 50-year RP
#' ThamesPOT <- POTextract(ThamesPQ[,c(1,3)], thresh = 0.90)
#' GenParetoPOT(ThamesPOT$peak, ppy = 1.867, RP = 50)
#' #Estimate the RP for a 600m3/s discharge
#' GenParetoPOT(ThamesPOT$peak, ppy = 1.867, q = 600)
#' @return quantile as a function of RP or vice versa
#' @author Anthony Hammond
GenParetoPOT <- function(x, ppy = 1, RP = 100, q = NULL)
{
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x, na.rm = TRUE)
  b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  L3 <- 6*b2-6*b1+b0
  L4 <- 20*b3-30*b2+12*b1-b0
  Lcv <- L2/L1
  LSkew <- L3/L2
  k <- (1-3*LSkew)/(1+LSkew)
  a <- (1+k)*(2+k)*L2
  loc <- L1-(2+k)*L2
  if(is.null(q) == TRUE) {res <- loc+a*(1-(1-(1-(1/RP)/ppy))^k)/k} else {
    y <- -k^-1*log(1-k*(q-loc)/a)
    P <- 1-(1-exp(-y))
    RPPOT <- 1/P
    res <- RPPOT/ppy
  }
  return(res)
}


#'Generalised logistic distribution estimates from parameters
#'
#'Estimated quantiles as function of return period (RP) and vice versa, from user input parameters
#'
#'If the argument q is used, it overrides RP and provides RP as a function of q (magnitude of variable) as opposed to q as a function of RP.
#' @param loc location parameter
#' @param scale scale parameter
#' @param shape shape parameter
#' @param q quantile. magnitude of the variable under consideration
#' @param RP return period
#' @examples
#' #Get an annual maximum sample, estimate the parameters and estimate 50-year RP
#' AM.27090 <- GetAM(27090)
#' GenLogPars(AM.27090$Flow)
#' GenLogEst(298.6416, 51.37096, -0.04211332, RP = 50)
#' #Estimate the RP for a 600m3/s discharge
#' GenLogEst(298.6416, 51.37096, -0.04211332, q = 600)
#' @return quantile as a function of RP or vice versa
#' @author Anthony Hammond
GenLogEst <- function(loc, scale, shape, q = NULL, RP = 100) {
  if(is.null(q) == TRUE) {res <- loc+ scale/shape * (1-(RP-1) ^-shape)}
  else {
    y <- -shape^(-1) * log(1 - shape * (q - loc)/scale)
    P <- 1-(1/(1+exp(-y)))
    res <- 1/P
  }
  return(res)
}


#'Generalised Pareto distribution estimates from parameters
#'
#'Estimated quantiles as function of return period (RP) and vice versa, from user input parameters
#'
#'If the argument q is used, it overrides RP and provides RP as a function of q (magnitude of variable) as opposed to q as a function of RP. The average number of peaks per year argument (ppy) is for the function to convert from the peaks over threshold (POT) scale to the annual scale. For example, if there are 3 peaks per year, the probability associated with the 100-yr return period estimate would be 0.01/3 (i.e. an RP of 300 on the POT scale) rather than 0.01.
#' @param loc location parameter
#' @param scale scale parameter
#' @param shape shape parameter
#' @param q quantile. magnitude of the variable under consideration
#' @param RP return period
#' @param ppy peaks per year. Default is one
#' @examples
#' #Get a POT sample, estimate the parameters, and estimate 50-year RP
#' ThamesPOT <- POTextract(ThamesPQ[,c(1,3)], thresh = 0.90)
#' GenParetoPars(ThamesPOT$peak)
#' GenParetoEst(174.2862, 127.4085, 0.1805716, ppy = 1.867, RP = 50)
#' #Estimate the RP for a 600m3/s discharge
#' GenParetoEst(174.2862, 127.4085, 0.1805716, ppy = 1.867, q = 600)
#' @return quantile as a function of RP or vice versa
#' @author Anthony Hammond
GenParetoEst <- function(loc, scale, shape, q = NULL, RP = 100, ppy = 1) {
  if(is.null(q) == TRUE) {res <- loc+scale*(1-(1-(1-(1/RP)/ppy))^shape)/shape} else {
    y <- -shape^-1*log(1-shape*(q-loc)/scale)
    P <- 1-(1-exp(-y))
    RPPOT <- 1/P
    res <- RPPOT/ppy
  }
  return(res)
}


#'Generalised extreme value distribution estimates from parameters
#'
#'Estimated quantiles as function of return period (RP) and vice versa, from user input parameters
#'
#'If the argument q is used, it overrides RP and provides RP as a function of q (magnitude of variable) as opposed to q as a function of RP.
#' @param loc location parameter
#' @param scale scale parameter
#' @param shape shape parameter
#' @param q quantile. magnitude of the variable under consideration
#' @param RP return period
#' @examples
#' #Get an annual maximum sample, estimate the parameters and estimate 50-year RP
#' AM.27090 <- GetAM(27090)
#' GEVPars(AM.27090$Flow)
#' GEVEst(298.6416, 51.37096, -0.04211332, RP = 50)
#' #Estimate the RP for a 600m3/s discharge
#' GEVEst(298.6416, 51.37096, -0.04211332, q = 600)
#' @return quantile as a function of RP or vice versa
#' @author Anthony Hammond
GEVEst <- function(loc, scale, shape, q = NULL, RP = 100) {
  if(is.null(q) == TRUE) {res <- loc+scale/shape*(1-(-log(1-1/RP))^shape)} else {
    y <- -shape^(-1) * log(1 - shape * (q - loc)/scale)
    P <- 1-(exp(-exp(-y)))
    res <- 1/P}
  return(res)
}


#'Generalised extreme value distribution parameter estimates
#'
#'Estimated parameters from a sample (with Lmoments or maximum likelihood estimation) or from L1 (first L-moment), Lcv (linear coefficient of variation), and LSkew (linear skewness)
#'
#'@details The L-moment estimated parameters are by the method detailed in 'Hosking J. Wallis J. 1997 Regional Frequency Analysis: An Approach Based on L-moments. Cambridge University Press, New York'
#'
#' @param x numeric vector. The sample
#' @param mle logical argument with a default of FALSE. If FALSE the parameters are estimated with Lmoments, if TRUE the parameters are estimated by maximum likelihood estimation
#' @param L1 first Lmoment
#' @param LCV linear coefficient of variation
#' @param LSKEW linear skewness
#' @examples
#' #Get an annual maximum sample and estimate the parameters using Lmoments
#' AM.27090 <- GetAM(27090)
#' GEVPars(AM.27090$Flow)
#' #Estimate parameters using MLE
#' GEVPars(AM.27090$Flow, mle = TRUE)
#' #calculate Lmoments and estimate the parmeters with L1, Lcv and Lskew
#' Lmoms(AM.27090$Flow)
#' GEVPars(L1 = 302.2076, LCV = 0.1704826, LSKEW = 0.04211332)
#' @return Parameter estimates (location, scale, shape)
#' @author Anthony Hammond
GEVPars <- function(x = NULL, mle = FALSE, L1, LCV, LSKEW) {
  if(mle == FALSE){
    if(is.null(x)) {C <- (2/(3+LSKEW)) - (log(2)/log(3))
    Shape <- 7.859*C+2.95548*C^2
    L2 <- L1*LCV
    Scale <- (L2*Shape)/((1-2^-Shape)*gamma(1+Shape))
    Loc <- L1-Scale*((1-gamma(1+Shape))/Shape)
    return(data.frame(Loc, Scale, Shape))}
    else {
      L1 <- mean(x, na.rm = TRUE)
      LCV <- Lcv(x)
      LSKEW <- LSkew(x)
      C <- (2/(3+LSKEW)) - (log(2)/log(3))
      Shape <- 7.859*C+2.95548*C^2
      L2 <- L1*LCV
      Scale <- (L2*Shape)/((1-2^-Shape)*gamma(1+Shape))
      Loc <- L1-Scale*((1-gamma(1+Shape))/Shape)
      return(data.frame(Loc, Scale, Shape))}}
  else {
    pars <- c(mean(x), sd(x)/1.5, 0.01)
    max.lhd <- function(q, par) {
      abs(sum(log(gev.pdf(q, loc = par[1], scale = par[2], shape = par[3]))))
    }
    gev.pdf <- function(q, loc, scale, shape) {
      y <- -shape^-1*log(1-shape*(q-loc)/scale)
      p <- scale^-1*exp(1)^(-(1-shape)*y-exp(1)^(-y))
      return(p)
    }
    result <- suppressWarnings(optim(par = pars, fn = max.lhd, q = x))
    loc <- result$par[1]
    scale <- result$par[2]
    shape <- result$par[3]
    log.likelihood <- -result$value[1]
    message <- result$message
    Res <- data.frame(loc, scale, shape, log.likelihood)
    return(Res)
  }
}

#'Generalised logistic distribution parameter estimates
#'
#'Estimated parameters from a sample (with Lmoments or maximum likelihood estimation) or from L1 (first L-moment), Lcv (linear coefficient of variation), and LSkew (linear skewness)
#'
#'@details The L-moment estimated parameters are by the method detailed in 'Hosking J. Wallis J. 1997 Regional Frequency Analysis: An Approach Based on L-moments. Cambridge University Press, New York'
#' @param x numeric vector. The sample
#' @param mle logical argument with a default of FALSE. If FALSE the parameters are estimated with Lmoments, if TRUE the parameters are estimated by maximum likelihood estimation.
#' @param L1 first Lmoment
#' @param LCV linear coefficient of variation
#' @param LSKEW linear skewness
#' @examples
#' #Get an annual maximum sample and estimate the parameters using Lmoments
#' AM.27090 <- GetAM(27090)
#' GenLogPars(AM.27090$Flow)
#' #Estimate parameters using MLE
#' GenLogPars(AM.27090$Flow, mle = TRUE)
#' #calculate Lmoments and estimate the parmeters with L1, Lcv and Lskew
#' Lmoms(AM.27090$Flow)
#' GenLogPars(L1 = 302.2076, LCV = 0.1704826, LSKEW = 0.04211332)
#' @return Parameter estimates (location, scale, shape)
#' @author Anthony Hammond
GenLogPars <- function(x = NULL, mle = FALSE, L1, LCV, LSKEW) {
  if(mle == FALSE){
    if(is.null(x)) {
      Shape <- -LSKEW
      L2 <- L1*LCV
      Scale <- (L2*sin(Shape*pi))/(Shape*pi)
      Loc <- L1-Scale*((1/Shape)-(pi/sin(Shape*pi)))
      return(data.frame(Loc, Scale, Shape))}
    else {
      L1 <- mean(x, na.rm = TRUE)
      LCV <- Lcv(x)
      LSKEW <- LSkew(x)
      Shape <- -LSKEW
      L2 <- L1*LCV
      Scale <- (L2*sin(Shape*pi))/(Shape*pi)
      Loc <- L1-Scale*((1/Shape)-(pi/sin(Shape*pi)))
      return(data.frame(Loc, Scale, Shape))}}
  else {
    pars <- c(mean(x), sd(x)/1.5, 0.01)
    min.SLS <- function(q, par) {
      abs(sum(log(gl.pdf(q, loc = par[1], scale = par[2], shape = par[3]))))
    }
    gl.pdf <- function(q, loc, scale, shape) {
      y <- -shape^-1*log(1-shape*(q-loc)/scale)
      f <- (scale^-1*exp(1)^(-(1-shape)*y))/(1+exp(1)^-y)^2
      return(f)
    }
    result <- suppressWarnings(optim(par = pars, fn = min.SLS, q = x))
    loc <- result$par[1]
    scale <- result$par[2]
    shape <- result$par[3]
    log.likelihood <- -result$value[1]
    message <- result$message
    Res <- data.frame(loc, scale, shape, log.likelihood)
    return(Res)
  }

}

#'Generalised Pareto distribution parameter estimates
#'
#'Estimated parameters from a sample (with Lmoments or maximum likelihood estimation) or from L1 (first L-moment), Lcv (linear coefficient of variation), and LSkew (linear skewness)
#'
#'@details The L-moment estimated parameters are by the method detailed in 'Hosking J. Wallis J. 1997 Regional Frequency Analysis: An Approach Based on L-moments. Cambridge University Press, New York'
#'
#' @param x numeric vector. The sample
#' @param mle logical argument with a default of FALSE. If FALSE the parameters are estimated with Lmoments, if TRUE the parameters are estimated by maximum likelihood estimation
#' @param L1 first Lmoment
#' @param LCV linear coefficient of variation
#' @param LSKEW linear skewness
#' @examples
#' #Get a peaks over threshold sample and estimate the parameters using Lmoments
#' ThamesPOT <- ThamesPOT <- POTextract(ThamesPQ[,c(1,3)], thresh = 0.90)
#' GenParetoPars(ThamesPOT$peak)
#' #Estimate parameters using MLE
#' GenParetoPars(ThamesPOT$peak, mle = TRUE)
#' #calculate Lmoments and estimate the parmeters with L1, Lcv and Lskew
#' Lmoms(ThamesPOT$peak)
#' GenParetoPars(L1 = 282.2071, LCV = 0.1753749, LSKEW = 0.2576356)
#' @return Parameter estimates (location, scale, shape)
#' @author Anthony Hammond
GenParetoPars <- function(x = NULL, mle = FALSE, L1, LCV, LSKEW) {
  if(mle == FALSE){
    if(is.null(x)) {
      Shape <- (1-3*LSKEW)/(1+LSKEW)
      L2 <- L1*LCV
      Scale <- (1+Shape)*(2+Shape)*L2
      Loc <- L1-(2+Shape)*L2
      return(data.frame(Loc, Scale, Shape))}
    else {
      L1 <- mean(x, na.rm = TRUE)
      LCV <- Lcv(x)
      LSKEW <- LSkew(x)
      Shape <- (1-3*LSKEW)/(1+LSKEW)
      L2 <- L1*LCV
      Scale <- (1+Shape)*(2+Shape)*L2
      Loc <- L1-(2+Shape)*L2
      return(data.frame(Loc, Scale, Shape))}
  } else {
    pars <- c(sd(x)/1.5, 0.01)
    max.lhd <- function(q, par) {
      abs(sum(log(gp.pdf(q, loc = min(x), scale = par[1], shape = par[2]))))}
    gp.pdf <- function(q, loc, scale, shape) {
      y <- -shape^-1*log(1-shape*(q-loc)/scale)
      p <- scale^-1*exp(1)^(-(1-shape)*y)
      return(p)
    }
    result <- suppressWarnings(optim(par = pars, fn = max.lhd, q = x))
    loc <- min(x)
    scale <- result$par[1]
    shape <- result$par[2]
    log.likelihood <- -result$value[1]
    message <- result$message
    Res <- data.frame(loc, scale, shape, log.likelihood)
    return(Res)
  }
}

#'Data simulator
#'
#'Simulation of a random sample from the generalised extreme value, generalised logistic, or generalised Pareto distributions
#'
#'The simulated sample can be generated using distribution parameters, or the growth factor (GF) inputs; linear coefficient of variationn (Lcv), linear skewness (LSkew) & the median annual maximum (QMED).
#' @param n sample size to be simulated
#' @param pars vector of parameters in the order of location, scale, shape
#' @param dist choice of distribution. Either "GEV", "GenLog", or "GenPareto"
#' @param GF vector of GF inputs in the order of Lcv, LSkew, QMED
#' @examples
#' #Get an annual maximum sample, estimate the parameters and simulate a sample of
#' #size 30 the parameters using Lmoments (with GenLog)
#' AM.27090 <- GetAM(27090)
#' GenLogPars(AM.27090$Flow)
#' SimData(30, pars = c(298.6416, 51.37096, -0.04211332), dist = "GenLog")
#' #calculate the Lmoments and simulate a sample of 30 with GF inputs
#' Lmoms(AM.27090$Flow)
#' SimData(30, GF = c(0.1704826, 0.04211332, median(AM.27090$Flow)), dist = "GenLog")
#' @return A random sample of size n for the chosen distribution.
#' @author Anthony Hammond
SimData <- function(n, pars = NULL, dist, GF = NULL) {
  if(is.null(GF) == TRUE){
    if(dist == "GenPareto") {res <- GenParetoEst(loc = pars[1], scale = pars[2], shape = pars[3], RP = 1/runif(n), ppy = 1)}
    if(dist == "GEV") {res <- GEVEst(loc = pars[1], scale = pars[2], shape = pars[3], RP = 1/runif(n))}
    if(dist == "GenLog") {res <- GenLogEst(loc = pars[1], scale = pars[2], shape = pars[3], RP = 1/runif(n))}
    return(res)} else
    {if(dist == "GenPareto") {res <- GenParetoGF(lcv = GF[1], lskew = GF[2], RP = 1/runif(n))}
      if(dist == "GEV") {res <- GEVGF(lcv = GF[1], lskew = GF[2], RP = 1/runif(n))}
      if(dist == "GenLog") {res <- GenLogGF(lcv = GF[1], lskew = GF[2], RP = 1/runif(n))}
      return(res*GF[3])
    }
}


# QMED --------------------------------------------------------------------

#'QMED (median annual maximum flow) estimate from catchment descriptors
#'
#'Estimated median annual maximum flow from catchment descriptors and donor sites
#'
#'QMED is estimated from catchment descriptors: QMED = 8.3062*AREA^0.8510 0.1536^(1000/SAAR) FARL^3.4451 0.0460^(BFIHOST^2) as derived in Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation. The single donor method is from the same paper. The method for two donors is outlined in 'Kjeldsen, T. (2019). Adjustment of QMED in ungauged catchments using two donor sites. Circulation - The Newsletter of the British Hydrological Society, 4'. When UrbAdj = TRUE, urban adjustment is applied to the QMED estimate according to the method outlined in the guidance by Hydro Wallingford solutions: 'WINFAP 4 Urban Adjustment Procedures'. For flexibility there is the option to input the relevant catchment descriptors directly rather than a CDs object.
#' @param CDs catchment descriptors derived from either GetCDs or ImportCDs
#' @param Don1 numeric site reference for the a signle donor (for donor candidates see DonAdj function)
#' @param Don2 vector of two site references for two donors (for donor candidates see DonAdj function)
#' @param UrbAdj logical argument with a default of FALSE. True applies an urban adjustment
#' @param AREA catchment area in km2
#' @param SAAR standard average annual rainfall (mm)
#' @param FARL flood attenuation from reservoirs and lakes
#' @param BFIHOST baseflow index calculated from the catchment hydrology of soil type classification
#' @param URBEXT2000 measure of catchment urbanisation
#' @examples
#' #Get some catchment descriptors and calculate QMED as if it was ungauged, with
#' #no donors, one donor, and two donors
#' CDs.55004 <- GetCDs(55004)
#' QMED(CDs.55004)
#' QMED(CDs.55004, Don1 = 55012)
#' QMED(CDs.55004, Don2 = c(55012, 60007))
#' #Get CDs for urban gauge and calculate QMED with urban adjustment
#' CDs.27083 <- GetCDs(27083)
#' QMED(CDs.27083, UrbAdj = TRUE)
#' @return An estimate of QMED from catchment descriptors
#' @author Anthony Hammond
QMED <- function(CDs = NULL, Don1 = NULL, Don2 = NULL, UrbAdj = FALSE, AREA, SAAR, FARL, BFIHOST, URBEXT2000 = NULL){
  Donor1 <- function(CDs, DonSite){
    QMED.cd <- 8.3062*CDs[1,2]^0.8510*0.1536^(1000/CDs[15,2])*CDs[8,2]^3.4451*0.0460^(CDs[5,2]^2)
    Site <- DonSite
    Donors <- DonAdj(CDs = CDs, rows = 500)
    Rw <- which(rownames(Donors) == DonSite)
    Result <- Donors[Rw, 27]
    return(Result)
  }
  Donor2 <- function(CDs, Sites) {
    rij <- function(d) {0.4598*exp(-0.0200*d)+(1-0.4598)*exp(-0.4785*d)}
    NGRDist <- function(i, j) {sqrt((i[1]-j[1])^2+(i[2]-j[2])^2)/1000}
    Site1 <- Sites[1]
    Site2 <- Sites[2]
    CDs.Site1 <- GetCDs(Site1)
    CDs.Site2 <- GetCDs(Site2)
    Dist1 <- NGRDist(c(CDs[19,2], CDs[20,2]), c(CDs.Site1[19,2], CDs.Site1[20,2]))
    Dist2 <- NGRDist(c(CDs[19,2], CDs[20,2]), c(CDs.Site2[19,2], CDs.Site2[20,2]))
    Dist12 <- NGRDist(c(CDs.Site1[19,2], CDs.Site1[20,2]), c(CDs.Site2[19,2], CDs.Site2[20,2]))
    ps1 <- rij(Dist1)
    p12 <- rij(Dist12)
    ps2 <- rij(Dist2)
    a1 <- (ps1-p12*ps2)/(1-p12^2)
    a2 <- (ps2-p12*ps1)/(1-p12^2)
    QMEDscd <- 8.3062*CDs[1,2]^0.8510*0.1536^(1000/CDs[15,2])*CDs[8,2]^3.4451*0.0460^(CDs[5,2]^2)
    QMED1cd <- 8.3062*CDs.Site1[1,2]^0.8510*0.1536^(1000/CDs.Site1[15,2])*CDs.Site1[8,2]^3.4451*0.0460^(CDs.Site1[5,2]^2)
    QMED2cd <- 8.3062*CDs.Site2[1,2]^0.8510*0.1536^(1000/CDs.Site2[15,2])*CDs.Site2[8,2]^3.4451*0.0460^(CDs.Site2[5,2]^2)
    QMED1obs <- QMEDData$QMED[which(rownames(QMEDData) == Site1)]
    QMED2obs <- QMEDData$QMED[which(rownames(QMEDData) == Site2)]
    QMEDs.adj <- QMEDscd*(QMED1obs/QMED1cd)^a1 * (QMED2obs/QMED2cd)^a2
    return(QMEDs.adj)
  }
  if(is.null(CDs) == TRUE) {
    QMED.cd <- 8.3062*AREA^0.8510*0.1536^(1000/SAAR)*FARL^3.4451*0.0460^(BFIHOST^2)
    if(is.null(URBEXT2000) == TRUE & UrbAdj == TRUE) stop ("If UrbAdj is TRUE, URBEXT2000 is required")
    if(UrbAdj == TRUE) {
    Q.ua <- as.numeric(UAF(URBEXT2000 = URBEXT2000, BFIHOST = BFIHOST)[2])*QMED.cd}
    if (UrbAdj == FALSE) {QMED <- QMED.cd} else {QMED <- Q.ua}
    if (is.null(URBEXT2000) == TRUE & UrbAdj == FALSE) {print("No input for URBEXT2000. If it is above > 0.03, urban adjustment is recommended")}
    if(is.null(URBEXT2000) == FALSE){
    if(UrbAdj == FALSE & URBEXT2000 > 0.03){print("URBEXT > 0.03, urban adjustment is recommended")}}
    return(QMED)
  } else {
    QMED.cd <- 8.3062*CDs[1,2]^0.8510*0.1536^(1000/CDs[15,2])*CDs[8,2]^3.4451*0.0460^(CDs[5,2]^2)
    if(is.null(Don1) == TRUE) {QMED.cd <- QMED.cd} else {QMED.cd <- Donor1(CDs = CDs, Don1)}
    if(is.null(Don2) == TRUE) {QMED.cd <- QMED.cd} else {QMED.cd <- Donor2(CDs = CDs, Don2)}
    Q.ua <- as.numeric(UAF(CDs = CDs)[2])*QMED.cd
    if (UrbAdj == FALSE) {QMED <- QMED.cd} else {QMED <- Q.ua}
    if (CDs[18,2] > 0.03 & UrbAdj == FALSE) {print("URBEXT > 0.03, urban adjustment is recommended")}
    return(QMED)
    }
}

#' Empirical estimate of QMED from peaks over threshold (POT) data
#'
#' Estimates the median annual maximum flow (QMED) from peaks over threshold data
#'
#'@details If there are multiple peaks per year, the peaks per year (ppy) argument is used to convert to the annual scale to derive QMED. If ppy is one, then the median of the POT sample is returned (the median of x).
#' @param x numerical vector. POT data
#' @param ppy number of peaks per year in the POT data
#' @examples
#' #Extract some POT data and estimate QMED
#' ThamesPOT <- POTextract(ThamesPQ[,c(1,3)], thresh = 0.90)
#' QMEDPOT(ThamesPOT$peak, ppy = 1.867263)
#' @author Anthony Hammond
QMEDPOT <- function(x, ppy){
  qu <- 1-(0.5/ppy)
  qmed <- quantile(x, qu, na.rm = TRUE)
  return(as.numeric(qmed))
}


#' QMED Linking equation
#'
#' Estimates the median annual maximum flow (QMED) from non-flood flows
#'
#'The QMED Linking equation estimates QMED as a function of the flow that is exceeded five percent of the time, the flow that is exceeded 10 percent of the time, the baseflow index, and the catchment desciptor; drainage path slope (DPSBAR). All of these can be found for sites on the National River Flow Archive (NRFA) website. The method is provided in the guidance note 'WINFAP 4 QMED Linking equation' (2016) by Hydro Wallingford Solutions.
#' @param Q5dmf numeric. The daily mean flow that is exceeded 5 percent of the time
#' @param Q10dmf numeric. The daily mean flow that is exceeded 10 percent of the time
#' @param DPSBAR a catchment descriptor. The average drainage path slope of the catchment
#' @param BFI the baseflow index of the gauged flow
#' @examples
#' #Calculate the QMED for site 1001 (Wick at Tarroul)
#' QMEDLink(10.14, 7.352, 29.90, 0.39)
#' @author Anthony Hammond
QMEDLink <- function(Q5dmf, Q10dmf, DPSBAR, BFI) {
  GRAD <- log(Q10dmf/Q5dmf)
  1.762*Q5dmf^0.866*(1+GRAD)^-0.775*DPSBAR^0.265*0.2388^(BFI^2)
}


#' QMED donor adjustment
#'
#' Applies a donor adjustment to the median annual maximum flow (QMED) estimate
#'
#'Although a single donor adjustment can be applied with the DonAdj() function and the QMED(), this is provided for flexibility. The method is that of Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation (2008).
#' @param AREA catchment area in km2
#' @param SAAR standardised average annual rainfall in mm
#' @param FARL flood attenuation from reservoirs and lakes
#' @param BFIHOST the baseflow index as a function of soil type
#' @param QMEDgObs the observed QMED at the donor site
#' @param QMEDgCds the QMED equation derived QMED at the donor site
#' @param xSI the catchment centroid easting for the site of interest
#' @param ySI the catchment centroid northing for the site of interest
#' @param xDon the catchment centroid easting for the donor site
#' @param yDon the catchment centroid northing for the donor site
#' @param alpha a logical argument with a default of TRUE. When FALSE the exponent in the donor equation is set to one. Otherwise it is determined by the distance between the donor and the subject site
#'
#' @examples
#' #Get observed QMED for site 96003
#' Qob <- median(GetAM(96003)[,2])
#' #Get QMED equation estimated QMED for the donor site
#' QCD <- QMED(CDs = GetCDs(96003))
#' #display CDs for site 96001 & note the easting and northing
#' GetCDs(96001)
#' #display CDs for site 96003 & note the easting and northing
#' GetCDs(96003)
#' #Apply the QMEDDonEq function with the information gained
#' QMEDDonEq(194, 1096, 0.955, 0.297, Qob, QCD, xSI = 289289,ySI = 947523,xDon = 280908,yDon = 953653)
#' @author Anthony Hammond
QMEDDonEq <- function(AREA, SAAR, FARL, BFIHOST, QMEDgObs, QMEDgCds, xSI, ySI, xDon, yDon, alpha = TRUE) {
  QMED.scd <- 8.3062*AREA^0.8510*0.1536^(1000/SAAR)*FARL^3.4451*0.0460^(BFIHOST^2)
  d <- NGRDist(i = c(xSI,ySI), j = c(xDon,yDon))
  rij <- function(d) {0.4598*exp(-0.0200*d)+(1-0.4598)*exp(-0.4785*d)}
  if(alpha == TRUE) {a <- rij(d)} else{a <- 1}
  QMED.adj <- QMED.scd*(QMEDgObs/QMEDgCds)^a
  return(QMED.adj)
}



#'Donor adjustment candidates & results
#'
#'Provides donor adjustment candidates, descriptors, and results in order of the proximity to the centroid of the subject catchment.
#'
#'When d2 is FALSE the results for single donor adjustment are in the final column headed 'QMED.adj' for each site. If alpha is set to FALSE, the results in this column are from the same donor equation but with an exponent of 1. The donor adjustment method is as outlined in Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation. The method for two donors is outlined in 'Kjeldsen, T. (2019). Adjustment of QMED in ungauged catchments using two donor sites. Circulation - The Newsletter of the British Hydrological Society, 4'. When two donors are used, only the result is returned, rather than donor candidates. The QMEDfse column provides the gauged factorial standard error for the median of the annual maximum sample. It is worth considering this when choosing a donor site (a high value indicates a poor donor). When choosing between two donors, the site with a lower QMEDfse would be an appropriate choice (all else being equal). The QMEDfse is calculated with the QMEDfseSS() function.
#' @param CDs catchment descriptors derived from either GetCDs or ImportCDs
#' @param x catchment centroid easting (for when CDs isn't used)
#' @param y catchment centroid northing (for when CDs isn't used)
#' @param QMEDscd QMED estimate for the catchment of interest (for when CDs isn't used)
#' @param alpha logical argument with a default of TRUE. If FALSE the exponent of the donor adjustment equation is set to one
#' @param rows number of sites provided; default is 10
#' @param d2 a numeric vector of length two; the two site references for the donor catchments chosen for the two donor case
#' @examples
#' #Get some CDs and output candidate donor sites
#' CDs.54022 <- GetCDs(54022)
#' DonAdj(CDs.54022)
#' #Get results with inputs of x,y, and QMEDscd
#' DonAdj(x = 283261, y = 288067, QMEDscd = 17.931)
#' #Get a result with two donors
#' DonAdj(CDs.54022, d2 = c(54092, 54091))
#' @return A data.frame with rownames of site references and columns of catchment descriptors, distance from subect site, and associated results. When two donors are used, only the resulting adjusted QMED is returned
#' @author Anthony Hammond

DonAdj <- function(CDs = NULL, x,y, QMEDscd = NULL, alpha = TRUE, rows = 10, d2 = NULL){
  Donor2 <- function(CDs, Sites) {
    rij <- function(d) {0.4598*exp(-0.0200*d)+(1-0.4598)*exp(-0.4785*d)}
    NGRDist <- function(i, j) {sqrt((i[1]-j[1])^2+(i[2]-j[2])^2)/1000}
    Site1 <- Sites[1]
    Site2 <- Sites[2]
    CDs.Site1 <- GetCDs(Site1)
    CDs.Site2 <- GetCDs(Site2)
    Dist1 <- NGRDist(c(CDs[19,2], CDs[20,2]), c(CDs.Site1[19,2], CDs.Site1[20,2]))
    Dist2 <- NGRDist(c(CDs[19,2], CDs[20,2]), c(CDs.Site2[19,2], CDs.Site2[20,2]))
    Dist12 <- NGRDist(c(CDs.Site1[19,2], CDs.Site1[20,2]), c(CDs.Site2[19,2], CDs.Site2[20,2]))
    ps1 <- rij(Dist1)
    p12 <- rij(Dist12)
    ps2 <- rij(Dist2)
    a1 <- (ps1-p12*ps2)/(1-p12^2)
    a2 <- (ps2-p12*ps1)/(1-p12^2)
    QMEDscd <- 8.3062*CDs[1,2]^0.8510*0.1536^(1000/CDs[15,2])*CDs[8,2]^3.4451*0.0460^(CDs[5,2]^2)
    QMED1cd <- 8.3062*CDs.Site1[1,2]^0.8510*0.1536^(1000/CDs.Site1[15,2])*CDs.Site1[8,2]^3.4451*0.0460^(CDs.Site1[5,2]^2)
    QMED2cd <- 8.3062*CDs.Site2[1,2]^0.8510*0.1536^(1000/CDs.Site2[15,2])*CDs.Site2[8,2]^3.4451*0.0460^(CDs.Site2[5,2]^2)
    QMED1obs <- QMEDData$QMED[which(rownames(QMEDData) == Site1)]
    QMED2obs <- QMEDData$QMED[which(rownames(QMEDData) == Site2)]
    QMEDs.adj <- QMEDscd*(QMED1obs/QMED1cd)^a1 * (QMED2obs/QMED2cd)^a2
    return(QMEDs.adj)
  }
  if(is.null(QMEDscd) == TRUE) {QMEDscd <- 8.3062*CDs[1,2]^0.8510*0.1536^(1000/CDs[15,2])*CDs[8,2]^3.4451*0.0460^(CDs[5,2]^2)} else {QMEDscd <- QMEDscd}
  suppressWarnings(if(is.null(CDs) == TRUE) {
    NGRDist <- function(i, j) {sqrt((i[1]-j[1])^2+(i[2]-j[2])^2)/1000}
    Dists <- NULL
    for(i in 1:length(QMEDData$QMED)) {Dists[i] <- NGRDist(i = c(x,y), j = c(QMEDData$X[i],QMEDData$Y[i]))}
    Dists.Table <- data.frame(QMEDData, Dists)
    Dists.Order <- Dists.Table[order(Dists.Table$Dists),]
    rij <- function(d) {0.4598*exp(-0.0200*d)+(1-0.4598)*exp(-0.4785*d)}
    if(alpha == TRUE) {a <- rij(Dists.Order$Dists)} else{a <- 1}
    Dists.Order <- cbind(Dists.Order, a)
    QMED.adj <- QMEDscd*(Dists.Order$QMED/Dists.Order$QMEDcd)^a
    Dists.Order <- cbind(Dists.Order, QMED.adj)
    if(is.null(d2) == TRUE){
      return(Dists.Order[1:rows,])
    } else {
      Qd2 <- Donor2(CDs, Sites = d2)
      return(Qd2)
    }} else {
      NGRDist <- function(i, j) {sqrt((i[1]-j[1])^2+(i[2]-j[2])^2)/1000}
      Dists <- NULL
      for(i in 1:length(QMEDData$QMED)) {Dists[i] <- NGRDist(i = c(CDs$Value[19],CDs$Value[20]), j = c(QMEDData$X[i],QMEDData$Y[i]))}
      Dists.Table <- data.frame(QMEDData, Dists)
      Dists.Order <- Dists.Table[order(Dists.Table$Dists),]
      rij <- function(d) {0.4598*exp(-0.0200*d)+(1-0.4598)*exp(-0.4785*d)}
      if(alpha == TRUE) {a <- rij(Dists.Order$Dists)} else{a <- 1}
      Dists.Order <- cbind(Dists.Order, a)
      QMED.adj <- QMEDscd*(Dists.Order$QMED/Dists.Order$QMEDcd)^a
      Dists.Order <- cbind(Dists.Order, QMED.adj)
      if(is.null(d2) == TRUE){
        return(Dists.Order[1:rows,])
      } else {
        Qd2 <- Donor2(CDs, Sites = d2)
        return(Qd2)
      }

    })
}


#' QMED factorial standard error for gauged sites
#'
#' Estimates the median annual maximum flow (QMED) factorial standard error (FSE) by bootstrapping the sample
#'
#'The bootstrapping procedure resamples from the sample N*500 times with replacement. After splitting into 500 samples of size N, the median is calculated for each. Then the exponent of the standard deviation of the log transformed residuals is taken as the FSE. i.e. exp(sd(log(x)-mean(log(x)))), where x is the bootstrapped medians.
#' @param x a numeric vector. The sample of interest
#' @examples
#' #Extract an AMAX sample and estimate the QMED factorial standard error
#' AM.203018 <- GetAM(203018)
#' QMEDfseSS(AM.203018$Flow)
#' @return The factorial standard error for the median of a sample.
#' @author Anthony Hammond
QMEDfseSS <- function(x) {
  resample <- sample(x, size = length(x)*500, replace = TRUE)
  mat <- matrix(resample, nrow = length(x), ncol = 500)
  res <- apply(mat, 2, median)
  FSE <- function(x) {exp(sd(log(x) - mean(log(x))))}
  fse <- FSE(res)
  return(fse)
}


#' QMED from a gauged site suitable for QMED
#'
#' Provides QMED (median annual maximum flow) from a site suitable for QMED, using the site reference.
#'
#' @param x the gauged reference
#' @examples
#' #Get the observed QMED from sites 55002
#' GetQMED(55002)
#' @return the median annual maximum
#' @author Anthony Hammond
GetQMED <- function(x) {
  MedianAM <- QMEDData[which(rownames(QMEDData) == x),19]
  if(length(MedianAM) < 1) stop("Site reference not recognised. Site not suitable for QMED or pooling.")
  return(MedianAM)
  }


# UrbFuncs ----------------------------------------------------------------

#' Urban adjustment for the linear coefficient of variation (Lcv)
#'
#' Urbanises or de-urbanises the Lcv using the methods outlined in the guidance by Hydro Wallingford solutions: 'WINFAP 4 Urban Adjustment Procedures'
#'
#'The method for de-urbanisation isn't explicitly provided in 'WINFAP 4 Urban Adjustment Procedures', but the procedure is a re-arrangment of the urbanisation equation, solving for Lcv rather than Lcv-urban.
#' @param lcv the Lcv (numeric)
#' @param URBEXT2000 quantiication of urban and suburbanisation for the subject catchment
#' @param DeUrb logical argument with a default of FALSE. If set to TRUE, de-urbanisation adjustment is performed, if FALSE, urbanisation adjustment is performed
#' @examples
#' #Choose an urban site (site 53006) from the NRFA data then apply a de-urban
#' #adjustment using the Lcv and URBEXT2000 displayed
#' NRFAData[which(rownames(NRFAData) == 53006),]
#' LcvUrb(0.21, 0.1138, DeUrb = TRUE)
#' #Get the pooled Lmoment ratios results for catchment 53006 and apply the
#' #urban adjustment using the pooled Lcv, and the URBEXT2000 for site 53006.
#' CDs.53006 <- GetCDs(53006)
#' QuickResults(CDs.53006)[[2]]
#' LcvUrb(0.196, 0.1138)
#' @return The urban adjust Lcv or the de-urbanised Lcv
#' @author Anthony Hammond

LcvUrb <- function(lcv, URBEXT2000, DeUrb = FALSE) {if (DeUrb == FALSE) {lcv*0.68654^(1.567*URBEXT2000)} else {lcv/(0.68654^(1.567*URBEXT2000))}}


#' Urban adjustment for the linear skewness (LSkew)
#'
#' Urbanises or de-urbanises the LSkew using the methods outlined in the guidance by Hydro Wallingford solutions: 'WINFAP 4 Urban Adjustment Procedures'
#'
#'The method for de-urbanisation isn't explicitly provided in 'WINFAP 4 Urban Adjustment Procedures', but the procedure is a re-arrangment of the urbanisation equation, solving for LSkew rather than LSkew-urban.
#' @param lskew the LSkew (numeric)
#' @param URBEXT2000 quantiication of urban and suburbanisation for the subject site
#' @param DeUrb logical argument with a default of FALSE. If set to TRUE, de-urbanisation adjustment is performed, if FALSE, urbanisation adjustment is performed
#' @examples
#' #Choose an urban site (site 53006) from the NRFA data then apply a de-urban
#' #adjustment using the Lcv and URBEXT2000 displayed
#' NRFAData[which(rownames(NRFAData) == 53006),]
#' LSkewUrb(0.124, 0.1138, DeUrb = TRUE)
#' #Get the pooled Lmoment ratios results for catchment 53006 and apply the urban
#' #Get the CDS & adjustment using the pooled LSkew, and the URBEXT2000 for site 53006.
#' CDs.53006 <- GetCDs(53006)
#' QuickResults(CDs.53006)[[2]]
#' LSkewUrb(0.194, 0.1138)
#' @return The urban adjust Lcv or the de-urbanised Lcv
#' @author Anthony Hammond

LSkewUrb <- function(lskew, URBEXT2000, DeUrb = FALSE) {if(DeUrb == FALSE) {((lskew+1)*1.096017^(1.567*URBEXT2000))-1} else {((lskew+1)/1.096017^(1.567*URBEXT2000))-1}}


#' Urban adjustment factor (UAF) and percentage runoff urban adjustment factor (PRUAF)
#'
#' UAF and PRUAF from catchment descriptors for QMED estimation in ungauged urban catchments
#'
#' @param CDs catchment descriptors derived from either GetCDs or ImportCDs
#' @param URBEXT2000 quantification of catchment urbanisation (used when CDs is not)
#' @param BFIHOST baseflow index as a function of hydrological soil type of the catchment (used when CDs is not)
#' @examples
#' #Get some catchment descriptors for an urban catchment calculate the UAF & PRUAF
#' CDs.53006 <- GetCDs(53006)
#' UAF(CDs.53006)
#' #Calculate UAF and PRUAF using a user input URBEXT2000 and BFIHOST
#' UAF(URBEXT2000 = 0.1138, BFIHOST = 0.3620)
#' @return a data.frame with columns UAF and PRUAF
#' @author Anthony Hammond

UAF <- function(CDs = NULL, URBEXT2000, BFIHOST) {
  if(is.null(CDs) == TRUE) {PRUAF <- 1+0.3*1.567*URBEXT2000*(70/(69.366-65.686*BFIHOST)-1)} else {PRUAF <-   1+0.3*1.567*CDs[18,2]*(70/(69.366-65.686*CDs[5,2])-1)}
  if(is.null(CDs) == TRUE) {UAF <- (1+0.3*(1.567*URBEXT2000))^1.25*PRUAF^1.33} else {UAF <- (1+0.3*(1.567*CDs[18,2]))^1.25*PRUAF^1.33}
  return(data.frame(PRUAF, UAF))
}




# DataFuncs ---------------------------------------------------------------
#' Get an annual maximum sample from the National River Flow Archive sites suitable for pooling
#'
#' Extracts the annual maximum peak flow sample and associated dates for the site of interest.
#' @param ref the site reference of interest (numeric)
#' @examples
#' #Get an AMAX sample and display it in the console
#' GetAM(203018)
#' #Save an AMAX sample as an object
#' AM.203018 <- GetAM(203018)
#' @return A data.frame with columns; Date, Flow, and id
#' @author Anthony Hammond

GetAM <- function(ref) {
  Test <- which(AMSP$id == ref)
  if(length(Test) < 1) stop("Only sites suitable for pooling are available via this function. Check the reference or use ImportAM to get sites not suitable for pooling")
  AM <- subset(AMSP, id == ref)
  rws <- seq(1, length(AM$Flow))
  Date <- as.Date(AM[,1])
  AM <- AM[,-1]
  AM <- cbind(Date, AM)
  rownames(AM) <- rws
  return(AM)
}


#' Import catchment descriptors from .CD3 files
#'
#' Imports catchment descriptors from CD3 files either from an FEH webservice download or from the Peakflows dataset downloaded from the national river flow archive (NRFA) website
#'
#' The CD3 files downloaded from the FEH webserver are formatted differently from the CD3 files of the peak flows dataset. For this reason it is necessary to specify web as TRUE or FALSE. TRUE indicates an FEH webserver derived CD3 and FALSE indicates a peak flows dataset CD3. File paths for importing data require forward slashes. On some operating systems, such as windows, the copy and pasted file paths will have backward slashes and would need to be changed accordingly.
#' @param x the CD3 file path
#' @param web logical argument with a default of TRUE. TRUE is to be used to import CDs from a FEH webserver downloaded CD3 file. FALSE is to be used to import CDs from a peak flows dataset CD3 file
#' @examples
#' #Import catchment descriptors from a NRFA peakflows CD3 file and display in console
#' \donttest{CDs.4003 <- ImportCDs("C:/Data/NRFAPeakFlow_v9/Suitable for QMED/4003.CD3", web = FALSE)}
#' \donttest{CDs.4003}
#' #Import catchment descriptors from a FEH webserver CD3 file and display CDs in the console
#' \donttest{CDs.MySite <- ImportCDs("C:/Data/FEH_Catchment_384200_458200.CD3")}
#' @return A data.frame with columns; Descriptor and Value.
#' @author Anthony Hammond
ImportCDs <- function(x, web = TRUE) {
  if(web == TRUE) {ImportWebCDs <- function(x) {
    IniCDs <- read.table(x, skip = 20, nrows = 23, header = FALSE, sep = ",", stringsAsFactors = FALSE)
    Excl <- IniCDs[-c(9,10,19,20,21,22),]
    Area <- read.table(x, skip = 18, nrows = 1, header = FALSE, sep = ",", stringsAsFactors = FALSE)
    Centroid <- read.table(x, skip = 19, nrows = 1, header = FALSE, sep = ",", stringsAsFactors = FALSE)
    Easting <- Centroid[2:3]
    colnames(Easting) <- c("V1", "V2")
    Northing <- Centroid[3:4]
    colnames(Northing) <- c("V1", "V2")
    CDSet <- rbind(Area, Excl, Easting, Northing)
    rownames(CDSet) <- seq(1,20)
    CDSet[19,1] <- "Easting"
    CDSet[20,1] <- "Northing"
    CDSet[1,1] <- "AREA"
    colnames(CDSet) <- c("Descriptor", "Value")
    return(CDSet)
  }
  CDs <- ImportWebCDs(x)}
  else {
    ImportNRFACDs <- function(x)
    {cds.table <- read.table(x, sep = ",", fill = T, skip = 17, col.names = c("Descriptor", "Value"), nrows = 22)
    Area <- read.table(x, sep = ",", fill = T, skip = 16, col.names = c("Descriptor", "Value"), nrows = 1)
    Area[1]$Descriptor <- "AREA"
    NGRs <- read.table(x, sep = ",", skip = 15, nrows = 1)
    NGR.table <- data.frame(c("Easting", "Northing"), c(NGRs$V3, NGRs$V4))
    colnames(NGR.table) <- c("Descriptor", "Value")
    CDs <- rbind(cds.table, NGR.table)
    CDs <- rbind(Area, CDs)
    CDs <- CDs[-c(18,19,20,21,23), ]
    rws <- seq(1,20)
    row.names(CDs) <- rws
    return(CDs)
    }
  CDs <- ImportNRFACDs(x)
  }
  return(CDs)
}


#' Get catchment descriptors from the National River Flow Archive sites considered suitable for median annual maximum flow estimation (QMED).
#'
#' Extracts the catchment descriptors for a site of interest from those suitable for QMED.
#' @param x the site reference of interest (numeric)
#' @examples
#' #Get CDs and display in the console
#' CDs.203018 <- GetCDs(203018)
#' CDs.203018
#' @return A data.frame with columns; Descriptor and Value.
#' @author Anthony Hammond
GetCDs <- function(x) {
  Site.id <- which(row.names(QMEDData) == x)
  Site <- QMEDData[Site.id,]
  Site <- Site[,-c(19,20)]
  colnames(Site)[colnames(Site) == "X"] <-  "Easting"
  colnames(Site)[colnames(Site) == "Y"] <-  "Northing"
  Site <- t(Site)
  rws <- as.vector(row.names(Site))
  values <- NULL
  for(i in 1:22) {values[i] <- Site[i,1]}
  dframe <- data.frame(rws, values)
  colnames(dframe) <- c("Descriptor", "Value")
  dframe[1:14,2] <- round(dframe[1:14,2], 4)
  dframe[c(15,16,19,20,22),2] <- round(dframe[c(15,16,19,20,22),2])
  dframe[21,2] <- round(dframe[21,2], 3)
  return(dframe)
}

#' Import an annual maximum (AMAX) sample from NRFA peak flow .AM files
#'
#' Imports the peak flows and dates from from NRFA peak flow .AM files, exluding the rejected years
#'
#'  File paths for importing data require forward slashes. On some operating systems, such as windows, the copy and pasted file paths will have backward slashes and would need to be changed accordingly.
#' @param x the file path for the .AM file
#' @examples
#' #Import an AMAX sample and display the first six rows in the console
#' \donttest{AM.4003 <- ImportAM("C:/Data/NRFAPeakFlow_v9/Suitable for QMED/4003.AM")}
#' \donttest{head(AM.4003)]
#' @return A data.frame with columns; Date and Flow
#' @author Anthony Hammond
ImportAM <- function(x)
{
  AMAX <- read.table(x, sep = ",", col.names = c("Date", "Flow", "Stage"), colClasses = c("character", "numeric", "NULL"), fill = T, skip = 6) # read in AM file
  Row.Strt <- 1+which(AMAX[,1] == "[AM Values]")
  AM <- AMAX[Row.Strt:length(AMAX[,1])-1,]
  AM <- AM[-1,]
  Dates <- data.frame(as.Date(AM[,1], format = "%d %b %Y"), AM[,2])
  colnames(Dates) <- c("Date", "Flow")
  AM.c1 <- AMAX[,1] # use only the first column
  Rej <- AM.c1[2:which(AM.c1 == "[AM Values]")-2][-1] # Select all the rejected dates
  WYDate <- as.Date(paste(Rej, "- 10 - 01"), format = "%Y - %m - %d") # convert this to the water year start
  Date.Func <- function(x, y)
  {
    isTRUE(x >= y & x <= y+365)
  }
  Pass <- function(Dates, Rej, n)
  {
    Logic <- NULL
    for (i in 1:length(Dates$Date))
    {Logic[i] <- Date.Func(Dates$Date[i], Rej[n])}
    Rem <- which(Logic == TRUE)
    if (length(Rem > 0)) {return(Rem)} else{return(0)}
  }
  LOGIC <- NULL
  for (i in 1:length(WYDate))
    LOGIC[i] <- Pass(Dates, WYDate, i)
  LOGIC <- LOGIC[LOGIC>0]
  AMG <- Dates[-LOGIC,]
  if (length(AMG$Flow) < 1) {return(Dates)} else {return(AMG)}
}



#' Peaks over threshold (POT) data extraction
#'
#' Extracts independent peaks over a threshold from a sample
#'
#'  If the x argument is a numeric vector, the peaks will be extracted with no time information. x can instead be a data.frame with dates in the first column and the numeric vector in the second. In this latter case, the peaks will be timestamped and a hydrograph including POT will be plotted by default. The method of extracting independent peaks assumes that there is a value either side of which, events can be considered independent. For example, if two peaks above the chosen threshold are separated by the daily mean flow, they could be considered independent, but not if flow hasn't returned to daily mean at any time between the peaks. Daily mean flow may not always be appropriate, in which case the 'div' argument can be adjusted. In some cases, where the baseflow component is very high, there won't be more than one independent peak per year, rendering the AM extraction and block maxim method preferable. A good example of this would be the River Alre at Drove Lane Alresford. The function was coded primarily for river flow but for extracting daily duration POT rainfall a div of zero could be used (making the assumption that rainfall events separated by a period of 24 hours, with no rain, are independent). For sub-daily rainfall, further work, after use of the function, would be necessary. For example, a div of zero could be used, and if two peaks are extracted but not separted by more than 24 hours, the lower of the two could be discarded. For this approach a data.frame with dates would be required. When plotted, the blue line is the threshold and the green line is the independence line (div).
#' @param x either a numeric vector or dataframe with date in the first column and hydrological variable in the second
#' @param div user chosen value, either side of which two peaks over the threshold are considered independent. Default is the mean of the sample
#' @param thresh user chosen threshold. Default is 0.975
#' @param Plot logical argument with a default of TRUE. When TRUE, the full hydrograph with the peaks over the threshold highlighted is plotted
#' @examples
#' #Extract POT data from Thames mean daily flow 1970-10-01 to 2015-09-25 with
#' #div = mean and threshold = 0.95. Then display the first six rows
#' ThamesQPOT <- POTextract(ThamesPQ[, c(1,3)], thresh = 0.90)
#' head(ThamesQPOT)
#' #Extract Thames POT from only the numeric vector of flows and display the
#' #first six rows
#' ThamesQPOT <- POTextract(ThamesPQ[, 3], thresh = 0.90)
#' ThamesQPOT
#' #Extract the Thames POT precipitation with a div of 0 and the default
#' #threshold. Then display the first six rows
#' ThamesPPOT <- POTextract(ThamesPQ[, c(1,2)], div = 0)
#' head(ThamesPPOT)
#' @return Prints the number of peaks per year and returns a data.frame with columns; Date and peak, with the option of a plot. Or a numeric vector of peaks is returned if only a numeric vector of the hydrological variable is input.
#' @author Anthony Hammond

POTextract <- function(x, div = NULL, thresh = 0.975, Plot = TRUE)
{
  Low.Func <- function(TS)
  {
    L <- length(TS)-2
    L1 <- length(TS)-1
    L2 <- length(TS)
    Vec1 <- TS[1:L]
    Vec2 <- TS[2:L1]
    Vec3 <- TS[3:L2]
    P1 <- ifelse(Vec2 <= Vec1 & Vec2 <= Vec3 & Vec1!= Vec2, Vec2, NA)
    return(P1)
  }

  P.Func <- function(TS)
  {
    L <- length(TS)-2
    L1 <- length(TS)-1
    L2 <- length(TS)
    Vec1 <- TS[1:L]
    Vec2 <- TS[2:L1]
    Vec3 <- TS[3:L2]
    P1 <- ifelse(Vec2 >= Vec1 & Vec2 >= Vec3 & Vec1!= Vec2, Vec2, NA)
    return(P1)
  }

  VP <- function(j, mu)
  {
    maxll <-  suppressWarnings(max(which(lows[1:j] <= mu), na.rm = T))
    if(maxll == -Inf) {maxll <- j} else {maxll <- maxll}
    minlr <-   suppressWarnings(min(which(lows[j:length(lows)] <= mu), na.rm = T))
    if(minlr == Inf) {minlr <- j} else {minlr <- j+(minlr-1)}
    if(peaks[j] == max(peaks[maxll:minlr], na.rm = T)) {vp <- peaks[j]} else {vp <- NA}
    return(vp)
  }
  NAs <- FALSE
  if(class(x) != "data.frame") {
    if(is.null(div)) {mu <- mean(x,na.rm = TRUE)} else {mu <- div}
    if(mu >= quantile(x,thresh, na.rm = TRUE)) stop("The event division must be significantly lower than the event threshold")
    QThresh <- as.numeric(quantile(x, thresh, na.rm = TRUE))
    MinMuP <- min(which(x <= mu))
    MaxMuP <- max(which(x <= mu))
    PkBegin <- max(x[1:MinMuP])
    PkEnd <- max(x[MaxMuP:length(x)])
    x <- x[MinMuP:MaxMuP]
    lows <- Low.Func(x)
    peaks <- P.Func(x)
    MinMuL <- min(which(lows <= mu))
    MaxMuL <- max(which(lows <= mu))
    pt.ind <- which(peaks > QThresh)
    pt <- peaks[pt.ind]
    l <- length(pt.ind)
    POT <- NULL
    {for (i in 1:l) {POT[i] <- VP(pt.ind[i], mu)}}
    if(PkBegin > QThresh) {POT <- append(PkBegin, POT)}
    if(PkEnd > QThresh) {POT <- append(POT, PkEnd)}
    if(NAs == TRUE) {POT <- POT} else {POT <- POT[which(is.na(POT) == FALSE)]}
    return(POT)}
  else {
    if(is.null(div)) {mu <- mean(x[,2],na.rm = TRUE)} else {mu <- div}
    if(mu >= quantile(x[,2],thresh, na.rm = TRUE)) stop("The event division must be significantly lower than the event threshold")
    QThresh <- as.numeric(quantile(x[,2], thresh, na.rm = TRUE))
    MinMuP <- min(which(x[,2] <= mu), na.rm = TRUE)
    MaxMuP <- max(which(x[,2] <= mu), na.rm = TRUE)
    PkBegin <- which(x[1:MinMuP,2] == max(x[1:MinMuP,2], na.rm = TRUE))
    PkEnd <- which(x[MaxMuP:length(x[,2]),2] == max(x[MaxMuP:length(x[,2]),2], na.rm = TRUE))
    DBegin <- x[PkBegin,]
    DEnd <- x[((MaxMuP-1)+PkEnd),]
    colnames(DBegin) <- c("Date", "peak")
    colnames(DEnd) <- c("Date", "peak")
    xUse <- x[MinMuP:MaxMuP,]
    lows <- Low.Func(xUse[,2])
    peaks <- P.Func(xUse[,2])
    pt.ind <- which(peaks > QThresh)
    pt <- peaks[pt.ind]
    L <- length(pt.ind)
    POT <- NULL
    {for (i in 1:L) {POT[i] <- VP(pt.ind[i], mu)}}
    POT.Dates <- (xUse[,1][pt.ind])+1
    res <- data.frame(POT.Dates, POT)
    colnames(res) <- c("Date", "peak")
    if(DBegin$peak > QThresh) {res <- rbind(DBegin, res)}
    if(DEnd$peak > QThresh) {res <- rbind(res, DEnd)}
    rownames(res) <- seq(1, length(res[,2]))
    if(NAs == TRUE) {res <- res} else {res <- res[which(is.na(res$peak) == FALSE), ]}
    if(Plot == TRUE) {plot(x, type = "l", main = "Peaks over threshold", ylab = "Quantile", xlab= "Date")
      abline(h = quantile(x[,2], thresh, na.rm = TRUE), col = "blue")
      points(res, col = "red")}
    abline(h = mu, col = rgb(0, 0.7, 0.3))
    Years <- as.numeric((x[length(x[,1]),1]-x[1,1])/365.25)
    PPY <- length(res[,1])/Years
    print(paste("Peaks per year:", format(PPY, trim = TRUE), sep = " "))
    return(res)
  }
}

#' Annual maximum extraction
#'
#' Extracts the annual maximum peaks from a data.frame which has dates in the first column and variable in the second.
#'
#'  The peaks are extracted based on the UK hydrological year, which starts October 1st and ends September 30th. If there are NAs for full years in the data, an -Inf will be returned for that year.
#' @param x a data.frame with dates in the first column and variable in the second
#' @param Plot a logical argument with a default of TRUE. If TRUE the extracted annual maximum is plotted
#' @examples
#' #Extract the Thames AMAX daily mean flow and display the first six rows
#' ThamesAM <- AMextract(ThamesPQ[,c(1,3)])
#' head(ThamesAM)
#' @return a data.frame with columns; WaterYear and AM
#' @author Anthony Hammond
AMextract <- function(x, Plot = TRUE){
  suppressWarnings(if(class(x[,1]) != "Date") stop ("Column one is not a date object"))
  Date1 <- x[1,1]
  DateLst <- x[length(x[,1]),1]
  DateExtract <- function(d){
    yr <- as.POSIXlt(d)$year+1900
    mnth <- as.POSIXlt(d)$mon+1
    return(c(yr, mnth))
  }
  Date1.ext <- DateExtract(Date1)
  DateLst.ext <- DateExtract(DateLst)
  if(Date1.ext[2] < 10) {WY <- Date1.ext[1]-1} else {WY <- Date1.ext[1]}
  if(DateLst.ext[2] < 10) {WYend <- DateLst.ext[1]-1} else {WYend <- DateLst.ext[1]}
  WYrSt <- as.Date(paste(WY, "10", "01", sep = "-"))
  WYrSt.to <- as.Date(paste(WYend, "10", "01", sep = "-"))
  YrStarts <- seq(WYrSt, WYrSt.to, by = "year")
  WYendst <- as.Date(paste(WY+1, "09", "30", sep = "-"))
  YrEnds <- seq(WYendst, length.out = length(YrStarts), by = "year")
  AM <- NULL
  for (i in 1:length(YrStarts)) {AM[i] <- max(x[,2][x[,1] >= YrStarts[i] & x[,1] <= YrEnds[i]], na.rm = TRUE)}
  WaterYear <- seq(WY, WYend)
  if(Plot == TRUE) {plot(WaterYear, AM, type = "h", col = rgb(0,0.3,0.7), main = "Hydrological annual maximum sequence", ylab = "Annual maximum quantiles")}
  return(data.frame(WaterYear, AM))
}


# Uncertainty -------------------------------------------------------------

#' Uncertainty quantification for gauged and ungauged pooled estimates
#'
#' Quantification of aleatoric uncertainty for pooling results for the gauged and ungauged case
#'
#'  Uncertainty in the ungauged case is calulated as equations 2.11 to 2.13 in Science Report – SC130009/R: 'Making better use of local data in flood frequency estimation'. The 68 percent and 95 percent intervals are returned. For the gauged case the pooled group is bootstrapped 500 times and the enhanced single site weighted linear skewness (LSkew) and linear coefficient of variation (Lcv) are calulcated 500 times accordingly and 500 associated growth factors are calculated. Each  growth factor (GF) is multiplied by a randomly selected median annual maximum flow (QMED) from the uncertainty distribution of median estimates for the gauged subject site. The distribution of medians is derived from bootstrapping the gauged site 500 times. The intervals are then the upper and lower quantiles (depending on the conf input) of the distribution of median * GFs. For the gauged case the user can choose the level for the intervals. The default is 0.95. Occasionally the single site estimate will be outside the intervals. In these cases the intervals are widened to incorporate it. i.e. if above the intervals, the upper interval is increased to the single site estimate and vice versa if below. This occurs regardless of the confidence setting. For details about the calcuations of weighted growth curves & urban adjustment see the PoolEst() function details. A trend option is not included within the Uncertainty function and would need be considered separately if used in PoolEst. An indication of the uncertainty for trend applied in PoolEst is provided in the PoolEst function details. The method and considerations of covariance between the index flood and the Lmoment ratios, as well covariance of the sites within the pooling group, are detailed in a paper submitted for publication. The reference of which will be included here once published.
#' @param x the pooled group derived from the Pool() function
#' @param gauged a logical argument with a default of FALSE. If FALSE the uncertainty intervals are calculated for the ungauged case. If TRUE they are calculated for the gauged case
#' @param RP the return period of interest. Default is 100
#' @param dist a choice of distribution to use for the estimates. Choices are "GEV or "GenLog". The default is "GenLog"
#' @param qmed the QMED estimate for the ungauged case. Or for the gauged if the user wishes to override the median from the NRFA data
#' @param no.Donors number of donors used for estimation of QMED in the ungauged case
#' @param UrbAdj applies an urban adjustment to the growth curves
#' @param CDs catchment descriptors derived from either GetCDs or ImportCDs. Necessary if a UrbAdj is TRUE
#' @param conf the confidence level of the intervals for the gauged case. Default is 0.95. Must be between 0 and 1
#' @examples
#' #Get CDs, form an ungauged pooling group and quantify the uncertainty of the
#' #50-year pooled estimate when using a CDs estimate of QMED with no donors
#' CDs.203018 <- GetCDs(203018)
#' Pool.203018 <- Pool(CDs.203018, exclude = 203018)
#' Uncertainty(Pool.203018, qmed  = QMED(CDs.203018), no.Donors = 0, RP = 50)
#' #Form pooling group with subject site included. Quantify the uncertainty of the
#' #50-year pooled estimate at the 99% level.
#'  Pool.203018 <- Pool(CDs.203018)
#'  Uncertainty(Pool.203018, gauged = TRUE, RP = 50, conf = 0.99)
#' @return For the ungauged case a data.frame of four values relating to the lower 68 and upper 68 percent interval and the lower 95 and upper 95 percent intervals. These are headed by the associated percentiles. For the gauged case a numeric vector of two values is provided with the lower and upper intervals of the chosen conf level. The uncertainty function doesn't have a trend option; if trend is used in the pooled estimate this would need to be considered and intervals adjused accordingly. However a greater uncertainty should be considered.
#' @author Anthony Hammond
Uncertainty <- function(x, gauged = FALSE, RP = 100, dist = "GenLog", qmed = NULL, no.Donors = 2, UrbAdj = FALSE, CDs = NULL, conf = 0.95){
  if(UrbAdj == TRUE & is.null(CDs) == TRUE) stop("if UrbAdj = TRUE, CD object is necessary")
  if(is.null(CDs) == FALSE) {URBEXT2000 <- CDs[18,2]}
  if(gauged == FALSE) {
    if(is.null(qmed) == TRUE) stop("Need to input qmed & no.of donors")
    y <- -log(-log(1-1/RP))
    if(no.Donors == 0) {FSE <- 1.461 + 0.0055*y + 0.0034*y^2}
    if(no.Donors == 1) {FSE <- 1.429 + 0.0041*y + 0.0038*y^2}
    if(no.Donors == 2) {FSE <- 1.421 + 0.0028*y + 0.0039*y^2}
    Central <- as.numeric(PoolEst(x = x, gauged = gauged, RP = RP, dist = dist, QMED = qmed, trend = FALSE, UrbAdj = UrbAdj, CDs = CDs)[[1]][2])
    L68 <- Central/FSE
    U68 <- Central*FSE
    L95 <- Central/FSE^2
    U95 <- Central*FSE^2
    df <- data.frame(L68, U68, L95, U95)
    colnames(df) <- c("16%", "84%", "2.5%", "97.5%")
    return(df)
  } else {
    Unc.gauged <- function(x, RP, dist = "GenLog", UrbAdj = FALSE, trend = FALSE, qmed = NULL, fse = FALSE, conf = 0.68) {
      if(dist == "GenLog") {func = GenLogGF} else {func = GEVGF}
      Boot <- function(AM.ref) {
        x <- GetAM(AM.ref)
        x <- x[,2]
        resample <- sample(x, size = length(x)*500, replace = TRUE)
        mat <- matrix(resample, nrow = length(x), ncol = 500)
        lcvs <- apply(mat, 2, Lcv)
        lskews <- apply(mat, 2, LSkew)
        dfRatios <- data.frame(lcvs, lskews)
        return(dfRatios)
      }

      SiteRatios <- NULL
      for(i in 1:length(x$N)) {SiteRatios[[i]] <- Boot(rownames(x)[i])}

      LratTemp <- function(x, j){
        lcvs1 <- NULL
        for (i in 1:length(x$N)) {lcvs1[i] <- SiteRatios[[i]][j,1]}
        lskews1 <- NULL
        for (i in 1:length(x$N)) {lskews1[i] <- SiteRatios[[i]][j,2]}
        PoolTemp <- x
        PoolTemp[,16] <- lcvs1
        PoolTemp[,17] <- lskews1
        lcvGTemp <- WGaugLcv(PoolTemp)
        lskewGTemp <- WGaugLSkew(PoolTemp)
        df <- data.frame(lcvGTemp, lskewGTemp)
        return(df)
      }

      Lratios <- LratTemp(x, 1)
      for(j in 2:500) {Lratios <- rbind(Lratios, LratTemp(x, j))}
      if(UrbAdj == TRUE) {LCVs <- LcvUrb(Lratios[,1], URBEXT2000)} else {LCVs <- Lratios[,1]}
      if(UrbAdj == TRUE) {LSKEWs <- LSkewUrb(Lratios[,2], URBEXT2000)} else {LSKEWs <- Lratios[,2]}
      Zts <- func(LCVs, LSKEWs, RP = RP)
      AM <- GetAM(rownames(x)[1])
      AM <- AM[,2]
      resample <- sample(AM, size = length(AM)*500, replace = TRUE)
      mat <- matrix(resample, nrow = length(AM), ncol = 500)
      Meds <- apply(mat, 2, median)
      if(is.null(qmed) == TRUE) {QMEDCentral <- median(AM)} else {QMEDCentral <- qmed}
      lcvCentral <- WGaugLcv(x)
      lskewCentral <- WGaugLSkew(x)
      if(UrbAdj == TRUE) {lcvCentral <- LcvUrb(lcvCentral, URBEXT2000 = URBEXT2000)}
      if(UrbAdj == TRUE) {lskewCentral <- LSkewUrb(lskewCentral, URBEXT2000 = URBEXT2000)}
      ZtCentral <- func(lcv = lcvCentral, lskew = lskewCentral, RP = RP)
      FSEest <- function(x) {exp(sd(log(x) - mean(log(x))))}
      #if(trend == TRUE){Meds <- Meds*1.082} else {Meds <- Meds}
      Res500 <- Meds*Zts
      FSE <- FSEest(Res500)
      Intervals <- quantile(Res500, c((1-conf)/2, 1-(1-conf)/2))
      Res <- list(fse, Intervals)
      names(Res) <- c("Factorial standard error", "Intervals")
      if(fse == TRUE) {return(Res)} else {return(Intervals)}
    }
    Res <- Unc.gauged(x = x, RP = RP, dist = dist, trend = FALSE, UrbAdj = UrbAdj, qmed = qmed, fse = FALSE, conf = conf)
    if(dist == "GenLog") {func = GenLogGF} else {func = GEVGF}
    MedianAM <- GetQMED(rownames(x)[1])
    if(is.null(qmed) == TRUE) {QMEDCentral <- MedianAM} else {QMEDCentral <- qmed}
    SSEst <- QMEDCentral*func(x[1,16],x[1,17], RP = RP)
    if(SSEst < Res[1]) {Res[1] <- SSEst}
    if(SSEst > Res[2]) {Res[2] <- SSEst}
  }
  return(Res)
}


#' Uncertainty for the single site
#'
#' Quantifies the aleatoric uncertainty for a single site estimate, by bootstrapping the sample
#'
#'The bootstrapping procedure resamples from a sample N*500 times with replacement. After splitting into 500 samples of size N, the statsitic of interest is calculated on each. upper and lower quantiles of the resulting distribution are used as the quantification of uncertainty. Any function that provides an estimate based on a sample of data can be used. Including any function that provides estimates as a function of return period.
#' @param x a numeric vector. The sample of interest
#' @param func the function to be applied
#' @param conf the confidence level of the intervals
#' @param RP return period. Necessary if func requires RP
#' @examples
#' #Extract an AMAX sample and quantify uncertainty for the GEV estimated 50-year flow
#' AM.203018 <- GetAM(203018)
#' UncSS(AM.203018$Flow, func = GEVAM, RP = 50)
#' #Quantify uncertainty for the sample standard deviation at the 90 percent confidence level
#' UncSS(AM.203018$Flow, func = sd, conf = 0.90)
#' @return A data.frame of three values; central, lower, and upper bootstrapped estimates.
#' @author Anthony Hammond
UncSS <- function(x, func, conf = 0.95, RP = FALSE) {
  resample <- sample(x, size = length(x)*500, replace = TRUE)
  mat <- matrix(resample, nrow = length(x), ncol = 500)
  if(RP == FALSE) {res <- apply(mat, 2, func)} else  {res <- apply(mat, 2, func, RP)}
  lint <- (1-conf)/2
  uint <- ((1-conf)/2)+conf
  lower <-  quantile(res, lint, na.rm = TRUE)
  upper <- quantile(res, uint, na.rm = TRUE)
  centre <- quantile(res, 0.5,na.rm = TRUE)
  frame <- data.frame(centre, lower, upper)
  rownames(frame) <- ""
  return(frame)
}



# Diagnostics -------------------------------------------------------------

#' Goodness of tail fit (GoTF).
#'
#' Provides a GoTF score for the generalised extreme value (GEV), generalised Pareto (GenPareto), or generalised logistic (GenLog) distribution. Also for any simulated numeric distribution
#'
#' The GoTF is calculated by simulating the sample 5000 times with the desired distribution and calculating a statistic (in this case the coefficient of variation (CV) & mean) for the upper 25 percent of each sample. The same is calculated for the subject sample and compared to the distribution. The number of statistics from the simulated samples that are greater than the sample statistics is divided by 5000. The GoTF is this latter number where it is <0.5 and 1 minus this latter number where is it >0.5. If any further distributions are of interest, the representative distribution (RepDist) argument can be used. In this case a simulation of 5000*length(x) from that distribution can be used as RepDist, in place of using the dist input. If a sample that is not equal to 5000 time length(x) is in the RepDist argument, it will be resampled with replacement. An alternative is to use the pars or GF arguments which simulate from the distribution choice (dist) based on the parameters (location, scale, shape) or the growth factor (GF) inputs; the median annual maximum flow (QMED), linear coefficient of variation (Lcv), and linear skewnes (LSkew). The resulting probabilities for each statistic (the GoTF score) represent the probability of observing that statistic if the sample distribution has the same underlying distribution as the one under scrutiny.
#' @param x a numeric vector. The sample of interest
#' @param dist a choice of distribution to be assessed. The choices are "GenLog", "GEV", or "GenPareto". The default is "GenLog"
#' @param pars numeric vector of length three which are the parameters for the GEV, GenLog, or GenPareto distributions. In the order of location, scale, shape
#' @param GF numeric vector of length three which are the growth factor statistics & QMED, in the order of Lcv, Lskew, & QMED
#' @param RepDist a simulated sample (ideally of size = 5000*n) of a representative distribution to compare to the sample of interest
#' @examples
#' #Get an AMAX sample and derive GoTF score against the GenLog and the GEV distributions
#' \donttest{AM <- GetAM(203018)}
#' \donttest{GoTF(AM$Flow, dist = "GenLog")}
#' \donttest{GoTF(AM$Flow, dist = "GEV")}
#' #Derive the GF parameters for the ungauged pooled estimate for the AM and
#' #calculate a GoTF for GenLog (assuming the gauged QMED)
#' #For this assume 0.16 and 0.2 as the ungauged Lcv & LSkew pooled estimates
#'  \donttest{GoTF(AM$Flow, GF = c(0.16, 0.2, median(AM$Flow)))}
#' #calculate the GoTF based on parameters of the GenLog estimated inadequately.
#' \donttest{Loc <- mean(AM$Flow)}
#' \donttest{Scale <- sd(AM$Flow)}
#' \donttest{Skew <- 1-(median(AM$Flow)/mean(AM$Flow))}
#' \donttest{GoTF(AM$Flow, pars = c(Loc, Scale, Skew))}
#' @return A data.frame with one row of probabilities representing the GoTF. The first column is the Tail cv and the second is the tail mean.
#' @author Anthony Hammond

GoTF <- function(x, dist = "GenLog", pars = NULL, GF = NULL, RepDist = NULL){
  if(is.null(RepDist) == FALSE) {
    if(length(RepDist) != (5000*length(x))) {print("Warning: RepDist not equal to 5000 * length(x), resampling has been used")}
    if(length(RepDist) != (5000*length(x))) {RepDist <- sample(RepDist, 5000*length(x), replace = TRUE)}
    MMR <- function(x) {sd(x[x > quantile(x, 0.75)])/mean(x[x > quantile(x, 0.75)])}
    Mat.1 <- matrix(RepDist, nrow = length(x), ncol = 5000)
    MMRs <- apply(Mat.1, 2, MMR)
    MMRo <- MMR(x)
    SortMMRs <- sort(MMRs)
    Ind <- suppressWarnings(min(which(SortMMRs >  MMRo)))
    if(Ind == Inf) {Prop <- 0.0002} else {Prop <- Ind/5000}
    if(Prop > 0.5) {res <- 1-Prop} else {res <- Prop}
    if(Ind == Inf) {res <- "< 0.0002"} else {res <- res/0.5}

    TailMean <- function(x) {mean(x[x > quantile(x, 0.75)])}
    Mat.2 <- matrix(RepDist, nrow = length(x), ncol = 5000)
    TMs <- apply(Mat.1, 2, TailMean)
    TMo <- TailMean(x)
    SortTMs <- sort(TMs)
    Ind2 <- suppressWarnings(min(which(SortTMs >  TMo)))
    if(Ind2 == Inf) {Prop2 <- 0.0002} else {Prop2 <- Ind2/5000}
    if(Prop2 > 0.5) {res2 <- 1-Prop2} else {res2 <- Prop2}
    if(Ind2 == Inf) {res2 <- "< 0.0002"} else {res2 <- res2/0.5}

    ResDF <- data.frame(res, res2)
    colnames(ResDF) <- c("p(Tail cv)", "p(Tail mean)")
    return(ResDF)

  } else {

    if(dist == "GenLog") {funcX <- GenLogAM
    funcPars <- GenLogEst
    funcGF <- GenLogGF}
    if(dist == "GEV")
    {funcX <- GEVAM
    funcPars <- GEVEst
    funcGF <- GEVGF}
    if(dist == "GenPareto")
    {funcX <- GenParetoPOT
    funcPars <- GenParetoEst
    funcGF <- GenParetoGF}
    MMR <- function(x) {sd(x[x > quantile(x, 0.75)])/mean(x[x > quantile(x, 0.75)])}
    Rands <- 1/runif(length(x)*5000)
    if(is.null(pars) == TRUE & is.null(GF) == TRUE) {Sims <- funcX(x, RP = Rands)}
    if(is.null(pars) == FALSE) {Sims <- funcPars(pars[1], pars[2], pars[3], RP = Rands)}
    if(is.null(GF) == FALSE)  {Sims <- funcGF(GF[1], GF[2], RP = Rands)*GF[3]}

    Mat.1 <- matrix(Sims, nrow = length(x), ncol = 5000)
    MMRs <- apply(Mat.1, 2, MMR)
    MMRo <- MMR(x)
    SortMMRs <- sort(MMRs)
    Ind <- suppressWarnings(min(which(SortMMRs >  MMRo)))
    if(Ind == Inf) {Prop <- 0.0002} else {Prop <- Ind/5000}
    if(Prop > 0.5) {res <- 1-Prop} else {res <- Prop}
    if(Ind == Inf) {res <- "< 0.0002"} else {res <- res/0.5}

    TailMean <- function(x) {mean(x[x > quantile(x, 0.75)])}
    Mat.2 <- matrix(Sims, nrow = length(x), ncol = 5000)
    TMs <- apply(Mat.1, 2, TailMean)
    TMo <- TailMean(x)
    SortTMs <- sort(TMs)
    Ind2 <- suppressWarnings(min(which(SortTMs >  TMo)))
    if(Ind2 == Inf) {Prop2 <- 0.0002} else {Prop2 <- Ind2/5000}
    if(Prop2 > 0.5) {res2 <- 1-Prop2} else {res2 <- Prop2}
    if(Ind2 == Inf) {res2 <- "< 0.0002"} else {res2 <- res2/0.5}

    ResDF <- data.frame(round(res, 4), round(res2, 4))
    colnames(ResDF) <- c("p(Tail cv)", "p(Tail mean)")
    return(ResDF)

    }
}

#' Goodness of tail fit (GoTF) for pooling groups
#'
#' Calculates GoTF scores for pooling groups for both generalised extreme value (GEV) and generalised logistic (GenLog) distributions
#'
#'  The GoTF for pooling groups is calculated by standardising all the sites in the group (dividing by median) and calculating the linear coefficient of variation (Lcv) and linear skewness (Lskew) of the pooled data as if it was one sample. The GoTF() function is then applied to the pooled data with the GF arguments using the aforementioned Lcv and Lskew, and QMED equal to one. The GoTF scores are calculated for the GEV and GenLog distributions and can be used to assist the decision of which distribution to use for the final estimates. See details for the GoTF function for information about the resulting values. The closer the scores are to one, the better the tail fit.
#' @param x pooling group derived from the Pool function
#' @examples
#' #Get CDs, create pooled group and calculate GoTFs.
#' \donttest{CDs.203018 <- GetCDs(203018)}
#' \donttest{Pool.203018 <- Pool(CDs.203018)}
#' \donttest{GoTFPool(Pool.203018)}
#' @return A list of two data.frames. Each with one row of the two GoTF values related to the columns; p(Tail cv) & p(Tail mean). See GoTF details. The first data.frame is for the GEV distribution and the second is for the GenLog distribution.
#' @author Anthony Hammond
GoTFPool <- function(x) {
  Sites <- rownames(x)
  SiteScale <- GetAM(Sites[1])[,2]/median(GetAM(Sites[1])[,2])
  for(i in 2:length(Sites)) {SiteScale <- append(SiteScale,GetAM(Sites[i])[,2]/median(GetAM(Sites[i])[,2]))}
  lcv <- Lcv(SiteScale)
  lskew <- LSkew(SiteScale)
  pGEV <- GoTF(SiteScale, dist = "GEV", GF = c(lcv, lskew, 1))
  pGenLog <- GoTF(SiteScale, dist = "GenLog", GF = c(lcv, lskew, 1))
  ResList <- list(pGEV, pGenLog)
  names(ResList) <- c("GEV", "GenLog")
  return(ResList)
}

#' Zdist Goodness of fit measure for pooling groups
#'
#' Calculates the goodness of fit score for pooling groups with the method outlined in the Flood Estimation Hanbook (1999), volume 3.
#'
#'   The goodness of fit measure was developed by Hosking & Wallis and can be found in their book 'Regional Frequency Analysis: an approach based on LMoments (1997), as well as Flood Estimation Handbook volume 3.
#' @param x pooling group derived from the Pool() function
#' @examples
#' #Get CDs, form a pooling group and calculate the Zdist
#' CDs.203018 <- GetCDs(203018)
#' Pool.203018 <- Pool(CDs.203018)
#' Zdists(Pool.203018)
#' @return A list with the first element a data.frame of two GoF scores related to the columns; "GEV and "GenLog". The second element is a character stating which has the best fit.
#' @author Anthony Hammond

Zdists <- function(x)
{
  tR4 <- mean(x$LKurt)
  tR3 <- mean(x$LSkew)
  Pool.Kap.pars <- function(x)
  {
    l1 <- 1
    l2 <- mean(x$Lcv)
    lskew <- mean(x$LSkew)
    lkurt <- mean(x$LKurt)
    pars <- c(l1,l2,lskew, lkurt)
    return(pars)
  }
  Lcv <- function(x)
  {
    Sort.x <- sort(x)
    Rank <- seq(1, length(x))
    b0 <- mean(x)
    b1 <- mean((Rank-1)/(length(x)-1)*Sort.x)
    b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x)
    b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x)
    L1 <- b0
    L2 <- 2*b1-b0
    Lcv <- L2/L1
    return(Lcv)
  }
  LSkew <- function(x)
  {
    Sort.x <- sort(x)
    Rank <- seq(1, length(x))
    b0 <- mean(x)
    b1 <- mean((Rank-1)/(length(x)-1)*Sort.x)
    b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x)
    b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x)
    L1 <- b0
    L2 <- 2*b1-b0
    L3 <- 6*b2-6*b1+b0
    LSkew <- L3/L2
    return(LSkew)
  }

  LKurt <- function(x)
  {
    Sort.x <- sort(x)
    Rank <- seq(1, length(x))
    b0 <- mean(x)
    b1 <- mean((Rank-1)/(length(x)-1)*Sort.x)
    b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x)
    b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x)
    L2 <- 2*b1-b0
    L4 <- 20*b3-30*b2+12*b1-b0
    LKurt <- L4/L2
    return(LKurt)
  }

  Kap.pars <- function(L1, L2, LSkew, LKurt)
  {

    Kap.opt <- function(LSkew,LKurt)
    {
      min.SSR <- function(par)
      {

        if (par[2]>0)
        {
          g1 <- (1*gamma(1+par[1])*gamma(1/par[2]))/(par[2]^(1+par[1])*gamma(1+par[1]+1/par[2]))
          g2 <- (2*gamma(1+par[1])*gamma(2/par[2]))/(par[2]^(1+par[1])*gamma(1+par[1]+2/par[2]))
          g3 <- (3*gamma(1+par[1])*gamma(3/par[2]))/(par[2]^(1+par[1])*gamma(1+par[1]+3/par[2]))
          g4 <- (4*gamma(1+par[1])*gamma(4/par[2]))/(par[2]^(1+par[1])*gamma(1+par[1]+4/par[2]))
        }
        else
        {
          g1 <- (1*gamma(1+par[1])*gamma(-par[1]-1/par[2]))/((-par[2])^(1+par[1])*gamma(1-1/par[2]))
          g2 <- (2*gamma(1+par[1])*gamma(-par[1]-2/par[2]))/((-par[2])^(1+par[1])*gamma(1-2/par[2]))
          g3 <- (3*gamma(1+par[1])*gamma(-par[1]-3/par[2]))/((-par[2])^(1+par[1])*gamma(1-3/par[2]))
          g4 <- (4*gamma(1+par[1])*gamma(-par[1]-4/par[2]))/((-par[2])^(1+par[1])*gamma(1-4/par[2]))
        }
        t3.kap <- (-g1+3*g2-2*g3)/(g1-g2)
        t4.kap <- (g1 - 6*g2 + 10*g3 -5*g4)/(g1-g2)
        ss <- sum((t3.kap - LSkew)^2)+((t4.kap-LKurt)^2)
      }
      Op <- suppressWarnings(optim(par = c(0.01, -0.4), fn = min.SSR))
      return(Op)

    }

    Kap.kh <- Kap.opt(LSkew, LKurt)$par
    K <- Kap.kh[1]
    H <- Kap.kh[2]

    gr <- function(k, h)
    {
      if (h>0)
      {
        g1 <- (1*gamma(1+k)*gamma(1/h))/(h^(1+k)*gamma(1+k+1/h))
        g2 <- (2*gamma(1+k)*gamma(2/h))/(h^(1+k)*gamma(1+k+2/h))
        g3 <- (3*gamma(1+k)*gamma(3/h))/(h^(1+k)*gamma(1+k+3/h))

      }
      else
      {
        g1 <- (1*gamma(1+k)*gamma(-k-1/h))/((-h)^(1+k)*gamma(1-1/h))
        g2 <- (2*gamma(1+k)*gamma(-k-2/h))/((-h)^(1+k)*gamma(1-2/h))
        g3 <- (3*gamma(1+k)*gamma(-k-3/h))/((-h)^(1+k)*gamma(1-3/h))
      }
      vec <- c(g1,g2)
      return(vec)
    }
    g12 <- gr(k = K,h = H)
    G1 <- g12[1]
    G2 <- g12[2]

    a <- L2/((G1-G2)/K)
    loc <- L1 - a*(1-G1)/K
    pars <- c(loc, a, K, H)
    return(pars)
  }

  Qt.kap <- function(loc, scale, k, h, T = 100) {loc + (scale/k)*((1-((1-(1-(1/T))^h)/h)^k))}
  Ls <- Pool.Kap.pars(x)
  Pars <- Kap.pars(Ls[1],Ls[2], Ls[3], Ls[4])
  LK.Sim <- function(x)
  {
    LKurt.sim <- NULL
    for (i in 1:nrow(x)) {LKurt.sim[i] <- LKurt(Qt.kap(Pars[1],Pars[2],Pars[3],Pars[4], T = 1/runif(x$N[i])))}
    t4m <- mean(LKurt.sim)
    return(t4m)
  }
  LK.vec <- NULL
  for (i in 1:500) {LK.vec[i] <- LK.Sim(x)}
  B4 <- mean(LK.vec - tR4)
  sig4 <- ((500-1)^-1 * (sum(LK.vec - tR4)^2 - 500*B4^2))^0.5
  t4.GEV <- function(k) {(5*(1-4^-k)-10*(1-3^-k)+6*(1-2^-k))/(1-2^-k)}
  t4.GLO <- function(k) {(1+5*k^2)/6}
  t4.LN3 <- function(k) {1.2260172*10^-1+k^2*((1.8756590*10^-1+(-2.5353147*10^-3)*k^2+2.6995102*10^-4*k^4+(-1.8446680*10^-6)*k^6)/(1+8.2325617*10^-2*k^2+4.26814448*10^-3*k^4+1.1653690*10^-4*k^6))}
  #t4.gum <- 0.150375
  Z.GEV <- (t4.GEV(tR3)-tR4 + B4)/sig4
  Z.GLO <- (t4.GLO(tR3)-tR4 + B4)/sig4
  #Z.LN3 <- (t4.LN3(tR3)-tR4 + B4)/sig4
  #Z.gum <- (t4.gum-tR4 + B4)/sig4
  #Z.frame <- data.frame(Z.GEV, Z.GLO, Z.LN3, Z.gum)
  Z.frame <- data.frame(Z.GEV, Z.GLO)
  bestInd <- which(abs(Z.frame[1,]) == min(abs(Z.frame)))
  #colnames(Z.frame) <- c("GEV", "GL", "LN3", "Gumbel")
  colnames(Z.frame) <- c("GEV", "GenLog")
  if (bestInd == 1) {Result <- "GEV has the best fit"}
  if (bestInd == 2) {Result <- "GenLog has the best fit"}
  #if (bestInd == 3) {Result <- "LN3 has the best fit"}
  #if (bestInd == 4) {Result <- "Gumbel has the best fit"}
  return(list(Z.frame, Result))
}

#' Heterogeneity measure (H2) for pooling groups.
#'
#' Quantifies the heterogeneity of a pooled group
#'
#' The H2 measure was developed by Hosking & Wallis and can be found in their book 'Regional Frequency Analysis: an approach based on LMoments (1997). It was also adopted for use by the Flood Estimation Handbook (1999) and is described in volume 3.
#' @param x pooling group derived from the Pool() function
#' @examples
#' #Get CDs, form a pooling group and calculate H2
#' CDs.203018 <- GetCDs(203018)
#' Pool.203018 <- Pool(CDs.203018)
#' H2(Pool.203018)
#' @return A vector of two characters; the first representing the H2 score and the second stating a qualitative measure of heterogeneity.
#' @author Anthony Hammond

H2 <- function(x)
{
  Pool.Kap.pars <- function(x)
  {
    l1 <- 1
    l2 <- mean(x$Lcv)
    lskew <- mean(x$LSkew)
    lkurt <- mean(x$LKurt)
    pars <- c(l1,l2,lskew, lkurt)
    return(pars)
  }

  v2 <- function(x)
  {
    t2r <- mean(x$Lcv)
    t3r <- mean(x$LSkew)
    ni <- ((x$Lcv-t2r)^2 + (x$LSkew-t3r)^2)
    nni <- sum(x$N*ni)
    mn <- sum(x$N)
    v2 <- (nni/mn)^0.5
    return(v2)
  }

  Lcv <- function(x)
  {
    Sort.x <- sort(x)
    Rank <- seq(1, length(x))
    b0 <- mean(x)
    b1 <- mean((Rank-1)/(length(x)-1)*Sort.x)
    b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x)
    b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x)
    L1 <- b0
    L2 <- 2*b1-b0
    Lcv <- L2/L1
    return(Lcv)
  }
  LSkew <- function(x)
  {
    Sort.x <- sort(x)
    Rank <- seq(1, length(x))
    b0 <- mean(x)
    b1 <- mean((Rank-1)/(length(x)-1)*Sort.x)
    b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x)
    b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x)
    L1 <- b0
    L2 <- 2*b1-b0
    L3 <- 6*b2-6*b1+b0
    LSkew <- L3/L2
    return(LSkew)
  }

  Kap.pars <- function(L1, L2, LSkew, LKurt)
  {

    Kap.opt <- function(LSkew,LKurt)
    {
      min.SSR <- function(par)
      {

        if (par[2]>0)
        {
          g1 <- (1*gamma(1+par[1])*gamma(1/par[2]))/(par[2]^(1+par[1])*gamma(1+par[1]+1/par[2]))
          g2 <- (2*gamma(1+par[1])*gamma(2/par[2]))/(par[2]^(1+par[1])*gamma(1+par[1]+2/par[2]))
          g3 <- (3*gamma(1+par[1])*gamma(3/par[2]))/(par[2]^(1+par[1])*gamma(1+par[1]+3/par[2]))
          g4 <- (4*gamma(1+par[1])*gamma(4/par[2]))/(par[2]^(1+par[1])*gamma(1+par[1]+4/par[2]))
        }
        else
        {
          g1 <- (1*gamma(1+par[1])*gamma(-par[1]-1/par[2]))/((-par[2])^(1+par[1])*gamma(1-1/par[2]))
          g2 <- (2*gamma(1+par[1])*gamma(-par[1]-2/par[2]))/((-par[2])^(1+par[1])*gamma(1-2/par[2]))
          g3 <- (3*gamma(1+par[1])*gamma(-par[1]-3/par[2]))/((-par[2])^(1+par[1])*gamma(1-3/par[2]))
          g4 <- (4*gamma(1+par[1])*gamma(-par[1]-4/par[2]))/((-par[2])^(1+par[1])*gamma(1-4/par[2]))
        }
        t3.kap <- (-g1+3*g2-2*g3)/(g1-g2)
        t4.kap <- (g1 - 6*g2 + 10*g3 -5*g4)/(g1-g2)
        ss <- sum((t3.kap - LSkew)^2)+((t4.kap-LKurt)^2)
      }
      Op <- suppressWarnings(optim(par = c(0.01, -0.4), fn = min.SSR))
      return(Op)

    }

    Kap.kh <- Kap.opt(LSkew, LKurt)$par
    K <- Kap.kh[1]
    H <- Kap.kh[2]

    gr <- function(k, h)
    {
      if (h>0)
      {
        g1 <- (1*gamma(1+k)*gamma(1/h))/(h^(1+k)*gamma(1+k+1/h))
        g2 <- (2*gamma(1+k)*gamma(2/h))/(h^(1+k)*gamma(1+k+2/h))
        g3 <- (3*gamma(1+k)*gamma(3/h))/(h^(1+k)*gamma(1+k+3/h))

      }
      else
      {
        g1 <- (1*gamma(1+k)*gamma(-k-1/h))/((-h)^(1+k)*gamma(1-1/h))
        g2 <- (2*gamma(1+k)*gamma(-k-2/h))/((-h)^(1+k)*gamma(1-2/h))
        g3 <- (3*gamma(1+k)*gamma(-k-3/h))/((-h)^(1+k)*gamma(1-3/h))
      }
      vec <- c(g1,g2)
      return(vec)
    }
    g12 <- gr(k = K,h = H)
    G1 <- g12[1]
    G2 <- g12[2]

    a <- L2/((G1-G2)/K)
    loc <- L1 - a*(1-G1)/K
    pars <- c(loc, a, K, H)
    return(pars)
  }


  Qt.kap <- function(loc, scale, k, h, T = 100) {loc + (scale/k)*((1-((1-(1-(1/T))^h)/h)^k))}
  V.2 <- v2(x)
  Ls <- Pool.Kap.pars(x)
  Pars <- Kap.pars(Ls[1],Ls[2], Ls[3], Ls[4])

  V2.Sim <- function(x)
  {
    Pars <- Pool.Kap.pars(x)
    Ns <- x$N
    LCV.sim <- NULL
    for (i in 1:nrow(x)) {LCV.sim[i] <- Lcv(Qt.kap(Pars[1],Pars[2],Pars[3],Pars[4], T = 1/runif(x$N[i])))}
    LSKEW.sim <- NULL
    for (i in 1:nrow(x)) {LSKEW.sim[i] <- LSkew(Qt.kap(Pars[1],Pars[2],Pars[3],Pars[4], T = 1/runif(x$N[i])))}
    LR.Group <- data.frame(LCV.sim, LSKEW.sim, Ns)
    colnames(LR.Group) <- c("Lcv", "LSkew", "N")
    v2.sim <- v2(LR.Group)
    return(v2.sim)
  }

  V2.500 <- numeric(500)
  for (i in 1:500) {V2.500[i] <- V2.Sim(x)}
  H2 <- (V.2-mean(V2.500))/sd(V2.500)
  H.list <- as.list(H2)
  if (H.list < 2) {res <- "Group is homogenous"}
  if (H.list > 2 & H.list < 4) {res <- "Group is heterogenous: a review of the group is desirable"}
  if (H.list > 4) {res <- "Group is strongly heterogenous: a review of the group is essential"}
  H.list <- as.numeric(format(H.list, digits = 3))
  return(c(H.list, res))
}



# Plots -------------------------------------------------------------------

#' Extreme value plot (frequency and growth curves)
#'
#' Plots the extreme value frequency curve or growth curve with observed sample points. With the option of generalised extreme value (GEV), generalised Pareto (GenPareto), or generalised logistic (GenLog) distributions.
#' @param x a numeric vector. The sample of interest
#' @param dist a choice of distribution. "GEV", "GenLog", or "GenPareto". The default is "GenLog"
#' @param scaled logical argument with a default of TRUE. If TRUE the plot is a growth curve (scaled by the QMED). If FALSE, the plot is a frequency curve
#' @param Title a character string. The user chosen plot title. The default is "Extreme value plot"
#' @param ylabel a character string. The user chosen label for the y axis. The default is "Q/QMED" if scaled = TRUE and "Discharge (m3/s)" if scaled = FALSE
#' @examples
#' #Get an AMAX sample and plot the growth curve with the GEV distribution
#' AM.203018 <- GetAM(203018)
#' EVPlot(AM.203018$Flow, dist = "GEV")
#' @return An extreme value plot
#' @author Anthony Hammond

EVPlot <- function(x, dist = "GenLog", scaled = TRUE, Title = "Extreme value plot", ylabel = NULL) {
  if(class(x) != "numeric") stop ("x must be a numeric vector")
  if(dist == "GenLog") {func <- GenLogGF}
  if(dist == "GEV") {func <- GEVGF}
  if(dist == "GenPareto") {func <- GenParetoGF}
    Ranks <- seq(500, 1)
    Gringorten <- function(Rank, n) {(Rank-0.44)/(n+0.12)}
    Gring <- Gringorten(Ranks, 500)
    Log.Red.Var <- log((1/Gring)-1)
    Ranks.obs <- seq(length(x), 1)
    Gring.obs <- Gringorten(Ranks.obs, length(x))
    LRV.obs <- log((1/Gring.obs)-1)
    Scale <- x/median(x)
    if(scaled == TRUE) {AM.sort <- sort(Scale, decreasing = F)} else {AM.sort <- sort(x, decreasing = F)}
    ss.lcv <- Lcv(x)
    ss.lskew <- LSkew(x)
    if(scaled == TRUE) {SimSS <- func(ss.lcv, ss.lskew, RP = 1/Gring)} else {SimSS <- func(ss.lcv, ss.lskew, RP = 1/Gring)*median(x)}
    if(is.null(ylabel) == TRUE) {
    if(scaled == TRUE) {YLab <- "Q/QMED"} else {YLab <- "Discharge (m3/s)"}} else {YLab = ylabel}
    Ymax <- median(c(max(AM.sort), max(SimSS)))
    ymin <- median(AM.sort)-(Ymax*0.5)
    if(Ymax < max(AM.sort)) {Ymax <- max(AM.sort)} else {Ymax <- Ymax}
    if(ymin <= min(AM.sort)) {ymin <- ymin} else {ymin <- min(AM.sort)}
    plot(Log.Red.Var, SimSS, type = "l", xlim = c(-5.5,7), ylim = c(ymin, Ymax), main = Title, ylab = YLab, xlab = "logistic reduced variate", lwd = 2)
points(LRV.obs, AM.sort, col = "blue", lwd = 1.5)
if(scaled == FALSE) {legend("topleft", legend = c("Frequency curve", "Observed"), col = c("black", "blue"), lty = c(1,0), pch = c(NA, 1), bty = "n", lwd = c(2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)} else {legend("topleft", legend = c("Growth curve", "Observed"), col = c("black", "blue"), lty = c(1,0), pch = c(NA, 1), bty = "n", lwd = c(2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
T.Plot.Lab <- c(2,5,10,20,50,100, 500)
At <- log(T.Plot.Lab-1)
AxisPos <- median(c(ymin, median(SimSS)))
axis(side = 1, at = At, pos = AxisPos, lty = 1, tck = -0.02, labels = T.Plot.Lab, cex.axis = 0.7, padj = -1.5)
TextY <- as.numeric(quantile(seq(ymin, median(SimSS), by = abs(ymin/10)), 0.7))
text(2, TextY, labels = "Return Period (yrs)", cex = 0.75, pos = 4)
abline(v = 0, lty = 3)
if(scaled == TRUE) {abline(h = 1, lty = 3)} else {abline(h = median(x), lty = 3)}
}

#' Extreme value plot for pooling groups
#'
#' Plots the extreme value frequency curve or growth curve for gauged or ungauged pooled groups
#' @param x pooling group derived from the Pool() function
#' @param AMAX the AMAX sample to be plotted in the case of gauged. If NULL, & gauged equals TRUE, the AMAX from the first site in the pooling group is used
#' @param gauged logical argument with a default of FALSE. If FALSE, the plot is the ungauged pooled curve accompanied by the single site curves of the group members. If TRUE, the plot is the gauged curve and single site curve with the observed points added
#' @param dist a choice of distribution. Choices are "GEV" or "GenLog". The default is "GenLog"
#' @param QMED a chosen QMED to convert the curve from a growth curve to the frequency curve
#' @param Title a character string. The user chosen plot title. The default is "Pooled growth curve"
#' @param UrbAdj a logical argument with a default of FALSE. If TRUE and urban adjustment is applied to the pooled growth curve
#' @param CDs catchment descriptors derived from either GetCDs or ImportCDs. Only necessary if UrbAdj is TRUE
#' @examples
#' #Get some CDs, form an ungauged pooling group and apply EVPlot.
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001, exclude = 96001)
#' EVPool(Pool.96001)
#' #Do the same for the gauged case, change the title, and convert with a QMED of 105.5.
#' PoolG.96001 <- Pool(CDs.96001)
#' EVPool(PoolG.96001, gauged = TRUE, Title = "Gauged frequency curve - Site 96001", QMED = 105.5)
#' #Pretend we have an extra AMAX for the gauge. Amend the pooling group Lcv and LSkew
#' #for the site accordingly then apply EVPool with the updated AMAX.
#' #Firstly, get the AMAX sample
#' AM.96001 <- GetAM(96001)
#' #Add an extra AMAX flow of 350m3/s
#' Append96001 <- append(AM.96001$Flow, 350)
#' #Amend the Lcv and Lskew in the pooling group
#' PoolG.96001[1, c(16, 17)] <- c(Lcv(Append96001), LSkew(Append96001))
#' #Now plot gauged with the updated AMAX
#' EVPool(PoolG.96001, AMAX = Append96001, gauged = TRUE)
#' @return An extreme value plot for gauged or ungauged pooling groups
#' @author Anthony Hammond

EVPool <- function(x, AMAX = NULL, gauged = FALSE, dist = "GenLog", QMED = NULL, Title = "Pooled growth curve", UrbAdj = FALSE, CDs){
  if(ncol(x) != 24) stop ("x must be a pooled group. Pooled groups can be created with the Pool() function")
  if(dist == "GenLog") {dist <- GenLogGF} else {dist <- GEVGF}
  if(is.null(QMED) == TRUE) {
    Ranks <- seq(500, 1)
    Gringorten <- function(Rank, n) {(Rank-0.44)/(n+0.12)}
    Gring <- Gringorten(Ranks, 500)
    Log.Red.Var <- log((1/Gring)-1)
    GN <- nrow(x)
    n <- c(1:GN)
    LoV <- list()
    for (i in n){
      LoV[[i]]<- dist(x$Lcv[i], x$LSkew[i], 1/Gring)}
    LoV <- data.frame(LoV)
    if(UrbAdj == TRUE) {URBEXT2000 <- CDs[18,2]}
    if(gauged == TRUE) {L.cv <- WGaugLcv(x)} else {L.cv = WungLcv(x)}
    if(gauged == TRUE) {L.Skew <- WGaugLSkew(x)} else {L.Skew <- WungLSkew(x)}
    if(UrbAdj == TRUE) {L.cv <- L.cv*0.68654^(1.567*URBEXT2000)} else {L.cv <- L.cv}
    if(UrbAdj == TRUE) {L.Skew <- ((L.Skew+1)*1.096017^(1.567*URBEXT2000))-1} else {L.Skew <- L.Skew}
    Sim <- dist(L.cv, L.Skew, RP = 1/Gring)
    if(gauged == FALSE){
      if(max(LoV) > 12) {ymax <- 12} else {ymax <- max(LoV)}
      matplot(x = Log.Red.Var, LoV, type = "l", col = "black", lty = 1, xlim = c(-2,7), main = Title, ylab = "Q/QMED", xlab = "logistic reduced variate", ylim = c(-0.7, ymax))
      lines(x = Log.Red.Var, y = Sim, col = "red", lwd = 2)
      legend("topleft", legend = c("Pooled curve", "Single sites"), lty = 1, col = c("red", "black"), lwd = 2, bty = "n", seg.len = 1, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)
      T.Plot.Lab <- c(2,5,10,20,50,100, 500)
      At <- log(T.Plot.Lab-1)
      axis(side = 1, at = At, pos = 0, lty = 1, tck = -0.02, labels = T.Plot.Lab, cex.axis = 0.7, padj = -1.5)
      text(2, 0.3, labels = "Return Period (yrs)", cex = 0.75, pos = 4)
      abline(v = 0, lty = 3)
      abline(h = 1, lty = 3)

    } else {
      if(is.null(AMAX) == TRUE) {AM <- GetAM(row.names(x[1,]))} else {AM <- AMAX}
      if(is.null(AMAX) == TRUE) {AM <- AM[,2]} else {AM <- AM}
      Ranks.obs <- seq(length(AM), 1)
      Gring.obs <- Gringorten(Ranks.obs, length(AM))
      LRV.obs <- log((1/Gring.obs)-1)
      Scale <- AM/median(AM)
      AM.sort <- sort(Scale, decreasing = F)
      ss.lcv <- Lcv(AM)
      ss.lskew <- LSkew(AM)
      SimSS <- dist(ss.lcv, ss.lskew, RP = 1/Gring)
      Ymax <- median(c(max(AM.sort), max(Sim)))
      ymin <- median(AM.sort)-(Ymax*0.5)
      if(Ymax < max(AM.sort)) {Ymax <- max(AM.sort)} else {Ymax <- Ymax}
      if(ymin <= min(AM.sort)) {ymin <- ymin} else {ymin <- min(AM.sort)}
      plot(Log.Red.Var, Sim, type = "l", xlim = c(-5.5,7), ylim = c(ymin, Ymax), main = Title, ylab = "Q/QMED", xlab = "logistic reduced variate", lwd = 2)
      points(LRV.obs, AM.sort, col = "blue", lwd = 1.5)
      points(Log.Red.Var, SimSS, type = "l", lty = 2, col = rgb(0,0.6,0.3), lwd = 2)
      legend("topleft", legend = c("Pooled", "Single site", "Observed"),
             col = c("black", rgb(0,0.6,0.3), "blue"),
             lty = c(1,2, 0), pch = c(NA, NA, 1), bty = "n", lwd = c(2,2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)
      T.Plot.Lab <- c(2,5,10,20,50,100, 500)
      At <- log(T.Plot.Lab-1)
      AxisPos <- median(c(ymin, median(AM.sort)))
      axis(side = 1, at = At, pos = AxisPos, lty = 1, tck = -0.02, labels = T.Plot.Lab, cex.axis = 0.7, padj = -1.5)
      TextY <- as.numeric(quantile(seq(ymin, median(AM.sort), by = abs(ymin/10)), 0.7))
      text(2, TextY, labels = "Return Period (yrs)", cex = 0.75, pos = 4)
      abline(v = 0, lty = 3)
      abline(h = 1, lty = 3)
    }
  } else {
    Ranks <- seq(500, 1)
    Gringorten <- function(Rank, n) {(Rank-0.44)/(n+0.12)}
    Gring <- Gringorten(Ranks, 500)
    Log.Red.Var <- log((1/Gring)-1)
    GN <- nrow(x)
    n <- c(1:GN)
    LoV <- list()
    for (i in n){
      LoV[[i]]<- dist(x$Lcv[i], x$LSkew[i], 1/Gring)*QMED}
    LoV <- data.frame(LoV)
    if(UrbAdj == TRUE) {URBEXT2000 <- CDs[18,2]}
    if(gauged == TRUE) {L.cv <- WGaugLcv(x)} else {L.cv = WungLcv(x)}
    if(gauged == TRUE) {L.Skew <- WGaugLSkew(x)} else {L.Skew <- WungLSkew(x)}
    if(UrbAdj == TRUE) {L.cv <- L.cv*0.68654^(1.567*URBEXT2000)} else {L.cv <- L.cv}
    if(UrbAdj == TRUE) {L.Skew <- ((L.Skew+1)*1.096017^(1.567*URBEXT2000))-1} else {L.Skew <- L.Skew}
    Sim <- dist(L.cv, L.Skew, RP = 1/Gring)*QMED
    if(gauged == FALSE){
      if(max(LoV)/QMED > 12) {ymax <- 12*QMED} else {ymax <- max(LoV)}
      matplot(x = Log.Red.Var, LoV, type = "l", col = "black", lty = 1, xlim = c(-2,7), main = Title, ylab = "Discharge (m3/s)", xlab = "logistic reduced variate", ylim = c((QMED-(QMED*1.5)), ymax))
      lines(x = Log.Red.Var, y = Sim, col = "red", lwd = 2)
      legend("topleft", legend = c("Pooled curve", "Single sites"), lty = 1, col = c("red", "black"), lwd = 2, bty = "n", seg.len = 1, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)
      T.Plot.Lab <- c(2,5,10,20,50,100, 500)
      At <- log(T.Plot.Lab-1)
      axis(side = 1, at = At, pos = (QMED-(QMED*0.8)), lty = 1, tck = -0.02, labels = T.Plot.Lab, cex.axis = 0.7, padj = -1.5)
      text(2, (QMED-(QMED*0.5)), labels = "Return Period (yrs)", cex = 0.75, pos = 4)
      abline(v = 0, lty = 3)
      abline(h = QMED, lty = 3)
    } else {
      if(is.null(AMAX) == TRUE) {AM <- GetAM(row.names(x[1,]))} else {AM <- AMAX}
      if(is.null(AMAX) == TRUE) {AM <- AM[,2]} else {AM <- AM}
      Ranks.obs <- seq(length(AM), 1)
      Gring.obs <- Gringorten(Ranks.obs, length(AM))
      LRV.obs <- log((1/Gring.obs)-1)
      AM.sort <- sort(AM, decreasing = F)
      ss.lcv <- Lcv(AM)
      ss.lskew <- LSkew(AM)
      SimSS <- dist(ss.lcv, ss.lskew, RP = 1/Gring)*QMED
      Ymax <- median(c(max(AM.sort), max(Sim)))
      ymin <- median(AM.sort)-(Ymax*0.5)
      if(Ymax < max(AM.sort)) {Ymax <- max(AM.sort)} else {Ymax <- Ymax}
      if(ymin <= min(AM.sort)) {ymin <- ymin} else {ymin <- min(AM.sort)}
      plot(Log.Red.Var, Sim, type = "l", xlim = c(-5.5,7),ylim = c(ymin, Ymax) , main = Title, ylab = "Discharge (m3/s)", xlab = "logistic reduced variate", lwd = 2)
      points(LRV.obs, AM.sort, col = "blue", lwd = 1.5)
      points(Log.Red.Var, SimSS, type = "l", lty = 2, col = rgb(0,0.6,0.3), lwd = 2)
      legend("topleft", legend = c("Pooled", "Single site", "Observed"),
             col = c("black", rgb(0,0.6,0.3), "blue"),
             lty = c(1,2, 0), pch = c(NA, NA, 1), bty = "n", lwd = c(2,2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)
      T.Plot.Lab <- c(2,5,10,20,50,100, 500)
      At <- log(T.Plot.Lab-1)
      AxisPos <- median(c(ymin, median(AM.sort)))
      axis(side = 1, at = At, pos = AxisPos, lty = 1, tck = -0.02, labels = T.Plot.Lab, cex.axis = 0.7, padj = -1.5)
      TextY <- as.numeric(quantile(seq(ymin, median(AM.sort), by = abs(ymin/10)), 0.7))
      text(2, TextY, labels = "Return Period (yrs)", cex = 0.75, pos = 4)
      abline(v = 0, lty = 3)
      abline(h = QMED, lty = 3)}
  }
}

#' Hydrological plot of concurrent discharge and precipitation
#'
#' Plots concurrent precipitation and discharge with precipitation along the top and discharge along the bottom
#' @param x a data.frame with three columns in the order of date, precipitation, and discharge
#' @param Title a character string. The user chosen plot title. The default is "Concurrent Rainfall & Discharge"
#' @param from a starting time for the plot. In the form of a date or POSIXct object. The default is the first row of x
#' @param to an end time for the plot. In the form of a date or POSIXct object. The default is the last row of x
#' @param adj.y a numeric value to adjust the closeness of the preciptation and discharge in the plot. Default is 1.5. A lower value brings them closer and a larger value further apart
#' @param plw a numeric value to adjust the width of the precipitation lines. Default is one. A larger value thickens them and vice versa
#' @param qlw a numeric value to adjust the width of the discharge line. Default is 1.8. A larger value thickens them and vice versa
#' @examples
#' #Plot the Thames precipitation and discharge for the 2013 hydrological year,
#' #adjusting the y axis to 1.8.
#' HydroPlot(ThamesPQ, from = "2013-10-01", to = "2014-09-30", adj.y = 1.8)
#' @return A plot of concurrent precipitation and discharge. With the former at the top and the latter at the bottom.
#' @author Anthony Hammond

HydroPlot <- function(x, Title = "Concurrent Rainfall & Discharge", from = NULL, to = NULL, adj.y = 1.5, plw = 1, qlw = 1.8){
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  ind1 <- 1
  ind2 <- length(x[,1])
  suppressWarnings(if(class(x[1,1]) == "Date"){
    if(is.null(from)) {ind1 <- ind1} else {ind1 <- which(x[,1] == as.Date(from))}
    if(is.null(to)) {ind2 <- ind2} else {ind2 <- which(x[,1] == as.Date(to))} } else
    {
      if(is.null(from)) {ind1 <- ind1} else {ind1 <- which(x[,1] == as.POSIXct(from))}
      if(is.null(to)) {ind2 <- ind2} else {ind2 <- which(x[,1] == as.POSIXct(to))} })
  par(mar=c(5.1, 5, 4.1, 5))
  with(x, plot(x[ind1:ind2,1],x[ind1:ind2,3],  type = "l", col = rgb(0, 0.6, 0.3), main = Title, xlab = "Time", ylab = "Discharge (m3/s)", lwd = qlw, ylim = c(min(x[ind1:ind2,3],na.rm = TRUE), adj.y*max(x[ind1:ind2,3],na.rm = TRUE))))
  par(new = T)
  with(x, plot(x[ind1:ind2,2],  type = "h", lwd = plw, axes = F, xlab = NA, ylab = NA, col = rgb(0,0.3,0.6), ylim = rev(c(0, adj.y*max(x[,2], na.rm = TRUE)))))
  axis(side = 4)
  mtext(side = 4, line = 3, "Rainfall (mm)")
  par(mar = c(5.1, 4.1, 4.1, 2.1))
}


#' Plot of the annual maximum sample
#'
#' Provides two plots. First, a histogram of the sample, second, a barplot
#'
#' The barplot is daily so that, although it's an annual maximum (AM) sequence, some bars will be closer together depending on the number of days between them.
#' @param x a data.frame with two columns. The first a date column and the second the annual maximum (AM) sequence. An AM object derived from the GetAM or ImportAM functions can be used.
#' @examples
#' #Get an AMAX sample and plot
#' AMplot(GetAM(58002))
#' @return a histogram of the AMAX sample and a barplot
#' @author Anthony Hammond
AMplot <- function(x){
  SiteRef <- as.character(x[1,3])
  hist(x[,2], main = paste("Annual maximum histogram", SiteRef, sep = ": "), xlab = "Discharge (m3/s)")
  plot(x[, 1:2], type = "h", col = rgb(0,0.3,0.6), lwd = 1.5, main = paste("Annual maximum peak flows", SiteRef, sep = ": "), ylab = "Discharge (m3/s)", xlab = "Water Years (axis is daily scale)")
}


#' Diagnostic plots for pooling groups
#'
#' Provides 10 plots to compare the sites in the pooling group
#'
#' @param x pooling group derived from the Pool() function
#' @param gauged logical argument with a default of FALSE. TRUE adds the top site in the pooling group to the plots in a different colour
#' @examples
#' #Form a gauged pooling group and plot the diagnostics with gauged = TRUE
#' Pool.96001 <- Pool(GetCDs(96001))
#' DiagPlots(Pool.96001, gauged = TRUE)
#' #Form an ugauged pooling group and plot the diagnostics
#' Pool.96001 <- Pool(GetCDs(96001), exclude = 96001)
#' DiagPlots(Pool.96001)
#' @return ten diagnostic plots for pooling groups
#' @author Anthony Hammond
DiagPlots <- function(x, gauged = FALSE)
{
  if(gauged == TRUE) {AMAX <- GetAM(rownames(x)[1])}
  if(gauged == TRUE) {CDs <- GetCDs(rownames(x)[1])}
  Min.A <- min(x$AREA)
  Max.A <- max(x$AREA)
  hist(NRFAData$AREA[NRFAData$AREA > Min.A*0.5 & NRFAData$AREA < Max.A*1.1], col="grey", lty=0, main = "", xlab = "AREA")
  text(x = x$AREA, y = 0, labels = rep("x", length(x$AREA)))
  suppressWarnings(if(gauged == TRUE) {text(x = CDs[1,2], y = 0, labels = "x", col = "red", cex = 2)})

  hist(NRFAData$SAAR, col="grey", lty=0, main = "", xlab = "SAAR")
  text(x = x$SAAR, y = 0, labels = rep("x", length(x$SAAR)))
  suppressWarnings(if(gauged == TRUE) {text(x = CDs[15,2], y = 0, labels = "x", col = "red", cex = 2)})

  hist(NRFAData$PROPWET, col="grey", lty=0, main = "", xlab = "PROPWET")
  text(x = x$PROPWET, y = 0, labels = rep("x", length(x$PROPWET)))
  suppressWarnings(if(gauged == TRUE) {text(x = CDs[11,2], y = 0, labels = "x", col = "red", cex = 2)})

  hist(NRFAData$FARL, col="grey", lty=0, main = "", xlab = "FARL")
  text(x = x$FARL, y = 0, labels = rep("x", length(x$FARL)))
  suppressWarnings(if(gauged == TRUE) {text(x = CDs[8,2], y = 0, labels = "x", col = "red", cex = 2)})

  hist(NRFAData$FPEXT, col="grey", lty=0, main = "", xlab = "FPEXT")
  text(x = x$FPEXT, y = 0, labels = rep("x", length(x$FPEXT)))
  suppressWarnings(if(gauged == TRUE) {text(x = CDs[9,2], y = 0, labels = "x", col = "red", cex = 2)})

  hist(NRFAData$BFIHOST, col="grey", lty=0, main = "", xlab = "BFIHOST")
  text(x = x$BFIHOST, y = 0, labels = rep("x", length(x$BFIHOST)))
  suppressWarnings(if(gauged == TRUE) {text(x = CDs[5,2], y = 0, labels = "x", col = "red", cex = 2)})

  NRFARural <- subset(NRFAData, URBEXT2000 <= 0.03)
  hist(NRFARural$URBEXT2000, col="grey", lty=0, main = "", xlab = "URBEXT2000", breaks = 50)
  text(x = x$URBEXT2000, y = 0, labels = rep("x", length(x$URBEXT2000)))
  suppressWarnings(if(gauged == TRUE) {text(x = CDs[18,2], y = 0, labels = "x", col = "red", cex = 2)})

  plot(NRFAData$LSkew, NRFAData$Lcv, main = "", xlab = "LSkew", ylab = "Lcv", pch = 19, cex = 0.4)
  points(x$LSkew, x$Lcv, pch = 21, cex = 1.15, bg = "blue")
  if(gauged == TRUE) {points(LSkew(AMAX$Flow), Lcv(AMAX$Flow), pch = 19, col = "red")}

  plot(NRFAData$LSkew, NRFAData$LKurt, main = "", xlab = "LSkew", ylab = "LKurtosis", pch = 19, cex = 0.4)
  points(x$LSkew, x$LKurt, pch = 21, cex = 1.15,  bg = "blue")
  if(gauged == TRUE) {points(LSkew(AMAX$Flow), LKurt(AMAX$Flow), pch = 19, col = "red")}

  plot(UKOutline, pch = 19, cex = 0.25, xlab = "Easting", ylab = "Northing", xlim = c(25272, 650000))
  Rows <- row.names(x)
  QMED.Pool <- QMEDData[Rows, 21:22]
  points(QMED.Pool, pch = 19, col = "red")
  if(gauged == TRUE) {points(CDs[19,2], CDs[20,2], pch = 19, col = "blue")}
}

#' Design hydrograph extraction
#'
#' Extracts a mean hydrograph from a flow series
#'
#'All the peaks over a user defined threshold are identified and separated by a user defined value 'qu', which is a quantile of flow. The top n peaks are selected and the hydrographs extracted. Each hydrograph is centred on the peak and truncated either side, where the flow falls below the 'qu' quantile flow. All events are scaled to have a peak flow of one, and the mean of these is taken as the scaled design hydrograph. After an initial view of the hydrograph, it can be truncated using the 'xst' and 'xend' arguments. The default is to select 10 hydrographs for averaging, however, there may well be less if the sample is short.
#' @param x a numeric vector. The flow series of interest
#' @param qu the quantile of flow which separates peaks and truncates either side of the peak to form the event hydrograph. The default is 0.8
#' @param n number of event hydrographs from which to derive the mean hydrograph. Default is 10. Depending on the length of x, there may be less than 10
#' @param thr threshold above which event peaks are selected. The default is 0.975
#' @param RetAll logical argument with a default of false. If TRUE, all the hydrographs from which the mean is derived are returned in a data.frame. If FALSE, the mean hydrograph is returned
#' @param xst an integer to truncate the x axis of the plot and resulting design hydrograph. The first point of the design hydrograph
#' @param xend an integer to truncate the x axis of the plot and resulting design hydrograph. The last point of the design hydrograph
#' @examples
#' #Extract a design hydrograph from the Thames daily mean flow. Then print the resulting hydrograph
#' ThamesDesHydro <- DesHydro(ThamesPQ$Q)
#' ThamesDesHydro
#' #Do the same but truncate the design hydrograph and the plot from the first point to the 30th
#' DesHydro(ThamesPQ$Q, xst = 1, xend = 30)
#' #adjust the qu value to see the impact
#' ThamesDesHydro <- DesHydro(ThamesPQ$Q, qu = 0.7)
#' #Return all the hydrographs
#' ThamesHydros <- DesHydro(ThamesPQ$Q, xst = 1, xend = 30, RetAll = TRUE)
#' #view the first six rows of the hydrographs
#' head(ThamesHydros)
#' @return a numeric vector which is the mean of the top n peak events in the flow series. Also a plot of the n hydrographs and the design hydrograph. If the RetAll argument equals TRUE, a data.frame of the n hydrographs is returned instead.
#' @author Anthony Hammond

DesHydro <- function(x , qu = 0.8, n = 10, thr = 0.975, xst = NULL, xend = NULL, RetAll = FALSE) {
  func <- mean
  mAve = round(n/2)
  mu <- quantile(x, qu, na.rm = TRUE)
  maxxmu <- max(which(x <= mu), na.rm = T)
  minxmu <- min(which(x <= mu), na.rm = T)
  x <- x[minxmu:maxxmu]
  POT.extND <- function(x, mu, thresh, NAs = TRUE)
  {
    Low.Func <- function(TS) #with NAs
    {
      L <- length(TS)-2
      L1 <- length(TS)-1
      L2 <- length(TS)
      Vec1 <- TS[1:L]
      Vec2 <- TS[2:L1]
      Vec3 <- TS[3:L2]
      P1 <- ifelse(Vec2 <= Vec1 & Vec2 <= Vec3 & Vec1!= Vec2, Vec2, NA)
      return(P1)
    }

    P.Func <- function(TS) #with NAs
    {
      L <- length(TS)-2
      L1 <- length(TS)-1
      L2 <- length(TS)
      Vec1 <- TS[1:L]
      Vec2 <- TS[2:L1]
      Vec3 <- TS[3:L2]
      P1 <- ifelse(Vec2 >= Vec1 & Vec2 >= Vec3 & Vec1!= Vec2, Vec2, NA)
      return(P1)
    }

    VP <- function(x, mu)
    {
      maxll <-  max(which(lows[1:x] <= mu), na.rm = T)
      minlr <-   min(which(lows[x:length(lows)] <= mu), na.rm = T)
      minlr <- x+(minlr-1)
      if(peaks[x] == max(peaks[maxll:minlr], na.rm = T)) {vp <- peaks[x]} else {vp <- NA}
      return(vp)
    }

    lows <- Low.Func(x)
    peaks <- P.Func(x)
    pt.ind <- which(peaks > thresh)
    pt <- peaks[pt.ind]
    l <- length(pt.ind)-1
    POT <- NULL
    {for (i in 1:l) {POT[i] <- VP(pt.ind[i], mu)}}
    if(NAs == TRUE) {POT <- POT} else {POT <- POT[which(is.na(POT) == FALSE)]}
    return(POT)
  }
  xPOTnd <- POT.extND(x, mu, quantile(x, thr, na.rm = TRUE), NAs = FALSE)
  TopEvents <- head(sort(xPOTnd, decreasing = TRUE), n)
  PeakIndex <- match(TopEvents, x)
  hydr <- function(x,y, mu){
    MinInd <- min(which(x[y:length(x)] < mu))
    MaxInd <- max(which(x[1:y] < mu))
    Hydrograph <- x[MaxInd: (MinInd+y)]
    Hydrograph <- Hydrograph/max(Hydrograph)
  }
  Hydros <- list()
  for(i in 1:n) {Hydros[[i]]<- hydr(x, PeakIndex[i], mu)}
  Lngths <- NULL
  for(i in 1:n) {suppressWarnings(Lngths[i] <- which(Hydros[[i]] == max(Hydros[[i]])))}
  NAstrt <- max(Lngths)-Lngths
  HydrosNA <- list()
  for(i in 1:n) {HydrosNA[[i]] <- append(rep(NA,NAstrt[i]), Hydros[[i]])}
  TotLn <- NULL
  for(i in 1:n) {TotLn[i] <- length(HydrosNA[[i]])}
  NAend <- max(TotLn)-TotLn
  for(i in 1:n) {HydrosNA[[i]] <- append(HydrosNA[[i]], rep(NA,NAend[i]))}
  HydrosDF <- HydrosNA[[1]]
  for(i in 2:n) {HydrosDF <- cbind(HydrosDF, HydrosNA[[i]])}
  AveHydro <- apply(HydrosDF, 1, func, na.rm = TRUE)
  AllHydro <- NULL
  for (i in 1:length(HydrosDF[,1])) {AllHydro[i] <- length(which(is.na(HydrosDF[i, ]) == FALSE))}
  PlotInd <- which(AllHydro >= mAve)
  if(is.null(xst) == TRUE){xst <- 1} else {xst <- xst}
  if(is.null(xend) == TRUE) {xend <- length(PlotInd)} else {xend <- xend}
  matplot(HydrosDF[PlotInd, ], type = "l", main = "Average hydrograph shape", ylab = "Scaled discharge", xlab = "Timestep", xlim = c(xst, xend))
  points(AveHydro[PlotInd], type = "l", lwd = 3)
  Hydros <- as.data.frame(HydrosDF[PlotInd,][xst:xend,])
  colnames(Hydros)[1] <- "V1"
  if(RetAll == FALSE) {return(AveHydro[PlotInd][xst:xend])} else {return(Hydros)}
}

#' Extreme rank plot
#'
#' A plot to inspect the distribution of ordered data
#'
#' By default the parameters of the distribution for comparison with the sample are estimated from the sample. However, the pars argument can be used to compare the distribution with parameters estimated separately. Similarly the growth factor (GF) parameters, linear coefficient of variation (Lcv) & linear skewness (LSkew), with the median can be entered. In this way the pooling estimated disrtibution can be compared to the sample. The ERplot is described in Hammond, A. (2019). Proposal of the ‘extreme rank plot’ for extreme value analysis: with an emphasis on flood frequency studies. Hydrology Research, 50 (6), 1495–1507.
#' @param x numeric vector. A sample for inspection
#' @param Title a charcter string to change the default title, which is "Extreme Rank Plot"
#' @param dist a choice of distribution. The choices are "GenLog", "GEV", and "GenPareto"
#' @param pars a vector of length three. In the order of location, scale, & shape. If left null the parameters are estimated from x
#' @param GF a vector of length three, in the order of; Lcv, LSkew and Median
#' @param ln logical TRUE or FALSE with a default of FALSE. If TRUE, the variable under consideration is log transformed for the plot
#' @examples
#' #Get an AMAX sample and plot
#' \donttest{AM.27083 <- GetAM(27083)}
#' \donttest{ERPlot(AM.27083$Flow)}
#' #Get some pooled estimate of Lcv & LSkew to use with the GF argument
#' \donttest{QuickResults(GetCDs(27083), gauged = TRUE)}
#' #Use the resulting Lcv, Lskew and RP2 for the GF argument and change the title
#' \donttest{ERPlot(AM.27083$Flow, Title = "Site 27083 pooled comparison", GF = c(0.2286109, 0.1536903, 12.513))}
#' @return The extreme rank plot with GoTF scores
#' @author Anthony Hammond

ERPlot <- function(x, Title = "Extreme Rank Plot", dist = "GenLog", pars = NULL, GF = NULL, ln = FALSE)
{
  Score <- GoTF(x = x, dist = dist, pars = pars, GF = GF)
  RandRP <- 1/runif(length(x)*10000)
  if(dist == "GenPareto") {
    if(is.null(pars) == TRUE) {parsCalc <- as.numeric(GenParetoPars(x))}
    if(is.null(pars) == FALSE) {Sims <- GenParetoEst(pars[1], pars[2], pars[3], RP = RandRP, ppy = 1)}
    if(is.null(GF) == FALSE) {Sims <- GF[3]*GenParetoGF(GF[1], GF[2], RP = RandRP, ppy = 1)}
    if(is.null(pars) == TRUE & is.null(GF) == TRUE) {Sims <- GenParetoEst(parsCalc[1], parsCalc[2], parsCalc[3], RP = RandRP, ppy = 1)}
  }
  if(dist == "GenLog") {
    if(is.null(pars) == TRUE) {parsCalc <- as.numeric(GenLogPars(x))}
    if(is.null(pars) == FALSE) {Sims <- GenLogEst(pars[1], pars[2], pars[3], RP = RandRP)}
    if(is.null(GF) == FALSE) {Sims <- GF[3]*GenLogGF(GF[1], GF[2], RP = RandRP)}
    if(is.null(pars) == TRUE & is.null(GF) == TRUE) {Sims <- GenLogEst(parsCalc[1], parsCalc[2], parsCalc[3], RP = RandRP)}
  }
  if(dist == "GEV") {
    if(is.null(pars) == TRUE) {parsCalc <- as.numeric(GEVPars(x))}
    if(is.null(pars) == FALSE) {Sims <- GEVEst(pars[1], pars[2], pars[3], RP = RandRP)}
    if(is.null(GF) == FALSE) {Sims <- GF[3]*GEVGF(GF[1], GF[2], RP = RandRP)}
    if(is.null(pars) == TRUE & is.null(GF) == TRUE) {Sims <- GEVEst(parsCalc[1], parsCalc[2], parsCalc[3], RP = RandRP)}
  }
  Mat.1 <- matrix(Sims, nrow = length(x), ncol = 10000)
  Mat.Sort <- apply(Mat.1, 2, sort)
  Sorted.AM <- sort(x)
  Quants.Lower <- apply(Mat.Sort,1, quantile,0.05)
  Quants.Upper <- apply(Mat.Sort,1, quantile,0.95)
  Quants.Middle <- apply(Mat.Sort,1, quantile,0.5)
  if(ln == TRUE) {PlotData <- data.frame(log(Sorted.AM), log(Quants.Lower), log(Quants.Upper),log(Quants.Middle))} else {PlotData <- data.frame(Sorted.AM, Quants.Lower, Quants.Upper, Quants.Middle)}
  if(ln == FALSE) {
    matplot(PlotData, type = c("p", "l", "l", "l"), pch =1, lty = c(1,2,2), col = c("blue", "black", "black","black"),lwd = 1.5, main = Title, xlab = "Rank", ylab = "magnitude", ylim = c(min(x)-0.1*min(x), max(x)+0.1*max(x)))
    legend("topleft", legend = c("Observed", "Modelled Central", "Modelled 90% Intervals"), lty = c(0, 1, 2), pch = 1, pt.cex = c(1, 0, 0), lwd = 1.5, col = c("blue","black","black"), bty = "n", y.intersp = 1, x.intersp = 0.3, seg.len = 1)
    text(x = quantile(seq(1:length(x)), 0.65), y = quantile(x, 0.01), labels = paste("GoTF cv:", Score[1], sep = " "), cex = 0.8, adj = 0)
    text(x = quantile(seq(1:length(x)), 0.65), y = quantile(x, 0.13), labels = paste("GoTF mean:", Score[2], sep = " "), cex = 0.8, adj = 0)
  } else {
    matplot(PlotData, type = c("p", "l", "l", "l"), pch =1, lty = c(1,2,2), col = c("blue", "black", "black","black"),lwd = 1.5, main = Title, xlab = "Flow Ranks", ylab = "log discharge (m3/s)", ylim = c(min(log(x))-0.1*min(log(x)), max(log(x))+0.1*max(log(x))))
    legend("topleft", legend = c("Observed", "Modelled Central", "Modelled 90% Intervals"), lty = c(0, 1, 2), pch = 1, pt.cex = c(1, 0, 0), lwd = 1.5, col = c("blue","black","black"), bty = "n", y.intersp = 1, x.intersp = 0.3, seg.len = 1)
  }
}


# LMoments ----------------------------------------------------------------
#' Lmoments & Lmoment ratios
#'
#' Calculates the Lmoments and Lmoment ratios from a sample of data
#'
#' Lmoments calculated according to methods outlined by Hosking & Wallis (1997): Regional Frequency Analysis and approach based on LMoments. Also in the Flood Estimation Handbook (1999), volume 3.
#' @param x a numeric vector. The sample of interest
#' @examples
#' #Get an AMAX sample and calculate the Lmoments
#' AM.96001 <- GetAM(96001)
#' Lmoms(AM.96001$Flow)
#' @return A data.frame with one row and column headings; L1, L2, L3, L4, Lcv, LSkew, and LKurt. The first four are the Lmoments and the next three are the Lmoment ratios.
#' @author Anthony Hammond

Lmoms <- function(x)
{
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x, na.rm = TRUE)
  b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  L3 <- 6*b2-6*b1+b0
  L4 <- 20*b3-30*b2+12*b1-b0
  Lcv <- L2/L1
  LSkew <- L3/L2
  LKurt <- L4/L2
  Frame <- data.frame(L1, L2, L3, L4, Lcv, LSkew, LKurt)
  colnames(Frame) <- c("L1", "L2", "L3", "L4", "Lcv", "LSkew", "LKurt")
  return(Frame)
}


#' Ungauged pool weighted linear coefficient of variation (Lcv)
#'
#' Calculates the ungauged weighted Lcv from a pooling group
#'
#' Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
#' @param x pooling group derived with the Pool() function
#' @examples
#' #Get some CDs, form an ungauged pooling group, and estimate ungauged Lcv
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001, exclude = 96001)
#' WungLcv(Pool.96001)
#' @return the ungauged weighted Lcv from a pooling group
#' @author Anthony Hammond

WungLcv <- function(x)
{
  Ck.LCV <- function(n) {0.02609/(n-1)}
  bj.Lcv <- function(SDM) {0.0047*sqrt(SDM) + (0.0023/2)}
  Weight <- cbind(x$N,x$SDM , x$Lcv, bj.Lcv(x$SDM), Ck.LCV(x$N), (bj.Lcv(x$SDM) + Ck.LCV(x$N))^-1)
  colnames(Weight) <- c("N", "SDM", "Lcv",  "bLCV", "cLCV", "bc.LCV")
  bc.vector <- Weight[,6]
  s.bc <- sum(bc.vector)
  Wjs.LCV <- bc.vector/s.bc
  Sum.prod <- sum(Wjs.LCV*Weight[,3])
  return(Sum.prod)
}

#' Ungauged pool weighted linear skewness (LSkew)
#'
#' Calculates the ungauged weighted LSkew from a pooling group
#'
#' Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
#' @param x pooling group derived with the Pool() function
#' @examples
#' #Get some CDs, form an ungauged pooling group, and estimate ungauged LSkew
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001, exclude = 96001)
#' WungLSkew(Pool.96001)
#' @return the ungauged weighted LSkew from a pooling group
#' @author Anthony Hammond

WungLSkew <- function(x)
{
  Ck.LSkew <- function(n) {0.2743/(n-2)}
  bj.LSkew <- function(SDM) {0.0219*(1-exp(-(SDM/0.2360)))}
  Weight <- cbind(x$N,x$SDM , x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
  colnames(Weight) <- c("N", "SDM", "LSkew",  "bLSkew", "cLSkew", "bc.LSkew")
  bc.vector <- Weight[,6]
  s.bc <- sum(bc.vector)
  Wjs.LSkew <- bc.vector/s.bc
  Sum.prod <- sum(Wjs.LSkew*Weight[,3])
  return(Sum.prod)
}


#' Gauged pool weighted linear skewness (LSkew)
#'
#' Calculates the gauged weighted LSkew from a pooling group (enhanced single site)
#'
#' Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
#' @param x pooling group derived with the Pool() function
#' @examples
#' #Get some CDs, form a gauged pooling group, and estimate gauged LSkew
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001)
#' WGaugLSkew(Pool.96001)
#' @return the gauged weighted LSkew from a pooling group
#' @author Anthony Hammond
WGaugLSkew <- function(x)
{
  WLSKEW1 <- function(x)
  {
    Ck.LSkew <- function(n) {0.2743/(n-2)}
    bj.LSkew <- function(SDM) {0.0219*(1-exp(-(SDM/0.2360)))}
    Weight <- cbind(x$N,x$SDM , x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "LSkew",  "bLSkew", "cLSkew", "bc.LSkew")
    bc.vector <- Weight[,6]
    s.bc <- sum(bc.vector)
    Weight.1b <- Weight[1,4]
    Weight.1c <- Weight[1,5]
    Wjs.LSkew <- (Weight.1b/(Weight.1c+Weight.1b))+((Weight.1c*(Weight.1c+Weight.1b)^-2)/s.bc)
    return(Wjs.LSkew)
  }
  WLSKEWj <- function(x)
  {
    Ck.LSkew <- function(n) {0.2743/(n-2)}
    bj.LSkew <- function(SDM) {0.0219*(1-exp(-(SDM/0.2360)))}
    Weight <- cbind(x$N,x$SDM , x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "LSkew",  "bLSkew", "cLSkew", "bc.LSkew")
    bc.vector <- Weight[,6]
    s.bc <- sum(bc.vector)
    bc.vector <- bc.vector[-1]
    Weight.1b <- Weight[1,4]
    Weight.1c <- Weight[1,5]
    Wjs.LSkew <- (Weight.1c*(Weight.1c+Weight.1b)^-1 * bc.vector)/s.bc
    return(Wjs.LSkew)
  }
  W1 <- WLSKEW1(x)
  Wjs <- WLSKEWj(x)
  W <- append(W1, Wjs)
  Sum.prod <- sum(x$LSkew*W)
  return(Sum.prod)
}


#' Gauged pool weighted linear coefficient of variation (Lcv)
#'
#' Calculates the gauged weighted Lcv from a pooling group (enhanced single site)
#'
#' Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
#' @param x pooling group derived with the Pool() function
#' @examples
#' #Get some CDs, form a gauged pooling group, and estimate gauged Lcv
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001)
#' WGaugLcv(Pool.96001)
#' @return the gauged weighted Lcv from a pooling group
#' @author Anthony Hammond
WGaugLcv <- function(x)
{
  WLCV1 <- function(x)
  {
    Ck.LCV <- function(n) {0.02609/(n-1)}
    bj.LCV <- function(SDM) {0.0047*sqrt(SDM) + (0.0023/2)}
    Weight <- cbind(x$N,x$SDM , x$Lcv, bj.LCV(x$SDM), Ck.LCV(x$N), (bj.LCV(x$SDM) + Ck.LCV(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "Lcv",  "bLCV", "cLCV", "bc.LCV")
    bc.vector <- Weight[,6]
    s.bc <- sum(bc.vector)
    Weight.1b <- Weight[1,4]
    Weight.1c <- Weight[1,5]
    Wjs.LCV <- (Weight.1b/(Weight.1c+Weight.1b))+((Weight.1c*(Weight.1c+Weight.1b)^-2)/s.bc)
    return(Wjs.LCV)
  }
  WLCVj <- function(x)
  {
    Ck.LCV <- function(n) {0.02609/(n-1)}
    bj.LCV <- function(SDM) {0.0047*sqrt(SDM) + (0.0023/2)}
    Weight <- cbind(x$N,x$SDM , x$Lcv, bj.LCV(x$SDM), Ck.LCV(x$N), (bj.LCV(x$SDM) + Ck.LCV(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "Lcv",  "bLCV", "cLCV", "bc.LCV")
    bc.vector <- Weight[,6]
    s.bc <- sum(bc.vector)
    bc.vector <- bc.vector[-1]
    Weight.1b <- Weight[1,4]
    Weight.1c <- Weight[1,5]
    Wjs.LCV <- (Weight.1c*(Weight.1c+Weight.1b)^-1 * bc.vector)/s.bc
    return(Wjs.LCV)
  }
  W1 <- WLCV1(x)
  Wjs <- WLCVj(x)
  W <- append(W1, Wjs)
  Sum.prod <- sum(x$Lcv*W)
  return(Sum.prod)
}


#' Site ungauged linear coefficient of variation (Lcv) weightings
#'
#' Provides the ungauged Lcv weights for each site in a pooling group
#'
#' Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
#' @param x pooling group derived with the Pool() function
#' @examples
#' #Get some CDs, form an ungauged pooling group, and estimate ungauged Lcv
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001, exclude = 96001)
#' WeightsUnLcv(Pool.96001)
#' @return A data.frame with site references in the first column and associated weights in the second
#' @author Anthony Hammond
WeightsUnLcv <- function(x)
{
  Ck.LCV <- function(n) {0.02609/(n-1)}
  bj.Lcv <- function(SDM) {0.0047*sqrt(SDM) + (0.0023/2)}
  Weight <- cbind(x$N,x$SDM , x$Lcv, bj.Lcv(x$SDM), Ck.LCV(x$N), (bj.Lcv(x$SDM) + Ck.LCV(x$N))^-1)
  colnames(Weight) <- c("N", "SDM", "Lcv",  "bLCV", "cLCV", "bc.LCV")
  bc.vector <- Weight[,6]
  s.bc <- sum(bc.vector)
  Wjs.LCV <- bc.vector/s.bc
  Table <- data.frame(row.names(x), Wjs.LCV)
  colnames(Table) <- c("Site", "Weight")
  return(Table)
}

#' Site ungauged linear skewness (LSkew) weightings
#'
#' Provides the ungauged LSkew weights for each site in a pooling group
#'
#' Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
#' @param x pooling group derived with the Pool() function
#' @examples
#' #Get some CDs, form an ungauged pooling group, and estimate ungauged LSkew
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001, exclude = 96001)
#' WeightsUnLSkew(Pool.96001)
#' @return A data.frame with site references in the first column and associated weights in the second
#' @author Anthony Hammond
WeightsUnLSkew <- function(x)
{
  Ck.LSkew <- function(n) {0.2743/(n-2)}
  bj.LSkew <- function(SDM) {0.0219*(1-exp(-(SDM/0.2360)))}
  Weight <- cbind(x$N,x$SDM , x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
  colnames(Weight) <- c("N", "SDM", "LSkew",  "bLSkew", "cLSkew", "bc.LSkew")
  bc.vector <- Weight[,6]
  s.bc <- sum(bc.vector)
  Wjs.LSkew <- bc.vector/s.bc
  Table <- data.frame(row.names(x), Wjs.LSkew)
  colnames(Table) <- c("Site", "Weight")
  return(Table)
}


#' Site gauged linear coefficient of variation (Lcv) weightings
#'
#' Provides the gauged Lcv weights for each site in a pooling group
#'
#' Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
#' @param x pooling group derived with the Pool() function
#' @examples
#' #Get some CDs, form a gauged pooling group, and estimate gauged Lcv
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001)
#' WeightsGLcv(Pool.96001)
#' @return A data.frame with site references in the first column and associated weights in the second
#' @author Anthony Hammond
WeightsGLcv  <- function(x)
{
  WLCV1 <- function(x)
  {
    Ck.LCV <- function(n) {0.02609/(n-1)}
    bj.LCV <- function(SDM) {0.0047*sqrt(SDM) + (0.0023/2)}
    Weight <- cbind(x$N,x$SDM , x$Lcv, bj.LCV(x$SDM), Ck.LCV(x$N), (bj.LCV(x$SDM) + Ck.LCV(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "Lcv",  "bLCV", "cLCV", "bc.LCV")
    bc.vector <- Weight[,6]
    s.bc <- sum(bc.vector)
    Weight.1b <- Weight[1,4]
    Weight.1c <- Weight[1,5]
    Wjs.LCV <- (Weight.1b/(Weight.1c+Weight.1b))+((Weight.1c*(Weight.1c+Weight.1b)^-2)/s.bc)
    return(Wjs.LCV)
  }
  WLCVj <- function(x)
  {
    Ck.LCV <- function(n) {0.02609/(n-1)}
    bj.LCV <- function(SDM) {0.0047*sqrt(SDM) + (0.0023/2)}
    Weight <- cbind(x$N,x$SDM , x$Lcv, bj.LCV(x$SDM), Ck.LCV(x$N), (bj.LCV(x$SDM) + Ck.LCV(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "Lcv",  "bLCV", "cLCV", "bc.LCV")
    bc.vector <- Weight[,6]
    s.bc <- sum(bc.vector)
    bc.vector <- bc.vector[-1]
    Weight.1b <- Weight[1,4]
    Weight.1c <- Weight[1,5]
    Wjs.LCV <- (Weight.1c*(Weight.1c+Weight.1b)^-1 * bc.vector)/s.bc
    return(Wjs.LCV)
  }
  W1 <- WLCV1(x)
  Wjs <- WLCVj(x)
  W <- append(W1, Wjs)
  Table <- data.frame(row.names(x), W)
  colnames(Table) <- c("Site", "Weight")
  return(Table)
}

#' Site gauged linear skewness (LSkew) weightings
#'
#' Provides the gauged LSkew weights for each site in a pooling group
#'
#' Weighting method as according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation
#' @param x pooling group derived with the Pool() function
#' @examples
#' #Get some CDs, form a gauged pooling group, and estimate gauged LSkew
#' CDs.96001 <- GetCDs(96001)
#' Pool.96001 <- Pool(CDs.96001)
#' WeightsGLSkew(Pool.96001)
#' @return A data.frame with site references in the first column and associated weights in the second
#' @author Anthony Hammond

WeightsGLSkew <- function(x)
{
  WLSKEW1 <- function(x)
  {
    Ck.LSkew <- function(n) {0.2743/(n-2)}
    bj.LSkew <- function(SDM) {0.0219*(1-exp(-(SDM/0.2360)))}
    Weight <- cbind(x$N,x$SDM , x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "LSkew",  "bLSkew", "cLSkew", "bc.LSkew")
    bc.vector <- Weight[,6]
    s.bc <- sum(bc.vector)
    Weight.1b <- Weight[1,4]
    Weight.1c <- Weight[1,5]
    Wjs.LSkew <- (Weight.1b/(Weight.1c+Weight.1b))+((Weight.1c*(Weight.1c+Weight.1b)^-2)/s.bc)
    return(Wjs.LSkew)
  }
  WLSKEWj <- function(x)
  {
    Ck.LSkew <- function(n) {0.2743/(n-2)}
    bj.LSkew <- function(SDM) {0.0219*(1-exp(-(SDM/0.2360)))}
    Weight <- cbind(x$N,x$SDM , x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "LSkew",  "bLSkew", "cLSkew", "bc.LSkew")
    bc.vector <- Weight[,6]
    s.bc <- sum(bc.vector)
    bc.vector <- bc.vector[-1]
    Weight.1b <- Weight[1,4]
    Weight.1c <- Weight[1,5]
    Wjs.LSkew <- (Weight.1c*(Weight.1c+Weight.1b)^-1 * bc.vector)/s.bc
    return(Wjs.LSkew)
  }
  W1 <- WLSKEW1(x)
  Wjs <- WLSKEWj(x)
  W <- append(W1, Wjs)
  Table <- data.frame(row.names(x), W)
  colnames(Table) <- c("Site", "Weight")
  return(Table)
}

#' Linear coefficient of variation (Lcv)
#'
#' Calculates the Lcv from a sample of data
#'
#' Lcv calculated according to methods outlined by Hosking & Wallis (1997): Regional Frequency Analysis and approach based on LMoments. Also in the Flood Estimation Handbook (1999), volume 3.
#' @param x a numeric vector. The sample of interest
#' @examples
#' #Get an AMAX sample and calculate the Lmoments
#' AM.96001 <- GetAM(96001)
#' Lcv(AM.96001$Flow)
#' @return Numeric. The Lcv of a sample.
#' @author Anthony Hammond

Lcv <- function(x)
{
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x, na.rm = TRUE)
  b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  Lcv <- L2/L1
  return(Lcv)
}


#' Linear Skewness (LSkew)
#'
#' Calculates the LSkew from a sample of data
#'
#' LSkew calculated according to methods outlined by Hosking & Wallis (1997): Regional Frequency Analysis and approach based on LMoments. Also in the Flood Estimation Handbook (1999), volume 3.
#' @param x a numeric vector. The sample of interest
#' @examples
#' #Get an AMAX sample and calculate the Lmoments
#' AM.96001 <- GetAM(96001)
#' LSkew(AM.96001$Flow)
#' @return Numeric. The LSkew of a sample.
#' @author Anthony Hammond
LSkew <- function(x)
{
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x, na.rm = TRUE)
  b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  L3 <- 6*b2-6*b1+b0
  LSkew <- L3/L2
  return(LSkew)
}


#' Linear Kurtosis (LKurt)
#'
#' Calculates the LKurtosis from a sample of data
#'
#' LKurtosis calculated according to methods outlined by Hosking & Wallis (1997): Regional Frequency Analysis and approach based on LMoments. Also in the Flood Estimation Handbook (1999), volume 3.
#' @param x a numeric vector. The sample of interest
#' @examples
#' #Get an AMAX sample and calculate the Lmoments
#' AM.96001 <- GetAM(96001)
#' LKurt(AM.96001$Flow)
#' @return Numeric. The LSkew of a sample.
#' @author Anthony Hammond
LKurt <- function(x) {
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  b2 <- mean(((Rank-1)*(Rank-2))/((length(x)-1)*(length(x)-2))*Sort.x, na.rm = TRUE)
  b3 <- mean(((Rank-1)*(Rank-2)*(Rank-3))/((length(x)-1)*(length(x)-2)*(length(x)-3))*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  L3 <- 6*b2-6*b1+b0
  L4 <- 20*b3-30*b2+12*b1-b0
  Lkurt <- L4/L2
  return(Lkurt)
}

# ReFH --------------------------------------------------------------------

#' Revitalised Flood Hydrograph Model (ReFH)
#'
#' Provides outputs of the ReFH model from catchment descriptors or user defined inputs
#'
#' The ReFH is described in the Flood Estimation Handbook Supplementary Report No.1 (2007). The method to derive design rainfall profiles is described in the Flood Estimation Handbook (1999), volume 2. Users can also, input their own rainfall with the 'Rain' argument. As a default, when catchment descriptors (CDs) are provided the ReFH function uses catchment descriptors to estimate the parameters of the ReFH model and the two year rainfall for the critical duration. The latter is based on a quadratic interpolation of the catchment descriptors RMED1H, RMED1D, and RMED2D. Parameters and initial conditions can also be inidvidually input by the user. If a parameter argument is used for one or more of the parameters, then these overwrite the CD derived parameters. If a value for the scaled argument is provided (m3/s), a scaled hydrograph is returned. The formulation of this function avoids the need for users to select a storm duration that is an odd integer multiple of the data interval. This is achieved as follows: for the design rainfall profile, the duration is split into a sequence by multiplying duration by the reciprocal of the data interval (timestep) and rounding up to the nearest odd integer. This forms the end of a sequence starting from one and progressing at an interval of two. This sequence is divided by the duration to form the vector to which the exponent b is raised in the calculation of z for the storm profile (see Flood Estimation Handbook (1999), volume 2)
#' @param CDs catchment descriptors derived from either GetCDs or ImportCD
#' @param Depth a numeric value. The depth of rainfall used as input in the estimation of a design hydrograph. The default is a two year rainfall
#' @param duration a numeric value. A duration for the design rainfall
#' @param timestep a numeric value. A user defined data interval. The default changes depending on the estimated time to peak to formulate a sensible looking result
#' @param scaled a numeric value of peak flow in m3/s
#' @param PlotTitle a character string. A user defined title for the ReFH plot
#' @param RP return period (Default = 2)
#' @param alpha a logical argument with default TRUE. If TRUE the alpha adjustment is applied based on RP. If FALSE, no alpha adjustment is made
#' @param season a choice of "summer" or "winter". The default is "summer" in urban catchments (URBEXT2000 > 0.03) and "winter" in rural catchments
#' @param AREA numeric. Catchment area in km2.
#' @param TP numeric. Time to peak parameter (hours)
#' @param BR numeric. Baseflow recharge parameter
#' @param BL numeric. Baseflow lag parameter (hours)
#' @param Cmax numeric. Maximum soil moisture capacity parameter (mm)
#' @param Cini numeric. Initial soil moisture content (mm)
#' @param BFini numeric. Initial baseflow (m3/s)
#' @param Rain numeric. User input rainfall. A numeric vector
#' @examples
#' #Get CDs and apply the ReFH function
#' CDs.203018 <- GetCDs(203018)
#' ReFH(CDs.203018)
#' #Apply the ReFH function, scale to a 100-year flow estimate and change the plot title accordingly
#' ReFH(CDs.203018, scaled = 182, PlotTitle = "100-Year Design Hydrograph - Site 203018")
#' #Apply the ReFH function with a user defined initial baseflow
#' ReFH(CDs.203018, BFini = 6)
#' @return A print out of parameters, a results data.frame, and a plot. First is a print of the parameters, initial conditions and the catchment area. The second is a data.frame with columns Rain, NetRain, Runoff, Baseflow, and TotalFlow. If the scale argument is used a numeric vector containing the scaled hydrograph is returned. The plot is of the ReFH output, with rainfall, net-rainfall, baseflow, runoff and total flow. If the scaled argument is used, a scaled hydrograph is plotted.
#' @author Anthony Hammond

ReFH <- function(CDs = NULL, Depth = NULL, duration = NULL, timestep = NULL, scaled = NULL, PlotTitle = NULL, RP = NULL, alpha = TRUE, season = NULL, AREA = NULL, TP = NULL, BR = NULL, BL = NULL, Cmax = NULL, Cini = NULL, BFini = NULL, Rain = NULL) {
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  if(is.null(season) == TRUE & is.null(CDs) == TRUE) stop ("CDs argument and/or season argument must be used")
  if(is.null(season) == TRUE) {
    if(CDs[18,2] > 0.03) {Season <- "summer"} else {Season <- "winter"}}
  else {Season <- season}
  if(is.null(CDs) == TRUE & is.null(Depth) == TRUE) {Depth <- sum(Rain)}
  if(is.null(CDs) == TRUE & is.null(duration) == TRUE) {duration <- length(Rain)}
  Params <- function(x, season, D = NULL, cini = NULL) {
    PROPWET <- x[11,2]
    DPLBAR <- x[6,2]
    URBEXT2000 <- x[18,2]
    DPSBAR <- x[7,2]
    SAAR <- x[15,2]
    BFIHOST <- x[5,2]
    AREA <- x[1,2]
    TP <- 1.56*PROPWET^-1.09*DPLBAR^0.6*(1+URBEXT2000)^-3.34*DPSBAR^-0.28
    if(is.null(D) == TRUE) {D <- TP*(1+(SAAR/1000))} else {D <- D}
    BL <- 25.5*BFIHOST^0.47*DPLBAR^0.21*PROPWET^-0.53*(1+URBEXT2000)^-3.01
    BR <- 3.75*BFIHOST^1.08*PROPWET^0.36
    Cmax <- 596.7*BFIHOST^0.95*PROPWET^-0.24
    if(is.null(cini) == TRUE) {
    Cini.win <- (Cmax/2)*(1.2-1.7*BFIHOST+0.82*PROPWET)
    Cini.sum <- (Cmax/2)*(0.9-0.82*BFIHOST-0.43*PROPWET)
    if(season == "winter") {Cini <- Cini.win}
    if(season == "summer") {Cini <- Cini.sum}} else {Cini <- cini}
    if(Cini <= 0) {Cini <- 0} else {Cini <- Cini}
    BFini.win <- (63.8*(Cini-120.8)+5.54*SAAR)*10^-5*AREA
    BFini.sum <- (33.9*(Cini - 85.4)+3.14*SAAR)*10^-5*AREA
    if(season == "winter") {BFini <- BFini.win}
    if(season == "summer") {BFini <- BFini.sum}
    if(BFini <= 0) {BFini <- 0} else {BFini <- BFini}
    df <- data.frame(AREA, TP, D, BR, BL, Cmax, Cini, BFini)
    return(df)
  }
  PProfile <- function(d, season = "winter", timestep = 1){
    d <- d*(1/timestep)
    OddSeq <- seq(1,ceiling(d), by = 2)
    vec <- OddSeq/d
    if(season == "winter") {a <- 0.060}
    if(season == "winter") {b <- 1.026}
    if(season == "summer") {a <- 0.1}
    if(season == "summer") {b <- 0.815}
    z <- vec^b
    y <- (1-a^z)/(1-a)
    peak <- y[1]
    props <- NULL
    for (i in length(vec):2) {props[i] <- (y[i]-y[i-1])/2}
    RainVec <- c(sort(props, decreasing = FALSE), peak, sort(props, decreasing = TRUE))
    return(RainVec)
  }

  Loss <- function(P, cini, Cmax, RP, alpha = FALSE, season) {
    if(alpha == TRUE){
      if (season == "winter") {if(RP < 5) {a <- 1} else{a <- 1.166*RP^-0.073}}
      if (season == "summer")  {if(RP < 5) {a <- 1} else {a <- 1.444*RP^-0.182}}
      ct1 <- a*cini
      ct <- P[2]+ct1
      for (i in 3:length(P)) {ct <- append(ct, ct[length(ct)]+P[i])}
      ct <-append(ct1, ct)
      pr1 <- a*(cini/Cmax)+(P[1]/(2*Cmax))
      pr <- NULL
      for (i in 2:(length(P))) {pr[i] <- (ct[i-1]/Cmax)+(P[i]/(2*Cmax))}
      pr[1] <- pr1
    }
    else {
      ct1 <- cini
      ct <- P[2]+ct1
      for (i in 3:length(P)) {ct <- append(ct, ct[length(ct)]+P[i])}
      ct <-append(ct1, ct)
      pr1 <- (cini/Cmax)+(P[1]/(2*Cmax))
      pr <- NULL
      for (i in 2:(length(P))) {pr[i] <- (ct[i-1]/Cmax)+(P[i]/(2*Cmax))}
      pr[1] <- pr1
    }
    NetP <- pr*P
    return(NetP)
  }

  UH <- function(CDs, tp = FALSE, timestep = 1){
    if(tp == FALSE) {TP <- 1.56*CDs[11,2]^-1.09*CDs[6,2]^0.6*(1+CDs[18,2])^-3.34*CDs[7,2]^-0.28} else {TP <- tp}
    TBt <- 2*(TP/0.65)
    Uc <- 0.65*((TBt - 2*TP)/(TBt-TP))
    TB <- TP*(1+2*((1-0.65))/(0.8*Uc))
    U2tp <- Uc*0.8
    y <- c(0, mean(c(0, 0.65)), 0.65,U2tp, mean(c(U2tp, 0)), 0)
    x <- c(0, mean(c(0,TP)), TP, 2*TP, mean(c(2*TP, TB)), TB)
    y1mod <- lm(y[1:3] ~ x[1:3])
    y2mod <- lm(y[3:4] ~ x[3:4])
    y3mod <- lm(y[4:6] ~ x[4:6])
    Coefs1 <- as.numeric(coefficients(y1mod))
    Coefs2 <- as.numeric(coefficients(y2mod))
    Coefs3 <- as.numeric(coefficients(y3mod))
    YModel1 <- function(x) {Coefs1[1] + Coefs1[2]*x}
    YModel2 <- function(x) {Coefs2[1] + Coefs2[2]*x}
    YModel3 <- function(x) {Coefs3[1] + Coefs3[2]*x}
    UnitHydro <- data.frame(x, y)
    uh1 <- YModel1(seq(0, UnitHydro[3,1],by = 0.01))
    uh2 <- YModel2(seq(UnitHydro[3,1], UnitHydro[4,1],by = 0.01))
    uh3 <- YModel3(seq(UnitHydro[4,1], UnitHydro[6,1],by = 0.01))
    UH <- c(uh1,uh2[-1],uh3[-1])
    Scurve <- NULL
    for(i in 1:length(UH)) {Scurve[i] <- sum(UH[1:i])}
    TSCoef <- timestep/0.01
    Zeros <- rep(0, TSCoef)
    S.t <- append(Zeros, Scurve)
    SDiff <- abs(Scurve-S.t[1:length(Scurve)])
    UHfin <- SDiff*(0.01/timestep)
    TSCoef <- timestep/0.01
    UHVec <- seq(1, (length(UH)), by = TSCoef)
    return(UHfin[UHVec])
  }
  Convolute <- function(UH, NR){
    Mults <- NULL
    for(i in 1:length(NR)) {Mults[[i]] <- NR[i]*UH}
    NAadd <- seq(1,(length(NR)))
    NAsAdd <- rep(NA, sum(NAadd))
    for(i in 1:length(NAadd)) {Mults[[i]] <- append(NAsAdd[1:i], Mults[[i]])}
    LenConv <- length(UH)+length(NR)
    RainSums <- NULL
    for(i in 1:LenConv) {RainSums[i] <- sum(sapply(Mults, function(Mults) Mults[i]), na.rm = TRUE)}
    return(RainSums)
  }
  BF <- function(BL, BR, Bfini, ro, timestep = 1) {
    k3 <- exp(-(timestep/BL))
    k2 <- BR*(1-(1-k3)*(BL/timestep))
    k1 <- BR*((BL/timestep)*(1-k3)-k3)
    zt <- Bfini
    for(i in 2:(length(ro))) {zt <- append(zt, k1*ro[(i-1)]+k2*ro[i]+k3*zt[length(zt)])}
    return(zt)
  }
  if(is.null(CDs) == FALSE) {Pars <- Params(CDs, season = Season, D = duration, cini = Cini)}
  if(is.null(AREA) == TRUE) {AREA <- NA} else {AREA <- AREA}
  if(is.null(TP) == TRUE) {TP <- NA} else {TP <- TP}
  if(is.null(duration) == TRUE) {D <- NA} else {D <- duration}
  if(is.null(BR) == TRUE) {BR <- NA} else {BR <- BR}
  if(is.null(BL) == TRUE) {BL <- NA} else {BL <- BL}
  if(is.null(Cmax) == TRUE) {Cmax <- NA} else {Cmax <- Cmax}
  if(is.null(Cini) == TRUE) {Cini <- NA} else {Cini <- Cini}
  if(is.null(BFini) == TRUE) {BFini <- NA} else {BFini <- BFini}
  ParsNull <- c(AREA, TP, D, BR, BL, Cmax, Cini, BFini)
  ParsNull <- as.data.frame(t(ParsNull))
  colnames(ParsNull) <- c("AREA", "TP", "D", "BR", "BL", "Cmax", "Cini", "BFini")
  if(is.null(CDs) == TRUE) {Pars <- ParsNull}
  ParInd <- which(is.na(ParsNull[1,]) == FALSE)
  if(is.null(CDs) == TRUE & length(ParInd) < 8) stop ("If no CDs are provided the following arguments are required: AREA, TP, BL, duration, BR, Cmax, Cini, BFini, Depth, RP and/or alpha, and season")
  if(length(ParInd) > 0) {Pars[ParInd] <- ParsNull[ParInd]}
  print(Pars)
  if(is.null(Rain) == FALSE) {duration <- length(Rain)}  else {duration <- Pars$D}
  if(duration < Pars$TP) warning ("duration shorter than time to peak")
  if(duration >= 4.5*Pars$TP) warning ("This is an event based model; duration > 4.5TP and is on the high side")
  if(is.null(timestep) == TRUE){
    if(Pars$TP <= 1) {timestep <- 0.1}
    if(Pars$TP  > 1 & Pars$TP < 3) {timestep <- 0.25}
    if(Pars$TP  >= 3 & Pars$TP < 5) {timestep <- 0.5}
    if(Pars$TP  >= 5) {timestep <- 1}
  } else {timestep <- timestep}
  if(timestep > 0.25*ceiling(Pars$TP)) warning ("If timestep is > 0.25TP results may be a touch 'blocky'")
  if(is.null(Rain) == FALSE) {Depth <- sum(Rain)}
  if(is.null(Rain) == TRUE) {Rain <- PProfile(duration, season =  Season, timestep = timestep)} else {Rain <- Rain/sum(Rain)}
  if(is.null(CDs) == FALSE) {Rmod <- lm(CDs[12:14, 2] ~ c(1, 24, 48) + I(c(1, 24, 48)^2))
  RMEDmod <- function(x) {Rmod$coefficients[1] + Rmod$coefficients[2]*x + Rmod$coefficients[3]*x^2}
  RMEDest <- RMEDmod(Pars$D)}
  if(is.null(Depth) == TRUE) {Rain <- Rain*RMEDest} else {Rain <- Rain*Depth}
  if(is.null(Depth) == FALSE & is.null(RP) == TRUE & alpha == TRUE) stop ("if a depth is supplied, RP is needed or change alpha to FALSE")
  if(Season == "winter") {Cini <- Pars$Cini.win}
  if(Season == "summer") {Cini <- Pars$Cini.sum}
  if(is.null(RP) == TRUE) {RP <- 2} else {RP <- RP}
  EffRain <- Loss(Rain, cini = Pars$Cini, RP = RP, Cmax = Pars$Cmax, season = Season, alpha = alpha)
  UnitHydro <- UH(tp = Pars$TP, timestep = timestep)
  SF <- Pars$AREA/(3.6*Pars$TP)
  UnitHydro <- UnitHydro*SF
  Runoff <- Convolute(UnitHydro, EffRain)
  Runoff <- Runoff[-1]
  if(Season == "winter") {BFini <- Pars$BFini.win}
  if(Season == "summer") {BFini <- Pars$BFini.sum}
  Baseflow <- BF(Pars$BL, Pars$BR, Bfini = Pars$BFini, ro = Runoff, timestep = timestep)
  TotalFlow <- Runoff+Baseflow
  L <- length(TotalFlow)
  NAs <- rep(NA, times = (L-length(Rain)))
  Rain <- append(Rain, NAs)
  res <- round(data.frame(Rain, TotalFlow, Runoff, Baseflow), 3)
  if(is.null(scaled) == FALSE) {ScaledHydro <- (TotalFlow/max(TotalFlow))*scaled
  res2 <- round(ScaledHydro, 3)
  }
  if(is.null(PlotTitle) == FALSE) {title <- PlotTitle} else {title <- "Runoff Hydrograph"}
  if(is.null(PlotTitle) == FALSE) {titleScaled <- PlotTitle} else {titleScaled <- "Design Hydrograph"}
  if(is.null(scaled) == TRUE) {
    par(mar=c(5.1, 4.1, 4.1, 4.3))
    adj.y <- 1.5
    if (timestep == 1) {TimeLab <- "Hours"} else {TimeLab <- paste(as.character(timestep), "Hours", sep = " ")}
    with(res, matplot(res[,2:4],  type = "l", lwd = 2, main = title, xlab = TimeLab, ylab = "Discharge (m3/s)", ylim = c(0, adj.y*max(res[,2],na.rm = TRUE))))
    legend("topright", legend = c("NetRain", "Rain", "TotalFlow", "DirectRunoff", "Baseflow"), bty = "n", col = c(rgb(0,0.7,0.3), rgb(0,0.3,0.7), "black", "red", rgb(0,0.8,0.2)), lty = c(1,1,1,2,3), lwd = c(3,3,2,2,2))
    par(new = T)
    with(res, plot(res[,1],  type = "h", lwd = 3, axes = F, xlab = NA, ylab = NA, col = rgb(0,0.3,0.6), ylim = rev(c(0, adj.y*max(res[,1], na.rm = TRUE)))))
    axis(side = 4)
    mtext(side = 4, line = 3, "Rainfall (mm)")
    NetRain <- append(EffRain, NAs)
    points(NetRain, type = "h", col = rgb(0,0.7,0.3), lwd = 3)
    par(mar=c(5.1,4.1,4.1,2.1))}
  else {if (timestep == 1) {TimeLab <- "Hours"} else {TimeLab <- paste(as.character(timestep), "Hours", sep = " ")}
    plot(res2, main = titleScaled, ylab = "Discharge (m3/s)", xlab = TimeLab, type = "l", lwd = 2)}
  NetRain <- append(EffRain, NAs)
  Output <- round(data.frame(Rain, NetRain, Runoff, Baseflow, TotalFlow), 2)
  if(is.null(scaled) == TRUE) {return(Output)}
  if(is.null(scaled) == FALSE) {return(res2)}
}


# Precipitation -----------------------------------------------------------

#' FEH99 depth duration frequency precipitation model
#'
#' Estimation of design rainfall depths, and the rarity of observed rainfall
#'
#' The depth duration frequency rainfall model is detailed in the Flood Estimation Handbook (1999), volume 2.
#' A note about the discretisation: The user can choose between "daily" or "hourly" for the sliding duration to fixed duration conversion. If the 'Depth' argument is used, it overrides the return period (RP) argument and provides RP as a function of depth. However, if both the 'Depth' and the 'disc' arguments are used, the sliding duration depth is provided as a function of the user input depth. This resulting depth can then be used without the 'disc' argument to determine the sliding duration RP.
#' @param D numeric. The duration of interest (in hours)
#' @param RP return period
#' @param pars a numeric vector of length six. The six catchment parameters for the DDF model in the order of: c, d1, d2, d3, e, f
#' @param disc converts from the sliding duration to fixed duration estimate. Choices are "hourly" or "daily"
#' @param Depth a user supplied rainfall depth for the duration under question
#' @examples
#' #Examples from FEH volume 2
#' #The parameters for these examples are from FEH v2
#' #What is the 2-day rainfall with return period 100-years for Norwich.
#' DDF99(D = 48, RP = 100, pars = c(-0.023, 0.273, 0.351, 0.236, 0.309, 2.488))
#' #What is the 4-hour rainfall with return period 20 years for a typical point in the Lyne catchment
#' DDF99(D = 4, RP = 20, pars = c(-0.025, 0.344, 0.485, 0.402, 0.287, 2.374))
#' #How rare was the rainfall of 6th August 1978 at Broughshane, County Antrim?
#' DDF99(D = 5, Depth = 47.7, pars = c(-0.022, 0.412, 0.551, 0.276, 0.261, 2.252))
#' @return the rainfall depth or rainfall return period
#' @author Anthony Hammond

DDF99 <- function(D, RP, pars, Depth = NULL, disc = NULL) {
  if(is.null(Depth) == TRUE) {y <- -log(-log(1-(1/RP)))}
  c <- pars[1]
  d1 <- pars[2]
  d2 <- pars[3]
  d3 <- pars[4]
  e <- pars[5]
  f <- pars[6]
  if(D > 192|D < 1) stop ("Duration outside calibration range")
  if(is.null(Depth) == FALSE){
    if(D <= 12) {Y <- (log(Depth)-f-d1*log(D))/(c*log(D)+e)}
    if(D > 12 & D <= 48) {Y <- (log(Depth)-f-d1*log(12)+d2*(log(12)-log(D))) / (c*log(D)+e)}
    if(D > 48) {Y <- (log(Depth)-f-d1*log(12)+d2*(log(12)-log(48))+d3*(log(48)-log(D))) / (c*log(D)+e)}
    ReturnPeriod <- 1/(1-exp(-exp(-Y)))
    if(is.null(disc) == FALSE) {
      ModDays <- function(days) {-0.055935*days + 0.003871*days^2 + 1.210000}
      HourMod <- function(h) {-0.13706*log(h) + 0.03122*log(h)^2 + 1.16000}
      if(disc == "daily") {if(D/24 <=8 ) {dpth <- Depth*round(ModDays(D/24),2)} else {dpth <- Depth}}
      if(disc == "hourly") {if(D <= 8) {dpth <- Depth*round(HourMod(D),2)} else {dpth <- Depth}}}
      if(is.null(disc) == TRUE) {print("Return Period")
        return(ReturnPeriod)} else {
          print("sliding duration depth")
          return(dpth)}
  }
  else {
  if(D <= 12) {lnR <- (c*y + d1)* log(D) + e*y +f}
  lnR12 <- (c*y + d1)* log(12) + e*y+f
  if(D > 12 & D <= 48) {lnR <- lnR12 + (c*y + d2) * (log(D) - log(12))}
  lnR48 <- lnR12 + (c*y + d2) * (log(48) - log(12))
  if(D > 48) {lnR <- lnR48 + (c*y+d3)*(log(D)-log(48))}
  res <- exp(lnR)
  resRound <- round(res, 3)
  if(is.null(disc) == TRUE) {resRound <- resRound} else {
    ModDays <- function(days) {-0.055935*days + 0.003871*days^2 + 1.210000}
    HourMod <- function(h) {-0.13706*log(h) + 0.03122*log(h)^2 + 1.16000}
    if(disc == "daily") {if(D/24 <=8 ) {resRound <- resRound/round(ModDays(D/24),2)} else {resRound <- resRound}}
    if(disc == "hourly") {if(D <= 8) {resRound <- resRound/round(HourMod(D),2)} else {resRound <- resRound}}}
  return(resRound)}
}




#' Areal reduction factor (ARF)
#'
#' The results of applying, to a rainfall depth, the ratio of the rainfall over an area to the rainfall depth of the same duration at a representative point in the area.
#'
#' The ARF and it's use is detailed in the Flood Estimation Handbook (1999), volume 2. The DDF model is calibrated on point rainfall and the areal reduction factor converts it to a catchment rainfall for use with a rainfall runoff model such as ReFH (see details for ReFH function). The ReFH model includes a design rainfall profile for winter and summer but the depth duration frequency (DDF) model is calibrated on annual maximum peaks as opposed to seasonal peaks. A seasonal correction factor (SCF) is necessary to convert the DDF estimate to a seasonal one. The final depth, therefore is; Depth = DDFdepth x ARF x SCF.
#' @param Depth depth of rainfall
#' @param Area catchment area in km2
#' @param D duration in hours
#' @examples
#' #Derive the ARF for a depth of 30, an area of 500km2 and a duration of 12 hours
#' ARF(30, 500, 12)
#' @return the rainfall depth or rainfall return period
#' @author Anthony Hammond
ARF <- function(Depth, Area, D){
  if(Area <= 20)
  {a <- 0.4 - 0.0208* log(4.6-log(Area))
  b <- 0.0394*Area^0.354}
  if(Area > 20 & Area < 100){
    a <- 0.4 - 0.00382*(4.6 - log(Area))^2
    b <- 0.0494*Area^0.354
  }
  if(Area >= 100 & Area <500) {
    a <- 0.4 - 0.00382 * (4.6 - log(Area))^2
    b <- 0.0627*Area^0.254
  }
  if(Area >= 500 & Area < 1000) {
    a <- 0.4 - 0.0208 * log(log(Area) - 4.6)
    b <- 0.0627*Area^0.254
  }
  if(Area >= 1000) {
    a <- 0.4 - 0.0208 * log(log(Area) - 4.6)
    b <- 0.1050 * Area^0.180
  }
  ARF <- 1-b*D^-a
  resARF <- Depth*ARF
  return(resARF)
}



#' Seasonal correction factor (SCF)
#'
#' The results of applying the ratio of the seasonal annual maximum rainfall for a given duration to the annual maximum rainfall for the same duration
#'
#' The SCF and it's use is detailed in R&D Technical Report FD1913/TR - Revitalisation of the FSR/FEH rainfall runoff method (2005). The ReFH model has a design rainfall profile included for winter and summer but the depth duration frequency (DDF) model is calibrated on annual maximum peaks as opposed to seasonal peaks. The SCF is necessary to convert the DDF estimate to a seasonal one. Similarly, the DDF model is calibrated on point rainfall and the area reduction factor converts it to a catchment rainfall for use with a rainfall runoff model such as ReFH (see details of the ReFH function).The final depth, therefore is; Depth = DDFdepth x ARF x SCF.
#' @param SAAR standardised average annual rainfall. Numeric
#' @param duration duration in hours. Numeric
#' @examples
#' #Derive the SCFs for a SAAR of 1981 and a duration of 6.5
#' SCF(1981, 6.5)
#' @return A data.frame of one row and two columns: SCFSummer and SCFWinter.
#' @author Anthony Hammond
SCF <- function(SAAR, duration) {
  if(duration < 1) {duration <- 1} else {duration <- duration}
  if(duration > 24) {duration <- 24} else {duration <- duration}
  alphaMod <- function(x) {7.353e-06*log(x) + 1.857e-05*log(x)^2 + -7.267e-06*log(x)^3 - 8.030e-05}
  betaMod <- function(x) {-0.012813*log(x) - 0.004378*log(x)^2 + 0.002958*log(x)^3 + 1.040000}
  phiMod <- function(x) {2.776e-04*log(x) + 2.515e-05*log(x)^2 - 1.359e-05*log(x)^3 + 4.000e-04}
  psiMod <- function(x) {0.09750*log(x) - 0.05416*log(x)^2 + 0.01154*log(x)^3 + 0.40000}
  alpha <- alphaMod(duration)
  beta <- betaMod(duration)
  phi <- phiMod(duration)
  psi <- psiMod(duration)
  if(SAAR == 500) {SCFSummer <- 1} else {SCFSummer <- alpha*SAAR+beta}
  SCFWinter <- (1-exp(-phi*SAAR))^psi
  ResDF <- data.frame(SCFSummer, SCFWinter)
  return(ResDF)
}


# EncounterProbs ----------------------------------------------------------
#' Encounter probabilities
#'
#' @description Calculates the probability of experiencing at least n events with a given return period (RP), over a given number of years
#'
#' @details  The choice of binomial or Poisson distributions for calculating encounter probablities is akin to annual maximum (AM) versus peaks over threshold (POT) approaches to extreme value analysis. AM and binomial assume only one "event" can occur in the blocked time period. Whereas Poisson and POT don't make this assumption. In the case of most catchments in the UK, it is rare to have less than two independent "events" per year; in which case the Poisson and POT choices are more suitable. In large catchments, with seasonally distinctive baseflow, there may only be one independent peak in the year. However, the results from both methods converge with increasing magnitude, yielding insignificant difference beyond a 20-year return period.
#' @param n number of events
#' @param yrs number of years
#' @param RP return period of the events
#' @param dist choice of probability distribution. Either "Poisson" or "Binomial"
#' @examples
#' #Calculate the probability of exceeding at least one 50-yr RP event
#' #over a 10 year period, using the Poisson distribution.
#' EncProb(n = 1, yrs = 10, RP = 50)
#' #Calculate the probability of exceeding at two 100-yr RP events over a
#' #100 year period, using the binomial distribution.
#' EncProb(n = 2, yrs = 100, RP = 100, dist = "Binomial")
#' @return A probability
#' @author Anthony Hammond

EncProb <- function(n, yrs, RP, dist = "Poisson") {
  if(dist != "Poisson" & dist !="Binomial") stop ("dist must be either Poisson or Binomial written with inverted commas")
  if(dist == "Poisson"){
  Enc.Prob <- function(n = 1, yrs, RP) {1-ppois(n-1, yrs*(1/RP))}
  }
  if(dist == "Binomial") {
  Enc.Prob <- function(n = 1, yrs, RP) {1-pbinom(n-1, yrs, (1/RP))}
  }
  Res <- Enc.Prob(n = n, yrs = yrs, RP = RP)
  return(Res)
}


# TrendTest ---------------------------------------------------------------

#' Trend hypothesis test
#'
#' A hypothesis test for the correlation between the variable of interest and time
#'
#'  The test can be performed on a numeric vector, or a data.frame with dates in the first column and the associated variable of interest in the second. A choice can be made between a Pearson's, Spearman's Rho or Kendall's tau test. The Spearman and Kendall are based on ranks and will therefore have the same results whether dates are included or not. The default is kendall.
#' @param x a numeric vector or a data.frame with dates in the first column and chronologically ordered variable in the second.
#' @param method a choice of test method. Choices are "pearson", "spearman", and "kendall"
#' @examples
#' #Get AMAX sample and apply a trend test with the default kendall test.
#' AM.27083 <- GetAM(27083)
#' TrendTest(AM.27083)
#' #Apply the test with the pearson method with dates included and not
#' TrendTest(AM.27083, method = "pearson")
#' TrendTest(AM.27083$Flow, method = "pearson")
#' @return A data.frame with columns and associated values: P_value, correlation coefficient, and method specific statistic.
#' @author Anthony Hammond

TrendTest <- function(x, method = "kendall"){
  if(class(x) == "numeric") {
    Res <- suppressWarnings(cor.test(x, seq(1, length(x)), method = method))} else {DayDiffs <- NULL
    for(i in 1:length(x[,1])) {DayDiffs[i] <- as.numeric(x[,1][i]-x[,1][1])}
    YrDiffs <- DayDiffs/365.25
    Res <- suppressWarnings(cor.test(x[,2], YrDiffs, method = method))}
  P_value <- Res[3]$p.value
  CorCoef <- Res[4]$estimate
  Statistic <- Res[1]$statistic
  return(data.frame(P_value, CorCoef, Statistic, row.names = "Result:"))
}



# NGRDist -----------------------------------------------------------------

#' British national grid reference (NGR) distances
#'
#' Calculates the euclidean distance between two british national grid reference points using the pythagorean method
#' @details Note, that the result is converted to km when six digits are used for easting and northing, when six digits would usually provide a result in metres.
#' @param i a numeric vector of length two. The first being the easting and the second being the northing of the first site
#' @param j a numeric vector of length two. The first being the easting and the second being the northing of the second site
#' @examples
#' #Calculate the distance between the catchment centroid for the
#' #Kingston upon Thames river gauge and the catchment centroid for the
#' #gauge at Ardlethen on the River Ythan. First view the eastings and northings
#' GetCDs(10001)
#' GetCDs(39001)
#' NGRDist(i = c(381355, 839183), j = c(462899, 187850))
#' @return A distance in kilometres (if six digits for easting and northing are used)
#' @author Anthony Hammond

NGRDist <- function(i, j) {sqrt((i[1]-j[1])^2+(i[2]-j[2])^2)/1000}


# BFI -----------------------------------------------------------------

#' Baseflow index (BFI)
#'
#'@description Calculates the baseflow index from a daily mean flow series
#'@details The baseflow index is calculated using the method outlined in Gustard, A. Bullock, A. Dixon, J. M.. (1992). Low flow estimation in the United Kingdom. Wallingford, Institute of Hydrology, 88pp. (IH Report No.108)
#'@param Q the daily mean flow series. Numeric vector
#'@param x.lim the x axis limits of the plot. Numeric vector of length two Default is the extents of the data
#'@param y.lim the y axis limits of the plot. Numeric vector of length two. Default is the extents of the data
#'@param PlotTitle the title of the plot. The default is "Baseflow plot"
#'@examples
#'# Calculate the BFI from daily discharge at Kingston upon Thames;
#'# which is in column three of the ThamesPQ data
#'BFI(ThamesPQ[,3])
#'@return the baseflow index and a plot showing the flow time series (black) and the associated baseflow (red)
#'@author Anthony Hammond
BFI <- function(Q, x.lim = NULL, y.lim = NULL, PlotTitle = "Baseflow plot") {
  LenNA <- length(Q[Q == "NA"])
  if(LenNA > 0) {print("There is missing data. The associated days have been removed")}
  if(LenNA > 0) {Q <- Q[-which(is.na(Q) == TRUE)]}
  SplitLength <- floor(length(Q)/5)
  Q <- Q[1:(SplitLength*5)]
  Mat <- matrix(Q, nrow = 5, ncol = SplitLength)
  MinInd <- NULL
  for(i in 1:SplitLength) {suppressWarnings(MinInd[i] <- which(Mat[,i] == min(Mat[,i], na.rm = TRUE)))}
  Mins <- apply(Mat, 2, min, na.rm = TRUE)
  TPLogic <- NULL #TurningPoints
  for(i in 2:(length(Mins)-1)){TPLogic[i] <-
    if(Mins[i]*0.9 < Mins[i-1] & Mins[i]*0.9 < Mins[i+1]) {TP <- TRUE} else {TP<- FALSE}}
  TPLogic <- append(TPLogic, TRUE)
  TPLogic[1] <- TRUE
  Seq5 <- seq(0, length.out = length(MinInd), by = 5)
  MinIndProp <- MinInd+Seq5
  TPind <- which(TPLogic == TRUE)
  TPMinInd <- MinIndProp[TPind]
  QNA <- rep(NA, length(Q))
  QNA[TPMinInd] <- Q[TPMinInd]
  TurnPts <- which(is.na(QNA) == FALSE)
  IntFunc <- function(j) {
    Dist <- (TurnPts[(j+1)]-TurnPts[j])
    Diff <- QNA[TurnPts[(j+1)]]-QNA[TurnPts[j]]
    if(Diff == 0) {Interp <- rep(QNA[TurnPts[j]], times = Dist+1)} else {
      IntVal <- Diff/Dist
      Interp <- seq(QNA[TurnPts[j]], QNA[TurnPts[(j+1)]], by = IntVal)}
    Interp <- Interp[-length(Interp)]
    return(Interp)
  }
  BFts <- NULL
  BFts <- IntFunc(1)
  for(i in 2:(length(TurnPts)-1)) {BFts <- append(BFts, IntFunc(i))}
  MinSt <- min(which(is.na(QNA) == FALSE))
  MaxSt <- max(which(is.na(QNA) == FALSE))
  DF <- data.frame(Q[MinSt:(MaxSt-1)], BFts)
  BF <- apply(DF, 1, min)
  if(is.null(x.lim) == TRUE) {x.ext <- c(1:length(Q[MinSt:MaxSt]))} else {x.ext <- x.lim}
  if(is.null(y.lim) == TRUE) {y.ext <- c(min(Q[MinSt:MaxSt]), max(Q[MinSt:MaxSt]))} else {y.ext <- y.lim}
  plot(Q[MinSt:MaxSt], type = "l", xlim = x.lim, ylim = y.lim, main = PlotTitle, ylab = "Daily mean flow", xlab = "Days")
  points(BF, type = "l", col = "red")
  return(sum(BF, na.rm = TRUE)/sum(Q[MinSt:(MaxSt-1)], na.rm = TRUE))
}

# Rating -----------------------------------------------------------------

#' Stage-Discharge equation optimisation
#'
#'@description Optimises a power law rating equation from observed discharge and stage
#'@details The power law rating equation optimised here has the form q = c(h+a)^n; where 'q' is flow, 'h' is the stage, c' and 'n' are constants, and 'a' is the stage when flow is zero. The optimisation uses all the data provided in the dataframe (x). If separate rating limbs are necessary, x can be subset per limb. i.e. the rating function would be used multiple times, once for each subset of x. There is the option, with the 'a' argument, to hold the stage correction parameter (a), at a user defined level. If 'a' is NULL it will be calibrated with 'c' & 'n' as part of the optimisation procedure.
#'@param x a data.frame with discharge in the first column and stage in the second
#'@param a a user defined stage correction
#'@examples
#'# Make up Some data:
#'Q <- c(177.685, 240.898, 221.954, 205.55, 383.051, 154.061, 216.582)
#'Stage <- c(1.855, 2.109, 2.037, 1.972, 2.574, 1.748, 2.016)
#'Observations <- data.frame(Q, Stage)
#'#apply the rating function:
#'Rating(Observations)
#'#Hold the stage correction at zero
#'Rating(Observations, a = 0)
#'@return A list with three elements. The first is a vetor of the three calibrated rating parameters. The second is the rating equation; discharge as a function of stage. The third is the rating equation; stage as a function of discharge. A rating plot is also returned.
#'@author Anthony Hammond
Rating <- function(x, a = NULL) {
  colnames(x) <- c("Flow", "Stage")
  if(is.null(a) == TRUE) {
  min.SLS <- function(data, par) {
    with(data, sum(((par[1]*(Stage+par[2])^par[3])-Flow)^2))
  }
  result <- optim(par = c(1,0, 1), fn = min.SLS, data = x)
  Params <- result$par} else {
    min.SLS <- function(data, par) {
      with(data, sum(((par[1]*(Stage+a)^par[2])-Flow)^2))
    }
    result <- optim(par = c(1, 1), fn = min.SLS, data = x)
    Params <- c(result$par[1], a, result$par[2])}
  Mod <- function(x) {Params[1]*(x+Params[2])^Params[3]}
  ModFlip <- function(Q) {((Q/Params[1]))^(1/Params[3])-Params[2]}
  plot(x, main = "Stage-Discharge relationship", ylab = "Stage", xlab = "Discharge")
  curve(ModFlip, from = min(x$Flow), to = max(x$Flow), add = TRUE, col = "red")
  Par1Char <- as.character(round(Params[1], 3))
  Par2Char <- as.character(round(Params[2], 3))
  Par3Char <- as.character(round(Params[3], 3))
  QEquation <- "Q = c(h+a)^n"
  QEquation <- gsub("c", replacement = Par1Char, x = QEquation)
  QEquation <- gsub("a", replacement = Par2Char, x = QEquation)
  QEquation <- gsub("n", replacement = Par3Char, x = QEquation)
  hEquation <- "h = ((Q/c)^(1/n))-a"
  hEquation <- gsub("c", replacement = Par1Char, x = hEquation)
  hEquation <- gsub("a", replacement = Par2Char, x = hEquation)
  hEquation <- gsub("n", replacement = Par3Char, x = hEquation)
  Equations <- list(Params, QEquation, hEquation)
  names(Equations) <- c("Parameters", "Discharge as a function of stage", "Stage as a function of discharge")
  return(Equations)
}

