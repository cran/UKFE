

# QMED_FEH08 --------------------------------------------------------------------

#' QMED (median annual maximum flow) estimate from catchment descriptors using the FEH 2008 method.
#'
#' Estimated median annual maximum flow from catchment descriptors.
#'
#' QMED is estimated from catchment descriptors: QMED = 8.3062 * AREA^0.851 * 0.1536^(1000 / SAAR) * FARL^3.4451 * 0.046^(BFIHOST^2) as specified by FEH2008. If the CDs argument is used then the SAAR used is SAAR6190, FARL is FARL (as opposed to FARL2015) and BFIHOST is BFIHOST (as opposed to BFIHOST19 or BFIHOST19scaled). Note that this function is a legacy function and you cannot do donor adjustment within the function. You can however use this in conjunction with the QMEDDonEq function.
#'
#' @param CDs catchment descriptors derived from either GetCDs or CDsXML
#' @param UrbAdj logical argument with a default of TRUE. If TRUE, an urban adjustment is made to the estimate.
#' @param AREA catchment area in km2
#' @param SAAR standard average annual rainfall (mm)
#' @param FARL flood attenuation from reservoirs and lakes
#' @param BFIHOST baseflow index calculated from the catchment hydrology of soil type classification
#' @param URBEXT measure of catchment urbanisation
#' @examples
#' # Get some catchment descriptors and calculate QMED as if it was ungauged
#'
#' cds_55004 <- GetCDs(55004)
#' QMED_FEH08(cds_55004)
#'
#' @return An estimate of QMED from catchment descriptors.
#' @author Anthony Hammond

QMED_FEH08 <- function(CDs = NULL, UrbAdj = TRUE, AREA, SAAR, FARL, BFIHOST, URBEXT) {
  if(is.null(CDs) == FALSE) {

    CDsCheck <- CDs[grep("SAAR", CDs$Descriptor),2]
    if(length(CDsCheck) == 2) {
      AREA <- CDs[grep("AREA", CDs$Descriptor),2]
      SAAR <- CDs[grep("SAAR", CDs$Descriptor)[1],2]
      FARL <- CDs[grep("FARL", CDs$Descriptor)[1],2]
      BFIHOST <- CDs[grep("BFIHOST", CDs$Descriptor)[1],2]
      URBEXT <- CDs[grep("URBEXT2000", CDs$Descriptor),2]
    }
    if(length(CDsCheck) == 3) {
    AREA <- CDs[grep("AREA", CDs$Descriptor),2]
    SAAR <- CDs[grep("SAAR6190", CDs$Descriptor),2]
    FARL <- CDs[grep("FARL", CDs$Descriptor)[2],2]
    BFIHOST <- CDs[grep("BFIHOST19", CDs$Descriptor)[1],2]
    URBEXT <- CDs[grep("URBEXT2000", CDs$Descriptor),2]
    }
  }

  QMEDEstimate <- 8.3062 * AREA^0.851 * 0.1536^(1000 / SAAR) * FARL^3.4451 * 0.046^(BFIHOST^2)
  if(UrbAdj == TRUE) {
        PRUAF <- 1 + 0.3 * 1.567 * URBEXT * (70 / (69.366 - 65.686 * BFIHOST) - 1)
        UAF <- (1 + 0.3 * (1.567 * URBEXT))^1.25 * PRUAF^1.33
  QMEDEstimate <- UAF * QMEDEstimate
  }
  return(QMEDEstimate)
}


# Pool_FEH08 --------------------------------------------------------------------

#' Create pooling group using the FEH08 method
#'
#' Legacy function to develop a pooling group based on catchment descriptors
#'
#' A pooling group is created from a CDs object, derived from GetCDs or CDsXML (CDsXML_Legacy). To change the default pooling group, one or more sites can be excluded using the 'exclude' option, which requires either a site reference or multiple site references in a vector. If this is done, the site with the next lowest similarity distance measure is added to the group (until the total number of years is at least N). Sites with URBEXT2000 (urban extent) > 0.03 are excluded from the pooling group by default. This threshold can be adjusted with UrbMax. If DeUrb is set as TRUE (the default), the LCV and LSKEW values for sites in the pooling group are de-urbanised.
#'
#' The pooling method is as specified by FEH2008. Accordingly the SAAR applied is SAAR6190, FARL is FARL (as opposed to FARL2015) and BFIHOST is BFIHOST (as opposed to BFIHOST19 or BFIHOST19scaled)
#' @param CDs catchment descriptors derived from either GetCDs or CDsXML
#' @param N minimum Number of total gauged record years for the pooling group
#' @param exclude sites to exclude from the pooling group. Either a single site reference or a vector of site references (numeric). If this is used the next site with the lowest SDM is included such that the total sample of AMAX is at least N.
#' @param include sites to include that otherwise would not be included by default. For example if it is a subject site that has URBEXT2015 above UrbMax. Or one that has not been selected automatically using the similarity distance measure.
#' @param UrbMax Maximum URBEXT2000 level with a default of 0.03. Any catchment with URBEXT2000 above this level will be excluded from the pooling group
#' @param DeUrb logical argument with a default of TRUE. If TRUE, the LCVs of all sites in the pooling group are "De-Urbanised" according to the FEH08 method.
#' @examples
#' # Get some catchment descriptors
#' cds_73005 <- GetCDs(73005)
#'
#' # Set up a pooling group object called pool_73005 excluding sites 79005 & 46003
#' # Then print the group to the console
#' pool_73005 <- Pool_FEH08(cds_73005, exclude = c(79005, 46003))
#' pool_73005
#'
#'
#' @return A data.frame of the pooling group with site reference row names and 14 columns, each providing catchment & gauge details for the sites in the pooling group.
#' @author Anthony Hammond

Pool_FEH08 <- function(CDs, N = 500, UrbMax = 0.03, DeUrb = TRUE, exclude = NULL, include = NULL) {
  if(class(CDs) != class(data.frame(c(1,2,3)))) stop("CDs must be a CDs dataframe object which can be derived using the GetCDs or CDsXML function")
  #CDsTest <- GetCDs(rownames(PeakFlowData)[1])
  #if(!identical(CDs[,1], CDsTest[,1])) stop("CDs must be a CDs dataframe object which can be derived using the GetCDs or CDsXML function")
  Suitability <- NULL
  CDsCheck <- CDs[grep("SAAR", CDs$Descriptor),2]

  SDM <- function(CDs, AREA, SAAR, FARL, FPEXT, BFIHOST) {
    CDs[,2] <- signif(CDs[,2], 4)

    if(length(CDsCheck) == 2) {
    AREAi <- CDs[grep("AREA",CDs[,1]),2]
    SAARi <- (CDs[grep("SAAR",CDs[,1])[1],2])
    FARLi <- (CDs[grep("FARL",CDs[,1])[1],2])
    FPEXTi <- CDs[grep("FPEXT",CDs[,1])[1],2]
    BFIHOSTi <- (CDs[grep("BFIHOST19",CDs[,1]),2]  )
    }

    if(length(CDsCheck) == 3) {
      AREAi <- CDs[grep("AREA",CDs[,1]),2]
      SAARi <- (CDs[grep("SAAR6190",CDs[,1]),2])
      FARLi <- (CDs[grep("FARL",CDs[,1])[2],2])
      FPEXTi <- CDs[grep("FPEXT",CDs[,1])[1],2]
      BFIHOSTi <- (CDs[grep("BFIHOST19",CDs[,1]),2]  )
    }

    AREAj <- (AREA)
    SAARj <- SAAR
    FARLj <- FARL
    FPEXTj <- FPEXT
    BFIHOSTj <- BFIHOST
    sqrt(
      (3.2 * ((log(AREAi) - log(AREAj)) / 1.28)^2)
      + (0.5 * ((log(SAARi) - log(SAARj)) / 0.37)^2)
      + (0.1 * ((FARLi - FARLj) / 0.05)^2)
      + (0.2 * ((FPEXTi - FPEXTj) / 0.04)^2)
    )
  }



  SDMs <- NULL
  for(i in 1:nrow(PeakFlowData)) {SDMs[i] <- SDM(CDs = CDs, PeakFlowData$AREA[i],
                                                 PeakFlowData$SAAR6190[i],
                                                 PeakFlowData$FARL[i],
                                                 PeakFlowData$FPEXT[i],
                                                 PeakFlowData$BFIHOST[i])}
  PeakFlowDataSDM <- data.frame(PeakFlowData, SDM = signif(SDMs, 4))
  PoolDataSDM <- subset(PeakFlowDataSDM, Suitability == "Pooling")
  #PoolDataSDM <- PoolDataSDM[PoolDataSDM$Suitability == "Pooling", ]
  URBEXT2000 <- CDs[grep("URBEXT",CDs[,1])[2],2]
  PoolDataSDM <- subset(PoolDataSDM, URBEXT2000 <= UrbMax)
  #PoolDataSDM <- PoolDataSDM[PoolDataSDM$URBEXT2015 <= UrbMax,]
  if(is.null(exclude) == FALSE) {
    Indices <- match(exclude, rownames(PoolDataSDM))
    if(any(is.na(Indices))) {Indices <- Indices[!is.na(Indices)]}
    if(length(Indices) == 0) {
      warning("The exclude index did not match any gauges that are suitable for pooling and have URBEXT2015 below UrbMax")
      PoolDataSDM <- PoolDataSDM}
    else {PoolDataSDM <- PoolDataSDM[-Indices,]}
  }

  PoolDataSDM <- PoolDataSDM[order(PoolDataSDM$SDM),]

  if(N > sum(PoolDataSDM$N)) {
    warning("The chosen N is greater than the sum of AMAX sample sizes")
    Result <- PoolDataSDM
  }
  if(N <= sum(PoolDataSDM$N)) {
    NCumSum <- cumsum(PoolDataSDM$N)
    MinN <- min(which(NCumSum >= N))
    Result <- PoolDataSDM[1:MinN,]
  }


  if(is.null(include) == FALSE) {
    if(length(include) != 1) stop("The 'include' argument must have a length of 1")
    IncludeIndex <- match(include, rownames(PeakFlowData))
    if(is.na(IncludeIndex)) stop("The site ID used in the include argument is not in the PeakFlowDataSet")
    RowAdd <- PeakFlowData[IncludeIndex,]
    SDMAdd <- SDM(CDs = CDs, PeakFlowData$AREA[IncludeIndex],
                  PeakFlowData$SAAR6190[IncludeIndex],
                  PeakFlowData$FARL[IncludeIndex],
                  PeakFlowData$FPEXT[IncludeIndex],
                  PeakFlowData$BFIHOST[IncludeIndex])
    RowAdd <- data.frame(RowAdd, SDM = signif(SDMAdd, 4))
    Result <- rbind(Result, RowAdd)
    if(length(unique(rownames(Result)))  != length(rownames(Result))) {
      Result <- Result[-nrow(Result),]
    }
    Result <- Result[order(Result$SDM),]
  }


  Ds <-  function(x)
  {
    u.hat <- apply(tf, 2, mean)
    Res <- numeric(1)
    for (i in 1:length(Result$N)) {Res <- Res+as.numeric(tf[i,]-u.hat)%*%t(as.numeric((tf[i,]-u.hat)))}
    D <- NULL
    for (i in 1:length(Result$N)) {D[i] <- ((1/3)*length(Result$N))*as.numeric(tf[i,]-u.hat)%*%solve(Res)%*%(as.numeric((tf[i,]-u.hat)))}
    return(D)
  }
  tf <- data.frame(Result$Lcv, Result$LSkew, Result$LKurt)
  Discordancy <- Ds(tf)
  crit.vs <- c(1.333, 1.648, 1.917, 2.140, 2.329, 2.491, 2.632, 2.757, 2.869, 2.971, 3)
  xd <- seq(5,15)
  Crit.frame <- data.frame(xd, crit.vs)
  Nsize <- nrow(Result)
  CritInd <- which.min(abs(Nsize - Crit.frame$xd))
  C.V <- Crit.frame$crit.vs[CritInd]
  Discordant <- NULL
  for (i in 1:length(Discordancy)) {Discordant[i] <- isTRUE(Discordancy[i] > C.V)}
  Result <- data.frame(Result, Discordancy = signif(Discordancy,3), Discordant)

  ColNamesKeep <- c("AREA", "SAAR6190", "FARL", "FPEXT", "BFIHOST", "URBEXT2000", "Lcv", "LSkew", "LKurt", "QMED", "N", "SDM", "Discordancy", "Discordant")
  if(DeUrb == TRUE) {
    LCVUrb <- function(lcv, URBEXT2000, DeUrb = FALSE) {
      if (DeUrb == FALSE) {
        lcv * 0.68654^(1.567 * URBEXT2000)
      } else {
        lcv / (0.68654^(1.567 * URBEXT2000))
      }
    }

    LSkewUrb <- function(lskew, URBEXT2000, DeUrb = FALSE) {
      if (DeUrb == FALSE) {
        ((lskew + 1) * 1.096017^(1.567 * URBEXT2000)) - 1
      } else {
        ((lskew + 1) / 1.096017^(1.567 * URBEXT2000)) - 1
      }
    }


    LCVs <- NULL
    for(i in 1:nrow(Result)) {LCVs[i] <- LCVUrb(Result$Lcv[i], URBEXT = Result$URBEXT2000[i], DeUrb = TRUE)}
    Result$Lcv <- LCVs
    LSKEWs <- NULL
    for(i in 1:nrow(Result)) {LSKEWs[i] <- LSkewUrb(Result$Lcv[i], URBEXT = Result$URBEXT2000[i], DeUrb = TRUE)}
    Result$LSkew <- LSKEWs

  }
  MatchCol <- match(ColNamesKeep, colnames(Result))
  Result <- Result[,MatchCol]
  return(Result)
}




# PoolEst_FEH08 -----------------------------------------------------------------

#' Pooled flood estimates using the FEH2008 method
#'
#' Provides pooled results from a pooling group - gauged, ungauged and with urban adjustment if necessary.
#'
#' PoolEst_FEH08 is a function to provide results from a pooling group derived using the FEH08Pool function. QMED (median annual maximum flow) needs to be supplied and can be derived from the FEH08QMED function for ungauged estimates or the annual maximum sample for gauged estimates. The UrbAdj argument can be set to TRUE to provide urbanised results. If this is done, either URBEXT (urban extent) needs to be provided or the catchment descriptors, derived from CDsXML or GetCDs. The methods for estimating pooled growth curves are according to Science Report: SC050050 - Improving the FEH statistical procedures for flood frequency estimation. The methods for estimating the L-moments and growth factors are outlined in the Flood Estimation Handbook (1999), volume 3. The methods for quantifying uncertainty are detailed in Hammond, A. (2022). Easy methods for quantifying the uncertainty of FEH pooling analysis. Circulation - The Newsletter of the British Hydrological Society (152). When UrbAdj = TRUE, urban adjustment is applied to the QMED estimate according to the method outlined in the guidance by Wallingford HydroSolutions: 'WINFAP 4 Urban Adjustment Procedures'. Note that if Gauged = TRUE, the functionality assumes that the top site in the pooling group (i.e. the first row) is the subject "gauged" catchment. It is important to check that this is the case because if the site is urban it may not be included by default. The estimation procedure assumes that the pooled AMAX samples are from the same underlying distribution (aside from the QMED scaling factor), that the distribution is correctly specified, that the individual samples are all independent and identically distributed, and that the samples are independent of each other.  The urban adjustment assumes that the growth curve associated with an annual maximum flow sample is impacted by urbanisation and that this impact can be modelled as a function of the catchment URBEXT2000.
#'
#' @param x pooling group derived from the Pool function
#' @param gauged logical argument with a default of FALSE. TRUE for gauged results and FALSE for ungauged
#' @param QMED estimate of the median annual maximum flow
#' @param dist a choice of distribution for the estimates. The choices are "GenLog", "GEV", "Kappa3", or "Gumbel"; the generalised logistic, generalised extreme value, Kappa3, and Gumbel distribution, respectively. The default is "GenLog"
#' @param RP return period of interest. By default the following RPs are provided: 2, 5, 10, 20, 50, 75, 100, 200, 500, 1000
#' @param UrbAdj logical argument with a default of FALSE. When TRUE, an urban adjustment (FEH08 method) is applied to the pooled Lcv and LSkew
#' @param CDs catchment descriptors derived from either GetCDs or CDsXML
#' @param URBEXT the catchment URBEXT2000, to be supplied if UrbAdj is TRUE and if the CDs argument is not used.
#' @examples
#' # Get some catchment descriptors and form a pooling group. It's urban and
#' # therefore the site of interest is not included
#' cds_27083 <- GetCDs(27083)
#' pool_27083 <- Pool_FEH08(cds_27083)
#'
#' # Get results for the ungauged case, with urban adjustment
#' PoolEst_FEH08(pool_27083, QMED = 12, UrbAdj = TRUE, CDs = cds_27083)
#'
#' # Form the group again with the urban gauge included & undertake a gauged estimate
#' # with urban adjustment. QMED in this example is estimated as the median of the annual
#' # maximum series for site 27083.
#' pool_g_27083 <- Pool_FEH08(cds_27083, include = 27083, DeUrb = TRUE)
#' PoolEst_FEH08(pool_g_27083, QMED = 12.5, UrbAdj = TRUE, CDs = cds_27083)
#'
#' @return If RP is default then a list of length 4. Element one is a data frame with columns; return period (a range from 2 - 1000), peak flow estimates (Q), growth factor estimates (GF), lower and upper intervals of uncertainty (68 percent intervals for ungauged and 95 percent for gauged). The second element is the estimated Lcv and Lskew. The third provides distribution parameters for the growth curve. The fourth provides distribution parameters for the frequency curve. If RP is not the default only the first two elements are returned.
#' @author Anthony Hammond



PoolEst_FEH08 <- function(x, gauged = FALSE, QMED, dist = "GenLog", RP = c(2, 5, 10, 20, 50, 75, 100, 200, 500, 1000), UrbAdj = FALSE, CDs = NULL, URBEXT = NULL) {
  if (dist != "GenLog" & dist != "GEV" & dist != "Gumbel" & dist != "Kappa3") stop("dist must equal one of the following, GEV, GenLog, Gumbel, Kappa3. Other growth curve functions can be applied separately to the resulting LCV and LSKEW")
  if (is.data.frame(x) == FALSE) {
    stop("x must be a pooled group. Pooled groups can be created with the Pool() function")
  }

  WungLcv <- function(x) {
    if (is.data.frame(x) == FALSE) {
      stop("x must be a pooled group. Pooled groups can be created with the Pool() function")
    }

    Ck.LCV <- function(n) {
      0.02609 / (n - 1)
    }
    bj.Lcv <- function(SDM) {
      0.0047 * sqrt(SDM) + (0.0023 / 2)
    }
    Weight <- cbind(x$N, x$SDM, x$Lcv, bj.Lcv(x$SDM), Ck.LCV(x$N), (bj.Lcv(x$SDM) + Ck.LCV(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "Lcv", "bLCV", "cLCV", "bc.LCV")
    bc.vector <- Weight[, 6]
    s.bc <- sum(bc.vector)
    Wjs.LCV <- bc.vector / s.bc
    Sum.prod <- sum(Wjs.LCV * Weight[, 3])
    return(Sum.prod)
  }

  WungLSkew <- function(x) {
    if (is.data.frame(x) == FALSE) {
      stop("x must be a pooled group. Pooled groups can be created with the Pool() function")
    }
    #if (ncol(x) != 24) stop("x must be a pooled group. Pooled groups can be created with the Pool() function")
    Ck.LSkew <- function(n) {
      0.2743 / (n - 2)
    }
    bj.LSkew <- function(SDM) {
      0.0219 * (1 - exp(-(SDM / 0.2360)))
    }
    Weight <- cbind(x$N, x$SDM, x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
    colnames(Weight) <- c("N", "SDM", "LSkew", "bLSkew", "cLSkew", "bc.LSkew")
    bc.vector <- Weight[, 6]
    s.bc <- sum(bc.vector)
    Wjs.LSkew <- bc.vector / s.bc
    Sum.prod <- sum(Wjs.LSkew * Weight[, 3])
    return(Sum.prod)
  }

  WGaugLcv <- function(x) {
    if (is.data.frame(x) == FALSE) {
      stop("x must be a pooled group. Pooled groups can be created with the Pool() function")
    }

    WLCV1 <- function(x) {
      Ck.LCV <- function(n) {
        0.02609 / (n - 1)
      }
      bj.LCV <- function(SDM) {
        0.0047 * sqrt(SDM) + (0.0023 / 2)
      }
      Weight <- cbind(x$N, x$SDM, x$Lcv, bj.LCV(x$SDM), Ck.LCV(x$N), (bj.LCV(x$SDM) + Ck.LCV(x$N))^-1)
      colnames(Weight) <- c("N", "SDM", "Lcv", "bLCV", "cLCV", "bc.LCV")
      bc.vector <- Weight[, 6]
      s.bc <- sum(bc.vector)
      Weight.1b <- Weight[1, 4]
      Weight.1c <- Weight[1, 5]
      Wjs.LCV <- (Weight.1b / (Weight.1c + Weight.1b)) + ((Weight.1c * (Weight.1c + Weight.1b)^-2) / s.bc)
      return(Wjs.LCV)
    }
    WLCVj <- function(x) {
      Ck.LCV <- function(n) {
        0.02609 / (n - 1)
      }
      bj.LCV <- function(SDM) {
        0.0047 * sqrt(SDM) + (0.0023 / 2)
      }
      Weight <- cbind(x$N, x$SDM, x$Lcv, bj.LCV(x$SDM), Ck.LCV(x$N), (bj.LCV(x$SDM) + Ck.LCV(x$N))^-1)
      colnames(Weight) <- c("N", "SDM", "Lcv", "bLCV", "cLCV", "bc.LCV")
      bc.vector <- Weight[, 6]
      s.bc <- sum(bc.vector)
      bc.vector <- bc.vector[-1]
      Weight.1b <- Weight[1, 4]
      Weight.1c <- Weight[1, 5]
      Wjs.LCV <- (Weight.1c * (Weight.1c + Weight.1b)^-1 * bc.vector) / s.bc
      return(Wjs.LCV)
    }
    W1 <- WLCV1(x)
    Wjs <- WLCVj(x)
    W <- append(W1, Wjs)
    Sum.prod <- sum(x$Lcv * W)
    return(Sum.prod)
  }

  WGaugLSkew <- function(x) {
    if (is.data.frame(x) == FALSE) {
      stop("x must be a pooled group. Pooled groups can be created with the Pool() function")
    }

    WLSKEW1 <- function(x) {
      Ck.LSkew <- function(n) {
        0.2743 / (n - 2)
      }
      bj.LSkew <- function(SDM) {
        0.0219 * (1 - exp(-(SDM / 0.2360)))
      }
      Weight <- cbind(x$N, x$SDM, x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
      colnames(Weight) <- c("N", "SDM", "LSkew", "bLSkew", "cLSkew", "bc.LSkew")
      bc.vector <- Weight[, 6]
      s.bc <- sum(bc.vector)
      Weight.1b <- Weight[1, 4]
      Weight.1c <- Weight[1, 5]
      Wjs.LSkew <- (Weight.1b / (Weight.1c + Weight.1b)) + ((Weight.1c * (Weight.1c + Weight.1b)^-2) / s.bc)
      return(Wjs.LSkew)
    }
    WLSKEWj <- function(x) {
      Ck.LSkew <- function(n) {
        0.2743 / (n - 2)
      }
      bj.LSkew <- function(SDM) {
        0.0219 * (1 - exp(-(SDM / 0.2360)))
      }
      Weight <- cbind(x$N, x$SDM, x$LSkew, bj.LSkew(x$SDM), Ck.LSkew(x$N), (bj.LSkew(x$SDM) + Ck.LSkew(x$N))^-1)
      colnames(Weight) <- c("N", "SDM", "LSkew", "bLSkew", "cLSkew", "bc.LSkew")
      bc.vector <- Weight[, 6]
      s.bc <- sum(bc.vector)
      bc.vector <- bc.vector[-1]
      Weight.1b <- Weight[1, 4]
      Weight.1c <- Weight[1, 5]
      Wjs.LSkew <- (Weight.1c * (Weight.1c + Weight.1b)^-1 * bc.vector) / s.bc
      return(Wjs.LSkew)
    }
    W1 <- WLSKEW1(x)
    Wjs <- WLSKEWj(x)
    W <- append(W1, Wjs)
    Sum.prod <- sum(x$LSkew * W)
    return(Sum.prod)
  }


  if (UrbAdj == TRUE) {
    if (is.null(URBEXT) == TRUE & is.null(CDs) == TRUE) stop("if Urbadj = TRUE, URBEXT or CDs must be provided")
    if (is.null(URBEXT) == TRUE) {
      URBEXT2000 <- CDs[grep("URBEXT2000", CDs$Descriptor), 2]
    } else {
      URBEXT2000 <- URBEXT
    }
  }
  if (dist == "GenLog") {
    func <- GenLogGF
  }
  if (dist == "GEV") {
    func <- GEVGF
  }
  if (dist == "Kappa3") {
    func <- Kappa3GF
  }
  if (dist == "Gumbel") {
    func <- GumbelGF
  }
  if (gauged == FALSE) {
    lcv <- WungLcv(x)
  } else {
    lcv <- WGaugLcv(x)
  }
  if (gauged == FALSE) {
    lskew <- WungLSkew(x)
  } else {
    lskew <- WGaugLSkew(x)
  }
  if (UrbAdj == TRUE) {
    lcv <- lcv * 0.68654^(1.567 * URBEXT2000)
  } else {
    lcv <- lcv
  }
  if (UrbAdj == TRUE) {
    lskew <- ((lskew + 1) * 1.096017^(1.567 * URBEXT2000)) - 1
  } else {
    lskew <- lskew
  }
  if (dist == "Gumbel") {
    Zt <- func(lcv, RP = RP)
  } else {
    Zt <- func(lcv, lskew, RP = RP)
  }
  GF <- as.numeric(format(round(Zt, 3), nsmall = 3))
  Qt <- Zt * QMED
  Q <- as.numeric(format(round(Qt, 3), nsmall = 3))
  PooledLcv <- lcv
  PooledLSkew <- lskew
  res <- data.frame(RP, Q, GF)

  Pars <- cbind(PooledLcv, PooledLSkew)
  if (suppressWarnings(sd(RP - c(2, 5, 10, 20, 50, 75, 100, 200, 500, 1000))) == 0) {
    ResLocScaSha <- OptimPars(res[, c(1, 3)], dist = dist)
    ResLocScaSha <- signif(ResLocScaSha[1, ], 3)
    ResLocScaShaDist <- OptimPars(res[, c(1, 2)], dist = dist)
    ResLocScaShaDist <- signif(ResLocScaShaDist[1, ], 3)
    return(list(res, Pars, ResLocScaSha, ResLocScaShaDist))
  } else {
    return(list(res, Pars))
  }
}

