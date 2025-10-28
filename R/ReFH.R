# ReFH --------------------------------------------------------------------
#' Revitalised Flood Hydrograph Model (ReFH)
#'
#' @description
#' Provides outputs for the ReFH model from catchment descriptors or user defined inputs.
#'
#' @details
#' As default this function is the ReFH model as described in the Flood Estimation Handbook Supplementary Report No.1 (2007). However, optional extras have been added such as an urban loss model and an option to ensure a water balance if it has been violated. The urban loss model is applied with default urban parameters (IF = 0.7, DS = 0.5, IRF = 0.4) and is described in the Wallingford Hydrosolutions report "ReFH2 Science Report Closing a Water Balance" (2019). The method to derive design rainfall profiles is described in the Flood Estimation Handbook (1999), volume 2. This has been slightly adjusted so that even profiles are possible. Users can also input their own rainfall with the 'Rain' argument. As a default, when catchment descriptors (CDs) are provided the ReFH function uses catchment descriptors to estimate the parameters of the ReFH model and an approximate two-year rainfall for the critical duration. If a parameter argument is used for one or more of the parameters, then these overwrite the CD derived parameters. This ReFH function is recommended for analysing the plausible catchment response to an input of rainfall. For this reason, and as noted, multiple additional features are available. The user can change components such as the unit hydrograph and the loss. The WaterBalance option can be applied to ensure it is not violated. Also, the baseflow component can be set as a constant (as opposed to a function of the runoff) by setting BR to zero. The rainfall can also be changed by choosing a range of different randomised profiles.
#'
#' @param CDs catchment descriptors derived from either GetCDs or ImportCD
#' @param Depth a numeric value. The depth of rainfall used as input in the estimation of a design hydrograph. The default, when Depth = NULL, is a two year rainfall.
#' @param Duration a numeric value. A duration (hrs) for the design rainfall
#' @param Timestep a numeric value. A user defined data interval. The default changes depending on the estimated time to peak to formulate a sensible looking result. This will need updating if the Rain argument is used.
#' @param RainProfile This is a choice of the temporal rainfall pattern. The default is the FSR profile. However, you can also choose a randomly generated profile with a different loading. The choices are: "Centre", "Back", "Front", "Uniform", or "Random". The latter randomly chooses between the former four.
#' @param PlotTitle a character string. A user defined title for the ReFH plot
#' @param RPa return period for alpha adjustment. This is only for the purposes of the alpha adjustment, it doesn't change the rainfall input
#' @param alpha a logical argument with default TRUE. If TRUE the alpha adjustment is applied based on RPa. If FALSE, no alpha adjustment is made
#' @param WaterBalance A logical argument with a default of FALSE. If it is TRUE, the water balance is checked, if it is not voilated the BR parameter is as per the default estimate. Otherwise BR is set as a function of the proportion of net-rain to rain (NetProp) as BR = (1/NetProp)-1.
#' @param Season a choice of "summer" or "winter". The default is "summer" in urban catchments (URBEXT2015 > 0.03) and "winter" in rural catchments
#' @param AREA numeric. Catchment area in km2.
#' @param TP numeric. Time to peak parameter (hours)
#' @param BR numeric. Baseflow recharge parameter. If BR is set to zero, a constant baseflow of BFini is the result.
#' @param BL numeric. Baseflow lag parameter (hours)
#' @param Cmax numeric. Maximum soil moisture capacity parameter (mm)
#' @param Cini numeric. Initial soil moisture content (mm)
#' @param BFini numeric. Initial baseflow (m3/s)
#' @param Rain numeric. User input rainfall. A numeric vector. If this is used, the Timestep argument needs to be applied.
#' @param UHShape User choice of unit hydrograph shape. The default is "KT" (Kinked triangle). The other options are FSR and Gamma.
#' @param UrbanLoss Logical with a default of FALSE. If this is TRUE, the urban loss model is applied.
#' @param Loss A value between 0 and 1. This overrides the default loss model which uses Cini and Cmax and instead NetRain is calculated as Rain * (1-Loss).
#' @param LossCini A value between 0 and 1. This Adjusts the Cini value according to a user input loss. i.e. if the user wants 70 percent loss and inputs 0.7, the Cini will be updated to ensure this is the overall loss. i.e. Cini will be calculated as Cini = ((1-LossCini)-(Depth/(2Cmax))) * Cmax.
#' @examples
#' # Get CDs and apply the ReFH function
#' cds_203018 <- GetCDs(203018)
#' ReFH(cds_203018)
#'
#' # Apply the ReFH function with a user defined initial baseflow
#' ReFH(cds_203018, BFini = 6)
#'
#' @return A list with two elements, and a plot. First element of the list is a data.frame of parameters, initial conditions and the catchment area. The second is a data.frame with columns Rain, NetRain, Runoff, Baseflow, and TotalFlow. If the scale argument is used a numeric vector containing the scaled hydrograph is returned instead of the results dataframe. The plot is of the ReFH output, with rainfall, net-rainfall, baseflow, runoff and total flow. If the scaled argument is used, a scaled hydrograph is plotted.
#' @author Anthony Hammond


ReFH <- function(CDs = NULL, Depth = NULL, Duration = NULL, Timestep = NULL, RainProfile = "FSR", PlotTitle = NULL, RPa = NULL, alpha = FALSE, WaterBalance = FALSE, Season = NULL, AREA = NULL, TP = NULL, BR = NULL, BL = NULL, Cmax = NULL, Cini = NULL, BFini = NULL, Rain = NULL, UHShape = "KT", UrbanLoss = FALSE, Loss = NULL, LossCini = NULL) {
  if (alpha == FALSE & is.null(RPa) == FALSE) {
    print("Warning: You've chosen an RPa value and have alpha = FALSE. The RPa argument, in this case, does nothing")
  }
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if(class(CDs) != class(data.frame(c(1,2,3)))) stop("CDs must be a CDs dataframe object which can be derived using the GetCDs or CDsXML function")
  CDsTest <- GetCDs(rownames(PeakFlowData)[1])
  if(!identical(colnames(CDs), colnames(CDsTest))) stop("CDs must be a CDs dataframe object which can be derived using the GetCDs or CDsXML function")


  RainEventSim <- function(Duration, Timestep = 1, Depth, FSE = NULL, Rainprofile = "Random", season = "winter") {

    if(Rainprofile != "FSR") {
      Types <- c("Random", "Centre", "Back", "Front", "Uniform")
      if(Rainprofile != "Random" & Rainprofile != "Centre" & Rainprofile != "Back" & Rainprofile != "Front") stop("RainProfile must be one of, FSR, Random, Centre, Back, Front, or Uniform")
      Duration <- Duration / Timestep
      RainSeq <- seq(1, Duration)
      Props <- 1/(round(Duration/2))
      CumProps <- cumsum(rep(Props, round(Duration/2)))
      Props <- c(sort(CumProps, decreasing = TRUE), CumProps)
      if(length(Props) < Duration) {Props <- c(sort(CumProps, decreasing = TRUE), CumProps[1]/2, CumProps)}
      if(length(Props) > Duration) {Props <- c(sort(CumProps, decreasing = TRUE), CumProps[-1])}
      Props <- Props/sum(Props)
      if(Rainprofile == "Random") {Rainprofile <- sample(c(rep("Centre", 10), rep("Front", 10), rep("Back", 10), rep("Uniform", 10)), 1)}
      if(Rainprofile  == "Centre") {Props <- Props}
      if(Rainprofile == "Back") {Props <- sort(Props)}
      if(Rainprofile == "Front") {Props <- sort(Props, decreasing = TRUE)}
      P <- NULL
      for(i in 1:length(Props)) {P[i] <- rexp(1,Props[i])}
      #This next line tempers the difference between the highest pulses and the rest. Useful especially for longer durations when we can get absurd individual pulses in the middle.
      if(max(P) > (1.5 * sort(P, decreasing = TRUE)[2])) {P[which.max(P)] <- sort(P, decreasing = TRUE)[2] * 1.5}
      if(Rainprofile == "Uniform") {P <- qexp(c(runif(Duration, 0.35, 0.65)))}
      if(is.null(FSE)) {Depth <- Depth}
      if(is.null(FSE) == FALSE) {
        Depth <- exp(rnorm(1, log(Depth), log(FSE)))}

      P <- (P/sum(P)) * Depth
      P[P < 0.1] <- 0
      if(P[1] == 0) {P[1] <- 0.1}
      if(P[length(P)] == 0) {P[length(P)] <- 0.1}
      P <- (P/sum(P)) * Depth
      return(P)}

    if(Rainprofile == "FSR") {
      PProfile <- function(d, season = "winter", timestep = 1){
        dhrs <- d
        d <- d * (1 / timestep)
        d_rounded <- round(d)
        d_odd <- ifelse(d_rounded %% 2 == 0,
                        d_rounded + ifelse(d_rounded < d, 1, -1),
                        d_rounded)
        vec <- seq(1, d_odd, 2) / d_odd
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
        LCheck <- length(RainVec)*timestep
        if(LCheck != dhrs) {
          MaxInd <- which.max(RainVec)
          Rain.5<- RainVec[1:MaxInd]
          Rain.1 <- sort(Rain.5, decreasing = TRUE)
          RainVecTemp <- c(Rain.5, Rain.1)
          CloserInd <- which.min(abs(c(length(RainVecTemp), length(RainVec)) - dhrs))
          if(CloserInd == 2) {RainVec <- RainVec/sum(RainVec)}
          if(CloserInd == 1) {RainVec <- RainVecTemp/sum(RainVecTemp)}
        }
        return(RainVec)
      }
      P <- PProfile(d = Duration, season = season, timestep = Timestep)
      P <- P*Depth
      P <- (P / sum(P)) * Depth
      return(P)
    }

  }


  if(is.null(CDs) == FALSE) {
    PROPWET <- CDs[which(CDs$Descriptor == "PROPWET"), 2]
    DPLBAR <- CDs[which(CDs$Descriptor == "DPLBAR"), 2]
    URBEXT <-  CDs[grep("URBEXT", CDs$Descriptor)[1], 2]
    DPSBAR <- CDs[which(CDs$Descriptor == "DPSBAR"), 2]
    SAAR <- CDs[grep("SAAR", CDs$Descriptor)[1], 2]
    BFIHOST <- CDs[grep("BFIHOST", CDs$Descriptor)[1], 2]

  }



  Params <- function(x) {
    PROPWET <- x[which(x$Descriptor == "PROPWET"), 2]
    DPLBAR <- x[which(x$Descriptor == "DPLBAR"), 2]
    DPSBAR <- x[which(x$Descriptor == "DPSBAR"), 2]
    URBEXT <-  x[grep("URBEXT", x$Descriptor)[1], 2]
    SAAR <-  x[grep("SAAR", x$Descriptor)[1], 2]
    BFIHOST <- x[grep("BFIHOST", x$Descriptor)[1], 2]
    AREA <- x[which(x$Descriptor == "AREA"), 2]

    TP <- 1.56 * PROPWET^-1.09 * DPLBAR^0.6 * (1 + URBEXT)^-3.34 * DPSBAR^-0.28
    Duration <- TP * (1 + (SAAR / 1000))
    BL <- 25.5 * BFIHOST^0.47 * DPLBAR^0.21 * PROPWET^-0.53 * (1 + URBEXT)^-3.01
    BR <- 3.75 * BFIHOST^1.08 * PROPWET^0.36
    Cmax <- 596.7 * BFIHOST^0.95 * PROPWET^-0.24

    Cini.win <- (Cmax / 2) * (1.2 - 1.7 * BFIHOST + 0.82 * PROPWET)
    Cini.sum <- (Cmax / 2) * (0.9 - 0.82 * BFIHOST - 0.43 * PROPWET)


    if(is.null(Season)) {
      if(URBEXT > 0.03) {Season <- "summer"}
      if(URBEXT <= 0.03) {Season <- "winter"}
    }
    if (Season == "winter") {
      Cini <- Cini.win
    }
    if (Season == "summer") {
      Cini <- Cini.sum
    }

    if (Cini <= 0) {
      Cini <- 0
    } else {
      Cini <- Cini
    }
    BFini.win <- (63.8 * (Cini - 120.8) + 5.54 * SAAR) * 10^-5 * AREA
    BFini.sum <- (33.9 * (Cini - 85.4) + 3.14 * SAAR) * 10^-5 * AREA
    if (Season == "winter") {
      BFini <- BFini.win
    }
    if (Season == "summer") {
      BFini <- BFini.sum
    }
    if (BFini <= 0) {
      BFini <- 0
    } else {
      BFini <- BFini
    }
    Pars <- data.frame(AREA, TP, Duration, BR, BL, Cmax, Cini, BFini, Season)
    return(Pars)
  }


  if(is.null(CDs) == FALSE) {
    Pars <- Params(CDs)
  }
  if(is.null(CDs)) {
    Pars <- data.frame(AREA = NA, TP = NA, Duration = NA, BR = NA,
                       BL = NA, Cmax = NA, Cini = NA, BFini = NA,
                       Season = NA)
  }

  if(is.null(AREA) == FALSE) {Pars$AREA <- AREA}
  if(is.na(Pars$AREA)) stop("The AREA argument is needed or input CDs")
  if(is.null(TP) == FALSE) {
    Pars$TP <- TP
    if(is.null(Duration)) {Pars$Duration <- TP * (1 + (SAAR/1000))}
  }
  if(is.na(Pars$TP)) stop("The TP argument is needed or input CDs")
  if(is.null(Duration) == FALSE) {Pars$Duration <- Duration}
  if(is.na(Pars$Duration) & is.null(Rain)) stop("A Duration or Rain input is needed or input CDs")
  if(is.null(BR) == FALSE) {Pars$BR <- BR}
  if(is.null(BL) == FALSE) {Pars$BL <- BL}
  if(is.null(Cmax) == FALSE) {Pars$Cmax <- Cmax}
  if(is.na(Pars$Cmax) & is.null(Loss)) stop("Cini and Cmax are required, or the Loss argument should be used, or input CDs")
  if(is.null(Cini) == FALSE) {Pars$Cini <- Cini}
  if(is.na(Pars$Cini) & is.null(Loss)) stop("Cini and Cmax are required, or the Loss argument should be used, or input CDs")
  if(is.null(BFini) == FALSE) {Pars$BFini <- BFini}
  if(is.na(Pars$BFini)) stop("A BFini argument is required or input CDs")
  if(is.null(Season) == FALSE) {Pars$Season <- Season}
  if(is.na(Pars$Season)) {Pars$Season <- "winter"}
  if(is.na(Pars$BR)) {Pars$BR <- 0}
  if(is.na(Pars$BL)) {Pars$BL <- 45}

  if(is.null(Depth) & is.null(Rain)) {

    if(is.null(CDs) == FALSE) {
      RMEDIndex <- grep("RMED", CDs$Descriptor)
      Rmod <- lm(CDs[RMEDIndex, 2] ~ c(1, 24, 48) + I(c(1, 24, 48)^2))
      RMEDmod <- function(x) {
        Rmod$coefficients[1] + Rmod$coefficients[2] * x + Rmod$coefficients[3] * x^2
      }
      RMEDest <- RMEDmod(Pars$Duration)
      scf <- SCF(SAAR = SAAR, duration = Pars$Duration)
      if (Pars$Season == "summer") {
        scf <- scf[, 1]
      }
      if (Pars$Season == "winter") {
        scf <- scf[, 2]
      }
      Depth <- as.numeric(RMEDest * scf)
    }

  }

  if(is.null(Depth) & is.null(Rain) == FALSE) {Depth <- sum(Rain)}

  if (is.null(Timestep) & is.null(Rain)) {
    if (Pars$TP <= 1) {
      Timestep <- 0.1
    }
    if (Pars$TP > 1 & Pars$TP < 3) {
      Timestep <- 0.25
    }
    if (Pars$TP >= 3 & Pars$TP < 5) {
      Timestep <- 0.5
    }
    if (Pars$TP >= 5) {
      Timestep <- 1
    }
  } else {
    Timestep <- Timestep
  }



  if(is.null(Rain) == FALSE & is.null(Timestep)) stop("If you input Rain you must specify a Timestep for the rainfall")
  if(is.null(Rain) == FALSE) {Pars$Duration <- length(Rain) * Timestep}

  if(is.null(Depth)) stop("A depth argument is needed or input CDs")
  if(is.null(Rain)) {Rain <- RainEventSim(Duration = Pars$Duration, Rainprofile = RainProfile, Depth = Depth, season = Pars$Season, Timestep = Timestep)}

  if(is.null(Pars$Duration)) {Pars$Duration <- length(Rain) * Timestep}

  if(is.null(Depth) & is.null(Rain) == FALSE) {Depth <- sum(Rain)}


  LossFunc <- function(P, cini, Cmax, RPa, alpha = FALSE, season) {
    if (alpha == TRUE) {
      if (season == "winter") {
        if (RPa < 5) {
          a <- 1
        } else {
          a <- 1.166 * RPa^-0.073
        }
      }
      if (season == "summer") {
        if (RPa < 5) {
          a <- 1
        } else {
          a <- 1.444 * RPa^-0.182
        }
      }
      ct1 <- a * cini
      ct <- P[2] + ct1
      for (i in 3:length(P)) {
        ct <- append(ct, ct[length(ct)] + P[i])
      }
      ct <- append(ct1, ct)
      pr1 <- a * (cini / Cmax) + (P[1] / (2 * Cmax))
      pr <- NULL
      for (i in 2:(length(P))) {
        pr[i] <- (ct[i - 1] / Cmax) + (P[i] / (2 * Cmax))
      }
      pr[1] <- pr1
    } else {
      ct1 <- cini
      ct <- P[2] + ct1
      for (i in 3:length(P)) {
        ct <- append(ct, ct[length(ct)] + P[i])
      }
      ct <- append(ct1, ct)
      pr1 <- (cini / Cmax) + (P[1] / (2 * Cmax))
      pr <- NULL
      for (i in 2:(length(P))) {
        pr[i] <- (ct[i - 1] / Cmax) + (P[i] / (2 * Cmax))
      }
      pr[1] <- pr1
    }
    NetP <- pr * P
    NetP <- pmin(P, NetP)
    return(NetP)
  }


  UH <- function(TP, timestep = 1) {
    TBt <- 2 * (TP / 0.65)
    Uc <- 0.65 * ((TBt - 2 * TP) / (TBt - TP))
    TB <- TP * (1 + 2 * ((1 - 0.65)) / (0.8 * Uc))
    U2tp <- Uc * 0.8
    y <- c(0, mean(c(0, 0.65)), 0.65, U2tp, mean(c(U2tp, 0)), 0)
    x <- c(0, mean(c(0, TP)), TP, 2 * TP, mean(c(2 * TP, TB)), TB)
    y1mod <- lm(y[1:3] ~ x[1:3])
    y2mod <- lm(y[3:4] ~ x[3:4])
    y3mod <- lm(y[4:6] ~ x[4:6])
    Coefs1 <- as.numeric(coefficients(y1mod))
    Coefs2 <- as.numeric(coefficients(y2mod))
    Coefs3 <- as.numeric(coefficients(y3mod))
    YModel1 <- function(x) {
      Coefs1[1] + Coefs1[2] * x
    }
    YModel2 <- function(x) {
      Coefs2[1] + Coefs2[2] * x
    }
    YModel3 <- function(x) {
      Coefs3[1] + Coefs3[2] * x
    }
    UnitHydro <- data.frame(x, y)
    uh1 <- YModel1(seq(0, UnitHydro[3, 1], by = 0.01))
    uh2 <- YModel2(seq(UnitHydro[3, 1], UnitHydro[4, 1], by = 0.01))
    uh3 <- YModel3(seq(UnitHydro[4, 1], UnitHydro[6, 1], by = 0.01))
    UH <- c(uh1, uh2[-1], uh3[-1])
    Scurve <- NULL
    for (i in 1:length(UH)) {
      Scurve[i] <- sum(UH[1:i])
    }
    TSCoef <- timestep / 0.01
    Zeros <- rep(0, TSCoef)
    S.t <- append(Zeros, Scurve)
    SDiff <- abs(Scurve - S.t[1:length(Scurve)])
    UHfin <- SDiff * (0.01 / timestep)
    TSCoef <- timestep / 0.01
    UHVec <- seq(1, (length(UH)), by = TSCoef)
    return(UHfin[UHVec])
  }

  Convolute <- function(UH, NR) {
    Mults <- NULL
    for (i in 1:length(NR)) {
      Mults[[i]] <- NR[i] * UH
    }
    NAadd <- seq(1, (length(NR)))
    NAsAdd <- rep(NA, sum(NAadd))
    for (i in 1:length(NAadd)) {
      Mults[[i]] <- append(NAsAdd[1:i], Mults[[i]])
    }
    LenConv <- length(UH) + length(NR)
    RainSums <- NULL
    for (i in 1:LenConv) {
      RainSums[i] <- sum(sapply(Mults, function(Mults) Mults[i]), na.rm = TRUE)
    }
    return(RainSums)
  }


  SHGamma <- function(TB = 5, TP, TimeStep = 1, Exp = 2) {
    TSeq <- seq(0, TP*TB, by = TimeStep)
    Res <- ((TSeq/TP) *exp(1- (TSeq/TP)))^Exp
    return(Res)
  }

  UHFSR <- function(TP, timestep, AREA) {
    TB <- 2.52 * TP
    UP <- 0.1*2.2/TP*AREA
    TimeIndex <- seq(0, ceiling(TB), by = timestep)
    qUH <- NULL
    for(i in 1:length(TimeIndex)) {
      if(TimeIndex[i] <=TP) {qUH[i] <- TimeIndex[i]*UP/TP} else {qUH[i] <- (TB-TimeIndex[i])*UP/(TB-TP)}}
    qUH <- qUH[!(qUH < 0)]
    return(qUH)
  }


  BF <- function(BL, BR, Bfini, ro, timestep = 1) {

    k3 <- exp(-(timestep/BL))
    k2 <- BR*(1-(1-k3)*(BL/timestep))
    k1 <- BR*((BL/timestep)*(1-k3)-k3)
    zt <- 0
    for(i in 2:(length(ro))) {
      zt <- append(zt, k1*ro[(i-1)]+k2*ro[i]+k3*zt[length(zt)])
    }
    zt <- zt + Bfini
    return(zt)
  }



  # print(Pars)
  if (!is.null(Rain) && length(Rain) == 1) {
    stop("Rain must be a vector of rainfall depths over time steps, not a single value.")
  }

  if (Pars$Duration < Pars$TP) warning("duration shorter than time to peak")
  if (Pars$Duration >= 4.5 * Pars$TP) warning("This is an event based model; duration > 4.5TP and is on the high side")

  if (Timestep > 0.25 * ceiling(Pars$TP)) warning("If Timestep is > 0.25TP results may be a touch 'blocky'")

  if (alpha == TRUE & is.null(RPa)) stop("if alpha equals TRUE and associated return periods is needed for the RPa argument")
  if (Pars$Season == "winter") {
    Cini <- Pars$Cini.win
  }
  if (Pars$Season == "summer") {
    Cini <- Pars$Cini.sum
  }
  if (is.null(RPa) == TRUE) {
    RPa <- 2
  } else {
    RPa <- RPa
  }

  if(is.null(Loss) == FALSE) {
    if(Loss >=1 | Loss < 0) stop("If the Loss argument is used it must be a value below one and equal to or above zero.")
    EffRain <- Rain * (1-Loss)}
  if(is.null(LossCini) == FALSE) {
    if(LossCini >=1 | LossCini < 0) stop("If the LossCini argument is used it must be a value below one and equal to or above zero.")
    Pars$Cini <- ((1-LossCini)-(Depth/(2*Pars$Cmax))) * Pars$Cmax
  }
  if(is.null(Loss)){
    if(Pars$Cini < 0) {Pars$Cini <- 0}}
  if(is.null(Loss)) {
    EffRain <- LossFunc(Rain, cini = Pars$Cini, RPa = RPa, Cmax = Pars$Cmax, season = Pars$Season, alpha = alpha)}

  if(UrbanLoss == TRUE) {
    if(is.null(Loss) == FALSE | is.null(LossCini) == FALSE) warning("As UrbanLoss is TRUE, the eventual loss will be greater than the loss you input. Perhaps set UrbanLoss to FALSE")
    DS <- 0.5
    IRF <- 0.7
    IF <- 0.4

    CumSumRainDF <- cumsum(Rain) - DS
    DOF <- rep(0, length(Rain))
    MinusIndex <- which(CumSumRainDF < 0)
    if(length(MinusIndex) > 0) {DOF[MinusIndex] <- 0}
    Above1 <- which(CumSumRainDF > 1)
    if(length(Above1) > 0) {DOF[Above1] <- 1}
    Zero1 <- which(CumSumRainDF > 0 & CumSumRainDF <= 1)
    if(length(Zero1) > 0) {DOF[Zero1] <- CumSumRainDF[Zero1]}
    PR <- IRF*IF*Rain + ((1 - IF) + (1 - IRF)*IF*DOF)*EffRain

    UrbProp <- URBEXT / 0.7806
    if(UrbProp > 1) {UrbProp <- 1}
    NetRain <- (PR*UrbProp) + (EffRain*(1-UrbProp))
    EffRain <- pmin(NetRain, Rain)
  }

  UHCheck <- c("Gamma", "KT", "FSR")
  MatchUH <- match(UHShape, UHCheck)
  if(is.na(MatchUH)) stop("UHShape must be one of KT, FSR, or Gamma")

  if(UHShape == "Gamma") {
    Vols <- (Pars$AREA * 1000000 * (EffRain/1000))
    UH <- SHGamma(TP = Pars$TP, TimeStep = Timestep, Exp = 2)
    TCoef <- Timestep*60*60
    DF <- data.frame(   c(((UH/sum(UH)) * Vols[1])/TCoef, rep(0, (length(Vols)))))
    for(i in 2:length(Vols)) {DF <- cbind(DF, c(rep(0,i), ((UH/sum(UH)) * Vols[i])/TCoef, rep(0, (length(Vols)-i)))    )}
    colnames(DF) <- seq(1,ncol(DF))
    Runoff <- apply(DF, 1, sum)
  }

  if(UHShape == "KT") {
    UnitHydro <- UH(TP = Pars$TP, timestep = Timestep)
    SF <- Pars$AREA / (3.6 * Pars$TP)
    UnitHydro <- UnitHydro * SF
    Runoff <- Convolute(UnitHydro, EffRain)
    Runoff <- Runoff[-1]
  }

  if(UHShape == "FSR"){
    ConvoluteFSR <- function(UH, NetRain) {
      UHList <- list()
      for(i in 1:length(UH)) {UHList[[i]] <- UH[i]*NetRain}
      Matrix <- matrix(NA ,nrow = length(NetRain) + length(UH), ncol = length(NetRain))
      ConDF <- as.data.frame(Matrix[-nrow(Matrix),])
      for(i in 1:length(UHList)) {ConDF[ (i:(i+length(NetRain)-1)) , i] <- UHList[[i]]  }
      Q <- as.numeric(apply(ConDF,1,sum, na.rm = TRUE))
      return(Q)
    }

    UnitHydro <- UHFSR(AREA = Pars$AREA, TP = Pars$TP, timestep = Timestep)
    Runoff <- ConvoluteFSR(UnitHydro, EffRain)
  }


  if(WaterBalance == TRUE) {
    #EffProp1 <- (Pars$Cini/Pars$Cmax) + (Depth/(2*Pars$Cmax))
    EffProp <- (sum(EffRain, na.rm = TRUE) / sum(Rain, na.rm = TRUE))
    TotalEffProp <- EffProp + Pars$BR*EffProp
    if(TotalEffProp > 1) {
      warning("Water balance was violated with the initial BR setting. To correct this, BR was set as a function of the proportion of net-rain to rain (NetProp) as BR = (1/NetProp)-1")
      #Pars$BR <- (1-EffProp)/EffProp}
      Pars$BR <- (1/EffProp) - 1}
  }

  Baseflow <- BF(Pars$BL, Pars$BR, Bfini = Pars$BFini, ro = Runoff, timestep = Timestep)
  TotalFlow <- Runoff + Baseflow
  L <- length(TotalFlow)
  NAs <- rep(NA, times = (L - length(Rain)))
  Rain <- append(Rain, NAs)
  res <- round(data.frame(Rain, TotalFlow, Runoff, Baseflow), 3)
  Scaled <- NULL
  if (is.null(Scaled) == FALSE) {
    ScaledHydro <- (TotalFlow / max(TotalFlow)) * Scaled
    res2 <- round(ScaledHydro, 3)
  }
  if (is.null(PlotTitle) == FALSE) {
    title <- PlotTitle
  } else {
    title <- "Runoff Hydrograph"
  }
  if (is.null(PlotTitle) == FALSE) {
    titleScaled <- PlotTitle
  } else {
    titleScaled <- "Design Hydrograph (Scaled to Estimated Peak Flow)"
  }
  if (is.null(Scaled) == TRUE) {
    par(mar = c(5.1, 4.5, 4.1, 4.3))
    adj.y <- 1.5
    TimeLab <- "Time (Hours)"
    time_hours <- seq(0, by = Timestep, length.out = nrow(res))
    matplot(time_hours, res[, 2:4],
            type = "l", lwd = 2, main = title,
            xlab = TimeLab, ylab = expression("Discharge (m"^3 * "/s)"),
            ylim = c(0, adj.y * max(res[, 2], na.rm = TRUE)),
            col = c("black", "red", "forestgreen")
    )

    legend(
      "topright",
      legend = c("NetRain", "Rain", "TotalFlow", "DirectRunoff", "Baseflow"),
      bty = "n",
      fill = c("#008080", "darkblue", rep("transparent", 3)),
      density = c(20, rep(NA, 4)),
      angle = c(45, NA, NA, NA, NA),
      border = c("#008080", "darkblue", rep("transparent", 3))
    )

    legend(
      "topright",
      legend = c("NetRain", "Rain", "TotalFlow", "DirectRunoff", "Baseflow"),
      bty = "n",
      col = c("transparent", "transparent", "black", "red", "forestgreen"),
      lty = c(NA, NA, 1, 2, 3),
      lwd = c(NA, NA, 2, 2, 2),
    )

    par(new = T)
    NetRain <- append(EffRain, NAs)
    plot(NA,
         xlim = range(time_hours),
         ylim = rev(c(0, adj.y * max(res[, 1], na.rm = TRUE))),
         axes = FALSE, xlab = NA, ylab = NA
    )

    axis(side = 4)
    mtext(side = 4, line = 3, "Rainfall (mm)")

    bar_width <- diff(range(time_hours)) / (2 * length(time_hours))

    # Plot bars for res[,1]
    for (i in seq_along(time_hours)) {
      if (!is.na(res[i, 1]) && res[i, 1] != 0) {
        rect(
          xleft = time_hours[i] - bar_width,
          xright = time_hours[i] + bar_width,
          ybottom = 0,
          ytop = res[i, 1],
          col = "darkblue",
          border = "darkblue"
        )
      }
    }

    # NetRain bars (patterned)
    for (i in seq_along(time_hours)) {
      if (!is.na(NetRain[i]) && NetRain[i] != 0) {
        rect(
          xleft = time_hours[i] - bar_width,
          xright = time_hours[i] + bar_width,
          ybottom = 0,
          ytop = NetRain[i],
          col = "#008080",
          border = "#008080",
          density = 20,
          angle = 45,
          lwd = 2
        )
      }
    }
    par(mar = c(5.1, 4.1, 4.1, 2.1))
  } else {
    TimeLab <- "Time (Hours)"
    time_hours <- seq(0, by = Timestep, length.out = length(res2))
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(mar = c(5.1, 4.5, 4.1, 2.1))
    plot(time_hours, res2, main = titleScaled, ylab = expression("Discharge (m"^3 * "/s)"), xlab = TimeLab, type = "l", lwd = 2)
  }
  NetRain <- append(EffRain, NAs)
  Output <- signif(data.frame(Rain, NetRain, Runoff, Baseflow, TotalFlow), 3)

  if (is.null(Scaled) == TRUE) {
    Results <- Output
  }
  if (is.null(Scaled) == FALSE) {
    Results <- res2
  }
  NAIndex <- which(is.na(Results$Rain) == TRUE)
  Results$Rain[NAIndex] <- 0
  Results$NetRain[NAIndex] <- 0
  Pars[1, 1:8] <- signif(as.numeric(Pars[1,1:8]), 3)
  if(is.null(Loss) == FALSE) {
    Pars$Cini <- NA
    Pars$Cmax <- NA
    Pars <- data.frame(Pars, Loss)
  }
  if(Pars$BR == 0) {Pars$BL <- NA}
  Pars <- data.frame(Pars, Depth = signif(Depth,3))
  Pars <- data.frame(Pars, Timestep)
  DurationFinal <- length(EffRain) * Timestep
  Pars <- data.frame(Pars, DurationFinal, PeakFlow = max(Results$TotalFlow))
  Time <- seq(0, length.out = nrow(Results), by = Timestep)
  Results <- data.frame(Time_hrs = Time, Results)
  Results <- list(Pars, Results)
  return(Results)
}
