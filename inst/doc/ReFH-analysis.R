## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(UKFE)

## ----fig.alt="Runoff hydrograph showing rainfall inputs and resulting flow components. The x-axis shows time in hours. The left y-axis shows discharge, and the right y-axis shows rainfall in millimetres. Rainfall is shown as inverted filled blue bars along the top of the plot, and net rainfall as inverted green striped bars; rainfall is consistently greater than net rainfall, both rising and peaking around 15 hours. Flow is shown as lines: total flow in solid black, baseflow in short dashed green, and direct runoff in dashed red. Total flow and direct runoff peak later than rainfall, with sharp peaks at around 30 hours, while baseflow lags further and peaks more broadly at about 47 hours. The total flow reaches the highest magnitude, followed by direct runoff, with baseflow peaking at much lower levels in a flatter, wider shape."----
# Obtain catchment descriptors for NRFA gauge 55002
CDs.55002 <- GetCDs(55002)

# Obtain outputs of the ReFH model from catchment descriptors for a default 2-year 
# rainfall event
ReFHResult55002 <- ReFH(CDs.55002)
# print to console
ReFHResult55002

## -----------------------------------------------------------------------------
# Create an ungauged pooling group for site 55002 
PoolUG.55002 <- Pool(CDs.55002, exclude = 55002)

# Estimate QMED for site 55002 using catchment descriptors and two donor gauges
CDsQmed.55002 <- QMED(CDs.55002, DonorIDs = c(55007, 55016))

# Estimate design flows using the above pooling group and QMED
Results55002 <- PoolEst(PoolUG.55002, QMED = CDsQmed.55002, CDs = CDs.55002)

## ----fig.alt="Design hydrograph scaled to the estimated peak flow, illustrating the typical storm response. The x-axis is an index of time steps (hours in this case). The hydrograph follows the classic shape, rising steeply to a peak at about 32 hours, then receding more gradually with an elongated, tapering descent. This provides a standardised representation of flow behaviour during a peak event."----
# Extract the 100-year flow estimate (the [[1]] denotes the first element of the list - which is a dataframe, then [8,2] is the 8th row and 2nd column of that dataframe)
Q100.55002 <- Results55002[[1]][8, 2]

# Obtain the final flow outputs of the ReFH model from catchment descriptors for a default 2-year 
# rainfall
TotalFlow <- ReFHResult55002[[2]]$TotalFlow

#Scale this by dividing by the maximum of the flow
ScaledFlow <- TotalFlow / max(TotalFlow)

#Multiply the scaled flow by our peak flow estimate and plot
plot(ScaledFlow * Q100.55002, type = "l", lwd = 2, ylab = "Discharge (m3/s)", xlab = "Time (hours)")


## ----eval = FALSE-------------------------------------------------------------
# # Save the ReFH design hydrograph to an object called 'DesignHydro55002'.
# DesignHydro55002 <- ScaledFlow * Q100.55002
# 
# # Write to csv
# write.csv(DesignHydro55002, "my/file/path/DesHydro55002.csv", row.names = FALSE)
# 

## ----fig.alt="ReFH output with settings adjusted to result in the FSR/FEH rainfall runoff model. The baseflow is constant and the shape is less elongated than the default ReFH output which uses a kinked triangle unit hydrograph. The rainfall profile is randomised but centrally loaded."----
ReFH.FSR <- ReFH(CDs.55002, RainProfile = "Centre", UHShape = "FSR", Loss = 0.3, BR = 0)

