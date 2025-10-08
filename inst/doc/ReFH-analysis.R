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
ReFH(CDs.55002)

## -----------------------------------------------------------------------------
# Create an ungauged pooling group for site 55002 
PoolUG.55002 <- Pool(CDs.55002, exclude = 55002)

# Estimate QMED for site 55002 using catchment descriptors and two donor gauges
CDsQmed.55002 <- QMED(CDs.55002, Don2 = c(55007, 55016))$QMEDs.adj

# Estimate design flows using the above pooling group and QMED
Results55002 <- PoolEst(PoolUG.55002, QMED = CDsQmed.55002)

## ----fig.alt="Design hydrograph scaled to the estimated peak flow, illustrating the typical storm response. The x-axis shows time in hours and the y-axis shows discharge. The hydrograph follows the classic shape, rising steeply to a peak at about 32 hours, then receding more gradually with an elongated, tapering descent. This provides a standardised representation of flow behaviour during a peak event."----
# Extract the 100-year flow estimate
Q100.55002 <- Results55002[[1]][7, 2]

# Obtain outputs of the ReFH model from catchment descriptors for a default 2-year 
# rainfall, scaling to the 100-year peak flow estimate
ReFH(CDs.55002, scaled = Q100.55002)

## ----eval = FALSE-------------------------------------------------------------
#  # Save the ReFH design hydrograph to an object called 'DesignHydro.55002.unscaled'
#  DesignHydro.55002.unscaled <- ReFH(CDs.55002)
#  
#  # Write to csv
#  write.csv(DesignHydro.55002.unscaled, "my/file/path/DesHydro55002_unscaled.csv", row.names = FALSE)
#  
#  # Save the ReFH design hydrograph to an object called 'DesignHydro.55002.scaled'
#  DesignHydro.55002.scaled <- ReFH(CDs.55002, scaled = Q100.55002)
#  
#  # Write to csv
#  write.csv(DesignHydro.55002.scaled, "my/file/path/DesHydro55002_scaled.csv", row.names = FALSE)

