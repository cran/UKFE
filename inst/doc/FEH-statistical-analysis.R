## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = FALSE, results = 'asis'-------------------------------------------
library(knitr)
kable(data.frame(Function = c("GetCDs/CDsXML", "GetAM/AMImport", "QuickResults", "QMED", "DonAdj", "Pool", "H2", "DiagPlots", "Zdists", "PoolEst", "Uncertainty", "GenLog/GEV/GenPareto/Gumbel/Kappa3", "POTextract or POTt", "AnnualStat"), Purpose = c("Get catchment descriptors using the NRFA gauge reference number or import them from XML files downloaded from the FEH Web Service", "Get annual maximum data from sites suitable for pooling or from AM files", "Provides default pooling results (gauged, ungauged & fake ungauged), directly from catchment descriptors", "Estimates QMED from catchment descriptors with the option of zero, one or two donors", "Provides a list of donor candidates based on proximity to the study catchment", "Creates a pooling group with the option of excluding one or more sites", "The heterogeneity measure for pooling groups", "Provides a number of diagnostic plots to compare the sites in a pooling group", "The Zdist statistic to determine the distribution with the best fit to the pooling group", "Flood estimates from the pooling group", "Quantifies uncertainty for gauged (enhanced single-site) and ungauged pooling groups", "Four functions for each distribution to fit the distribution and obtain flood frequency estimates and growth factors", "Functions for extracting peaks over threshold data", "Extract annual statistics. The default is maximum, but any statistic can be used such as sum or mean.")))

## ----setup--------------------------------------------------------------------
library(UKFE)

## ----fig.alt="Plot showing growth curves from FEH QuickResults calculations. Pooled estimates are shown as a solid line, single-site estimates as a dashed green line, and observed flood data as blue circles. The x-axis shows the logistic reduced variate and the y-axis shows Q/QMED, that is, flow divided by the median annual flood. A scale for the return period is displayed at the bottom right. The plot illustrates how well the pooled and single-site growth curves map onto the observed flood data. Neither curve matches all observations exactly, but both provide a reasonable fit overall. Both curves follow the expected growth-curve shape, with increasing discharge at higher return periods."----
# Get catchment descriptors for NRFA site 55002 and save to an object called CDs.55002
CDs.55002 <- GetCDs(55002)

# Obtain estimates and plots for the gauged scenario using default settings
QuickResults(CDs.55002, gauged = TRUE)

## ----fig.alt="Plot showing how multiple single-site growth curves are pooled to form the default ‘fake ungauged’ growth curve. The x-axis shows the logistic reduced variate and the y-axis shows Q/QMED, that is, flow divided by the median annual flood. Several solid black lines represent growth curves from individual sites, and a red dashed line represents the pooled growth curve with the target site excluded. All curves follow the expected growth-curve shape, with increasing discharge at higher return periods. The pooled curve lies within the spread of the individual site curves and, as expected, closely resembles their average, illustrating how pooling smooths variability to produce a representative ungauged growth curve."----
# Obtain estimates and plots for the fake ungauged scenario using default settings
QuickResults(CDs.55002, FUngauged = TRUE) 

## -----------------------------------------------------------------------------
# Estimate QMED from the catchment descriptors for site 55002
QMED(CDs.55002)

## -----------------------------------------------------------------------------
# Identify donor catchments based on the catchment descriptors for site 55002
DonAdj(CDs.55002)

## -----------------------------------------------------------------------------
# Estimate QMED from the catchment descriptors for site 55002 using gauges with NRFA
# IDs 55007 and 55016 as donor sites
QMED(CDs.55002, Don2 = c(55007, 55016))

## -----------------------------------------------------------------------------
# Create a pooling group based on the catchment descriptors for site 55002 and 
# save to an object called Pool.55002
Pool.55002 <- Pool(CDs = CDs.55002)

# View the pooling group
Pool.55002

## ----echo = FALSE-------------------------------------------------------------
set.seed(5)

## -----------------------------------------------------------------------------
# Check for heterogeneity
H2(Pool.55002)

## ----fig.alt="Diagnostic plot consisting of 10 panels used to aid review of catchment descriptors. The first seven panels show histograms of all catchments, with each of the ten sites marked by small crosses, for the following variables: Area, SAAR, PROPWET, FARL, FPEXT, BFIHOST19, and URBEXT2000. Two scatter plots display L-skew versus L-CV and L-skew versus L-kurtosis. The final panel shows the ten catchment locations as points on a UK outline, plotted by Eastings and Northings. Together, the plots allow comparison of these sites against the distribution of all catchments. The ten sites appear generally central and unremarkable across descriptor space, indicating they are fairly typical relative to the wider catchment set."----
# Create diagnostic plots to aid review of the pooling group
DiagPlots(Pool.55002)

## -----------------------------------------------------------------------------
# Recreate the pooling group, excluding NRFA sites 8010 and 76017
Pool.55002 <- Pool(CDs = CDs.55002, exclude = c(8010, 76017))

# View the new pooling group
Pool.55002

# Check for heterogeneity
H2(Pool.55002)

## -----------------------------------------------------------------------------
# Calculate goodness-of-fit measure
Zdists(Pool.55002)

## ----eval = FALSE-------------------------------------------------------------
#  MyData <- scan()

## -----------------------------------------------------------------------------
# Update the pooling group to use user-supplied L-CV and L-skew values for site 55002
UpdatedPool <- LRatioChange(Pool.55002, SiteID = 55002, lcv = 0.187, lskew = 0.164)

# View the updated pooling group
UpdatedPool

## -----------------------------------------------------------------------------
# Extract QMED from the QMEDdata data frame within the UKFE package
GetQMED(55002)

## -----------------------------------------------------------------------------
# Derive the growth curve and final flow estimates based on the pooling group and QMED
PoolEst(Pool.55002, QMED = GetQMED(55002), gauged = TRUE)

## ----fig.alt="Plot showing growth curves from FEH QuickResults calculations. Pooled estimates are shown as a solid line, single-site estimates as a dashed green line, and observed flood data as blue circles. The x-axis shows the logistic reduced variate and the y-axis shows Q/QMED, that is, flow divided by the median annual flood. A scale for the return period is displayed at the bottom right. The plot illustrates how well the pooled and single-site growth curves map onto the observed flood data. Neither curve matches all observations exactly, but both provide a reasonable fit overall. Both curves follow the expected growth-curve shape, with increasing discharge at higher return periods."----
# Plot the growth curve
EVPool(Pool.55002, gauged = TRUE)

## ----fig.alt="Plot showing how multiple single-site growth curves are pooled to form the default ‘fake ungauged’ growth curve. The x-axis shows the logistic reduced variate and the y-axis shows Q/QMED, that is, flow divided by the median annual flood. Several solid black lines represent growth curves from individual sites, and a red dashed line represents the pooled growth curve with the target site excluded. All curves follow the expected growth-curve shape, with increasing discharge at higher return periods. The pooled curve lies within the spread of the individual site curves and, as expected, closely resembles their average, illustrating how pooling smooths variability to produce a representative ungauged growth curve."----
# Recreate the pooling group, excluding NRFA gauge 55002 to make it an ungauged analysis
PoolUG.55002 <- Pool(CDs.55002, exclude = 55002)

# View the new pooling group
PoolUG.55002

# Estimate QMED from the catchment descriptors with two donors (note that the QMED 
# value needs to be extracted using `$QMEDs.adj` as there are three elements to the 
# output when two donors are used and the PoolEst function expects just the QMED value)
CDsQmed.55002 <- QMED(CDs.55002, Don2 = c(55007, 55016))$QMEDs.adj

# Use the pooling group and the ungauged QMED estimate to provide the pooled estimates
Results55002 <- PoolEst(PoolUG.55002, QMED = CDsQmed.55002)

# Print the results
Results55002

# Plot a growth curve for the pooling group
EVPool(PoolUG.55002)

## ----eval = FALSE-------------------------------------------------------------
#  GenLogAM(MyAMAX, RP = 75)

## ----fig.alt="Bar chart of annual maximum river flow. The x-axis shows years, and the y-axis shows peak flow in cubic meters per second. Each bar represents the highest flow in that year. The flows vary from year to year, with several notably high peaks in recent years."----
# Extract the AMAX data for NRFA site 55002
AM.55002 <- GetAM(55002)

# View the head of the AMAX series
head(AM.55002)

# Plot the AMAX data
AMplot(AM.55002)

## ----fig.alt="Extreme rank frequency curve illustrating uncertainty around the estimated growth curve. The x-axis shows the rank and the y-axis shows the percent difference. Observed data are shown as blue circles, plotting along the 0% line. The central modelled curve, shown as a solid line, tracks closely to the observations, while dashed lines represent the 90% bootstrapped confidence interval. The uncertainty bounds remain narrow through the central portion of the curve and expand slightly toward the lower and upper ranks."----
# Generate an extreme rank plot for the AMAX data for NRFA site 55002 and the GEV 
# distribution and set a user-defined title (using the 'main' argument)
ERPlot(AM.55002$Flow, dist = "GEV", main = "Extreme rank plot for NRFA site 55002: GEV")

## ----fig.alt="Extreme rank frequency curve illustrating uncertainty around the estimated growth curve. The x-axis shows the logistic reduced variate and the y-axis shows Q/QMED, that is, flow divided by the median annual flood. Observed data are shown as blue circles, forming a concave-up increasing curve. The central modelled estimate, shown as a solid line, tracks these observations closely. Dashed lines represent the 90% bootstrapped confidence interval, which remains tight across the centre of the distribution and widens toward the extremes."----
# Generate an extreme value plot for the AMAX data for NRFA site 55002 and the GEV
# distribution and set a user-defined title (using the 'Title' argument)
EVPlot(AM.55002$Flow, dist = "GEV", Title = "Extreme value plot for NRFA site 55002: GEV")

## -----------------------------------------------------------------------------
# Estimate the 100-year flow directly from the AMAX data for NRFA site 55002 using
# the GEV distribution
GEVAM(AM.55002$Flow, RP = 100)

# Estimate the return period of the maximum observed flow directly from the AMAX 
# data using the GEV distribution
GEVAM(AM.55002$Flow, q = max(AM.55002$Flow))

## -----------------------------------------------------------------------------
# Estimate the parameters of the GEV distribution from the AMAX data for NRFA site 55002
GEVPars55002 <- GEVPars(AM.55002$Flow)

# View the parameters
GEVPars55002

# Estimate the 75-year flow using these GEV parameters 
GEVEst(loc = GEVPars55002$Loc, scale = GEVPars55002$Scale, shape = GEVPars55002$Shape, RP = 75)

## -----------------------------------------------------------------------------
# Estimate the 75-year GEV growth factor from the L-moments for the AMAX data for 
# NRFA site 55002
GEVGF(lcv = Lcv(AM.55002$Flow), lskew= LSkew(AM.55002$Flow), RP = 75)

## -----------------------------------------------------------------------------
# Estimate the 75-year flow by multiplying the growth factor by the median AMAX flow
GEVGF(lcv = Lcv(AM.55002$Flow), lskew= LSkew(AM.55002$Flow), RP = 75) * median(AM.55002$Flow)

## ----fig.alt="Time series plot of flow showing extraction of peaks over threshold events. The x-axis shows time and the y-axis shows flow magnitude. Red circles mark exceedances above the blue threshold line. A green line indicates div, which defines independence between peaks; in this case it is set to the mean. Exceedances occur throughout the record, with a lull around 2005, followed by a marked increase in frequency afterwards."----
# Extract a POT series from the Thames at Kingston daily mean flow data, selecting 
# the first and third columns from the ThamesPQ data frame which contain the date 
# and flow data respectively. The threshold is set as the 90th percentile.
POT.Thames <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.9)

# View the output
POT.Thames

## -----------------------------------------------------------------------------
# Estimate the 100-year flow for the Thames at Kingston using the extracted POT series 
GenParetoPOT(POT.Thames$peak, ppy = 1.867, RP = 100)

## -----------------------------------------------------------------------------
# Extract the AMAX for NRFA site 55002
AM.55002 <- GetAM(55002)

## -----------------------------------------------------------------------------
# Estimate uncertainty for the GEV-estimated 100-year flow
Bootstrap(AM.55002$Flow, Stat = GEVAM, RP = 100)

## ----fig.alt="Return level plot showing estimated flow values for a range of return periods. The x-axis shows the return period on a logarithmic scale, and the y-axis shows flow. The central estimate is shown as a solid black line that is nearly straight, sloping upward. Dashed black lines represent the confidence interval: the lower bound lies close to horizontal at around 250, rising only slightly with increasing return period, while the upper bound increases much more rapidly, forming a steep concave-up curve. Together, the lines illustrate how design flows increase with return period and the asymmetric uncertainty in these estimates."----
# Estimate QMED for site 55002 from catchment descriptors using two donors
QMEDEst <- QMED(CDs.55002, Don2 = c(55007, 55016))$QMEDs.adj

# Create an ungauged pooling group for site 55002
PoolUG.55002 <- Pool(CDs.55002, exclude = 55002)

# Estimate the uncertainty of the pooling results 
Uncertainty(PoolUG.55002, qmed = QMEDEst)

## -----------------------------------------------------------------------------
# Create a gauged pooling group for site 55002
Pool.55002 <- Pool(CDs = CDs.55002, exclude = c(8010, 76017))

## ----fig.alt="Return level plot showing estimated flow values for a range of return periods. The x-axis shows the return period on a logarithmic scale, and the y-axis shows flow. The central estimate is shown as a solid black line, which forms a fairly straight diagonal with a moderate slope, slightly shallower than a one-to-one (x = y) line. Dashed black lines represent the confidence interval and diverge from the central estimate, forming a cone-shaped band that widens as the return period increases. The plot illustrates how design flows increase with return period and highlights the growing uncertainty at longer return periods."----
# Estimate the uncertainty of the pooling results 
Uncertainty(Pool.55002, Gauged = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#  # Save the peak flow estimates to an object called 'Results.55002'
#  Results.55002 <- PoolEst(PoolUG.55002, QMED = CDsQmed.55002)[[1]]
#  
#  # Write to csv
#  write.csv(Results.55002, "my/file/path/Results55002.csv", row.names = FALSE)

