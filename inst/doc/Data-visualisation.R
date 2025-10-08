## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(UKFE)

## ----fig.alt="Bar chart of annual maximum (AMAX) flow values. Each bar represents the highest recorded flow in a given year, with year index on the x-axis and flow magnitude on the y-axis. The chart shows variation across years, with particularly large peaks in 1980, 1999, and 2020."----

# Get an AMAX sample and plot
AMplot(GetAM(58002))

## ----fig.alt="Time series plot of flow showing extraction of peaks over threshold events. The x-axis shows time and the y-axis shows flow magnitude. Red circles mark exceedances above the blue threshold line. A green line indicates div, which defines independence between peaks; in this case it is set to the mean. Exceedances occur throughout the record, with a lull around 2005, followed by a marked increase in frequency afterwards."----
# Extract POT flows for the Thames at Kingston (NRFA gauge 39001) and plot. Note that 
# the indexing 'ThamesPQ[, c(1, 3)]' is necessary because the date is in the first 
# column of the 'ThamesPQ' data frame and the flow is in the third.
ThamesQPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.9, Plot = TRUE)

## ----fig.alt="Plot showing extraction of peaks over threshold (POT) events from a rainfall time series. The x-axis shows time and the y-axis shows flow magnitude. Red circles mark rainfall values exceeding the blue threshold line, which identifies independent extreme events. Exceedances occur throughout, with particularly large peaks around 2000, 2007, and 2014."----

# Extract POT rainfall for the Thames at Kingston (NRFA gauge 39001) and plot. Note that
# the indexing 'ThamesPQ[, 1:2]' is necessary because the date is in the first column 
# of the 'ThamesPQ' data frame and the rainfall is in the second.
ThamesPPOT <- POTt(ThamesPQ[, 1:2], threshold = 0.95, div = 14, Plot = TRUE)

## ----fig.alt="“Time series plot showing precipitation and river discharge together. The x-axis shows time. River discharge is shown as a line with flow values on the left y-axis, while precipitation is displayed as inverted bars with rainfall on the right y-axis. This allows comparison of rainfall events with resulting river flows. A sustained period of elevated rainfall and associated discharge is evident from December 2013 through to March 2014.”"----
# Plot the 2013 water year precipitation and discharge for the Thames at Kingston 
# (NRFA gauge 39001), adjusting the closeness of the precipitation and discharge 
# on the y-axis to 1.8.
HydroPlot(ThamesPQ, From = "2013-10-01", To = "2014-09-30", adj.y = 1.8)

## ----fig.alt="Bar plot showing mean river discharge for each calendar month, with the x-axis representing months and mean flow values on the y-axis. Flows are highest in winter, with January showing the maximum, and lowest in late summer, with September showing the minimum. As expected. Discharge is elevated from November through March, and decreases gradually into summer before rising again into autumn."----
# Calculate the mean flows for each month for the Thames at Kingston (NRFA gauge 39001)
# and set the plot title using the 'main' argument
QMonThames <- MonthlyStats(ThamesPQ[, c(1, 3)], Stat = mean, ylab = "Discharge (m3/s)", 
                           main = "Thames at Kingston monthly mean flow", Plot = TRUE)

## ----fig.alt="Bar plot showing the mean total rainfall for each calendar month. The x-axis shows months and the y-axis shows rainfall totals, allowing comparison of rainfall patterns across the year. Rainfall is relatively high throughout, with a clear minimum in September and a maximum in November. The wettest period extends from October through January."----
# Calculate the monthly sums of rainfall for the Thames at Kingston and set the plot 
# title using the 'main' argument
PMonThames <- MonthlyStats(ThamesPQ[, c(1, 2)], Stat = sum, ylab = "Rainfall (mm)", 
                           main = "Thames at Kingston monthly rainfall", Plot = TRUE)

## ----fig.alt="Line plot showing river discharge over time. The x-axis shows time in days and the y-axis shows daily mean flow. Total flow is shown in black, and baseflow is shown in red. Flows are relatively constant overall, with peaks occurring at fairly regular intervals. Baseflow rises and falls with total flow, but remains lower in magnitude."----
# Calculate the BFI from daily discharge at Kingston upon Thames (which is in the 
# third column of the 'ThamesPQ' data)
BFI(ThamesPQ[, 3])

## ----fig.alt="Time series plot of streamflow and baseflow. The x-axis shows time and the y-axis shows flow. Total flow is shown in black, and baseflow is shown in red. During events, total flow increases sharply, whereas baseflow rises more gradually and remains much lower in magnitude, highlighting the contrast between quick flow responses and the slower baseflow contribution."----
# Extract a wet six-month period at the Thames at Kingston (NRFA gauge 39001) during 
# the 2006-2007 water year (flow is in the third column of ThamesPQ)
ThamesQ <- subset(ThamesPQ[, c(1, 3)], Date >= "2006-11-04" & Date <= "2007-05-06")
# Apply the flow split with default settings
QSplit <- FlowSplit(ThamesQ$Q)

## ----fig.alt="Time series plot showing a black line representing the averaged design hydrograph, derived by aligning individual hydrographs on their peak flow and then averaging. The observed hydrographs are shown as coloured and patterned lines. The x-axis represents time and the y-axis shows scaled discharge. The individual hydrographs vary considerably: some display additional smaller peaks around the main one, while others differ in steepness and shape. In general, hydrographs with higher pre-peak discharge also exhibit higher post-peak discharge. All converge at the peak, and the overall mean hydrograph follows the typical triangular, inverted-V shape."----
# Extract a design hydrograph from the Thames at Kingston (NRFA gauge 39001) daily 
# mean flow and print the resulting hydrograph
ThamesDesHydro <- DesHydro(ThamesPQ[, c(1, 3)], EventSep = 10, N = 10)

## ----fig.alt="Plot showing flow duration curves for a single flow series with seasonal variations. The x-axis shows the percentage of time flow is exceeded, and the y-axis shows discharge. The annual curve is shown as a black line, the winter curve as a blue line, and the summer curve as a green line. Winter flows are consistently highest, followed by annual, then summer. All curves follow the expected pattern of high discharges at low exceedance percentages and progressively lower discharges as exceedance percentages increase."----
# Plot a flow duration curve for the Thames at Kingston (NRFA gauge 39001) using 
# data from Oct 2000 to Sep 2015
FlowDurationCurve(ThamesPQ[, c(1, 3)])

## ----fig.alt="Plot showing the relationship between discharge (x-axis) and stage (y-axis). Observed measurements are shown as open circles with black borders, and a red line represents the optimised power law rating equation fitted to these observations. For this dataset, stage and discharge exhibit an almost linear relationship, with the fitted curve close to straight but showing a very gradual downward concavity while remaining continuously increasing.”"----
# Make up some flow (Q) and stage data to act as gaugings
Q <- c(177.685, 240.898, 221.954, 205.55, 383.051, 154.061, 216.582)
Stage <- c(1.855, 2.109, 2.037, 1.972, 2.574, 1.748, 2.016)
Observations <- data.frame(Q, Stage)

# Apply the rating function
Rating(Observations)

## ----fig.alt="Depth–Duration–Frequency (DDF) plot showing rainfall depth in millimetres (y-axis) as a function of storm duration in hours (x-axis) for a range of return periods. Each curve represents a different return period. All curves follow the characteristic DDF shape, with rainfall depth increasing as event rarity increases and with longer storm durations, particularly at the higher return periods."----
# Extract 15-minute rainfall from the St Ives (Cambridgeshire) rain gauge
StIves <- GetDataEA_Rain(WISKI_ID = "179365", Period = "15Mins", From = "2022-01-01", To = "2025-01-31")

# Apply the DDF function.
DDFExtract(StIves)

