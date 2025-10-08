# UKFE 1.0.0

## Major changes
 
* Data pre-processing script: Added to the package (note: file paths remain local to the package author).
 
* `GenParetoPars()`: Removed the `mle` option (difficult to verify correctness).
 
## Minor changes
 
* Input validation: Added handling for invalid inputs in `GenLogEst()`, `GEVEst()`, `GumbelEst()`, `Kappa3Est()`, `GenParetoEst()` (reject scale or RP <= 0; handle `shape = 0` where applicable).
 
* Error handling: Improved in `Kappa3Est()`, `GEVPars()`, `LSkew()`, and `LKurt()` when required arguments (`x`, `L1`, `LCV`, `LSKEW`) are missing.
 
* Error messaging: More informative message in `Pool()` when `DeUrb = TRUE` but no sites are available.
 
* H2 method: 
  - Regional average L-moments are now weighted by record length. 
  - Heavy optimisation penalties applied at the boundaries to prevent boundary solutions. 
  - Small correction applied to the kappa optimisation equation.
 
* `fse` implementation: Updated in `QuickResults()`.
 
* Styling: Applied `styler::style_pkg()` to align with tidyverse conventions (cosmetic only).
 
* Improved accessibility and readability of plots across multiple functions (better colour contrast, line types, text sizing, axis labelling, and more descriptive titles).
 
 
## Bug fixes
 
* `GenLogAM()`: Implementing support for `k = 0` (i.e. `LSkew = 0`).
 
* GEV shape equation: Minor correction in `GEVGF()`, `GEVAM()` and `GEVPars()`. 
 
* Exponential usage: Replaced `exp(1)^x` with `exp(x)`.
 
* `b` formulas: Reworked `b1`, `b2`, and `b3` in`Kappa3Pars()`, `GenLogAM()`, `GEVAM()`, `GenParetoPOT()`, `GumbelAM()`, `LMoments()`, `Lcv()`, `LSkew()` and `LKurt()`.
 
* Optimisation bounds: Corrected for `Kappa3GF()` and `Kappa3Pars()`.
 
* `Column indexing`: Fixed `QuickResults()` selecting wrong column (now indexed by name).
 
* `ReFH()`: Fixed the calculation of the nearest odd multiple of the timestep when the duration is unspecified.
 
* `AMImport()`: Adjusted for updated date format.
 
* `Zdists()`: Corrected GEV and GenLog results which were the wrong way round.
 
* `ARF()`: Fixed typo in `b` equation when `20 < Area < 100`.
 
* Irish Grid & CRS: `CDsXML()` now uses Irish Grid CRS; `ConvertGridRef()` uses EPSG codes (instead of self defined CRS).
 
* Data structures: Corrected coordinates for Northern Irish gauges in `AMSP`, `NRFAData`, and `QMEDData`.

# UKFE 0.4.0

## Major changes

* `GetDataSEPA_QH()`: A new function to get level and flow data from the SEPA API.

* `DDFExtract()`: A new function which extracts depth duration frequency curves from hourly or sub-hourly rainfall data.

* `FlowSplit()`: A new function for hydrograph splitting -  separating baseflow from runoff.  More for shorter event splitting as opposed to long flow series (i.e. it is not for deriving baseflow index, the BFI function does that).

## Minor changes

* `FlowDurationCurve()`: Users can now add additional horizontal flow lines to the plot with the `AddQs` argument. With the `ReturnData` argument users can also return a data frame with the flow and percentiles.

* `DesHydro()`: Have removed the `div` argument and simplified it so that the `EventSep` argument does both the division of events for the peak identification, as well as the event period. It now produces an error when fewer than two peaks are extracted.

* `GetDataEA_QH()`, `GetDataEA_Rain()`: These functions have been updated so that if the user puts in no start and/or end date, it defaults to the minimum and/or maximum dates available.


# UKFE 0.3.9

Changes combined with 0.4.0 above.


# UKFE 0.3.8

## Major changes

* `DesHydro()`: Vastly improved this function. Significantly quicker and it now centres around the main peak rather than the user having to truncate after inspecting the plot.

* `ConvertGridRef()`: New function to convert between latitude and longitude, British National Grid reference, and Irish Grid reference.

* `FlowDurationCurve()`: New function for plotting flow duration curves from a data frame with date or date-time in the first column and flow in the second column (plots annual, summer, and winter). Alternatively, you can provide a list of flow series to compare.

* `ERPlot()`: Added an option to the extreme rank plot to create a plot very similar to the original version, but the y-axis is the percentage difference from the observed. This allows better comparison across distributions. There is also the option for a plot of modelled versus observed values for each rank.

* `Bootstrap()`: Replaced the `UncSS()` function with this more generic bootstrap function for deriving the sampling distribution and quantifying associated uncertainty for a statistic of your choice (given an input sample).

* `Uncertainty()`: Now provides the full bespoke resampling methods to derive uncertainty for pooling groups detailed in "Hammond, A. (2021). Sampling uncertainty of UK design flood estimation. Hydrology Research. 1357-1371. 52 (6)". Previously, only the gauged version did this and the ungauged version applied the approximating equations (which are used by default within the `PoolEst()` function). The name of the `QMEDfse` argument has changed to `fseQMED` with the default changing from 1.46 to 1.55.

* `AnnualStat()`: Firstly, this is a name change for the `AMextract()` function (because it can be used for any statistic). It also now has a sliding option, so that maximum rainfall over any sliding period can be derived as opposed to just the maximum over fixed intervals.

* `LMoments()`: Name change because it has bugged me for a while that I initially called it `Lmoms`.

* `GetData...` functions. The naming convention of functions that extract data from the web has been homogenised so that they can easily be found. They all start with `GetData`. They are:
  * `GetDataEA_QH()` (get EA flow or stage, find gauges by river name or lat and lon)
  * `GetDataEA_Rain()` (get EA rain, find gauges by lat and lon)
  * `GetDataMetOffice()` (get regional monthly rainfall or temperature)
  * `GetDataNRFA()` (using gauge ID, get flow, catchment rainfall, AMAX, POT, and gaugings)
  * `GetDataSEPA_Rain()` (get SEPA rainfall, find gauges by lat & lon, or print a list of them all to the console).

## Minor change

* `MonthlyStats()`: As well as getting all the monthly stats, there is now an option to output a monthly time series of the stat in the form of a data frame with date in the first column (first of each month) and the statistic of interest in the second.


# UKFE 0.3.7

Changes combined with 0.3.8 above.


# UKFE 0.3.6

## Major changes

* Updated with the new NRFA Peak Flow Dataset (version 13).

* Get data functions. Added a range of functions to get data from various sources using the associated APIs. These are:
  * `GetNRFA()` (this can be used to extract daily catchment rainfall, daily mean flow, AMAX samples, or gaugings from the NRFA, using gauge ID).
  * `GetMetOffice()` (this can be used to get regional series of rainfall or temperature on a monthly, seasonal, and annual basis).
  * `GetRainEA()` (this can be used to extract rainfall data from the Environment Agency's Hydrometric Data Explorer. It is available under v0.3.5 but I have increased the number of rows the function can get – previously you could only get a little over 2 years of 15-minute data. Now you can theoretically get 57 years. In practice, it fails if the extraction takes more than one minute. This restricts it to more like 25 years (when I tested it anyway).
  * `GetRainSEPA()` (no change from v0.3.5).

* `ERPlot()`. Added this back but in revised form. It now directly compares the estimated flows for each rank with the observed for each rank.

* `GoF functions`: `GoFCompare()`, and `GoFComparePool()`. New goodness-of-fit functions: one for single samples and one for pooling groups. They are more straightforward to interpret than `Zdists()`. The `Zdists()` function is also very sensitive to slight differences in methodology. The one in WINFAP (based on FEH08) uses a theoretical L-kurtosis (calculated as a function of L-skewness) for comparison against the sampling distribution of L-kurtosis under the null hypothesis. This means that the Gumbel distribution can’t be compared. The `Zdists()` function in UKFE uses the weighted average (by sample size) L-kurtosis of the pooled AMAX  (see the associated details) – so that Gumbel can be included. For such reasons, I thought it would be good to have a more straightforward comparison, without worrying about hypothesis testing (hence them being called `GoFCompare`). The new `GoFCompare()` function simply calculates the RMSE of the ordered observed AMAX against mean simulated ordered AMAX (500 simulations) and provides the RMSE as a percentage of the AMAX mean (the lowest across the distributions is the best fit). The `GoFComparePool()` version does the same but with the pooled AMAX (standardised into a single large sample).

## Minor changes

* Mann Kendall is now the default trend test in the `TrendTest()` function.

* Error catching. Added some sensible error messages here and there where necessary. 

## Bug fix

* `QMED()` function for calculating QMED from catchment descriptors. The donor adjustment wasn’t working properly when the CDs were added individually (as opposed to using a `CDs` object).


# UKFE 0.3.5

## Major changes

* Kappa3 functionality. Better embedded the Kappa3 distribution in general. It can now be used with the other functions which take the `dist` argument, such as `PoolEst()`, `OptimPars()` and `SimData()`. Also added three functions which mirror those for the other distributions: `Kappa3AM()`, `Kappa3Pars()`, `Kappa3Est()`. The `Kappa3GF()` function is still there and has not changed.

* `Zdists()` function has been updated and now Kappa3 and Gumbel have been added to the results. 

* Added a function called `POTt()`. This is a POT extraction function which works on time only (to determine the independence). The other `POTextract()` function can also have a time element (as well as return to a low threshold) to determine independence, but the new one is significantly faster. This is useful for very long time series such as 10,000 years of simulation at an hourly sampling rate (`POTextract()` would struggle).

* I have retired the `GoTF` functions but may add them back in improved form. I have also temporarily retired the `ERPlot()` function, but this will be back. Temporary retirement is expedient because it has a `GoTF` function within it which requires removal/replacement.

## Minor change

* `AddGauge()` function. There is now functionality to add a new AMAX with CDs etc to the group of sites suitable for pooling so that it can be used in the FEH pooling process. Remember that there is also the `LRatioChange()` function which allows you to update the stats within the pooling group that is already there (in case you have an extra year to add for example).

## Bug fix

* `H2()` function. This was providing verdicts of heterogeneity more often than it should. The error has been fixed.


# UKFE 0.3.4

## Major changes

* `Rainfall API` functions. Two new functions: One is `RainEA()` and the other is `RainSEPA()`. These can be used to get rainfall from rain gauges in Scotland and England using the Environment Agency and Scottish Environment Protection Agency APIs.

* `Zdists()` update. The zdist for assessing the distribution fit was based on the FEH99 method. I've updated this to the FEH2008 method.

## Bug fix

* Fixed discordancy logic bug in the `Pool()` function. There is a column in the pooling group which states if a site is over the threshold for discordancy and is therefore considered discordant. When the pooling group had more than 15 catchments, this didn't work and all sites were set as FALSE (not discordant) no matter what the discordance score was. This has been fixed.


# UKFE 0.3.3

## Minor changes
* The `QMED()` function now provides the weightings when two donors are used. 

* The `PoolEst()` function now provides a list with four elements. First is the results, second is the pooled LCV and LSKEW, third is the distribution parameters for the growth curve, and fourth is the distribution parameters for the frequency curve. 

* Changed the `ReFH()` function to output a list with two data frames: one for the parameters, and one for the results. (As opposed to a print out of parameters and a data frame of results.)

* Removed erroneous warning messages from the functions `AggDayHour()`, `AMextract()`, and `MonthlyStats()`. 

* Added Easting and Northing into `QMEDData` and `NRFAData` which were missing for a couple of Northern Ireland catchments.


# UKFE 0.3.2

## Major change

* Updated to use the NRFA Peak Flow Dataset version 12.1.


# UKFE 0.3.1

## Minor change and bug fix

* `NonFloodAdjPool()` function. Added a useful message for when there are no sites in the pooling group that meet the non-flood criteria (before it was failing without a clear reason why). Also added an argument called `ReturnStats`. If this is set to `TRUE`, rather than providing an adjusted pooling group it returns the number of years, the number of non-flood years and the percentage of non-flood years, for each station.


# UKFE 0.3.0

## Major changes

* Updated to use the NRFA Peak Flow Dataset version 12. 

* Added new function called `AggDayHour()`, which allows you to aggregate a data frame based on the date time column. For example, you might have a time series with a 15-minute sampling rate and you want it at a daily or hourly rate (you can choose the number of hours).


# UKFE 0.2.9

## Major changes

* The `NonFloodAdj()` function has replaced the `PermAdj()` function and now provides the percentage of non-flood years as well as the adjusted L-CV and L-skewness (the result is now a list with two data frames).

* Added new function called `NonFloodAdjPool()`. This allows non-flood adjustment to pooling groups. You can update all sites, individual sites (one or more user-selected ones), or all sites above a given percentage of non-flood years. 

* The `DonUrbAdj` argument has been added to the `QMED()` function. This allows you to de-urbanise the QMEDcd estimate of the donor/s. It's for the unlikely and usually not recommended case where the subject site is rural and the donor is urban. 

## Bug fix

* Bug fixes for the `AMextract()` function and the `DDF99Pars()` function.


# UKFE 0.2.8

## Major changes

* The `DDF13Import()` function has been replaced with `DDFImport()`, given that the FEH22 rainfall depth duration frequency results have been made available for the peak flow sites as part of the NRFA update. The function now has the option of importing the 2013 or the 2022 version of the DDF model. 

## Minor change

* Updated the `Pool()` and `PoolSmall()` functions so that you can use an `UrbMax` argument to increase the URBEXT2000 level at which gauges are excluded from the pooling group. The default is 0.03. This is particularly useful for users in Wales where the guidance suggests an URBEXT2000 of 0.3. The `DeUrb` argument is also available which de-urbanises every site with URBEXT2000 above 0.03. 


# UKFE 0.2.7

## Major change

* Removed the `BivarSim()` function as it needs updating.

## Minor change

* `POTextract()` function. The user can now set the `div` argument the same as the threshold. This essentially reverts the function to a time-based separation, albeit a slow one. 


# UKFE 0.2.6

## Major changes

* Added a function called `MonthlyStats()` which can provide statistics such as mean flow or sum of rainfall for each month.

* Added a function called `BivarSim()`. This simulates bivariate extremes above a threshold using a Gaussian copula approach. See function details but also the multivariate extremes section at the bottom of this webpage: https://www.floodhydrostats.com/howitworks

* Added a function called `UEF()`, which is the Urban Expansion Factor.

## Minor changes

* Updated the `POTextract()` function as follows:
  * You can now change the y-axis label
  * You can now add a time element to the declustering
  * The `div` argument is now a percentile as opposed to an absolute value
  * Added better error messaging.

* Updated the `AMextract()` function as follows:
  * Calendar year is now an option. 
  * By default it now truncates the data to avoid partial years. 
  * Added better error messaging.
  
* Updated the `TrendTest()` function so that alternative hypotheses can be used. It was a two.sided test (still is by default), but now you can test for positive or negative trend specifically by using the `alternative` argument. See function details. 

## Bug fixes
* `QMED()` function. There was an error when a single donor was applied.

* `POTextract()` function.


# UKFE 0.2.5


# UKFE 0.2.4

## Major changes

* `ImportAM()` is now `AMImport()`. It's assumed this will make it easier to find. 

* `CDsXML()`. New function to read in catchment descriptors from XML files. 

* The `ImportCDs()` function has been removed because it's based on the .cd3 files which are being phased out. 

* `PoolSmall()`. New function for making pooling groups for small catchments. 

* `DDFImport()`. New function which imports the DDF13 results data frame from an xml file (from NRFA peak flows or the FEH Web Service). There is a logical `TRUE/FALSE` argument to make the ARF adjustment for the data frame. This function takes the output of the `DDF13Import()` function as an input. It then allows the user to select a return period and duration to get a rainfall estimate. 

* `Kappa3GF()`. New function to derive growth factors using the Kappa3 distribution as a function of L-CV and L-skewness. This hasn't been fully embedded in other functions such as `PoolEst()`, `Zdists()` and `EVPlot()`. It can however be used with the outputs of the L-CV and L-skewness from the `PoolEst()` function - or with any estimate of L-CV and L-skewness for that matter. 

## Minor changes

* `AMextract()` now has an argument called `func`. This allows the user to extract other statistics from the hydrological years, such as mean flow, or sum of precipitation. Any base function which is applied on a numeric vector with no further arguments can be used. 

* `Uncertainty()`. This function has been updated to make the estimation in ungauged catchments more flexible. The user can input the `QMEDfse` rather than stating number of donors (it has a default of 1.46). 

* `HydroPlot()`. This function has been updated so that the user can choose to return the data being plotted in a data frame. The function allows the choice of the date / date-time range to plot; this can be returned as a separate data frame.

## Bug fixes

* `SCF()` bug fix.


# UKFE 0.2.3


# UKFE 0.2.2

## Major changes

* Added the Gumbel distribution and associated functions.

* Added a function to add lines or points to the EV plot: `EVPlotAdd()`

## Minor changes

* Updated the `Uncertainty()` function based on Hammond (2021) ("Hammond, A. (2021). Sampling uncertainty of UK design flood estimation. Hydrology Research. 1357-1371. 52 (6)").

* Included uncertainty as a default for pooled estimates (for the `QuickResults()` function and the `PoolEst()` function; for the latter you can choose the QMED fse).

* `EVPlot()`: Improved the EV plot formatting and it now includes 95% confidence intervals.

* Added numerous error messages for better user information when things go wrong.

## Bug fixes

* Some bug fixing in the `OptimPars()`, `DesHydro()`, and `HydroPlot()` functions.


# UKFE 0.2.1


# UKFE 0.2.0

## Minor changes and bug fixes

* Corrected some grid referencing for the Northern Ireland gauge locations. 

* Further improved the `ImportAM()` function.


# UKFE 0.1.9


# UKFE 0.1.8

## Minor changes

* Updated the `ImportAM()` and `ImportCDs()` functions to make them more robust. 

* Added some better error messaging to the `GetCDs()` and the `Pool()` functions.


# UKFE 0.1.7

## Minor changes and bug fixes

* Corrected the `QMEDLink()` function.

* Made the `ImportCDs()` function work with both FEH Web Service and NRFA Peak Flow Dataset cd3 files without the need for the user to specify.

* Added a nice failure message for the `GetCDs()` function when an unknown gauge ID is entered.

* Added an option to avoid plotting when using the `BFI()` function.


# UKFE 0.1.6


# UKFE 0.1.4


# UKFE 0.1.3


# UKFE 0.1.2


# UKFE 0.1.1


# UKFE 0.1.0
