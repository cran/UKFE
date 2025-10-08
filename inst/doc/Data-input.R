## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Load the package
library(UKFE)

## -----------------------------------------------------------------------------
# Save the 'QMEDData' data frame within the UKFE package to an object within your R 
# environment
QMEDData <- QMEDData

# View the first rows of the data in the console
head(QMEDData)

## ----fig.alt="Bar chart of annual maximum river flow. The x-axis shows years, and the y-axis shows peak flow in cubic meters per second. Each bar represents the highest flow in that year. The flows vary from year to year, with several notably high peaks in recent years."----
# Extract the AMAX data for NRFA site 55002 and save to an object called 'AM.55002'
AM.55002 <- GetAM(55002)

# View the head of the AMAX series
head(AM.55002)

# Plot the AMAX data
AMplot(AM.55002)

## -----------------------------------------------------------------------------
# Extract and view catchment descriptors for NRFA gauge 39001
GetCDs(39001)

## -----------------------------------------------------------------------------
# Extract catchment descriptors for NRFA gauge 39001 and store in an object called 
# 'CDs.39001'
CDs.39001 <- GetCDs(39001)

## ----eval = FALSE-------------------------------------------------------------
#  # Extract catchment descriptors from an xml file and store in an object called
#  # 'CDs.MySite'
#  CDs.MySite <- CDsXML("C:/Data/FEH_Catchment_384200_458200.xml")
#  
#  # As above but retaining backslashes in the file path
#  CDs.MySite <- CDsXML(r"{C:\Data\FEH_Catchment_384200_458200.xml}")

## ----eval = FALSE-------------------------------------------------------------
#  # Extract catchment descriptors from an xml file and store in an object called
#  # 'CDs.27003'
#  CDs.27003 <- CDsXML("C:\Data\NRFAPeakFlow_v13-0-2\suitable-for-neither\027003.xml")

