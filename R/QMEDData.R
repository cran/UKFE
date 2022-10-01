#' National River Flow Archive descriptors and calculated statistics for sites suitable for QMED & pooling
#'
#' A data.frame of catchment & data descriptors relating to the median annual maximum flow (QMED). NRFA Peak Flow Dataset - Version 11
#'
#' @details The functions for QMED estimation and retreieval of catchment descriptors rely on this dataframe. However, the data frame is open for manipulation in case the user wishes to add sites that aren't included, or change parts where local knowledge has improved on the data. If changes are made, they will only remain within the workspace. If a new workspace is opened and the UKFE package is loaded, the data frame will have returned to it's original state.
#'
#' @format A data frame with 888 rows and 26 variables
#' @source \url{https://nrfa.ceh.ac.uk/peak-flow-dataset}
"QMEDData"
