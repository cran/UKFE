# GridRefConvert ---------------------------------------------------

#' Convert between British National Grid Reference (BNG) and Latitude and Longitude.
#'
#'@description Function to convert between BNG easting & northing and Latitude & Longitude (or vice versa).
#'@param x A vector of length 2. Either latitude and longitude (if fromBNG = FALSE) or easting and northing (if fromBNG = TRUE).
#'@param fromBNG A logical argument with a default of TRUE. When TRUE it converts from easting and northing to latitude and longitude. When FALSE it converts from latitude and longitude to easting and northing.
#'@examples
#'#Get Latitude and Longitude for a BNG numeric reference.
#'GridRefConvert(c(462899, 187850))
#'#Now we'll get easting and northing as a function of latitude and longitude
#'GridRefConvert(c(51.6, -1), fromBNG = FALSE)
#'@return A data.frame with the converted grid references. Either latitude and longitude if BNG = TRUE. Or easting and northing if fromBNG = FALSE.
#'@author Anthony Hammond


GridRefConvert <- function(x, fromBNG = TRUE) {
  if(fromBNG == FALSE) {
    if(x[1] > 61 | x[1] < 49.94) stop("Grid reference appears to be outside the UK")
    if(x[1] > 1000) stop("your latitude seems rather high. Is the BNG argument correct?")
    if(x[2] > 1.87 | x[2] < -8.5) stop("Grid reference appears to be outside the UK")

    latLonToBNG <- function(lat, lon) {
      # Create a simple feature object from the latitude and longitude
      pts <- st_sfc(st_point(c(lon, lat)), crs = 4326)

      # Transform the coordinates to British National Grid (EPSG: 27700)
      pts_bng <- st_transform(pts, 27700)

      # Extract the easting and northing
      coords <- st_coordinates(pts_bng)

      return(list(easting = coords[1], northing = coords[2]))
    }
    Result <- latLonToBNG(x[1], x[2])

  }

  if(fromBNG == TRUE) {
    if(x[1] > 695000 | x[1] < -20570) stop("Grid reference appears to be outside the UK")
    if(x[1] < 1000) stop("your easting seems rather low. Is the BNG argument correct?")
    if(x[2] > 1200000 | x[2] < 0) stop("Grid reference appears to be outside the UK")

    bngToLatLon <- function(easting, northing) {
      # Create a simple feature object from the easting and northing
      pts <- st_sfc(st_point(c(easting, northing)), crs = 27700)

      # Transform the coordinates to WGS 84 (EPSG: 4326)
      pts_latlon <- st_transform(pts, 4326)

      # Extract the latitude and longitude
      coords <- st_coordinates(pts_latlon)

      return(list(latitude = coords[2], longitude = coords[1]))
    }
    Result <- bngToLatLon(x[1], x[2])
  }

  ResultReturn <- data.frame(Result[[1]], Result[[2]])
  colnames(ResultReturn) <- names(Result)
  return(ResultReturn)
}
