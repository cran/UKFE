# ConvertGridRef ---------------------------------------------------

#' Convert between British National Grid Reference (BNG) and Latitude and Longitude or Irish Grid references.
#'
#'@description Function to convert between BNG easting & northing and Latitude & Longitude (or vice versa). Or to convert between BNG and Irish national grid (or vice versa)
#'@details To convert to Lat and Lon from BNG, ensure that the fromBNG argument is TRUE. To convert the other way, set fromBNG as FALSE. The same applies for converting between Irish grid and BNG. To convert Irish grid and BNG set the IGorLatLon argument to IG.
#'@param x A vector of length 2. Either latitude and longitude (if fromBNG = FALSE) or BNG easting and northing (if fromBNG = TRUE). Or Irish easting and northing if IGorLatLon is set to IG and fromBNG = FALSE.
#'@param fromBNG A logical argument with a default of TRUE. When TRUE it converts from BNG easting and northing to latitude and longitude (or to IG easting and northing if IGorLatLon is set to "IG"). When FALSE it converts the other way round.
#'@param IGorLatLon This argument allows you to choose between Latitude & Longitude and Irish grid reference. The acceptable options are "LatLon" or "IG". If you choose "IG" you are converting between BNG and IG. If you choose "LatLon", you are converting between BNG and Lat Lon.
#'@examples
#'#Get Latitude and Longitude for a BNG numeric reference.
#'ConvertGridRef(c(462899, 187850))
#'#Now we'll get easting and northing as a function of latitude and longitude
#'ConvertGridRef(c(51.6, -1), fromBNG = FALSE)
#'@return A data.frame with the converted grid references. Either latitude and longitude if BNG = TRUE. Or BNG easting and northing if fromBNG = FALSE. Or, IG easting & northing if fromBNG = TRUE and IGorLatLon = "IG".
#'@author Anthony Hammond


ConvertGridRef <- function(x, fromBNG = TRUE, IGorLatLon = "LatLon") {
  if(IGorLatLon != "LatLon" & IGorLatLon != "IG") stop("IGorLatLon argument must be either LatLon or IG")
  if(IGorLatLon == "LatLon") {
  if(length(x) != 2) stop("x must be a vector of length 2.")
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
  return(ResultReturn)}

  if(IGorLatLon == "IG") {
    if(x[1] < 181) stop("your grid reference seems rather low. Is the IGorLatLon argument correct?")
    if(x[2] < 181) stop("your grid reference seems rather low. Is the IGorLatLon argument correct?")


    easting <- x[1]
    northing <- x[2]
    # Define the coordinate reference systems for Irish Grid and British Grid
    crs_ig <- st_crs("+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    crs_bg <- st_crs("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894 +units=m +no_defs")

    # Create a point geometry with Irish Grid coordinates

    if(fromBNG == TRUE){point <- st_sfc(st_point(c(easting, northing)), crs = crs_bg)}
    if(fromBNG == FALSE){point <- st_sfc(st_point(c(easting, northing)), crs = crs_ig)}


    # Transform the coordinates to British Grid
    if(fromBNG == TRUE) {point_shp <- st_transform(point, crs_ig)}
    if(fromBNG == FALSE) {point_shp <- st_transform(point, crs_bg)}

    # Extract the coordinates and return them as a data frame
    coords <- st_coordinates(point_shp)
    Result <- data.frame(easting = coords[, "X"], northing = coords[, "Y"])
    if(fromBNG == TRUE) {rownames(Result) <- "IrishGrid"}
    if(fromBNG == FALSE) {rownames(Result) <- "BritishGrid"}
    return(Result)
  }



}
