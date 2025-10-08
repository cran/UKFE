# Load libraries
library(testthat)
library(lmomco)
library(evd)
library(nsRFA)

##### Data Functions #####

###### AMImport ######
testthat::test_that("Check that AMImport correctly processes data with multiple reject years", {
  expected_output <- data.frame(
    Date = as.Date(
      c(
        "1953-04-03", "1954-08-20", "1955-03-27", "1956-01-26",
        "1956-12-29", "1957-11-05", "1959-01-22", "1960-01-25",
        "1960-12-04", "1962-08-17", "1962-12-20", "1963-11-19",
        "1965-09-09", "1965-12-19", "1967-05-15", "1968-01-14",
        "1969-05-06", "1970-08-21", "1971-01-24", "1972-09-09",
        "1972-12-07", "1974-02-15", "1974-11-21", "1976-09-26",
        "1977-06-14", "1978-01-28", "1979-02-02", "1979-12-28",
        "1981-03-11", "1981-12-31", "1983-04-21", "1983-12-20",
        "1985-04-08", "1987-06-19", "1988-01-24", "1989-02-24",
        "1990-02-07", "1991-01-10", "1992-05-29", "1992-10-02"
      ),
      format = "%Y-%m-%d"
    ),
    Flow = c(
      11.716, 11.412, 52.200, 10.081, 14.747, 16.793, 17.143, 48.987, 21.552,
      12.645, 8.951, 10.518, 14.581, 20.415, 16.100, 21.552, 20.415, 31.600,
      14.252, 13.925, 17.496, 10.010, 10.100, 19.400, 18.517, 15.864, 20.959,
      24.143, 17.449, 45.455, 16.897, 15.732, 16.806, 20.156, 22.870, 19.597,
      21.399, 16.090, 19.673, 52.086
    )
  )

  actual_output <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))[1:40, ]

  testthat::expect_equal(actual_output$Date, expected_output$Date)
  testthat::expect_equal(actual_output$Flow, expected_output$Flow)
})

testthat::test_that("Check that AMImport correctly processes data with one reject year", {
  expected_output <- data.frame(
    Date = as.Date(c(
      "1971-04-25", "1972-02-04", "1973-07-17", "1974-02-12", "1974-11-21",
      "1976-09-25", "1977-02-25", "1978-02-04", "1979-04-08", "1980-03-19",
      "1981-04-27", "1982-06-23", "1983-05-02", "1984-03-24", "1985-04-12",
      "1986-05-20", "1987-04-08", "1987-10-21", "1989-04-05", "1990-01-28",
      "1990-10-18", "1992-09-26", "1993-06-11", "1993-10-07", "1995-01-28",
      "1996-02-13", "1996-12-20", "1998-06-02", "1998-10-27", "2000-04-04",
      "2000-11-07", "2001-10-26", "2002-12-30", "2004-08-24", "2004-10-21",
      "2005-11-09", "2007-06-26", "2008-01-21", "2009-06-11", "2010-01-16",
      "2011-02-26", "2012-04-30", "2012-11-25", "2014-02-11", "2015-01-01",
      "2016-03-28", "2016-11-22", "2018-04-02", "2019-09-29", "2019-11-08",
      "2021-02-07", "2022-08-16", "2022-11-18"
    )),
    Flow = c(
      7.330, 5.040, 8.760, 2.530, 3.110, 0.769, 9.510, 2.680, 9.670, 9.350,
      7.890, 10.700, 11.200, 5.600, 6.540, 8.690, 9.630, 5.880, 3.660, 6.100,
      2.970, 2.050, 2.590, 6.200, 5.310, 1.810, 5.450, 4.610, 4.240, 5.370,
      10.400, 3.610, 6.260, 4.390, 2.390, 2.870, 17.200, 5.490, 9.160, 5.160,
      2.530, 6.950, 9.830, 3.640, 2.920, 5.200, 6.190, 8.850, 5.668, 16.927,
      8.722, 7.816, 9.380
    )
  )

  actual_output <- AMImport(testthat::test_path("testdata", "One_reject-028049.am"))[1:53, ]

  testthat::expect_equal(actual_output$Date, expected_output$Date)
  testthat::expect_equal(actual_output$Flow, expected_output$Flow)
})

testthat::test_that("Check that AMImport correctly processes data with no reject years", {
  expected_output <- data.frame(
    Date = as.Date(c(
      "1977-02-13", "1978-02-07", "1978-12-31", "1980-02-10", "1981-03-13",
      "1982-01-05", "1983-05-08", "1984-02-04", "1985-01-29", "1986-01-14",
      "1987-04-10", "1988-02-01", "1989-04-13", "1990-02-13", "1991-07-13",
      "1992-01-08", "1993-01-13", "1993-10-15", "1995-01-31", "1995-12-22",
      "1997-08-31", "1998-01-05", "1999-03-13", "2000-04-26", "2000-11-11",
      "2002-08-09", "2003-01-01", "2004-08-13", "2005-07-14", "2006-08-17",
      "2007-06-25", "2008-01-21", "2008-11-09", "2010-03-01", "2011-07-21",
      "2012-07-18", "2012-12-24", "2014-02-01", "2015-08-14", "2016-03-09",
      "2016-11-21", "2018-06-02", "2019-06-13", "2019-11-11", "2021-07-30",
      "2022-08-16", "2022-11-24"
    )),
    Flow = c(
      1.220, 0.751, 0.917, 0.889, 0.911, 0.694, 0.604, 0.872, 0.473, 0.412,
      0.929, 0.653, 0.226, 0.330, 0.314, 0.341, 0.541, 0.633, 0.751, 0.223,
      0.334, 1.195, 0.699, 0.464, 0.998, 0.828, 1.052, 0.518, 0.486, 0.276,
      5.337, 0.923, 0.504, 0.653, 0.420, 1.695, 1.259, 0.623, 0.395, 0.509,
      0.395, 1.351, 0.608, 2.055, 1.214, 0.643, 0.513
    )
  )

  actual_output <- AMImport(testthat::test_path("testdata", "No_reject-030013.am"))[1:47, ]

  testthat::expect_equal(actual_output$Date, expected_output$Date)
  testthat::expect_equal(actual_output$Flow, expected_output$Flow)
})

testthat::test_that("Check that AMImport correctly processes data with the new date format", {
  expected_output <- data.frame(
    Date = as.Date(c(
      "1977-02-13", "1978-02-07", "1978-12-31", "1980-02-10", "1981-03-13",
      "1982-01-05", "1983-05-08", "1984-02-04", "1985-01-29", "1986-01-14",
      "1987-04-10", "1988-02-01", "1989-04-13", "1990-02-13", "1991-07-13",
      "1992-01-08", "1993-01-13", "1993-10-15", "1995-01-31", "1995-12-22",
      "1997-08-31", "1998-01-05", "1999-03-13", "2000-04-26", "2000-11-11",
      "2002-08-09", "2003-01-01", "2004-08-13", "2005-07-14", "2006-08-17",
      "2007-06-25", "2008-01-21", "2008-11-09", "2010-03-01", "2011-07-21",
      "2012-07-18", "2012-12-24", "2014-02-01", "2015-08-14", "2016-03-09",
      "2016-11-21", "2018-06-02", "2019-06-13", "2019-11-11", "2021-07-30",
      "2022-08-16", "2022-11-24"
    )),
    Flow = c(
      1.220, 0.751, 0.917, 0.889, 0.911, 0.694, 0.604, 0.872, 0.473, 0.412,
      0.929, 0.653, 0.226, 0.330, 0.314, 0.341, 0.541, 0.633, 0.751, 0.223,
      0.334, 1.195, 0.699, 0.464, 0.998, 0.828, 1.052, 0.518, 0.486, 0.276,
      5.337, 0.923, 0.504, 0.653, 0.420, 1.695, 1.259, 0.623, 0.395, 0.509,
      0.395, 1.351, 0.608, 2.055, 1.214, 0.643, 0.513
    )
  )

  actual_output <- AMImport(testthat::test_path("testdata", "No_reject-030013.am"))[1:47, ]

  testthat::expect_equal(actual_output$Date, expected_output$Date)
  testthat::expect_equal(actual_output$Flow, expected_output$Flow)
})

###### CDsXML ######

testthat::test_that("Check that CDsXML correctly processes data from a NRFA peakValues xml file.", {
  expected_output <- data.frame(
    Descriptor = c(
      "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR",
      "DPSBAR", "FARL", "FPEXT", "LDP", "PROPWET", "RMED-1H", "RMED-1D",
      "RMED-2D", "SAAR", "SAAR4170", "SPRHOST", "URBEXT2000", "Easting",
      "Northing", "URBEXT1990", "BFIHOST"
    ),
    Value = c(
      31.5100, 196.0000, 219.0000, 0.3800, 0.3180, 9.6000, 109.0000,
      1.0000, 0.0529, 15.4800, 0.6000, 11.0000, 43.1000, 57.8000,
      1361.0000, 1371.0000, 49.4200, 0.0000, 356444.0000, 444791.0000,
      0.0000, 0.3190
    )
  )

  actual_output <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))

  testthat::expect_equal(actual_output$Descriptor, expected_output$Descriptor)
  testthat::expect_equal(actual_output$Value, expected_output$Value)
})

testthat::test_that("Check that CDsXML correctly processes dummy data to replicate data from an FEH webserver xml.", {
  expected_output <- data.frame(
    Descriptor = c(
      "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR",
      "DPSBAR", "FARL", "FPEXT", "LDP", "PROPWET", "RMED-1H", "RMED-1D",
      "RMED-2D", "SAAR", "SAAR4170", "SPRHOST", "URBEXT2000", "Easting",
      "Northing", "URBEXT1990", "BFIHOST"
    ),
    Value = c(
      84.6925, 359.0000, 155.0000, 0.2000, 0.3650, 13.800, 222.5000,
      0.9260, 0.083, 24.8800, 0.7500, 11.3000, 73.0000, 95.5000,
      2520.0000, 2720.0000, 48.4600, 0.00050, 271900.0000, 357000.0000,
      0.00210, 0.3980
    )
  )

  actual_output <- CDsXML(testthat::test_path("testdata", "FEH_Catchment_Descriptors_Scenario4.xml"))

  testthat::expect_equal(actual_output$Descriptor, expected_output$Descriptor)
  testthat::expect_equal(actual_output$Value, expected_output$Value)
})

testthat::test_that("Check that CDsXML correctly processes data when BFIHOST19 is missing.", {
  expected_output <- data.frame(
    Descriptor = c(
      "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR",
      "DPSBAR", "FARL", "FPEXT", "LDP", "PROPWET", "RMED-1H", "RMED-1D",
      "RMED-2D", "SAAR", "SAAR4170", "SPRHOST", "URBEXT2000", "Easting",
      "Northing", "URBEXT1990", "BFIHOST"
    ),
    Value = c(
      84.6925, 359.0000, 155.0000, 0.2000, 0.3980, 13.800, 222.5000,
      0.9260, 0.083, 24.8800, 0.7500, 11.3000, 73.0000, 95.5000,
      2520.0000, 2720.0000, 48.4600, 0.00050, 271900.0000, 357000.0000,
      0.00210, 0.3980
    )
  )

  actual_output <- CDsXML(testthat::test_path("testdata", "FEH_Catchment_Descriptors_Scenario4_no_BFIHOST19.xml"))

  testthat::expect_equal(actual_output$Descriptor, expected_output$Descriptor)
  testthat::expect_equal(actual_output$Value, expected_output$Value)
})

testthat::test_that("Check that CDsXML correctly processes data when a catchment descriptor is missing.", {
  expected_output <- data.frame(
    Descriptor = c(
      "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR",
      "DPSBAR", "FARL", "FPEXT", "LDP", "PROPWET", "RMED-1H", "RMED-1D",
      "RMED-2D", "SAAR", "SAAR4170", "SPRHOST", "URBEXT2000", "Easting",
      "Northing", "URBEXT1990", "BFIHOST"
    ),
    Value = c(
      31.5100, 196.0000, 219.0000, 0.3800, 0.3180, 9.6000, 109.0000,
      NA, 0.0529, 15.4800, 0.6000, 11.0000, 43.1000, 57.8000,
      1361.0000, 1371.0000, 49.4200, 0.0000, 356444.0000, 444791.0000,
      0.0000, 0.3190
    )
  )

  actual_output <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6-missing-FARL.xml"))

  testthat::expect_equal(actual_output$Descriptor, expected_output$Descriptor)
  testthat::expect_equal(actual_output$Value, expected_output$Value)
})

###### DDFImport ######

testthat::test_that("Check that DDFImport correctly processes data for 2022 results", {
  # Define the row names (first column)
  row_names <- c(2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000, 1800, 3100, 5600, 10000)

  # Define the column names (first row)
  col_names <- c(0.25, 0.5, 0.75, 1, 2, 4, 6, 12, 18, 24, 48, 96, 192, 240)

  # Define the matrix data
  data <- c(
    8.1, 10.4, 11.8, 12.9, 17.4, 22.3, 25.4, 31.0, 34.5, 37.2, 44.4, 54.8, 72.7, 81.3,
    10.3, 13.2, 15.0, 16.3, 21.1, 26.3, 29.6, 35.6, 39.3, 42.2, 50.0, 61.5, 81.2, 90.6,
    13.7, 17.6, 20.0, 21.8, 26.9, 32.5, 36.0, 42.6, 46.7, 49.9, 58.7, 71.6, 93.9, 104.7,
    16.1, 20.7, 23.6, 25.7, 31.1, 36.9, 40.7, 47.6, 52.0, 55.4, 64.9, 78.8, 102.8, 114.5,
    18.8, 24.4, 27.8, 30.3, 35.9, 42.1, 46.1, 53.5, 58.3, 62.0, 72.2, 87.1, 113.0, 125.5,
    21.4, 27.7, 31.7, 34.6, 40.5, 47.1, 51.3, 59.3, 64.4, 68.4, 79.4, 95.0, 122.4, 135.7,
    24.2, 31.6, 36.1, 39.5, 45.7, 52.8, 57.4, 66.1, 71.7, 76.0, 87.8, 104.2, 132.8, 146.9,
    27.1, 35.6, 40.7, 44.5, 51.1, 58.8, 63.9, 73.5, 79.7, 84.5, 97.0, 113.8, 143.2, 158.1,
    30.2, 39.7, 45.5, 49.8, 57.1, 65.6, 71.3, 82.1, 89.1, 94.4, 107.6, 124.5, 154.2, 169.6,
    33.1, 43.9, 50.5, 55.2, 63.2, 72.7, 79.1, 91.4, 99.2, 104.9, 118.8, 135.4, 164.9, 180.6,
    37.0, 49.1, 56.5, 61.8, 70.8, 81.8, 89.1, 103.1, 111.7, 117.9, 132.3, 148.5, 177.0, 192.9,
    41.0, 54.7, 63.1, 69.0, 79.4, 92.1, 100.5, 115.9, 125.3, 131.9, 146.7, 162.6, 189.7, 205.5,
    45.8, 61.4, 70.8, 77.5, 89.8, 104.3, 113.5, 130.1, 140.0, 146.4, 162.0, 177.8, 203.3, 218.7,
    51.0, 68.5, 79.2, 86.8, 100.8, 116.6, 126.4, 143.5, 153.4, 160.4, 176.5, 192.6, 216.7, 231.6,
    57.6, 77.8, 90.0, 98.6, 114.1, 130.8, 141.0, 158.6, 168.7, 175.9, 192.6, 209.0, 232.2, 246.2,
    64.7, 87.7, 101.6, 111.4, 127.8, 145.1, 155.5, 173.5, 183.9, 191.3, 208.4, 225.4, 248.0, 261.2
  )

  # Create the matrix
  matrix_data <- matrix(data, nrow = length(row_names), byrow = TRUE)

  # Assign row and column names
  rownames(matrix_data) <- row_names
  colnames(matrix_data) <- col_names

  expected_output <- matrix_data

  actual_output <- DDFImport(testthat::test_path("testdata", "028115-maun-at-mansfield-the-dykes.xml"), Plot = FALSE, DDFVersion = 22)

  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that("Check that DDFImport correctly processes data for 2013 results", {
  # Define the row names (first column)
  row_names <- c(2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000, 1800, 3100, 5600, 10000)

  # Define the column names (first row)
  col_names <- c(0.25, 0.5, 0.75, 1, 2, 4, 6, 12, 18, 24, 48, 96, 192, 240)

  # Define the matrix data
  data <- c(
    7.6, 9.7, 11.1, 12.1, 16.4, 21.4, 24.6, 30.6, 34.3, 37.1, 44.6, 54.9, 72.6, 81.0,
    9.6, 12.3, 14.0, 15.2, 19.9, 25.2, 28.6, 35.0, 39.0, 41.9, 50.0, 61.2, 80.5, 89.8,
    12.7, 16.3, 18.5, 20.2, 25.4, 31.3, 35.0, 42.0, 46.3, 49.6, 58.3, 70.8, 92.6, 103.0,
    15.0, 19.3, 22.0, 23.9, 29.5, 35.7, 39.8, 47.2, 51.8, 55.2, 64.5, 77.7, 101.0, 112.3,
    17.6, 22.9, 26.0, 28.4, 34.4, 41.2, 45.6, 53.7, 58.5, 62.1, 71.8, 85.8, 110.7, 122.9,
    20.2, 26.2, 30.0, 32.7, 39.3, 46.8, 51.6, 60.3, 65.5, 69.2, 79.2, 93.6, 119.9, 132.8,
    23.2, 30.3, 34.7, 37.9, 45.2, 53.8, 59.2, 68.7, 74.2, 78.0, 88.2, 102.9, 130.2, 143.8,
    26.6, 34.9, 39.9, 43.6, 52.0, 61.8, 67.9, 78.3, 84.1, 88.1, 98.3, 112.9, 140.9, 155.1,
    30.4, 40.1, 45.9, 50.2, 60.0, 71.2, 78.1, 89.4, 95.6, 99.7, 109.9, 124.3, 152.5, 167.1,
    34.5, 45.7, 52.5, 57.4, 68.6, 81.1, 88.5, 100.5, 107.1, 111.4, 121.8, 136.0, 164.0, 178.9,
    39.7, 52.8, 60.7, 66.4, 79.1, 92.7, 100.6, 113.3, 120.1, 124.7, 135.4, 149.8, 177.7, 192.6,
    45.4, 60.6, 69.9, 76.4, 90.1, 104.6, 112.8, 126.1, 133.1, 137.9, 149.3, 164.3, 192.1, 207.0,
    51.6, 69.1, 79.8, 87.3, 101.8, 116.9, 125.5, 139.2, 146.5, 151.5, 163.5, 179.3, 207.7, 222.7,
    57.5, 77.3, 89.4, 97.9, 113.0, 128.5, 137.3, 151.4, 159.0, 164.2, 176.8, 193.5, 222.9, 238.1,
    64.1, 86.7, 100.2, 109.8, 125.3, 141.2, 150.3, 164.7, 172.6, 178.0, 191.3, 209.0, 240.0, 255.6,
    70.7, 95.8, 111.0, 121.7, 137.5, 153.7, 163.0, 177.9, 186.1, 191.7, 205.6, 224.4, 257.1, 273.3
  )

  # Create the matrix
  matrix_data <- matrix(data, nrow = length(row_names), byrow = TRUE)

  # Assign row and column names
  rownames(matrix_data) <- row_names
  colnames(matrix_data) <- col_names

  expected_output <- matrix_data

  actual_output <- DDFImport(testthat::test_path("testdata", "028115-maun-at-mansfield-the-dykes.xml"), Plot = FALSE, DDFVersion = 13)

  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that("Ensure no error when Plot is TRUE", {
  testthat::expect_silent(DDFImport(
    testthat::test_path("testdata", "028115-maun-at-mansfield-the-dykes.xml"),
    Plot = TRUE,
    DDFVersion = 22
  ))
})

testthat::test_that("Check that the ARF is applied correctly", {
  library(UKFE)

  # Define the row names (first column)
  row_names <- c(2, 3, 6, 10, 18, 31, 56, 100, 180, 310, 560, 1000, 1800, 3100, 5600, 10000)

  # Define the column names (first row)
  col_names <- c(0.25, 0.5, 0.75, 1, 2, 4, 6, 12, 18, 24, 48, 96, 192, 240)

  # Define the matrix data
  data <- c(
    8.1, 10.4, 11.8, 12.9, 17.4, 22.3, 25.4, 31.0, 34.5, 37.2, 44.4, 54.8, 72.7, 81.3,
    10.3, 13.2, 15.0, 16.3, 21.1, 26.3, 29.6, 35.6, 39.3, 42.2, 50.0, 61.5, 81.2, 90.6,
    13.7, 17.6, 20.0, 21.8, 26.9, 32.5, 36.0, 42.6, 46.7, 49.9, 58.7, 71.6, 93.9, 104.7,
    16.1, 20.7, 23.6, 25.7, 31.1, 36.9, 40.7, 47.6, 52.0, 55.4, 64.9, 78.8, 102.8, 114.5,
    18.8, 24.4, 27.8, 30.3, 35.9, 42.1, 46.1, 53.5, 58.3, 62.0, 72.2, 87.1, 113.0, 125.5,
    21.4, 27.7, 31.7, 34.6, 40.5, 47.1, 51.3, 59.3, 64.4, 68.4, 79.4, 95.0, 122.4, 135.7,
    24.2, 31.6, 36.1, 39.5, 45.7, 52.8, 57.4, 66.1, 71.7, 76.0, 87.8, 104.2, 132.8, 146.9,
    27.1, 35.6, 40.7, 44.5, 51.1, 58.8, 63.9, 73.5, 79.7, 84.5, 97.0, 113.8, 143.2, 158.1,
    30.2, 39.7, 45.5, 49.8, 57.1, 65.6, 71.3, 82.1, 89.1, 94.4, 107.6, 124.5, 154.2, 169.6,
    33.1, 43.9, 50.5, 55.2, 63.2, 72.7, 79.1, 91.4, 99.2, 104.9, 118.8, 135.4, 164.9, 180.6,
    37.0, 49.1, 56.5, 61.8, 70.8, 81.8, 89.1, 103.1, 111.7, 117.9, 132.3, 148.5, 177.0, 192.9,
    41.0, 54.7, 63.1, 69.0, 79.4, 92.1, 100.5, 115.9, 125.3, 131.9, 146.7, 162.6, 189.7, 205.5,
    45.8, 61.4, 70.8, 77.5, 89.8, 104.3, 113.5, 130.1, 140.0, 146.4, 162.0, 177.8, 203.3, 218.7,
    51.0, 68.5, 79.2, 86.8, 100.8, 116.6, 126.4, 143.5, 153.4, 160.4, 176.5, 192.6, 216.7, 231.6,
    57.6, 77.8, 90.0, 98.6, 114.1, 130.8, 141.0, 158.6, 168.7, 175.9, 192.6, 209.0, 232.2, 246.2,
    64.7, 87.7, 101.6, 111.4, 127.8, 145.1, 155.5, 173.5, 183.9, 191.3, 208.4, 225.4, 248.0, 261.2
  )

  data <- matrix(data, nrow = 16, byrow = T)

  cds_28115 <- GetCDs(28115)
  area <- cds_28115$Value[cds_28115$Descriptor == "AREA"]

  adjusted_data <- do.call(cbind, lapply(seq_len(ncol(data)), function(i) {
    ARF(data[, i], area, col_names[i])
  }))

  adjusted_data <- round(adjusted_data, 1)

  # Assign row and column names
  rownames(adjusted_data) <- row_names
  colnames(adjusted_data) <- col_names

  expected_output <- adjusted_data

  actual_output <- DDFImport(testthat::test_path("testdata", "028115-maun-at-mansfield-the-dykes.xml"), ARF = TRUE, Plot = FALSE, DDFVersion = 22)

  testthat::expect_equal(actual_output, expected_output)
})

###### GetAM ######
testthat::test_that("Check that AMImport correctly processes data with multiple reject years", {
  expected_output <- data.frame(
    Date = as.Date(
      c(
        "1953-04-03", "1954-08-20", "1955-03-27", "1956-01-26",
        "1956-12-29", "1957-11-05", "1959-01-22", "1960-01-25",
        "1960-12-04", "1962-08-17", "1962-12-20", "1963-11-19",
        "1965-09-09", "1965-12-19", "1967-05-15", "1968-01-14",
        "1969-05-06", "1970-08-21", "1971-01-24", "1972-09-09",
        "1972-12-07", "1974-02-15", "1974-11-21", "1976-09-26",
        "1977-06-14", "1978-01-28", "1979-02-02", "1979-12-28",
        "1981-03-11", "1981-12-31", "1983-04-21", "1983-12-20",
        "1985-04-08", "1987-06-19", "1988-01-24", "1989-02-24",
        "1990-02-07", "1991-01-10", "1992-05-29", "1992-10-02"
      ),
      format = "%Y-%m-%d"
    ),
    Flow = c(
      11.716, 11.412, 52.200, 10.081, 14.747, 16.793, 17.143, 48.987, 21.552,
      12.645, 8.951, 10.518, 14.581, 20.415, 16.100, 21.552, 20.415, 31.600,
      14.252, 13.925, 17.496, 10.010, 10.100, 19.400, 18.517, 15.864, 20.959,
      24.143, 17.449, 45.455, 16.897, 15.732, 16.806, 20.156, 22.870, 19.597,
      21.399, 16.090, 19.673, 52.086
    )
  )

  actual_output <- GetAM(054906)[1:40, ]

  testthat::expect_equal(actual_output$Date, expected_output$Date)
  testthat::expect_equal(actual_output$Flow, expected_output$Flow)
})

testthat::test_that("Check that AMImport correctly processes data with one reject year", {
  expected_output <- data.frame(
    Date = as.Date(c(
      "1971-04-25", "1972-02-04", "1973-07-17", "1974-02-12", "1974-11-21",
      "1976-09-25", "1977-02-25", "1978-02-04", "1979-04-08", "1980-03-19",
      "1981-04-27", "1982-06-23", "1983-05-02", "1984-03-24", "1985-04-12",
      "1986-05-20", "1987-04-08", "1987-10-21", "1989-04-05", "1990-01-28",
      "1990-10-18", "1992-09-26", "1993-06-11", "1993-10-07", "1995-01-28",
      "1996-02-13", "1996-12-20", "1998-06-02", "1998-10-27", "2000-04-04",
      "2000-11-07", "2001-10-26", "2002-12-30", "2004-08-24", "2004-10-21",
      "2005-11-09", "2007-06-26", "2008-01-21", "2009-06-11", "2010-01-16",
      "2011-02-26", "2012-04-30", "2012-11-25", "2014-02-11", "2015-01-01",
      "2016-03-28", "2016-11-22", "2018-04-02", "2019-09-29", "2019-11-08",
      "2021-02-07", "2022-08-16", "2022-11-18"
    )),
    Flow = c(
      7.330, 5.040, 8.760, 2.530, 3.110, 0.769, 9.510, 2.680, 9.670, 9.350,
      7.890, 10.700, 11.200, 5.600, 6.540, 8.690, 9.630, 5.880, 3.660, 6.100,
      2.970, 2.050, 2.590, 6.200, 5.310, 1.810, 5.450, 4.610, 4.240, 5.370,
      10.400, 3.610, 6.260, 4.390, 2.390, 2.870, 17.200, 5.490, 9.160, 5.160,
      2.530, 6.950, 9.830, 3.640, 2.920, 5.200, 6.190, 8.850, 5.668, 16.927,
      8.722, 7.816, 9.380
    )
  )

  actual_output <- GetAM(28049)[1:53, ]

  testthat::expect_equal(actual_output$Date, expected_output$Date)
  testthat::expect_equal(actual_output$Flow, expected_output$Flow)
})

testthat::test_that("Check that AMImport correctly processes data with no reject years", {
  expected_output <- data.frame(
    Date = as.Date(c(
      "1977-02-13", "1978-02-07", "1978-12-31", "1980-02-10", "1981-03-13",
      "1982-01-05", "1983-05-08", "1984-02-04", "1985-01-29", "1986-01-14",
      "1987-04-10", "1988-02-01", "1989-04-13", "1990-02-13", "1991-07-13",
      "1992-01-08", "1993-01-13", "1993-10-15", "1995-01-31", "1995-12-22",
      "1997-08-31", "1998-01-05", "1999-03-13", "2000-04-26", "2000-11-11",
      "2002-08-09", "2003-01-01", "2004-08-13", "2005-07-14", "2006-08-17",
      "2007-06-25", "2008-01-21", "2008-11-09", "2010-03-01", "2011-07-21",
      "2012-07-18", "2012-12-24", "2014-02-01", "2015-08-14", "2016-03-09",
      "2016-11-21", "2018-06-02", "2019-06-13", "2019-11-11", "2021-07-30",
      "2022-08-16", "2022-11-24"
    )),
    Flow = c(
      1.220, 0.751, 0.917, 0.889, 0.911, 0.694, 0.604, 0.872, 0.473, 0.412,
      0.929, 0.653, 0.226, 0.330, 0.314, 0.341, 0.541, 0.633, 0.751, 0.223,
      0.334, 1.195, 0.699, 0.464, 0.998, 0.828, 1.052, 0.518, 0.486, 0.276,
      5.337, 0.923, 0.504, 0.653, 0.420, 1.695, 1.259, 0.623, 0.395, 0.509,
      0.395, 1.351, 0.608, 2.055, 1.214, 0.643, 0.513
    )
  )

  actual_output <- GetAM(30013)[1:47, ]

  testthat::expect_equal(actual_output$Date, expected_output$Date)
  testthat::expect_equal(actual_output$Flow, expected_output$Flow)
})

###### GetCDs ######
testthat::test_that("Check that the catchment descriptors are correct for gauge 28115", {
  library(UKFE)

  expected_output <- data.frame(
    Descriptor = c(
      "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19",
      "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
      "PROPWET", "RMED-1H", "RMED-1D", "RMED-2D", "SAAR",
      "SAAR4170", "SPRHOST", "URBEXT2000", "Easting", "Northing",
      "QMEDfse", "N", "URBEXT1990", "BFIHOST"
    ),
    Value = c(
      30.5625, 137.0000, 51.0000, 0.3200, 0.7830,
      7.1100, 43.4000, 0.9150, 0.0539, 12.1200,
      0.3600, 11.1000, 32.8000, 40.7000, 714.0000,
      719.0000, 14.9200, 0.3886, 452205.0000, 360118.0000, 1.1020, 31, 0.2965, 0.8410
    )
  )

  actual_output <- GetCDs(28115)

  testthat::expect_equal(expected_output$Value, actual_output$Value)
})

testthat::test_that("Check that the catchment descriptors are correct for gauge 69048", {
  library(UKFE)

  expected_output <- data.frame(
    Descriptor = c(
      "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19",
      "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
      "PROPWET", "RMED-1H", "RMED-1D", "RMED-2D", "SAAR",
      "SAAR4170", "SPRHOST", "URBEXT2000", "Easting", "Northing",
      "QMEDfse", "N", "URBEXT1990", "BFIHOST"
    ),
    Value = c(
      40.6025, 317.0000, 240.0000, 0.1900, 0.3780,
      6.84, 145.9000, 0.9060, 0.0362, 15.0700,
      0.5700, 11.8000, 48.0000, 62.7000, 1359.0000,
      1318.0000, 43.300, 0.0227, 399678.0000, 408701.0000, 1.0760, 24, 0.0103, 0.3880
    )
  )

  actual_output <- GetCDs(69048)

  testthat::expect_equal(expected_output$Value, actual_output$Value)
})

###### GetQMED ######

testthat::test_that("Check that gauge 27003 produces an error as it is not suitable for pooling or QMED", {
  testthat::expect_error(GetQMED(27003))
})

testthat::test_that("Check that the QMED is correct for gauge 28115 by comparing with rnrfa", {
  library(UKFE)

  stations <- rnrfa::catalogue()

  expected_output <- stations$qmed[stations$id == "28115"]

  actual_output <- GetQMED(28115)

  testthat::expect_equal(expected_output, actual_output, tolerance = 0.1)
})

testthat::test_that("Check that the QMED is correct for gauge 30013 by comparing with rnrfa", {
  library(UKFE)

  stations <- rnrfa::catalogue()

  expected_output <- stations$qmed[stations$id == "30013"]

  actual_output <- GetQMED(30013)

  testthat::expect_equal(expected_output, actual_output, tolerance = 0.1)
})

##### GenLog Functions #####


###### GenLogAM ######
testthat::test_that("Check UKFE quantile output against lmomco package", {
  annual_maximums <- c(
    349.869, 235.865, 484.477, 116.946, 348.332, 273.782,
    271.535, 416.528, 358.385, 254.775, 157.288, 313.732,
    323.739, 292.347, 334.413, 388.639, 327.441, 306.276,
    244.864, 505.928, 249.986, 201.851, 189.041, 341.393,
    162.862, 264.363, 318.775, 409.163, 378.598, 224.528,
    195.956
  )

  # Predict the discharge using lmomco
  lmoments <- lmomco::lmoms(annual_maximums)
  glo_params <- lmomco::parglo(lmoments)
  return_period <- 50
  exceedance_prob <- 1 / return_period
  lmomco_discharge <- lmomco::quaglo(1 - exceedance_prob, glo_params)

  # Predict the discharge using UKFE
  ukfe_discharge <- GenLogAM(annual_maximums, RP = 50)

  expect_type(ukfe_discharge, "double") # Ensure numeric output
  expect_false(is.na(ukfe_discharge)) # Ensure no NA values
  expect_equal(ukfe_discharge, lmomco_discharge, tolerance = 1e-6)
})



testthat::test_that("Check UKFE return period output against lmomco package", {
  annual_maximums <- c(
    349.869, 235.865, 484.477, 116.946, 348.332, 273.782,
    271.535, 416.528, 358.385, 254.775, 157.288, 313.732,
    323.739, 292.347, 334.413, 388.639, 327.441, 306.276,
    244.864, 505.928, 249.986, 201.851, 189.041, 341.393,
    162.862, 264.363, 318.775, 409.163, 378.598, 224.528,
    195.956
  )

  # Predict the return period using lmomco
  lmoments <- lmomco::lmoms(annual_maximums)
  glo_params <- lmomco::parglo(lmoments)
  discharge_value <- 600
  lmomco_return_period <- 1 / (1 - cdfglo(discharge_value, glo_params))

  # Predict the return period using UKFE
  ukfe_return_period <- GenLogAM(annual_maximums, q = 600)

  expect_type(ukfe_return_period, "double") # Ensure numeric output
  expect_false(is.na(ukfe_return_period)) # Ensure no NA values
  expect_equal(ukfe_return_period, lmomco_return_period, tolerance = 1e-6)
})

###### GenLogEst ######
test_that("GenLogEst correctly estimates discharge for a given return period", {
  # Define known parameters
  loc <- 200
  scale <- 50
  shape <- 0.2
  return_period <- 50

  # Compute using GenLogEst
  q_est <- GenLogEst(loc, scale, shape, RP = return_period)

  # Compute using lmomco (for validation)
  glo_params <- list(type = "glo", para = c(loc, scale, shape))
  lmomco_q <- lmomco::quaglo(1 - 1 / return_period, glo_params)

  # Check if values match within tolerance
  expect_type(q_est, "double")
  expect_false(is.na(q_est))
  expect_equal(q_est, lmomco_q, tolerance = 1e-6)
})

test_that("GenLogEst correctly estimates return period for a given discharge", {
  # Define known parameters
  loc <- 200
  scale <- 50
  shape <- 0.2
  q_value <- 300 # Discharge to estimate RP for

  # Compute using GenLogEst
  RP_est <- GenLogEst(loc, scale, shape, q = q_value)

  # Compute using lmomco (for validation)
  glo_params <- list(type = "glo", para = c(loc, scale, shape))
  lmomco_RP <- 1 / (1 - lmomco::cdfglo(q_value, glo_params))

  # Check if values match within tolerance
  expect_type(RP_est, "double")
  expect_false(is.na(RP_est))
  expect_equal(RP_est, lmomco_RP, tolerance = 1e-6)
})

test_that("GenLogEst returns consistant values for larger RP", {
  loc <- 200
  scale <- 50
  shape <- 0.2

  expect_gt(GenLogEst(loc, scale, shape, RP = 1000), GenLogEst(loc, scale, shape, RP = 100))
})

test_that("GenLogEst returns consistant values for larger discharges", {
  loc <- 200
  scale <- 50
  shape <- 0.2

  expect_lt(GenLogEst(loc, scale, shape, q = 250), GenLogEst(loc, scale, shape, q = 400))
})

test_that("GenLogEst handles logistic case when shape = 0", {
  loc <- 200
  scale <- 50
  shape <- 0
  RP <- 50
  q <- 300

  q_est <- GenLogEst(loc, scale, shape, RP = RP)
  RP_est <- GenLogEst(loc, scale, shape, q = q)

  # Reference using logistic distribution
  p <- 1 / RP
  q_ref <- loc + scale * log(p / (1 - p))
  z <- (q - loc) / scale
  RP_ref <- 1 / (exp(z) / (1 + exp(z)))

  expect_equal(q_est, q_ref, tolerance = 1e-6)
  expect_equal(RP_est, RP_ref, tolerance = 1e-6)
})

test_that("GenLogEst throws error when scale <= 0", {
  expect_error(
    GenLogEst(loc = 200, scale = 0, shape = 0.2, RP = 50),
    "Scale parameter must be positive"
  )
  expect_error(
    GenLogEst(loc = 200, scale = -5, shape = 0.2, RP = 50),
    "Scale parameter must be positive"
  )
})

test_that("GenLogEst throws error for quantile beyond theoretical bounds", {
  loc <- 200
  scale <- 50

  # shape > 0: upper bound at loc + scale / shape
  shape_pos <- 0.2
  q_high <- loc + scale / shape_pos + 1
  expect_error(
    GenLogEst(loc, scale, shape_pos, q = q_high),
    "Quantile exceeds theoretical bound"
  )

  # shape < 0: lower bound at loc + scale / shape
  shape_neg <- -0.2
  q_low <- loc + scale / shape_neg - 1
  expect_error(
    GenLogEst(loc, scale, shape_neg, q = q_low),
    "Quantile exceeds theoretical bound"
  )
})

###### GenLogGF  ######

test_that("GenLogGF returns valid output", {
  lcv <- 0.2
  lskew <- 0.1
  RP <- 100

  gf <- GenLogGF(lcv, lskew, RP)

  expect_type(gf, "double")
  expect_length(gf, 1)
  expect_true(is.finite(gf))
  expect_gt(gf, 1)
})

test_that("GenLogGF increases with RP", {
  lcv <- 0.2
  lskew <- 0.1

  gf_10 <- GenLogGF(lcv, lskew, RP = 10)
  gf_100 <- GenLogGF(lcv, lskew, RP = 100)
  gf_1000 <- GenLogGF(lcv, lskew, RP = 1000)

  expect_lt(gf_10, gf_100)
  expect_lt(gf_100, gf_1000)
})

test_that("GenLogGF gives plausible values for RP = 100", {
  lcv_values <- c(0.15, 0.2, 0.25)
  lskew_values <- c(0.1, 0.2)

  for (lcv in lcv_values) {
    for (lskew in lskew_values) {
      gf <- GenLogGF(lcv, lskew, RP = 100)
      expect_gt(gf, 1.25)
      expect_lt(gf, 6)
    }
  }
})


test_that("GenLogGF handles near-zero LSkew", {
  lcv <- 0.2
  lskew <- 0.0001
  RP <- 100

  gf <- GenLogGF(lcv, lskew, RP)
  expect_true(is.finite(gf))
  expect_gt(gf, 1)
})
###### GenLogPars ######

test_that("GenLogPars correctly estimates parameters using L-moments", {
  # Generate sample data
  set.seed(123)
  sample_data <- rnorm(100, mean = 200, sd = 50) # Synthetic dataset

  # Compute L-moments and parameters using lmomco
  lmoments <- lmoms(sample_data)
  glo_params <- parglo(lmoments) # lmomco results
  glo_values <- unname(glo_params$para) # Unname numeric vector

  # Compute parameters using GenLogPars
  genlog_params <- GenLogPars(x = sample_data)

  # Ensure correct output type
  expect_type(genlog_params, "list") # Function returns list
  expect_true(all(c("Loc", "Scale", "Shape") %in% names(genlog_params))) # Check expected names

  # Extract values and unname for comparison
  loc_genlog <- genlog_params$Loc
  scale_genlog <- genlog_params$Scale
  shape_genlog <- genlog_params$Shape

  loc_lmomco <- unname(glo_values[1])
  scale_lmomco <- unname(glo_values[2])
  shape_lmomco <- unname(glo_values[3])

  # Compare estimates with tolerance
  expect_equal(loc_genlog, loc_lmomco, tolerance = 1e-3)
  expect_equal(scale_genlog, scale_lmomco, tolerance = 1e-3)
  expect_equal(shape_genlog, shape_lmomco, tolerance = 1e-3)
})

test_that("GenLogPars MLE estimates match direct optimisation of GLO PDF", {
  # Generate sample data
  set.seed(123)
  sample_data <- rnorm(100, mean = 200, sd = 50) # Synthetic dataset

  # Initial parameter guesses
  pars <- c(mean(sample_data), sd(sample_data) / 1.5, 0.01)

  # Define GLO PDF
  gl_pdf <- function(q, loc, scale, shape) {
    y <- -shape^(-1) * log(1 - shape * (q - loc) / scale)
    f <- (scale^-1 * exp(-(1 - shape) * y)) / (1 + exp(-y))^2
    return(f)
  }

  # Define negative log-likelihood function (to minimise)
  min_SLS <- function(q, par) {
    abs(sum(log(gl_pdf(q, loc = par[1], scale = par[2], shape = par[3]))))
  }

  # Optimise directly using base R (reference result)
  reference_result <- suppressWarnings(optim(par = pars, fn = min_SLS, q = sample_data))
  ref_pars <- reference_result$par
  ref_nllh <- reference_result$value

  # Estimate using your function
  genlog_result <- GenLogPars(x = sample_data, mle = TRUE)

  # Compare estimated parameters (rounded)
  expect_equal(signif(genlog_result$loc, 3), signif(ref_pars[1], 3))
  expect_equal(signif(genlog_result$scale, 3), signif(ref_pars[2], 3))
  expect_equal(signif(genlog_result$shape, 3), signif(ref_pars[3], 3))

  # Compare log-likelihood
  expect_equal(signif(-genlog_result$log.likelihood, 3), signif(ref_nllh, 3))
})
##### GenPareto Functions ####

###### GenParetoEst ######


test_that("GenParetoEst correctly estimates quantiles for a given return period", {
  # Define known parameters
  loc <- 200
  scale <- 50
  shape <- 0.2
  ppy <- 1.867
  RP <- 50

  # Compute using GenParetoEst
  q_est <- GenParetoEst(loc, scale, shape, RP = RP, ppy = ppy)

  # Manual calculation
  NEP_annual <- 1 - 1 / RP
  NEP_pot <- NEP_annual / ppy
  q_manual <- loc + scale * (1 - (1 - NEP_pot)^shape) / shape

  # Compare values
  expect_type(q_est, "double")
  expect_false(is.na(q_est))
  expect_equal(q_est, q_manual, tolerance = 1e-6)
})

test_that("GenParetoEst correctly estimates return period for a given discharge", {
  loc <- 200
  scale <- 50
  shape <- 0.2
  ppy <- 1.867
  q <- 300

  RP_est <- GenParetoEst(loc, scale, shape, q = q, ppy = ppy)

  # Manual calculation matching GenParetoEst
  y <- -1 / shape * log(1 - shape * (q - loc) / scale)
  P <- 1 - exp(-y)
  RP_expected <- 1 / (P * ppy)

  expect_equal(RP_est, RP_expected, tolerance = 1e-6)
})

test_that("GenParetoEst produces higher discharge estimates for higher return periods", {
  loc <- 200
  scale <- 50
  shape <- 0.2
  ppy <- 1.867

  q_50 <- GenParetoEst(loc, scale, shape, RP = 50, ppy = ppy)
  q_1000 <- GenParetoEst(loc, scale, shape, RP = 1000, ppy = ppy)

  expect_gt(q_1000, q_50) # Expect larger discharge for higher RP
})

###### GenParetoGF ######

test_that("GenParetoGF returns plausible GF values for RP = 100", {
  RP <- 100
  ppy <- 1.867

  # Typical hydrological range
  lcv_values <- c(0.1, 0.2, 0.3)
  lskew_values <- c(0, 0.1, 0.2)

  for (lcv in lcv_values) {
    for (lskew in lskew_values) {
      gf <- GenParetoGF(lcv, lskew, RP = RP, ppy = ppy)

      expect_gt(gf, 1.25)
      expect_lt(gf, 6.0)
    }
  }
})



test_that("GenParetoGF returns expected type and is finite", {
  lcv <- 0.2
  lskew <- 0.1
  RP <- 100
  ppy <- 1.867

  gf <- GenParetoGF(lcv, lskew, RP, ppy)

  expect_type(gf, "double")
  expect_length(gf, 1)
  expect_true(is.finite(gf))
  expect_gt(gf, 1) # GFs should be >1 for RP > 1
})

test_that("GenParetoGF increases with return period", {
  lcv <- 0.2
  lskew <- 0.1
  ppy <- 1.867

  gf_10 <- GenParetoGF(lcv, lskew, RP = 10, ppy)
  gf_100 <- GenParetoGF(lcv, lskew, RP = 100, ppy)
  gf_1000 <- GenParetoGF(lcv, lskew, RP = 1000, ppy)

  expect_lt(gf_10, gf_100)
  expect_lt(gf_100, gf_1000)
})

test_that("GenParetoGF handles edge cases with small or zero skew", {
  RP <- 100
  lcv <- 0.2
  ppy <- 1.867

  gf_zero_skew <- GenParetoGF(lcv, lskew = 0, RP, ppy)
  gf_small_skew <- GenParetoGF(lcv, lskew = 0.0001, RP, ppy)

  expect_true(is.finite(gf_zero_skew))
  expect_true(is.finite(gf_small_skew))
  expect_true(gf_zero_skew > 1)
})



###### GenParetoPars ######


test_that("GenParetoPars estimates parameters using L-moments from data", {
  set.seed(123)
  sample_data <- evd::rgpd(100, loc = 200, scale = 50, shape = 0.1)
  # Estimate using lmomco
  lmoments <- lmomco::lmoms(sample_data)
  lmomco_params <- lmomco::pargpa(lmoments)$para

  # Estimate using GenParetoPars
  gen_params <- GenParetoPars(x = sample_data)

  # Extract values and unname for comparison
  loc <- gen_params$Loc
  scale <- gen_params$Scale
  shape <- gen_params$Shape

  loc_lmomco <- gen_params[1][[1]]
  scale_lmomco <- gen_params[2][[1]]
  shape_lmomco <- gen_params[3][[1]]

  # Extract and unname for comparison
  expect_type(gen_params, "list")
  expect_true(all(c("Loc", "Scale", "Shape") %in% names(gen_params)))

  expect_equal(loc, loc_lmomco, tolerance = 1e-3)
  expect_equal(scale, scale_lmomco, tolerance = 1e-3)
  expect_equal(shape, shape_lmomco, tolerance = 1e-3)
})

test_that("GenParetoPars estimates parameters using L1, LCV, LSKEW", {
  # Manually define L-moments
  L1 <- 200
  LCV <- 0.25
  LSKEW <- 0.1

  # Estimate using GenParetoPars
  gen_params <- GenParetoPars(L1 = L1, LCV = LCV, LSKEW = LSKEW)

  # Manually compute expected values
  Shape <- (1 - 3 * LSKEW) / (1 + LSKEW)
  L2 <- L1 * LCV
  Scale <- (1 + Shape) * (2 + Shape) * L2
  Loc <- L1 - (2 + Shape) * L2

  expect_equal(unname(gen_params$Loc), Loc, tolerance = 1e-6)
  expect_equal(unname(gen_params$Scale), Scale, tolerance = 1e-6)
  expect_equal(unname(gen_params$Shape), Shape, tolerance = 1e-6)
})

test_that("GenParetoPars throws error for invalid input", {
  expect_error(GenParetoPars(x = "not numeric"), "x must be a numeric vector")
})

###### GenParetoPOT ######
test_that("GenParetoPOT estimates quantiles from data and matches lmomco", {
  set.seed(123)
  sample_data <- evd::rgpd(100, loc = 200, scale = 50, shape = 0.1)
  ppy <- 1.867
  RP <- 50

  # Estimate quantile using GenParetoPOT
  q_gen <- GenParetoPOT(x = sample_data, ppy = ppy, RP = RP)

  # Reference quantile from lmomco
  lmoments <- lmomco::lmoms(sample_data)
  gpa_params <- lmomco::pargpa(lmoments)
  prob <- 1 - (1 / RP) / ppy
  q_ref <- lmomco::quagpa(prob, gpa_params)

  expect_type(q_gen, "double")
  expect_false(is.na(q_gen))
  expect_equal(q_gen, q_ref, tolerance = 1e-3)
})

test_that("GenParetoPOT estimates return period from quantile", {
  set.seed(123)
  sample_data <- evd::rgpd(100, loc = 200, scale = 50, shape = 0.1)
  ppy <- 1.867
  q_target <- 350 # value within the simulated range

  # Estimate RP using GenParetoPOT
  RP_gen <- GenParetoPOT(x = sample_data, ppy = ppy, q = q_target)

  # Reference RP from lmomco
  lmoments <- lmomco::lmoms(sample_data)
  gpa_params <- lmomco::pargpa(lmoments)
  P_exceed <- 1 - lmomco::cdfgpa(q_target, gpa_params)
  RP_ref <- 1 / (P_exceed * ppy)

  expect_type(RP_gen, "double")
  expect_false(is.na(RP_gen))
  expect_equal(RP_gen, RP_ref, tolerance = 1e-3)
})

test_that("GenParetoPOT returns higher discharge for higher return period", {
  set.seed(123)
  sample_data <- rgpd(100, loc = 200, scale = 50, shape = 0.1)
  ppy <- 1.867

  q_20 <- GenParetoPOT(sample_data, ppy = ppy, RP = 20)
  q_200 <- GenParetoPOT(sample_data, ppy = ppy, RP = 200)

  expect_gt(q_200, q_20)
})

test_that("GenParetoPOT throws an error for non-numeric input", {
  expect_error(GenParetoPOT(x = "not numeric"), "x must be a numeric vector")
})

##### GEV Functions #####
###### GEVAM ######
test_that("GEVAM estimates quantile correctly from data", {
  set.seed(123)
  # Generate synthetic GEV data (mean = 200, scale = 50, shape = 0.1)
  sample_data <- evd::rgev(100, loc = 200, scale = 50, shape = 0.1)
  RP <- 50

  # Estimate quantile using GEVAM
  q_gevam <- GEVAM(x = sample_data, RP = RP)

  # Estimate quantile using lmomco
  lmoms <- lmomco::lmoms(sample_data)
  params <- lmomco::pargev(lmoms)
  q_ref <- lmomco::quagev(1 - 1 / RP, params)

  expect_type(q_gevam, "double")
  expect_false(is.na(q_gevam))
  expect_equal(q_gevam, q_ref, tolerance = 1e-3)
})

test_that("GEVAM estimates return period from quantile correctly", {
  set.seed(123)
  sample_data <- evd::rgev(100, loc = 200, scale = 50, shape = 0.1)
  q_val <- 350 # Reasonable value within simulated range

  # Estimate RP using GEVAM
  RP_gevam <- GEVAM(x = sample_data, q = q_val)

  # Reference RP from lmomco
  lmoms <- lmomco::lmoms(sample_data)
  params <- lmomco::pargev(lmoms)
  F_q <- lmomco::cdfgev(q_val, params)
  RP_ref <- 1 / (1 - F_q)

  expect_type(RP_gevam, "double")
  expect_false(is.na(RP_gevam))
  expect_equal(RP_gevam, RP_ref, tolerance = 1e-3)
})

test_that("GEVAM returns higher discharge for higher return periods", {
  set.seed(123)
  sample_data <- evd::rgev(100, loc = 200, scale = 50, shape = 0.1)

  q_20 <- GEVAM(sample_data, RP = 20)
  q_100 <- GEVAM(sample_data, RP = 100)
  q_500 <- GEVAM(sample_data, RP = 500)

  expect_gt(q_100, q_20)
  expect_gt(q_500, q_100)
})

test_that("GEVAM throws an error for non-numeric input", {
  expect_error(GEVAM(x = "not numeric"), "x must be a numeric vector")
})
###### GEVEst ######

test_that("GEVEst correctly estimates quantile from parameters", {
  loc <- 200
  scale <- 50
  shape <- 0.1
  RP <- 50

  q_gevest <- GEVEst(loc, scale, shape, RP = RP)

  # Reference using lmomco
  params <- list(type = "gev", para = c(loc, scale, shape))
  q_lmomco <- lmomco::quagev(1 - 1 / RP, params)

  expect_type(q_gevest, "double")
  expect_false(is.na(q_gevest))
  expect_equal(q_gevest, q_lmomco, tolerance = 1e-6)
})

test_that("GEVEst correctly estimates return period from quantile", {
  loc <- 200
  scale <- 50
  shape <- 0.1
  q <- 350

  RP_gevest <- GEVEst(loc, scale, shape, q = q)

  # Reference using lmomco
  params <- list(type = "gev", para = c(loc, scale, shape))
  F_q <- lmomco::cdfgev(q, params)
  RP_lmomco <- 1 / (1 - F_q)

  expect_type(RP_gevest, "double")
  expect_false(is.na(RP_gevest))
  expect_equal(RP_gevest, RP_lmomco, tolerance = 1e-6)
})

test_that("GEVEst gives higher discharge for higher return periods", {
  loc <- 200
  scale <- 50
  shape <- 0.1

  q_10 <- GEVEst(loc, scale, shape, RP = 10)
  q_100 <- GEVEst(loc, scale, shape, RP = 100)
  q_1000 <- GEVEst(loc, scale, shape, RP = 1000)

  expect_gt(q_100, q_10)
  expect_gt(q_1000, q_100)
})
test_that("GEVEst handles shape = 0 (Gumbel case) using GumbelEst", {
  loc <- 200
  scale <- 50
  shape <- 0
  RP <- 50
  q <- 350

  # Quantile estimation
  q_gev <- GEVEst(loc, scale, shape, RP = RP)
  q_gumbel <- GumbelEst(loc, scale, RP = RP)
  expect_equal(q_gev, q_gumbel, tolerance = 1e-6)

  # Return period estimation
  RP_gev <- GEVEst(loc, scale, shape, q = q)
  RP_gumbel <- GumbelEst(loc, scale, q = q)
  expect_equal(RP_gev, RP_gumbel, tolerance = 1e-6)
})

test_that("GEVEst throws error for invalid scale or RP", {
  expect_error(
    GEVEst(loc = 200, scale = 0, shape = 0.1, RP = 50),
    "Scale parameter must be positive"
  )

  expect_error(
    GEVEst(loc = 200, scale = 50, shape = 0.1, RP = 1),
    "Return period must be greater than 1"
  )
})

test_that("GEVEst throws error for quantile beyond domain limit", {
  loc <- 200
  scale <- 50
  shape_pos <- 0.2
  shape_neg <- -0.2

  # Positive shape → upper limit
  q_too_high <- loc + scale / shape_pos + 1
  expect_error(
    GEVEst(loc, scale, shape_pos, q = q_too_high),
    "Quantile exceeds theoretical bound"
  )

  # Negative shape → lower limit
  q_too_low <- loc + scale / shape_neg - 1
  expect_error(
    GEVEst(loc, scale, shape_neg, q = q_too_low),
    "Quantile exceeds theoretical bound"
  )
})




###### GEVPars ######
test_that("GEVPars returns expected parameters using L-moments from data", {
  set.seed(123)
  sample_data <- evd::rgev(100, loc = 200, scale = 50, shape = 0.1)

  # Estimate using GEVPars
  gen_params <- GEVPars(x = sample_data)

  # Estimate using lmomco
  lmoments <- lmomco::lmoms(sample_data)
  ref_params <- lmomco::pargev(lmoments)$para # loc, scale, shape

  expect_type(gen_params, "list")
  expect_true(all(c("Loc", "Scale", "Shape") %in% names(gen_params)))

  expect_equal(unname(gen_params$Loc), unname(ref_params[1]), tolerance = 1e-2)
  expect_equal(unname(gen_params$Scale), unname(ref_params[2]), tolerance = 1e-2)
  expect_equal(unname(gen_params$Shape), unname(ref_params[3]), tolerance = 1e-2)
})

test_that("GEVPars returns expected output using L1, LCV, LSKEW", {
  # Manually specify L-moments
  L1 <- 200
  LCV <- 0.2
  LSKEW <- 0.1

  gen_params <- GEVPars(L1 = L1, LCV = LCV, LSKEW = LSKEW)

  expect_type(gen_params, "list")
  expect_true(all(c("Loc", "Scale", "Shape") %in% names(gen_params)))
  expect_gt(gen_params$Scale, 0)
})

test_that("GEVPars MLE estimates match ismev::gev.fit()", {
  # Load sample data (already prepped)
  set.seed(123)
  sample_data <- evd::rgev(100, loc = 200, scale = 50, shape = 0.1)

  # Estimate using your implementation
  ukfe_result <- GEVPars(x = sample_data, mle = TRUE)

  # Estimate using ismev
  ismev_result <- ismev::gev.fit(sample_data, show = FALSE)

  # Compare parameters
  expect_equal(unname(ukfe_result$loc), unname(ismev_result$mle[1]), tolerance = 1e-2)
  expect_equal(unname(ukfe_result$scale), unname(ismev_result$mle[2]), tolerance = 1e-2)
  expect_equal(unname(ukfe_result$shape), unname(-ismev_result$mle[3]), tolerance = 1e-2)

  # Compare log-likelihood (ismev returns negative log-likelihood)
  expect_equal(-ukfe_result$log.likelihood, ismev_result$nllh, tolerance = 1e-5)
})
##### Gumbel Functions #####
###### GumbelAM ######

test_that("GumbelAM quantile estimates match theoretical Gumbel distribution", {
  set.seed(123)
  sample_data <- rgev(1000, loc = 300, scale = 50, shape = 0) # Simulated Gumbel data

  q_gumbel_am <- GumbelAM(sample_data, RP = 100)

  n <- length(sample_data)
  Sort.x <- sort(sample_data)
  L1 <- mean(Sort.x)
  b1 <- mean((seq(1, n) - 1) / (n - 1) * Sort.x)
  L2 <- 2 * b1 - L1
  scale <- L2 / log(2)
  loc <- L1 - 0.5772 * scale

  q_theoretical <- loc - scale * log(-log(1 - 1 / 100))

  expect_equal(q_gumbel_am, q_theoretical, tolerance = 1e-6)
})

test_that("GumbelAM return period estimates match theoretical inverse CDF", {
  set.seed(123)
  sample_data <- rgev(1000, loc = 300, scale = 50, shape = 0)

  n <- length(sample_data)
  Sort.x <- sort(sample_data)
  L1 <- mean(Sort.x)
  b1 <- mean((seq(1, n) - 1) / (n - 1) * Sort.x)
  L2 <- 2 * b1 - L1
  scale <- L2 / log(2)
  loc <- L1 - 0.5772 * scale

  q <- loc - scale * log(-log(1 - 1 / 100)) # Known quantile

  RP_est <- GumbelAM(sample_data, q = q)

  expect_equal(RP_est, 100, tolerance = 1e-6)
})


test_that("GumbelAM produces increasing quantiles with increasing RP", {
  set.seed(123)
  sample_data <- rgev(1000, loc = 300, scale = 50, shape = 0)

  q_10 <- GumbelAM(sample_data, RP = 10)
  q_100 <- GumbelAM(sample_data, RP = 100)
  q_1000 <- GumbelAM(sample_data, RP = 1000)

  expect_lt(q_10, q_100)
  expect_lt(q_100, q_1000)
})

test_that("GumbelAM returns finite values", {
  set.seed(123)
  sample_data <- rgev(1000, loc = 300, scale = 50, shape = 0)

  q_val <- GumbelAM(sample_data, RP = 50)
  RP_val <- GumbelAM(sample_data, q = q_val)

  expect_true(is.finite(q_val))
  expect_true(is.finite(RP_val))
  expect_gt(q_val, 0)
  expect_gt(RP_val, 1)
})

###### GumbelPars ######
test_that("GumbelPars estimates parameters using L-moments from data sample", {
  set.seed(101)
  sample_data <- rnorm(100, mean = 80, sd = 15)

  # Calculate L-moment equivalents
  L1 <- mean(sample_data)
  LCV <- Lcv(sample_data)

  # Internal L-moment estimate
  gumbel_from_x <- GumbelPars(x = sample_data)

  # Directly supplied L-moment values
  gumbel_direct <- GumbelPars(L1 = L1, LCV = LCV)

  expect_type(gumbel_from_x, "list")
  expect_named(gumbel_from_x, c("Loc", "Scale"))
  expect_equal(gumbel_from_x, gumbel_direct, tolerance = 1e-6)
})


test_that("GumbelPars throws error if x is non-numeric", {
  expect_error(GumbelPars(x = "invalid input"), "x must be a numeric vector")
})

test_that("GumbelPars MLE estimates match ismev::gum.fit()", {
  set.seed(131)
  sample_data <- rnorm(100, mean = 80, sd = 15)

  # Your function (UKFE) MLE estimates
  ukfe_result <- GumbelPars(x = sample_data, mle = TRUE)

  # ismev MLE estimates
  ismev_result <- ismev::gum.fit(sample_data, show = FALSE)


  # Compare parameters
  expect_equal(ukfe_result$loc, ismev_result$mle[1], tolerance = 1e-3)
  expect_equal(ukfe_result$scale, ismev_result$mle[2], tolerance = 1e-3)

  # Compare log-likelihoods (ismev gives negative log-likelihood)
  expect_equal(-ukfe_result$log.likelihood, ismev_result$nllh, tolerance = 1e-3)

  # Sanity checks
  expect_true(is.finite(ukfe_result$loc))
  expect_true(is.finite(ukfe_result$scale))
  expect_true(is.finite(ukfe_result$log.likelihood))
  expect_gt(ukfe_result$scale, 0)
})

###### GumbelEst ######
test_that("GumbelEst estimates quantile correctly from parameters", {
  loc <- 200
  scale <- 50
  RP <- 50

  # Expected quantile using lmomco with GEV shape = 0 (Gumbel)
  params <- list(type = "gev", para = c(loc, scale, 0))
  q_ref <- lmomco::quagev(1 - 1 / RP, params)

  q_gumbelest <- GumbelEst(loc = loc, scale = scale, RP = RP)

  expect_type(q_gumbelest, "double")
  expect_false(is.na(q_gumbelest))
  expect_equal(q_gumbelest, q_ref, tolerance = 1e-6)
})
test_that("GumbelEst estimates return period correctly from quantile", {
  loc <- 200
  scale <- 50
  q_val <- 350

  params <- list(type = "gev", para = c(loc, scale, 0))
  F_q <- lmomco::cdfgev(q_val, params)
  RP_ref <- 1 / (1 - F_q)

  RP_gumbelest <- GumbelEst(loc = loc, scale = scale, q = q_val)

  expect_type(RP_gumbelest, "double")
  expect_false(is.na(RP_gumbelest))
  expect_equal(RP_gumbelest, RP_ref, tolerance = 1e-6)
})
test_that("GumbelEst throws error for non-positive scale", {
  expect_error(
    GumbelEst(loc = 200, scale = 0, RP = 50),
    "Scale parameter must be positive"
  )
  expect_error(
    GumbelEst(loc = 200, scale = -1, RP = 50),
    "Scale parameter must be positive"
  )
})

test_that("GumbelEst throws error for RP <= 1", {
  expect_error(
    GumbelEst(loc = 200, scale = 50, RP = 1),
    "Return period must be greater than 1"
  )
  expect_error(
    GumbelEst(loc = 200, scale = 50, RP = 0.5),
    "Return period must be greater than 1"
  )
})

test_that("GumbelEst quantile increases with return period", {
  loc <- 200
  scale <- 50

  q_10 <- GumbelEst(loc = loc, scale = scale, RP = 10)
  q_100 <- GumbelEst(loc = loc, scale = scale, RP = 100)
  q_1000 <- GumbelEst(loc = loc, scale = scale, RP = 1000)

  expect_lt(q_10, q_100)
  expect_lt(q_100, q_1000)
})

test_that("GumbelEst return period increases with increasing discharge", {
  loc <- 200
  scale <- 50

  RP_1 <- GumbelEst(loc = loc, scale = scale, q = 300)
  RP_2 <- GumbelEst(loc = loc, scale = scale, q = 320)
  RP_3 <- GumbelEst(loc = loc, scale = scale, q = 340)

  expect_lt(RP_1, RP_2)
  expect_lt(RP_2, RP_3)
})

test_that("GumbelEst roundtrip RP -> q -> RP returns original value", {
  loc <- 200
  scale <- 50
  RP_in <- 50

  q <- GumbelEst(loc = loc, scale = scale, RP = RP_in)
  RP_out <- GumbelEst(loc = loc, scale = scale, q = q)

  expect_equal(RP_out, RP_in, tolerance = 1e-6)
})

###### GumbelGF ######

test_that("GumbelGF returns valid numeric output", {
  lcv <- 0.2
  RP <- 100

  gf <- GumbelGF(lcv, RP)

  expect_type(gf, "double")
  expect_length(gf, 1)
  expect_true(is.finite(gf))
  expect_gt(gf, 1)
})

test_that("GumbelGF increases with return period", {
  lcv <- 0.2

  gf_10 <- GumbelGF(lcv, RP = 10)
  gf_100 <- GumbelGF(lcv, RP = 100)
  gf_1000 <- GumbelGF(lcv, RP = 1000)

  expect_lt(gf_10, gf_100)
  expect_lt(gf_100, gf_1000)
})

test_that("GumbelGF returns realistic values for RP = 100", {
  lcv_values <- c(0.1, 0.2, 0.3)

  for (lcv in lcv_values) {
    gf <- GumbelGF(lcv, RP = 100)
    expect_gt(gf, 1.25)
    expect_lt(gf, 6)
  }
})

test_that("GumbelGF handles very small and large LCV", {
  gf_small <- GumbelGF(lcv = 0.001, RP = 100)
  gf_large <- GumbelGF(lcv = 0.4, RP = 100)

  expect_true(is.finite(gf_small))
  expect_true(is.finite(gf_large))
  expect_gt(gf_large, gf_small)
})

##### Stats Functions #####

###### Lcv ######
test_that("Lcv returns correct linear coefficient of variation", {
  set.seed(456)
  x <- rgamma(100, shape = 2, scale = 3)

  lcv_value <- Lcv(x)

  lmom_result <- lmomco::lmoms(x)
  lcv_ref <- lmom_result$lambdas[2] / lmom_result$lambdas[1]

  expect_type(lcv_value, "double")
  expect_false(is.na(lcv_value))
  expect_equal(lcv_value, lcv_ref, tolerance = 1e-6)
})

test_that("Lcv throws error on non-numeric input", {
  expect_error(Lcv("not a number"))
})

###### LcvUrb ######
test_that("LcvUrb correctly urbanises and de-urbanises Lcv", {
  lcv_original <- 0.25
  urbext <- 0.1

  # Urbanise
  lcv_urban <- LcvUrb(lcv_original, urbext)

  # De-urbanise back
  lcv_deurban <- LcvUrb(lcv_urban, urbext, DeUrb = TRUE)

  expect_type(lcv_urban, "double")
  expect_type(lcv_deurban, "double")
  expect_equal(lcv_deurban, lcv_original, tolerance = 1e-10)
})

test_that("LcvUrb returns original Lcv when URBEXT2000 is 0", {
  lcv <- 0.2
  urbext <- 0.0

  expect_equal(LcvUrb(lcv, urbext), lcv)
  expect_equal(LcvUrb(lcv, urbext, DeUrb = TRUE), lcv)
})

test_that("Urbanisation decreases and de-urbanisation increases Lcv", {
  lcv <- 0.3
  urbext <- 0.2

  lcv_urban <- LcvUrb(lcv, urbext)
  lcv_deurban <- LcvUrb(lcv, urbext, DeUrb = TRUE)

  expect_lt(lcv_urban, lcv)
  expect_gt(lcv_deurban, lcv)
})

test_that("LcvUrb handles invalid input types gracefully", {
  expect_error(LcvUrb("not numeric", 0.1))
  expect_error(LcvUrb(0.2, "not numeric"))
})
###### LKurt ######
test_that("LKurt result matches lmomco::lmoms for a simple sample", {
  x <- c(20, 500, 30, 40, 10)
  lmoms <- lmomco::lmoms(x)
  expected_LKurt <- lmoms$ratios[3] # L-kurtosis is τ₄ = L₄ / L₂

  lkurt_result <- LKurt(x)
  expect_equal(lkurt_result, expected_LKurt, tolerance = 1e-6)
})

test_that("LKurt throws error when input is non-numeric", {
  expect_error(LKurt(c("a", "b", "c")), "x must be a numeric vector")
  expect_error(LKurt(list(1, 2, 3)), "x must be a numeric vector")
})

test_that("LKurt handles NA values correctly", {
  x <- c(1, 2, NA, 4, 5)
  expect_silent(lk <- LKurt(x))
  expect_type(lk, "double")
  expect_false(is.na(lk))
})

test_that("LKurt returns NaN with fewer than 4 observations", {
  expect_true(is.nan(LKurt(c(1))))
  expect_true(is.nan(LKurt(c(1, 2))))
  expect_true(is.nan(LKurt(c(1, 2, 3))))
})


test_that("LKurt returns NaN for constant vector", {
  x <- rep(10, 10)
  result <- LKurt(x)
  expect_true(is.nan(result))
})

###### LMoments ######
test_that("LMoments returns a data frame with correct column names", {
  x <- c(10, 20, 30, 40, 50)
  result <- LMoments(x)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_named(result, c("L1", "L2", "L3", "L4", "Lcv", "LSkew", "LKurt"))
})

test_that("LMoments matches output from lmom::samlmu", {
  x <- c(15, 30, 45, 60, 90, 120, 150, 200)

  lmom_vals <- lmom::samlmu(x, nmom = 4)
  ukfe_vals <- LMoments(x)

  # L-moments
  expect_equal(ukfe_vals$L1, unname(lmom_vals["l_1"]), tolerance = 1e-6)
  expect_equal(ukfe_vals$L2, unname(lmom_vals["l_2"]), tolerance = 1e-6)

  # L-moment ratios from lmom
  expect_equal(ukfe_vals$LSkew, unname(lmom_vals["t_3"]), tolerance = 1e-6)
  expect_equal(ukfe_vals$LKurt, unname(lmom_vals["t_4"]), tolerance = 1e-6)

  # Lcv = L2 / L1
  expected_lcv <- lmom_vals["l_2"] / lmom_vals["l_1"]
  expect_equal(ukfe_vals$Lcv, unname(expected_lcv), tolerance = 1e-6)
})

###### LSkew ######
test_that("LSkew matches lmom::samlmu t3 (L-skewness)", {
  x <- c(15, 30, 45, 60, 90, 120, 150, 200)

  ref <- unname(lmom::samlmu(x, nmom = 4)["t_3"])
  custom <- LSkew(x)

  expect_type(custom, "double")
  expect_equal(custom, ref, tolerance = 1e-6)
})

test_that("LSkew handles NA values and returns numeric", {
  x <- c(15, NA, 30, 60, 90, 150)
  result <- LSkew(x)

  expect_type(result, "double")
  expect_false(is.na(result))
})
test_that("LSkew throws error for non-numeric input", {
  expect_error(LSkew(c("a", "b", "c")), "x must be a numeric vector")
  expect_error(LSkew(list(1, 2, 3)), "x must be a numeric vector")
})

test_that("LSkew returns NaN for too-short samples", {
  expect_true(is.nan(LSkew(c(1))))
  expect_true(is.nan(LSkew(c(1, 2))))
  expect_true(is.nan(LSkew(c(1, 2, 3))))
})

test_that("LSkew returns NaN for constant vector", {
  x <- rep(100, 10)
  result <- LSkew(x)
  expect_true(is.nan(result))
})

###### LSkewUrb ######
test_that("LSkewUrb correctly urbanises LSkew", {
  lskew <- 0.2
  urb <- 0.1

  result <- LSkewUrb(lskew, urb, DeUrb = FALSE)

  # Manual calculation
  expected <- ((lskew + 1) * 1.096017^(1.567 * urb)) - 1
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("LSkewUrb correctly de-urbanises LSkew", {
  lskew_urban <- 0.25
  urb <- 0.1

  result <- LSkewUrb(lskew_urban, urb, DeUrb = TRUE)

  # Manual calculation
  expected <- ((lskew_urban + 1) / 1.096017^(1.567 * urb)) - 1
  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("LSkewUrb roundtrip urbanise and de-urbanise returns original value", {
  lskew_original <- 0.18
  urb <- 0.15

  urbanised <- LSkewUrb(lskew_original, urb)
  deurbanised <- LSkewUrb(urbanised, urb, DeUrb = TRUE)

  expect_equal(deurbanised, lskew_original, tolerance = 1e-10)
})


test_that("LSkewUrb returns original LSkew if URBEXT2000 = 0", {
  lskew <- 0.3
  urb <- 0

  expect_equal(LSkewUrb(lskew, urb), lskew)
  expect_equal(LSkewUrb(lskew, urb, DeUrb = TRUE), lskew)
})
###### OptimPars ######
# Helper to simulate Q values from known distribution
generate_q <- function(RP, loc, scale, shape = 0, dist = "GenLog") {
  if (dist == "GenLog") {
    return(loc + (scale / shape) * (1 - (RP - 1)^(-shape)))
  }
  if (dist == "GEV") {
    return(loc + (scale / shape) * (1 - (-log(1 - 1 / RP))^shape))
  }
  if (dist == "Kappa3") {
    h <- -0.4
    A <- (1 - (1 - (1 - 1 / RP)^h) / h)^shape
    return(loc + (scale / shape) * (1 - A))
  }
  if (dist == "Gumbel") {
    return(loc + scale * (-log(-log(1 - 1 / RP))))
  }
}

test_that("OptimPars returns correct column names for GenLog", {
  RP <- c(2, 5, 10, 25, 50, 100)
  q <- generate_q(RP, loc = 100, scale = 30, shape = 0.1, dist = "GenLog")
  df <- data.frame(RP, q)

  pars <- OptimPars(df, dist = "GenLog")
  expect_named(pars, c("loc", "scale", "shape"))
  expect_equal(nrow(pars), 1)
})

test_that("OptimPars returns correct column names for GEV", {
  RP <- c(2, 5, 10, 25, 50, 100)
  q <- generate_q(RP, loc = 100, scale = 30, shape = 0.1, dist = "GEV")
  df <- data.frame(RP, q)

  pars <- OptimPars(df, dist = "GEV")
  expect_named(pars, c("loc", "scale", "shape"))
  expect_equal(nrow(pars), 1)
})

test_that("OptimPars returns correct column names for Kappa3", {
  RP <- c(2, 5, 10, 25, 50, 100)
  q <- generate_q(RP, loc = 100, scale = 30, shape = 0.1, dist = "Kappa3")
  df <- data.frame(RP, q)

  pars <- OptimPars(df, dist = "Kappa3")
  expect_named(pars, c("loc", "scale", "shape"))
  expect_equal(nrow(pars), 1)
})

test_that("OptimPars returns correct column names for Gumbel", {
  RP <- c(2, 5, 10, 25, 50, 100)
  q <- generate_q(RP, loc = 100, scale = 30, dist = "Gumbel")
  df <- data.frame(RP, q)

  pars <- OptimPars(df, dist = "Gumbel")
  expect_named(pars, c("loc", "scale"))
  expect_equal(nrow(pars), 1)
})

test_that("OptimPars parameters reproduce original GenLog input", {
  RP <- c(2, 5, 10, 25, 50, 100)
  true_pars <- list(loc = 100, scale = 30, shape = 0.1)
  q <- generate_q(RP, true_pars$loc, true_pars$scale, true_pars$shape, dist = "GenLog")
  df <- data.frame(RP, q)

  est <- OptimPars(df, dist = "GenLog")
  expect_equal(est$loc, true_pars$loc, tolerance = 2)
  expect_equal(est$scale, true_pars$scale, tolerance = 2)
  expect_equal(est$shape, true_pars$shape, tolerance = 0.01)
})

test_that("OptimPars throws error on non-dataframe input", {
  expect_error(OptimPars(1:10), "x must be a data.frame")
})

test_that("OptimPars throws error on invalid distribution name", {
  df <- data.frame(RP = c(2, 5, 10), Q = c(100, 150, 180))
  expect_error(OptimPars(df, dist = "invalid"), "dist must equal one of")
})

##### Kappa3 Functions #####
###### Kappa3AM ######
test_that("Kappa3AM matches Kappa3Pars and Kappa3Est", {
  set.seed(123)
  x <- rlnorm(100, meanlog = 5, sdlog = 0.5)

  result_wrapper_q <- Kappa3AM(x, RP = 50)
  result_wrapper_RP <- Kappa3AM(x, q = 600)

  pars <- as.numeric(Kappa3Pars(x))
  result_direct_q <- Kappa3Est(pars[1], pars[2], pars[3], RP = 50)
  result_direct_RP <- Kappa3Est(pars[1], pars[2], pars[3], q = 600)

  expect_equal(result_wrapper_q, result_direct_q, tolerance = 1e-8)
  expect_equal(result_wrapper_RP, result_direct_RP, tolerance = 1e-8)
})

###### Kappa3Est ######

test_that("Kappa3Est quantile matches reference equation from BHS Circulation 142", {
  loc <- 300
  scale <- 50
  shape <- 0.2
  RP <- 50
  h <- -0.4

  # Reference from equation in Kjeldsen (2019)
  q_ref <- loc + (scale / shape) * (1 - ((1 - (1 - 1 / RP)^h) / h)^shape)

  # Function output
  q_out <- Kappa3Est(loc, scale, shape, RP = RP)

  expect_type(q_out, "double")
  expect_equal(q_out, q_ref, tolerance = 1e-10)
})

test_that("Kappa3Est return period matches rearranged reference equation", {
  loc <- 300
  scale <- 50
  shape <- 0.2
  q <- 400
  h <- -0.4

  # Reference from rearranged formula
  RP_ref <- 1 / (1 - (1 - h * (1 - shape * (q - loc) / scale)^(1 / shape))^(1 / h))

  # Function output
  RP_out <- Kappa3Est(loc, scale, shape, q = q)

  expect_type(RP_out, "double")
  expect_equal(RP_out, RP_ref, tolerance = 1e-10)
})

test_that("Kappa3Est quantile increases with return period", {
  loc <- 300
  scale <- 50
  shape <- 0.2

  q_10 <- Kappa3Est(loc, scale, shape, RP = 10)
  q_100 <- Kappa3Est(loc, scale, shape, RP = 100)
  q_1000 <- Kappa3Est(loc, scale, shape, RP = 1000)

  expect_lt(q_10, q_100)
  expect_lt(q_100, q_1000)
})

test_that("Kappa3Est return period increases with quantile", {
  loc <- 300
  scale <- 50
  shape <- 0.2

  q1 <- 500
  q2 <- 520
  q3 <- 540

  RP_1 <- Kappa3Est(loc, scale, shape, q = q1)
  RP_2 <- Kappa3Est(loc, scale, shape, q = q2)
  RP_3 <- Kappa3Est(loc, scale, shape, q = q3)

  expect_lt(RP_1, RP_2)
  expect_lt(RP_2, RP_3)
})

test_that("Kappa3Est matches GumbelEst when shape = 0", {
  loc <- 300
  scale <- 50
  shape <- 0
  RP <- 50

  q_kappa <- Kappa3Est(loc, scale, shape, RP = RP)
  q_gumbel <- GumbelEst(loc, scale, RP = RP)

  expect_equal(q_kappa, q_gumbel, tolerance = 1e-10)

  q_val <- 400
  RP_kappa <- Kappa3Est(loc, scale, shape, q = q_val)
  RP_gumbel <- GumbelEst(loc, scale, q = q_val)

  expect_equal(RP_kappa, RP_gumbel, tolerance = 1e-10)
})

test_that("Kappa3Est throws error for non-positive scale", {
  expect_error(Kappa3Est(loc = 300, scale = 0, shape = 0.2, RP = 10), "Scale parameter must be positive")
  expect_error(Kappa3Est(loc = 300, scale = -10, shape = 0.2, RP = 10), "Scale parameter must be positive")
})

test_that("Kappa3Est throws error when q exceeds theoretical bounds", {
  loc <- 300
  scale <- 50

  # shape > 0, q too high
  shape <- 0.2
  q_high <- loc + scale / shape + 1
  expect_error(Kappa3Est(loc, scale, shape, q = q_high), "Quantile exceeds theoretical upper bound")

  # shape < 0, q too low
  shape <- -0.2
  q_low <- loc + scale / shape - 1
  expect_error(Kappa3Est(loc, scale, shape, q = q_low), "Quantile below theoretical lower bound")
})




###### Kappa3GF ######
test_that("Kappa3GF returns valid numeric output", {
  lcv <- 0.2
  lskew <- 0.1
  RP <- 100

  gf <- Kappa3GF(lcv, lskew, RP)

  expect_type(gf, "double")
  expect_length(gf, 1)
  expect_true(is.finite(gf))
  expect_gt(gf, 1)
})

test_that("Kappa3GF increases with return period", {
  lcv <- 0.2
  lskew <- 0.1

  gf_10 <- Kappa3GF(lcv, lskew, RP = 10)
  gf_100 <- Kappa3GF(lcv, lskew, RP = 100)
  gf_1000 <- Kappa3GF(lcv, lskew, RP = 1000)

  expect_lt(gf_10, gf_100)
  expect_lt(gf_100, gf_1000)
})

test_that("Kappa3GF increases with LCV", {
  lskew <- 0.1
  RP <- 100

  gf_small <- Kappa3GF(lcv = 0.1, lskew = lskew, RP = RP)
  gf_large <- Kappa3GF(lcv = 0.3, lskew = lskew, RP = RP)

  expect_lt(gf_small, gf_large)
})

test_that("Kappa3GF increases with LSkew", {
  lcv <- 0.2
  RP <- 100

  gf_low_skew <- Kappa3GF(lcv = lcv, lskew = 0, RP = RP)
  gf_high_skew <- Kappa3GF(lcv = lcv, lskew = 0.3, RP = RP)

  expect_lt(gf_low_skew, gf_high_skew)
})

test_that("Kappa3GF returns realistic values for RP = 100", {
  lcv_values <- c(0.1, 0.2, 0.3)
  lskew_values <- c(0, 0.1, 0.3)

  for (lcv in lcv_values) {
    for (lskew in lskew_values) {
      gf <- Kappa3GF(lcv, lskew, RP = 100)
      expect_gt(gf, 1.25)
      expect_lt(gf, 6)
    }
  }
})

test_that("Kappa3GF handles small and large LCV/LSkew", {
  RP <- 100

  gf_small <- Kappa3GF(lcv = 0.001, lskew = 0.001, RP = RP)
  gf_large <- Kappa3GF(lcv = 0.4, lskew = 0.4, RP = RP)

  expect_true(is.finite(gf_small))
  expect_true(is.finite(gf_large))
  expect_gt(gf_large, gf_small)
})

###### Kappa3Pars ######
test_that("Kappa3Pars returns numeric parameters with correct names", {
  set.seed(123)
  x <- rlnorm(100, meanlog = 5, sdlog = 0.5)
  result <- Kappa3Pars(x = x)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("Loc", "Scale", "Shape"))
  expect_true(all(sapply(result, is.numeric)))
  expect_equal(nrow(result), 1)
})

test_that("Kappa3Pars gives same result for x input and L-moment input", {
  set.seed(123)
  x <- rlnorm(100, meanlog = 5, sdlog = 0.5)

  # From x
  result_x <- as.numeric(Kappa3Pars(x = x))

  # Manually compute L1, LCV, LSKEW
  Sort.x <- sort(x)
  Rank <- seq_along(x)
  b0 <- mean(x)
  b1 <- mean((Rank - 1) / (length(x) - 1) * Sort.x)
  L1 <- b0
  L2 <- 2 * b1 - b0
  LCV <- L2 / L1
  LSKEW <- LSkew(x)

  result_lmom <- as.numeric(Kappa3Pars(L1 = L1, LCV = LCV, LSKEW = LSKEW))

  expect_equal(result_x, result_lmom, tolerance = 1e-8)
})
test_that("Kappa3Pars internal L-moment logic matches manual calculation", {
  set.seed(123)
  x <- rlnorm(100, meanlog = 5, sdlog = 0.5)

  Sort.x <- sort(x)
  n <- length(x)
  j <- 2:n
  b0_manual <- mean(Sort.x)
  b1_manual <- sum((j - 1) / (n - 1) * Sort.x[j]) / n
  L2_manual <- 2 * b1_manual - b0_manual

  # UKFE version
  Rank <- seq(1, n)
  b0_ukfe <- mean(x)
  b1_ukfe <- mean((Rank - 1) / (n - 1) * Sort.x)
  L2_ukfe <- 2 * b1_ukfe - b0_ukfe

  expect_equal(b1_manual, b1_ukfe, tolerance = 1e-10)
  expect_equal(L2_manual, L2_ukfe, tolerance = 1e-10)
})
test_that("Kappa3Pars handles invalid inputs correctly", {
  expect_error(Kappa3Pars(x = "not a numeric vector"))
  expect_error(Kappa3Pars(L1 = 100, LCV = "bad", LSKEW = 0.2))
})

##### QMED Functions #####



###### DonAdj ######

test_that("Output matches comparing UKFE WINFAP v5 for FEH methods report", {
  CDs <- CDsXML(testthat::test_path("testdata", "FEH_Catchment_Descriptors_Scenario4.xml"))

  result <- DonAdj(CDs, rows = 2)
  expect_true(all(c("66011", "65001") %in% rownames(result)))
})


test_that("Error is thrown when both CDs and QMEDscd are missing", {
  expect_error(
    DonAdj(x = 123, y = 456),
    "The QMED estimate must be an input"
  )
})

test_that("Returns donor candidates using GetCDs input", {
  CDs <- GetCDs(54022)
  result <- DonAdj(CDs = CDs, rows = 3)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("QMED.adj", "Dists") %in% names(result)))
})


test_that("Returns donor candidates using x/y and QMEDscd", {
  result <- DonAdj(x = 283261, y = 288067, QMEDscd = 17.931, rows = 3)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("QMED.adj", "Dists") %in% names(result)))
})

test_that("Exponent is set to 1 when alpha is FALSE", {
  result <- DonAdj(x = 283261, y = 288067, QMEDscd = 17.931, alpha = FALSE, rows = 3)
  expect_true(all(result$a == 1))
})

test_that("Two donor adjustment returns correct structure", {
  CDs <- GetCDs(54022)
  result <- DonAdj(CDs = CDs, d2 = c("54092", "54091"))
  expect_s3_class(result, "data.frame")
  expect_named(result, c("QMEDs.adj", "a1", "a2"))
  expect_equal(nrow(result), 1)
})

test_that("Returns top N rows only", {
  CDs <- GetCDs(54022)
  result <- DonAdj(CDs = CDs, rows = 5)
  expect_equal(nrow(result), 5)
})

###### NGRDist ######


test_that("Correct distance is calculated for known points", {
  i <- c(381355, 839183)
  j <- c(462899, 187850)
  expected <- sqrt((381355 - 462899)^2 + (839183 - 187850)^2) / 1000
  result <- NGRDist(i, j)
  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("Distance is 0 when both points are the same", {
  pt <- c(400000, 800000)
  expect_equal(NGRDist(pt, pt), 0)
})

test_that("Handles negative coordinates correctly", {
  i <- c(-1000, -1000)
  j <- c(1000, 1000)
  expected <- sqrt((2000)^2 + (2000)^2) / 1000
  result <- NGRDist(i, j)
  expect_equal(result, expected, tolerance = 1e-6)
})

test_that("Errors when inputs are not numeric vectors of length 2", {
  expect_error(
    NGRDist(i = c(1), j = c(2, 3)),
    "Argument 'i' must be a numeric vector of length 2"
  )

  expect_error(
    NGRDist(i = c(1, 2, 3), j = c(4, 5)),
    "Argument 'i' must be a numeric vector of length 2"
  )

  expect_error(
    NGRDist(i = c(1, 2), j = "not a vector"),
    "Argument 'j' must be a numeric vector of length 2"
  )

  expect_error(
    NGRDist(i = "not numeric", j = c(4, 5)),
    "Argument 'i' must be a numeric vector of length 2"
  )
})

test_that("Returns a numeric scalar", {
  result <- NGRDist(c(100, 200), c(300, 400))
  expect_type(result, "double")
  expect_length(result, 1)
})

###### QMED ######

testthat::test_that("Check that the QMED is calcualted correctly for gauge 55004 with no donors", {
  library(UKFE)

  CDs.55004 <- GetCDs(55004)

  expected_output <-
    8.3062 * (CDs.55004$Value[CDs.55004$Descriptor == "AREA"]^0.8510) * 0.1536^
      (1000 / CDs.55004$Value[CDs.55004$Descriptor == "SAAR"]) * (CDs.55004$Value[CDs.55004$Descriptor == "FARL"]^
      3.4451) * 0.0460^(CDs.55004$Value[CDs.55004$Descriptor == "BFIHOST19"]^2)

  actual_output <- QMED(CDs.55004)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("QMED urban adjustment without UEF", {
  CDs.55004 <- GetCDs(55004)
  actual_output <- QMED(CDs = CDs.55004, UrbAdj = TRUE)

  urbext <- CDs.55004$Value[CDs.55004$Descriptor == "URBEXT2000"]
  bfihost <- CDs.55004$Value[CDs.55004$Descriptor == "BFIHOST19"]
  qmed_cd <-
    8.3062 * CDs.55004$Value[CDs.55004$Descriptor == "AREA"]^0.8510 *
      0.1536^(1000 / CDs.55004$Value[CDs.55004$Descriptor == "SAAR"]) *
      CDs.55004$Value[CDs.55004$Descriptor == "FARL"]^3.4451 *
      0.0460^(bfihost^2)
  expected_output <-
    as.numeric(UAF(URBEXT2000 = urbext, BFIHOST = bfihost)[2]) * qmed_cd

  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that("QMED urban adjustment with UEF", {
  CDs.55004 <- GetCDs(55004)
  actual_output <- QMED(CDs = CDs.55004, UrbAdj = TRUE, uef = TRUE)

  yr <- as.POSIXlt(Sys.Date())$year + 1900
  urbext <- CDs.55004$Value[CDs.55004$Descriptor == "URBEXT2000"] * UEF(yr)
  bfihost <- CDs.55004$Value[CDs.55004$Descriptor == "BFIHOST19"]
  qmed_cd <- 8.3062 * CDs.55004$Value[CDs.55004$Descriptor == "AREA"]^0.8510 *
    0.1536^(1000 / CDs.55004$Value[CDs.55004$Descriptor == "SAAR"]) *
    CDs.55004$Value[CDs.55004$Descriptor == "FARL"]^3.4451 *
    0.0460^(bfihost^2)
  expected_output <- as.numeric(UAF(URBEXT2000 = urbext, BFIHOST = bfihost)[2]) * qmed_cd

  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that("QMED errors when URBEXT2000 missing but UrbAdj = TRUE", {
  testthat::expect_error(
    QMED(AREA = 100, SAAR = 1000, FARL = 0.9, BFIHOST = 0.4, UrbAdj = TRUE),
    "URBEXT2000 is required"
  )
})

testthat::test_that("QMED prints urban adjustment suggestion for URBEXT > 0.03", {
  CDs <- GetCDs(55004)
  CDs[CDs$Descriptor == "URBEXT2000", "Value"] <- 0.05
  testthat::expect_output(QMED(CDs = CDs), "URBEXT > 0.03, urban adjustment is recommended")
})

testthat::test_that("QMED errors on incorrect Don1 length", {
  testthat::expect_error(
    QMED(Don1 = c("55004", "55005"), Easting = 400000, Northing = 400000),
    "Don1 argument has length that is not 1"
  )
})

testthat::test_that("QMED errors on incorrect Don2 length", {
  testthat::expect_error(
    QMED(Don2 = c("55004"), Easting = 400000, Northing = 400000),
    "Don2 argument has length that is not 2"
  )
})

testthat::test_that("QMED errors if Don1 used without CDs and no coordinates", {
  testthat::expect_error(QMED(Don1 = "55004"), "you need to add the easting and northing")
})

testthat::test_that("Check that the QMED is calcualted correctly for gauge 55004 with one donor 55012", {
  library(UKFE)

  CDs.55004 <- GetCDs(55004)

  qmed_scd <-
    8.3062 * (CDs.55004$Value[CDs.55004$Descriptor == "AREA"]^0.8510) * 0.1536^
      (1000 / CDs.55004$Value[CDs.55004$Descriptor == "SAAR"]) * (CDs.55004$Value[CDs.55004$Descriptor == "FARL"]^
      3.4451) * 0.0460^(CDs.55004$Value[CDs.55004$Descriptor == "BFIHOST19"]^2)

  qmed_cd_don_adj <-
    DonAdj(
      x = CDs.55004$Value[CDs.55004$Descriptor == "Easting"],
      y = CDs.55004$Value[CDs.55004$Descriptor == "Northing"],
      QMEDscd = qmed_scd,
      rows = nrow(QMEDData)
    )

  expected_output <- qmed_cd_don_adj$QMED.adj[which(rownames(qmed_cd_don_adj) == 55012)]

  actual_output <- QMED(CDs.55004, Don1 = 55012)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Check that the QMED is calcualted correctly for gauge 55004 with two donors, 55012 and 60007", {
  library(UKFE)

  CDs.55004 <- GetCDs(55004)
  CDs.55012 <- GetCDs(55012)
  CDs.60007 <- GetCDs(60007)

  qmed_scd <-
    8.3062 * (CDs.55004$Value[CDs.55004$Descriptor == "AREA"]^0.8510) * 0.1536^
      (1000 / CDs.55004$Value[CDs.55004$Descriptor == "SAAR"]) * (CDs.55004$Value[CDs.55004$Descriptor == "FARL"]^
      3.4451) * 0.0460^(CDs.55004$Value[CDs.55004$Descriptor == "BFIHOST19"]^2)

  qmed_don1 <-
    8.3062 * (CDs.55012$Value[CDs.55012$Descriptor == "AREA"]^0.8510) * 0.1536^
      (1000 / CDs.55012$Value[CDs.55012$Descriptor == "SAAR"]) * (CDs.55012$Value[CDs.55012$Descriptor == "FARL"]^
      3.4451) * 0.0460^(CDs.55012$Value[CDs.55012$Descriptor == "BFIHOST19"]^2)

  qmed_don2 <-
    8.3062 * (CDs.60007$Value[CDs.60007$Descriptor == "AREA"]^0.8510) * 0.1536^
      (1000 / CDs.60007$Value[CDs.60007$Descriptor == "SAAR"]) * (CDs.60007$Value[CDs.60007$Descriptor == "FARL"]^
      3.4451) * 0.0460^(CDs.60007$Value[CDs.60007$Descriptor == "BFIHOST19"]^2)

  euc_dist <- function(x_east, x_nor, y_east, y_nor) {
    sqrt((x_east - y_east)^2 + (x_nor - y_nor)^2) / 1000
  }

  dist_s1 <-
    euc_dist(
      CDs.55004$Value[CDs.55004$Descriptor == "Easting"],
      CDs.55004$Value[CDs.55004$Descriptor == "Northing"],
      CDs.55012$Value[CDs.55012$Descriptor == "Easting"],
      CDs.55012$Value[CDs.55012$Descriptor == "Northing"]
    )
  dist_s2 <-
    euc_dist(
      CDs.55004$Value[CDs.55004$Descriptor == "Easting"],
      CDs.55004$Value[CDs.55004$Descriptor == "Northing"],
      CDs.60007$Value[CDs.60007$Descriptor == "Easting"],
      CDs.60007$Value[CDs.60007$Descriptor == "Northing"]
    )
  dist_12 <-
    euc_dist(
      CDs.55012$Value[CDs.55012$Descriptor == "Easting"],
      CDs.55012$Value[CDs.55012$Descriptor == "Northing"],
      CDs.60007$Value[CDs.60007$Descriptor == "Easting"],
      CDs.60007$Value[CDs.60007$Descriptor == "Northing"]
    )

  pij <- function(dij) {
    0.4598 * exp(-0.0200 * dij) + (1 - 0.4598) * exp(-0.4785 * dij)
  }

  ps1 <- pij(dist_s1)
  ps2 <- pij(dist_s2)
  p12 <- pij(dist_12)

  w1 <- (ps1 - p12 * ps2) / (1 - p12^2)
  w2 <- (ps2 - p12 * ps1) / (1 - p12^2)

  qmed_obs_don1 <- GetQMED(55012)
  qmed_obs_don2 <- GetQMED(60007)

  expected_output <- qmed_scd * (qmed_obs_don1 / qmed_don1)^w1 * (qmed_obs_don2 / qmed_don2)^w2

  actual_output <- QMED(CDs.55004, Don2 = c(55012, 60007))$QMEDs.adj

  testthat::expect_equal(expected_output, actual_output)
})

###### QMEDDonEq ######

testthat::test_that("Check that the adjusted QMED for 1 donor matches that calculated in Table 4 of Circulation 140 of the British Hydrological Society Newsletter ", {
  expected_output <- 31.3

  actual_output <- round(QMEDDonEq(63.27, 1299, 1, 0.494, 23.5, 23.9, 297622, 238444, 302454, 237136), 1)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Check that when alpha is set to FALSE then the calculation for one donor simplifies to QMED_cd * (QMED_don_obs / QMED_don_cd)", {
  expected_output <- round(31.6 * (23.5 / 23.9), 1)

  actual_output <- round(QMEDDonEq(63.27, 1299, 1, 0.494, 23.5, 23.9, 297622, 238444, 302454, 237136, alpha = FALSE), 1)

  testthat::expect_equal(expected_output, actual_output)
})

###### UEF ######

testthat::test_that("Check that the UEF is calculated correctly for 1998", {
  expected_output <- 0.7851 + 0.2124 * atan((1998 - 1967.5) / 20.32)

  actual_output <- UEF(1998)

  testthat::expect_equal(expected_output, actual_output)
})

###### UAF ######

testthat::test_that("Check that the UAF is calculate correctly for station 9001", {
  stations <- rnrfa::catalogue()

  pruaf <- 1 + 0.3 * (1.567 * stations$`urbext-2000`[stations$id == "9001"]) * ((70 / (69.366 - 65.686 * stations$bfihost[stations$id == "9001"])) - 1)

  uaf <- ((1 + 0.3 * (1.567 * stations$`urbext-2000`[stations$id == "9001"]))^(1.25)) * (pruaf^(1.33))
  expected_output <- data.frame(PRUAF = pruaf, UAF = uaf)

  actual_output <- UAF(URBEXT2000 = stations$`urbext-2000`[stations$id == "9001"], BFIHOST = stations$bfihost[stations$id == "9001"])

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Check that the UAF is calculate correctly for station 9001", {
  library(UKFE)

  stations <- GetCDs(9001)

  pruaf <- 1 + 0.3 * (1.567 * stations$Value[stations$Descriptor == "URBEXT2000"]) * ((70 / (69.366 - 65.686 * stations$Value[stations$Descriptor == "BFIHOST19"])) - 1)

  uaf <- ((1 + 0.3 * (1.567 * stations$Value[stations$Descriptor == "URBEXT2000"]))^(1.25)) * (pruaf^(1.33))
  expected_output <- data.frame(PRUAF = pruaf, UAF = uaf)

  actual_output <- UAF(CDs = stations)

  testthat::expect_equal(expected_output, actual_output)
})

###### NonFloodAdj ######

testthat::test_that("Check that the Non flood year adjusted Lcv and LSkew are calculated in accordance with FEH1999 Vol 3.", {
  flow <- GetAM(42006)

  flow_sub <- flow[flow$Date < as.Date("1993-10-01"), ]

  w <- length(flow_sub$Flow[flow_sub$Flow >= median(flow_sub$Flow) / 2]) / length(flow_sub$Flow)
  lcv <- Lcv(flow_sub$Flow[flow_sub$Flow >= median(flow_sub$Flow) / 2])
  k_dash <- -LSkew(flow_sub$Flow[flow_sub$Flow >= median(flow_sub$Flow) / 2])
  beta_dash <- (lcv * k_dash * sin(pi * k_dash)) / (k_dash * pi * (k_dash + lcv) - lcv * sin(pi * k_dash))

  f <- function(k_star) {
    abs((1 - 9^-k_star) / (1 - 49^-k_star) - ((1 - ((10 * w - 1) / (2 * w - 1))^-k_dash) / (1 - ((50 * w - 1) / (2 * w - 1))^-k_dash)))
  }

  k_star <- optimise(f, lower = -5, upper = 5)$minimum

  A <- ((2 * w - 1)^-k_dash - (10 * w - 1)^-k_dash) / (1 - 9^-k_star)
  B <- (2 * w - 1)^-k_dash
  beta_star <- (beta_dash * k_star * A) / (k_dash + beta_dash * (1 - B))

  lcv_adj <- (beta_star * k_star^2 * pi) / ((beta_star + k_star) * sin(k_star * pi) - beta_star * k_star * pi)
  lskew_adj <- -k_star

  actual_output <- NonFloodAdj(flow_sub$Flow)

  testthat::expect_equal(actual_output[[1]]$Lcv, lcv_adj, tolerance = 0.01)
  testthat::expect_equal(actual_output[[1]]$LSkew, lskew_adj, tolerance = 0.01)
})

###### NonFloodAdjPool ######

testthat::test_that("Check the NonFloodAdjPool calculates the NonFloodAdj Lcv and LSkew for each site in the pooling group as expected", {
  library(UKFE)

  Pool44013 <- Pool(GetCDs(44013))
  actual_output <- NonFloodAdjPool(Pool44013)

  expected_output <- data.frame(matrix(ncol = 2, nrow = 0))

  for (i in seq_along(rownames(Pool44013))) {
    gauge <- rownames(Pool44013)[i]
    expected_output <- rbind(expected_output, NonFloodAdj(GetAM(gauge)$Flow)[[1]])
  }

  testthat::expect_equal(actual_output$Lcv, expected_output$Lcv)
  testthat::expect_equal(actual_output$LSkew, expected_output$LSkew)
})

testthat::test_that("Error if x is not a data.frame", {
  testthat::expect_error(NonFloodAdjPool(matrix(1:10, nrow = 2)), "x must be a pooling group")
})

testthat::test_that("Error if x does not have 24 columns", {
  df <- data.frame(matrix(1:20, ncol = 20))
  testthat::expect_error(NonFloodAdjPool(df), "x must be a pooling group")
})

testthat::test_that("Warning if both Index and AutoP are provided", {
  library(UKFE)

  Pool44013 <- Pool(GetCDs(44013))
  testthat::expect_error(NonFloodAdjPool(Pool44013, Index = 1:2, AutoP = 0.2), "Either Index or AutoP should be applied")
})

testthat::test_that("Warn if no gauges exceed AutoP threshold", {
  library(UKFE)

  Pool44013 <- Pool(GetCDs(44013))

  testthat::expect_warning(NonFloodAdjPool(Pool44013, AutoP = 50), "None of the AMAX samples have greater")
})

testthat::test_that("ReturnStats = TRUE returns data frame of non-flood stats", {
  library(UKFE)

  Pool44013 <- Pool(GetCDs(44013))
  actual_output <- NonFloodAdjPool(Pool44013, ReturnStats = TRUE)

  expected_output <- data.frame(matrix(ncol = 4, nrow = 0))

  for (i in seq_along(rownames(Pool44013))) {
    gauge <- rownames(Pool44013)[i]
    expected_output <- rbind(expected_output, NonFloodAdj(GetAM(gauge)$Flow)[[2]])
  }

  rownames(expected_output) <- rownames(Pool44013)
  expected_output <- tibble::rownames_to_column(expected_output, "ID")

  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that("Index argument applies adjustment only to specified sites", {
  library(UKFE)

  Pool44013 <- Pool(GetCDs(44013))
  result_indexed <- NonFloodAdjPool(Pool44013, Index = 1)

  testthat::expect_equal(result_indexed$Lcv[1], NonFloodAdj(GetAM(rownames(Pool44013)[1])$Flow)[[1]]$Lcv)
  testthat::expect_equal(result_indexed$LSkew[1], NonFloodAdj(GetAM(rownames(Pool44013)[1])$Flow)[[1]]$LSkew)
  testthat::expect_equal(result_indexed$Lcv[2:nrow(result_indexed)], Pool44013$Lcv[2:nrow(Pool44013)])
  testthat::expect_equal(result_indexed$LSkew[2:nrow(result_indexed)], Pool44013$LSkew[2:nrow(Pool44013)])
})

##### Other Functions #####

###### ReFH ######
testthat::test_that("Check that ReFH calculates the baseflow, routing and loss model parameters correctly", {
  expected_output <- data.frame(AREA = 65.38, D = 6.5, BR = 0.928, BL = 31.652, Cmax = 228.058, Cini = 132.388, BFini = 7.659)

  actual_output <- ReFH(data.frame(
    Descriptor = c(
      "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST", "DPLBAR", "DPSBAR", "FARL", "FPEXT",
      "LDP", "PROPWET", "RMED-1H", "REMD-1D", "RMED-2D", "SAAR", "SAAR4170", "SPRHOST",
      "URBEXT2000", "Easting", "Northing", "URBEXT1990", "BFI"
    ),
    Value = c(
      65.38, 0, 0, 0, 0.322, 10.58, 131.7, 0, 0, 0,
      0.62, 0, 0, 0, 1981, 0, 0, 0, 291350, 208100, 0, 0
    )
  ), duration = 6.5, RPa = 50, alpha = TRUE, timestep = 0.5, Depth = 74.4)[[1]]

  actual_output$TP <- NULL

  testthat::expect_equal(expected_output, round(actual_output, 3))
})

testthat::test_that("Check that ReFH calculates total flow within 5% of the FEH supplementary report example 4.6", {
  expected_output <- c(
    9.2, 9.5, 10.7, 13.6, 19.0, 28.3, 42.8, 64.7, 93.6, 125.4, 155.1, 176.8, 185.6, 182.8,
    171.9, 156, 138.3, 120.6, 103.6, 88, 74, 61.4, 50.5, 41.9, 35.7, 31.6, 29, 27.3, 26.2
  )

  testthat::expect_warning(
    {
      actual_output <-
        ReFH(
          duration = 6.5,
          RPa = 50,
          Cmax = 293.4,
          Cini = 170,
          timestep = 0.5,
          TP = 2.26,
          BL = 43,
          alpha = TRUE,
          BR = 0.9,
          Rain = c(1.3, 2, 3.2, 4.9, 7.4, 11.1, 14.5, 11.1, 7.4, 4.9, 3.2, 2, 1.3),
          season = "winter",
          AREA = 65.38,
          BFini = 9.2
        )[[2]]

      testthat::expect_equal(expected_output, actual_output$TotalFlow, tolerance = 0.05)
    },
    regexp = "duration > 4.5TP"
  )
})

testthat::test_that("ReFH works with manually specified parameters and no CDs", {
  result <- ReFH(
    CDs = NULL,
    AREA = 65.38,
    TP = 2.5,
    duration = 6,
    BR = 1.2,
    BL = 30,
    Cmax = 220,
    Cini = 120,
    BFini = 8,
    Depth = 74.4,
    RPa = 10,
    alpha = TRUE,
    season = "winter",
    timestep = 0.25
  )
  testthat::expect_type(result, "list")
  testthat::expect_named(result[[1]], c("AREA", "TP", "D", "BR", "BL", "Cmax", "Cini", "BFini"))
})

testthat::test_that("ReFH warns if RPa is provided but alpha is FALSE", {
  testthat::expect_output(
    ReFH(
      CDs = NULL,
      AREA = 50, TP = 2, duration = 6, BR = 1.2, BL = 30,
      Cmax = 200, Cini = 100, BFini = 5, Depth = 50,
      RPa = 20, alpha = FALSE, season = "winter"
    ),
    "Warning: You've chosen an RPa value and have alpha = FALSE. The RPa argument, in this case, does nothing"
  )
})

###### Zdists ######
testthat::test_that("Check that Zdists is calculated correctly for station 69048's pooling group", {
  library(UKFE)

  CDs.69048 <- GetCDs(69048)
  Pool.69048 <- Pool(CDs.69048)

  simulate_pooling_group <- function(x, dist = "GEV", LCV, LSKEW) {
    simulated_data <- vector("list", length = nrow(x))

    for (i in seq_len(nrow(x))) {
      n <- x$N[i]
      if (dist == "GEV") {
        simulated_data[[i]] <- SimData(n = n, dist = "GEV", GF = c(LCV, LSKEW, 1))
      } else if (dist == "GenLog") {
        simulated_data[[i]] <- SimData(n = n, dist = "GenLog", GF = c(LCV, LSKEW, 1))
      } else if (dist == "Gumbel") {
        simulated_data[[i]] <- SimData(n = n, dist = "Gumbel", GF = c(LCV, 1))
      } else if (dist == "Kappa3") {
        simulated_data[[i]] <- SimData(n = n, dist = "Kappa3", GF = c(LCV, LSKEW, 1))
      } else {
        stop("Unsupported distribution")
      }
    }

    LKURTs <- vapply(simulated_data, LKurt, numeric(1))
    weighted_LKURT <- sum(LKURTs * x$N / sum(x$N))

    return(weighted_LKURT)
  }

  Zdists_test <- function(x, n_sims = 500) {
    # Weighted observed L-moments
    LCV <- sum(x$Lcv * x$N / sum(x$N))
    LSKEW <- sum(x$LSkew * x$N / sum(x$N))
    LKURT <- sum(x$LKurt * x$N / sum(x$N))

    # Function to simulate LKURTs
    simulate_dist <- function(dist) {
      replicate(n_sims, simulate_pooling_group(x, dist, LCV, LSKEW))
    }

    # Run simulations
    sim_GEV <- simulate_dist("GEV")
    sim_GenLog <- simulate_dist("GenLog")
    sim_Gumbel <- simulate_dist("Gumbel")
    sim_Kappa3 <- simulate_dist("Kappa3")

    # Z-scores
    Z_GEV <- (LKURT - mean(sim_GEV)) / sd(sim_GEV)
    Z_GenLog <- (LKURT - mean(sim_GenLog)) / sd(sim_GenLog)
    Z_Gumbel <- (LKURT - mean(sim_Gumbel)) / sd(sim_Gumbel)
    Z_Kappa3 <- (LKURT - mean(sim_Kappa3)) / sd(sim_Kappa3)

    z_scores <- data.frame(
      GEV     = signif(Z_GEV, 3),
      GenLog  = signif(Z_GenLog, 3),
      Gumbel  = signif(Z_Gumbel, 3),
      Kappa3  = signif(Z_Kappa3, 3)
    )

    best_fit <- names(which.min(abs(z_scores)))

    return(z_scores)
  }

  set.seed(7)
  zdists_test_res <- Zdists_test(Pool.69048)

  set.seed(7)
  zdists_ukfe_res <- Zdists(Pool.69048)[[1]]

  testthat::expect_equal(zdists_test_res, zdists_ukfe_res)
})

###### SCF ######

testthat::test_that("Check that SCF is calculate in accordance with Example 4.2 from the FEH Supplementary report", {
  expected_output <- (1 - exp(-1981 * 0.000906))^0.4690

  actual_output <- SCF(1981, 6.5)$SCFWinter

  testthat::expect_equal(round(expected_output, 3), actual_output)
})

testthat::test_that("SCF handles duration boundaries correctly", {
  # Lower boundary
  testthat::expect_equal(SCF(1000, 0.5), SCF(1000, 1)) # Should clamp to 1
  # Upper boundary
  testthat::expect_equal(SCF(1000, 25), SCF(1000, 24)) # Should clamp to 24
})

testthat::test_that("SCF handles SAAR = 500 case", {
  # When SAAR = 500, SCFSummer should be exactly 1
  result <- SCF(500, 10)
  testthat::expect_equal(result$SCFSummer, 1)
})

###### H2 ######

# Create tweaked nsRFA helper functions for benchmarking nsRFA HW.tests against UKFE
HW.tests_correct <- function(x, cod, Nsim = 500) {
  if (length(x) != length(cod)) {
    stop("x and cod must have the same length")
  }
  fac <- factor(cod)
  ni <- tapply(x, fac, length)
  k <- nlevels(fac)
  Lm <- sapply(split(x, fac), Lmoments)
  rLm <- regionalLmoments(x, fac)
  ti <- as.numeric(Lm[3, ])
  t3i <- as.numeric(Lm[4, ])
  t4i <- as.numeric(Lm[5, ])
  lambda1Reg <- as.numeric(1)
  lambda2Reg <- as.numeric(rLm[3])
  tauReg <- as.numeric(rLm[3])
  tau3Reg <- as.numeric(rLm[4])
  tau4Reg <- as.numeric(rLm[5])
  V1 <- (sum(ni * (ti - tauReg)^2) / sum(ni))^0.5
  V2 <- (sum(ni * ((ti - tauReg)^2 + (t3i - tau3Reg)^2)) / sum(ni))^0.5
  V3 <- (sum(ni * ((t3i - tau3Reg)^2 + (t4i - tau4Reg)^2)) / sum(ni))^0.5
  parkappa <- par.kappa_correct(lambda1Reg, lambda2Reg, tau3Reg, tau4Reg)
  xi <- parkappa$xi
  alfa <- parkappa$alfa
  kappa <- parkappa$k
  hacca <- parkappa$h
  V1s <- rep(NA, Nsim)
  V2s <- rep(NA, Nsim)
  V3s <- rep(NA, Nsim)
  for (i in 1:Nsim) {
    ti.sim <- rep(NA, k)
    t3i.sim <- rep(NA, k)
    t4i.sim <- rep(NA, k)
    for (j in 1:k) {
      campione <- rand.kappa_correct(ni[j], xi, alfa, kappa, hacca)
      lmom <- Lmoments(campione)
      ti.sim[j] <- lmom[3]
      t3i.sim[j] <- lmom[4]
      t4i.sim[j] <- lmom[5]
    }
    tauReg.sim <- sum(ni * ti.sim) / sum(ni)
    tau3Reg.sim <- sum(ni * t3i.sim) / sum(ni)
    tau4Reg.sim <- sum(ni * t4i.sim) / sum(ni)
    V1s[i] <- (sum(ni * (ti.sim - tauReg.sim)^2) / sum(ni))^0.5
    V2s[i] <- (sum(ni * ((ti.sim - tauReg.sim)^2 + (t3i.sim -
      tau3Reg.sim)^2)) / sum(ni))^0.5
    V3s[i] <- (sum(ni * ((t3i.sim - tau3Reg.sim)^2 + (t4i.sim -
      tau4Reg.sim)^2)) / sum(ni))^0.5
  }

  muV1 <- mean(V1s)
  stdV1 <- sd(V1s)
  muV2 <- mean(V2s)
  stdV2 <- sd(V2s)
  muV3 <- mean(V3s)
  stdV3 <- sd(V3s)
  H1 <- (V1 - muV1) / stdV1
  H2 <- (V2 - muV2) / stdV2
  H3 <- (V3 - muV3) / stdV3
  output <- c(H1, H2, H3)
  names(output) <- c("H1", "H2", "H3")
  return(output)
}

par.kappa_correct <- function(lambda1, lambda2, tau3, tau4) {
  lambda1 <- as.numeric(lambda1)
  lambda2 <- as.numeric(lambda2)
  tau3 <- as.numeric(tau3)
  tau4 <- as.numeric(tau4)
  sumquad.tau3tau4 <- function(k.h, t3.t4) {
    k <- k.h[1]
    h <- k.h[2]
    t3 <- t3.t4[1]
    t4 <- t3.t4[2]
    error <- FALSE
    if (((k < -1) && (h >= 0)) || ((h < 0) && ((k <= -1) ||
      (k >= -1 / h)))) {
      stop("L-moments are defined if h>=0 and k>-1, or if h<0 and -1<k<-1/h")
      error <- TRUE
    }
    g <- c(0, 0, 0, 0)
    if (h == 0) {
      tau3 <- 2 * (1 - 3^(-k)) / (1 - 2^(-k)) - 3
      tau4 <- (5 * (1 - 4^(-k)) - 10 * (1 - 3^(-k)) + 6 *
        (1 - 2^(-k))) / (1 - 2^(-k))
    } else {
      for (r in 1:4) {
        if (h > 0) {
          g[r] <- (r * gamma(1 + k) * gamma(r / h)) / (h^(1 +
            k) * gamma(1 + k + r / h))
        } else {
          g[r] <- (r * gamma(1 + k) * gamma(-k - r / h)) / ((-h)^(1 +
            k) * gamma(1 - r / h))
        }
      }
      tau3 <- (-g[1] + 3 * g[2] - 2 * g[3]) / (g[1] - g[2])
      tau4 <- -(-g[1] + 6 * g[2] - 10 * g[3] + 5 * g[4]) / (g[1] -
        g[2])
    }
    if (error == FALSE) {
      output <- (t3 - tau3)^2 + (t4 - tau4)^2
    } else {
      output <- -1
    }
    return(output)
  }
  xi.alfa <- function(lambda1, lambda2, k, h) {
    if (((k < -1) && (h >= 0)) || ((h < 0) && ((k <= -1) ||
      (k >= -1 / h)))) {
      stop("L-moments are defined if h>=0 and k>-1, or if h<0 and -1<k<-1/h")
    }
    g <- c(0, 0)
    if (h == 0) {
      alfa <- (lambda2 * k) / ((1 - 2^(-k)) * gamma(1 + k))
      xi <- lambda1 - alfa * (1 - gamma(1 + k)) / k
    } else {
      for (r in 1:2) {
        if (h > 0) {
          g[r] <- (r * gamma(1 + k) * gamma(r / h)) / (h^(1 +
            k) * gamma(1 + k + r / h))
        } else {
          g[r] <- (r * gamma(1 + k) * gamma(-k - r / h)) / ((-h)^(1 +
            k) * gamma(1 - r / h))
        }
      }
      alfa <- (lambda2 * k) / (g[1] - g[2])
      xi <- lambda1 - alfa * (1 - g[1]) / k
    }
    output <- list(xi = xi, alfa = alfa)
    return(output)
  }
  quanti <- length(tau3)
  k <- rep(NA, quanti)
  h <- rep(NA, quanti)
  xi <- rep(NA, quanti)
  alfa <- rep(NA, quanti)
  for (i in 1:quanti) {
    minimo <- suppressWarnings(optim(c(0.01, -0.4), sumquad.tau3tau4,
      t3.t4 = c(tau3[i], tau4[i])
    ))
    if (minimo$value != -1) {
      k[i] <- minimo$par[1]
      h[i] <- minimo$par[2]
      pp <- suppressWarnings(xi.alfa(
        lambda1[i], lambda2[i],
        k[i], h[i]
      ))
      xi[i] <- pp$xi
      alfa[i] <- pp$alfa
    }
  }
  output <- list(xi = xi, alfa = alfa, k = k, h = h)
  return(output)
}

rand.kappa_correct <- function(numerosita, xi, alfa, k, h) {
  F <- runif(numerosita, min = 1e-10, max = 0.9999999999)
  x <- invF.kappa((1 - F), xi, alfa, k, h)
  return(x)
}

testthat::test_that("Check that the H1 and H2 statistics calculated for gauge 203018 are the same as that from the tweaked nsRFA package function", {
  library(UKFE)

  CDs.203018 <- GetCDs(203018)
  Pool.203018 <- Pool(CDs.203018)

  flow_data <- data.frame()

  for (i in 1:length(rownames(Pool.203018))) {
    data <- GetAM(rownames(Pool.203018)[i])

    flow_data <- rbind(flow_data, data)
  }

  library(dplyr)

  pooled_df <- data.frame(x = flow_data$Flow, id = flow_data$id) %>%
    group_by(id) %>%
    summarise(
      Lcv = Lmoments(x)[3],
      LSkew = Lmoments(x)[4],
      LKurt = Lmoments(x)[5],
      N = n()
    ) %>%
    ungroup()

  set.seed(7)
  expected_output <- HW.tests_correct(flow_data$Flow, flow_data$id, Nsim = 500)

  set.seed(7)
  actual_output_h1 <- H2(pooled_df, H1 = TRUE)

  set.seed(7)
  actual_output_h2 <- H2(pooled_df)

  testthat::expect_equal(signif(expected_output[[1]], 3), as.numeric(actual_output_h1[1]))
  testthat::expect_equal(signif(expected_output[[2]], 3), as.numeric(actual_output_h2[1]))
})

###### Uncertainty ######


test_that("Results are consistent with equation 6 from Sampling uncertainty of UK design flood estimation", {
  df <- Pool(GetCDs(58006), exclude = 58006)

  set.seed(123) # Set seed for reproducibility
  result <- Uncertainty(df, qmed = QMED(GetCDs(58006)), fse = 1.46, Conf = 0.68, Plot = FALSE)

  rp2 <- result[result$RP == 2, ]

  expect_equal(rp2$Lower, 51.58, tolerance = 5)
  expect_equal(rp2$Central, 75.3, tolerance = 5)
  expect_equal(rp2$Upper, 110, tolerance = 5)
})


test_that("Uncertainty errors for invalid confidence level", {
  df <- Pool(GetCDs(203018), exclude = 203018)
  expect_error(Uncertainty(df, qmed = QMED(GetCDs(203018)), Conf = 1.5, Plot = FALSE), "Conf must be between 0 and 1")
})

test_that("Uncertainty errors if qmed missing for ungauged case", {
  df <- Pool(GetCDs(203018), exclude = 203018)
  expect_error(Uncertainty(df, Plot = FALSE), "a qmed value needs to be specified")
})

test_that("Uncertainty works for ungauged case with default arguments", {
  df <- Pool(GetCDs(203018), exclude = 203018)
  result <- Uncertainty(df, qmed = QMED(GetCDs(203018)), Plot = FALSE)
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), 10)
})

test_that("Uncertainty works for gauged case", {
  df <- Pool(GetCDs(203018)) # Include the target site for gauged case
  result <- Uncertainty(df, Gauged = TRUE, Plot = FALSE)
  expect_true(is.data.frame(result))
  expect_equal(ncol(result), 4)
  expect_equal(nrow(result), 10)
})

###### QuickResults ######

testthat::test_that("Check that QuickResults returns the same results as PoolEst, for gauged and ungauged, when the same workflow is required", {
  library(UKFE)

  CDs.73005 <- GetCDs(73005)

  actual_output_ungauged <- QuickResults(CDs.73005)
  actual_output_gauged <- QuickResults(CDs.73005, gauged = TRUE)

  expected_output_ungauged <- PoolEst(Pool(CDs.73005), QMED = median(GetAM(73005)$Flow), fseQMED = 1)
  expected_output_gauged <- PoolEst(Pool(CDs.73005), gauged = TRUE, QMED = median(GetAM(73005)$Flow), fseQMED = 1)

  testthat::expect_equal(actual_output_ungauged, expected_output_ungauged)
  testthat::expect_equal(actual_output_gauged, expected_output_gauged)
})

testthat::test_that("QuickResults throws error on invalid CDs format", {
  testthat::expect_error(QuickResults(data.frame(x = 1:10)), "CDs doesn't appear to be a CDs object")
})

testthat::test_that("Check that QuickResults produces the same output when QMED is specified and URBEXT2000 > 0.03", {
  library(UKFE)

  CDs.73005 <- GetCDs(73005)
  urbext <- 0.5
  CDs.73005[18, 2] <- urbext # Artificially inflate URBEXT2000 to go down the UrbAdj route

  actual_output <- QuickResults(CDs.73005, Qmed = median(GetAM(73005)$Flow))

  expected_output <- PoolEst(Pool(CDs.73005), QMED = median(GetAM(73005)$Flow), UrbAdj = TRUE, URBEXT = urbext, fseQMED = 1)

  testthat::expect_equal(actual_output, expected_output)
})

testthat::test_that("Check that QuickResults excludes the first gauge as expected", {
  library(UKFE)

  CDs.73005 <- GetCDs(73005)

  actual_output <- QuickResults(CDs.73005, Qmed = median(GetAM(73005)$Flow), FUngauged = TRUE)

  expected_output <- PoolEst(Pool(CDs.73005, exclude = 73005), QMED = median(GetAM(73005)$Flow), fseQMED = 1)

  testthat::expect_equal(actual_output, expected_output)
})

##### Weight Functions #####

###### WeightsGLcv ######

# Helper function to create test data
generate_WeightsG_test_df <- function(n_values) {
  sites <- c(37017, 37020, 36005, 33051, 38004, 33018, 35008, 34003, 30005, 33055, 20003, 39037, 21027, 54106, 33012)

  SDM <- c(0, 0.1159, 0.1690, 0.2010, 0.2277, 0.2785, 0.3638, 0.4044, 0.4046, 0.4577, 0.5447, 0.5588, 0.5731, 0.5752, 0.5812)
  lcv <- c(0.2232, 0.2062, 0.3074, 0.2403, 0.3050, 0.2633, 0.3174, 0.2953, 0.2881, 0.3455, 0.4042, 0.4243, 0.3271, 0.3482, 0.2799)
  lskew <- c(-0.0910, -0.2121, 0.1389, -0.1358, 0.1621, 0.2481, 0.0979, 0.2420, 0.0937, 0.3105, 0.2200, 0.3945, 0.2429, 0.3741, 0.0729)

  df <- data.frame(N = n_values, SDM = SDM, Lcv = lcv, LSkew = lskew)
  rownames(df) <- sites

  # Add missing columns to make it 24 columns
  num_new_cols <- 24 - ncol(df)
  new_col_names <- paste0("Col", seq_len(num_new_cols))
  new_cols_df <- as.data.frame(matrix(0, nrow = nrow(df), ncol = num_new_cols))
  colnames(new_cols_df) <- new_col_names

  df <- cbind(df, new_cols_df)
  return(df)
}

testthat::test_that("Weights of Lcv should match the calculated weights in table 6.6 of FEH2008", {
  df <- generate_WeightsG_test_df(c(34, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- c(0.6526, 0.0327, 0.0309, 0.0288, 0.0291, 0.0270, 0.0247, 0.0246, 0.0237, 0.0218, 0.0221, 0.0211, 0.0207, 0.0184, 0.0218)

  actual_output <- round(WeightsGLcv(df)$Weight, 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Weight of Lcv for site 37017 should match the calculated weight in table 6.8 of FEH2008 for n=4", {
  df <- generate_WeightsG_test_df(c(4, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- 0.1459

  actual_output <- round(WeightsGLcv(df)$Weight[1], 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Weight of Lcv for site 37017 should match the calculated weight in table 6.8 of FEH2008 for n=72", {
  df <- generate_WeightsG_test_df(c(72, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- 0.8017

  actual_output <- round(WeightsGLcv(df)$Weight[1], 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Weight of Lcv for site 37017 should match the calculated weight in table 6.8 of FEH2008 for n=120", {
  df <- generate_WeightsG_test_df(c(120, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- 0.8714

  actual_output <- round(WeightsGLcv(df)$Weight[1], 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("WeightsGLcv raises an error for invalid input types", {
  testthat::expect_error(WeightsGLcv(matrix(1:10, ncol = 2)), "x must be a pooled group")
  testthat::expect_error(WeightsGLcv(list(a = 1, b = 2)), "x must be a pooled group")
})

testthat::test_that("WeightsGLcv raises an error when x does not have 24 columns", {
  df_invalid <- data.frame(N = c(10, 20), SDM = c(0.1, 0.2), Lcv = c(0.3, 0.4)) # Only 3 columns
  testthat::expect_error(WeightsGLcv(df_invalid), "x must be a pooled group")
})

###### WeightsGLSkew ######

testthat::test_that("Weights of LSkew should match the calculated weights in table 6.6 of FEH2008", {
  df <- generate_WeightsG_test_df(c(34, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- c(0.1690, 0.0835, 0.0779, 0.0686, 0.0721, 0.0642, 0.0578, 0.0592, 0.0552, 0.0487, 0.0542, 0.0494, 0.0481, 0.0379, 0.0542)

  actual_output <- round(WeightsGLSkew(df)$Weight, 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Weight of LSkew for site 37017 should match the calculated weight in table 6.8 of FEH2008 for n=4", {
  df <- generate_WeightsG_test_df(c(4, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- 0.0126

  actual_output <- round(WeightsGLSkew(df)$Weight[1], 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Weight of LSkew for site 37017 should match the calculated weight in table 6.8 of FEH2008 for n=72", {
  df <- generate_WeightsG_test_df(c(72, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- 0.3080

  actual_output <- round(WeightsGLSkew(df)$Weight[1], 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("Weight of LSkew for site 37017 should match the calculated weight in table 6.8 of FEH2008 for n=120", {
  df <- generate_WeightsG_test_df(c(120, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- 0.4286

  actual_output <- round(WeightsGLSkew(df)$Weight[1], 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("WeightsGLSkew raises an error when x does not have 24 columns", {
  df_invalid <- data.frame(N = c(10, 20), SDM = c(0.1, 0.2), Lcv = c(0.3, 0.4)) # Only 3 columns
  testthat::expect_error(WeightsGLSkew(df_invalid), "x must be a pooled group")
})

testthat::test_that("WeightsGLSkew should stop if input is not a data frame", {
  testthat::expect_error(WeightsGLSkew(matrix(1:10, nrow = 5)),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

###### WeightsUnLcv ######

generate_WeightsUn_test_df <- function(n_values) {
  sites <- c(37020, 36005, 33051, 38004, 33018, 35008, 34003, 30005, 33055, 20003, 39037, 21027, 54106, 33012, 54018)

  SDM <- c(0.1159, 0.1690, 0.2010, 0.2277, 0.2785, 0.3638, 0.4044, 0.4046, 0.4577, 0.5447, 0.5588, 0.5731, 0.5752, 0.5812, 0.5952)
  lcv <- c(0.2062, 0.3074, 0.2403, 0.3050, 0.2633, 0.3174, 0.2953, 0.2881, 0.3455, 0.4042, 0.4243, 0.3271, 0.3482, 0.2799, 0.1546)
  lskew <- c(-0.2121, 0.1389, -0.1358, 0.1621, 0.2481, 0.0979, 0.2420, 0.0937, 0.3105, 0.2200, 0.3945, 0.2429, 0.3741, 0.0729, 0.1323)

  df <- data.frame(N = n_values, SDM = SDM, Lcv = lcv, LSkew = lskew)
  rownames(df) <- sites

  # Add missing columns to make it 24 columns
  num_new_cols <- 24 - ncol(df)
  new_col_names <- paste0("Col", seq_len(num_new_cols))
  new_cols_df <- as.data.frame(matrix(0, nrow = nrow(df), ncol = num_new_cols))
  colnames(new_cols_df) <- new_col_names

  df <- cbind(df, new_cols_df)
  return(df)
}

testthat::test_that("Weights should match the calculated weights for Lcv in table 6.7 of FEH2008", {
  df <- generate_WeightsUn_test_df(c(33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43, 41))

  expected_output <- c(0.0886, 0.0838, 0.0781, 0.0790, 0.0732, 0.0671, 0.0666, 0.0644, 0.0592, 0.0599, 0.0571, 0.0560, 0.0498, 0.0590, 0.0582)

  actual_output <- round(WeightsUnLcv(df)$Weight, 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("WeightsUnLcv throws an error when input is not a data frame", {
  testthat::expect_error(WeightsUnLcv(list(1, 2, 3)),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

testthat::test_that("WeightsUnLcv throws an error when input does not have 24 columns", {
  testthat::expect_error(WeightsUnLcv(data.frame(matrix(runif(23 * 5), nrow = 5, ncol = 23))),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

###### WeightsUnLSkew ######

testthat::test_that("Weights should match the calculated weights for LSkew in table 6.7 of FEH2008", {
  df <- generate_WeightsUn_test_df(c(33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43, 41))

  expected_output <- c(0.0945, 0.0880, 0.0776, 0.0816, 0.0726, 0.0654, 0.0669, 0.0624, 0.0551, 0.0612, 0.0559, 0.0544, 0.0428, 0.0613, 0.0603)

  actual_output <- round(WeightsUnLSkew(df)$Weight, 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("WeightsUnLSkew throws an error when input is not a data frame", {
  testthat::expect_error(WeightsUnLSkew(list(1, 2, 3)),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

testthat::test_that("WeightsUnLSkew throws an error when input does not have 24 columns", {
  testthat::expect_error(WeightsUnLSkew(data.frame(matrix(runif(23 * 5), nrow = 5, ncol = 23))),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

###### WGaugLcv ######

testthat::test_that("The calculated weighted Lcv from the FEH2008 table 6.6 example should match the weighted lcv from WGaugLcv for n=34", {
  df <- generate_WeightsG_test_df(c(34, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- 0.2514

  actual_output <- round(WGaugLcv(df), 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("WGaugLcv raises an error when x does not have 24 columns", {
  df_invalid <- data.frame(N = c(10, 20), SDM = c(0.1, 0.2), Lcv = c(0.3, 0.4)) # Only 3 columns
  testthat::expect_error(WGaugLcv(df_invalid), "x must be a pooled group")
})

testthat::test_that("WGaugLcv should stop if input is not a data frame", {
  testthat::expect_error(WGaugLcv(matrix(1:10, nrow = 5)),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

###### WGaugLSkew ######

testthat::test_that("The calculated weighted LSkew from the FEH2008 table 6.6 example should match the weighted LSkew from WGaugLSkew for n=34", {
  df <- generate_WeightsG_test_df(c(34, 33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43))

  expected_output <- 0.0975

  actual_output <- round(WGaugLSkew(df), 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("WGaugLSkew raises an error when x does not have 24 columns", {
  df_invalid <- data.frame(N = c(10, 20), SDM = c(0.1, 0.2), LSkew = c(0.3, 0.4)) # Only 3 columns
  testthat::expect_error(WGaugLSkew(df_invalid), "x must be a pooled group")
})

testthat::test_that("WGaugLSkew should stop if input is not a data frame", {
  testthat::expect_error(WGaugLSkew(matrix(1:10, nrow = 5)),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

###### WungLcv ######

testthat::test_that("The calculated weighted Lcv from the FEH2008 table 6.7 example should match the weighted lcv from WungLcv for n=34", {
  df <- generate_WeightsUn_test_df(c(33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43, 41))

  expected_output <- 0.2958

  actual_output <- round(WungLcv(df), 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("WungLcv raises an error when x does not have 24 columns", {
  df_invalid <- data.frame(N = c(10, 20), SDM = c(0.1, 0.2), Lcv = c(0.3, 0.4)) # Only 3 columns
  testthat::expect_error(WungLcv(df_invalid), "x must be a pooled group")
})

testthat::test_that("WungLcv should stop if input is not a data frame", {
  testthat::expect_error(WungLcv(matrix(1:10, nrow = 5)),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

###### WungLSkew ######

testthat::test_that("The calculated weighted LSkew from the FEH2008 table 6.7 example should match the weighted lskew from WungLSkew for n=34", {
  df <- generate_WeightsUn_test_df(c(33, 39, 34, 44, 39, 37, 44, 35, 27, 41, 31, 29, 17, 43, 41))

  expected_output <- 0.1357

  actual_output <- round(WungLSkew(df), 4)

  testthat::expect_equal(expected_output, actual_output)
})

testthat::test_that("WungLSkew raises an error when x does not have 24 columns", {
  df_invalid <- data.frame(N = c(10, 20), SDM = c(0.1, 0.2), Lcv = c(0.3, 0.4)) # Only 3 columns
  testthat::expect_error(WungLSkew(df_invalid), "x must be a pooled group")
})

testthat::test_that("WungLSkew should stop if input is not a data frame", {
  testthat::expect_error(WungLSkew(matrix(1:10, nrow = 5)),
    "x must be a pooled group. Pooled groups can be created with the Pool() function",
    fixed = TRUE
  )
})

##### Pooling Functions #####

###### Pool ######

testthat::test_that("Check that the correct stations are included in the pooling group for gauge 27081 when compared to WINFAP", {
  library(UKFE)
  CDs_site <- GetCDs(27081)

  Pool <- Pool(CDs_site, iug = TRUE, DeUrb = FALSE)

  Pool <- tibble::rownames_to_column(Pool, "Station")

  actual_result <- Pool[, c("Station")]

  expected_result <- c(
    "27081", "36010", "7009", "26014", "7011", "26016", "7012",
    "41020", "28058", "39033", "25019", "44008", "27010",
    "44013", "24007", "53017"
  )

  testthat::expect_equal(actual_result, expected_result)
})

testthat::test_that("Exclude removes specified stations from pooling group", {
  library(UKFE)
  CDs_site <- GetCDs(27081)
  Pool_all <- Pool(CDs_site)
  Pool_excluded <- Pool(CDs_site, exclude = c("7009", "7011"))

  testthat::expect_false("7009" %in% rownames(Pool_excluded))
  testthat::expect_false("7011" %in% rownames(Pool_excluded))
  testthat::expect_true("7009" %in% rownames(Pool_all)) # Confirm it was originally present
})

testthat::test_that("iug = TRUE does not duplicate site if URBEXT2000 is under UrbMax", {
  library(UKFE)
  CDs_site <- GetCDs(27081)
  testthat::expect_output(
    {
      Pool <- Pool(CDs_site, iug = TRUE, UrbMax = 1) # Force it to pass Urb check
    },
    regexp = "iug = TRUE and the closest site in catchment descriptor space has URBEXT2000"
  )

  testthat::expect_equal(nrow(Pool), length(unique(rownames(Pool)))) # No duplicate sites
})

testthat::test_that("DeUrb = TRUE adjusts Lcv and LSkew values", {
  library(UKFE)

  CDs_site <- GetCDs(27081)
  Pool_false <- Pool(CDs_site, iug = TRUE, DeUrb = FALSE)
  Pool_true <- Pool(CDs_site, iug = TRUE, DeUrb = TRUE)

  urb_sites <- which(Pool_true$URBEXT2000 > 0.03)
  testthat::expect_true(any(Pool_true$Lcv[urb_sites] != Pool_false$Lcv[urb_sites]))
  testthat::expect_true(any(Pool_true$LSkew[urb_sites] != Pool_false$LSkew[urb_sites]))
})

testthat::test_that("CDs = NULL branch functions as expected", {
  library(UKFE)

  Pool <- Pool(CDs = NULL, AREA = 500, SAAR = 1000, FARL = 0.9, FPEXT = 0.02)
  testthat::expect_true("SDM" %in% colnames(Pool))
  testthat::expect_gt(nrow(Pool), 0)
})

###### PoolEst ######

testthat::test_that("Check that various outputs from PoolEst align with WINFAP for gauge 27081", {
  library(UKFE)
  # Get CDs for the gauged site:
  CDs_site <- GetCDs(27081)

  # QMED for the gauged catchment:
  QMED <- median(GetAM(27081)[, 2])

  # Estimate growth curve
  Pool <- Pool(CDs_site, iug = TRUE, DeUrb = TRUE)
  Results <- PoolEst(Pool, gauged = TRUE, QMED = QMED, dist = "GenLog", UrbAdj = TRUE, CDs = CDs_site)

  GC_winfap <- data.frame(PooledLcv = 0.264, PooledLSkew = 0.285)
  GF_winfap <- data.frame(
    RP = c(2, 5, 10, 20, 50, 75, 100),
    Q = c(1, 1.444, 1.798, 2.205, 2.862, 3.209, 3.479)
  )
  fitted_param_winfap <- data.frame(loc = 1, scale = 0.261, shape = -0.285)
  freq_param <- fitted_param_winfap * c(loc = QMED, scale = QMED, shape = 1)

  winfap_var <- ((Pool$QMED[1] * Pool$Lcv[1])^2 / Pool$N[1]) * exp(1.3125 + 0.599 * log(GF_winfap$RP - 1) + 0.00399 * (log(GF_winfap$RP - 1))^2) # Eq 12 Sampling uncertainty of UK design flood estimation

  winfap_flow <- GF_winfap$Q * QMED
  winfap_low <- winfap_flow - 1.96 * sqrt(winfap_var)
  winfap_high <- winfap_flow + 1.96 * sqrt(winfap_var)

  RP_winfap <- round(data.frame(RP = GF_winfap$RP, Q = winfap_flow, GF = GF_winfap$Q, lower95 = winfap_low, upper95 = winfap_high), 3)

  testthat::expect_equal(as.data.frame(round(Results[[2]], 3)), GC_winfap, tolerance = 0.01) # Potential difference in rounding?
  testthat::expect_equal(RP_winfap, round(Results[[1]][1:7, ], 3), tolerance = 0.01) # Difference in rounding as WINFAP GF are rounded to 3dp
  testthat::expect_equal(Results[[3]], fitted_param_winfap, tolerance = 0.01) # Same difference in rounding as the first comparison
  testthat::expect_equal(Results[[4]], freq_param, tolerance = 0.01) # Same difference in rounding as above
})

testthat::test_that("PoolEst returns expected structure for GEV distribution", {
  CDs_site <- GetCDs(27081)
  QMED <- median(GetAM(27081)[, 2])
  Pool <- Pool(CDs_site, iug = TRUE, DeUrb = TRUE)

  result <- PoolEst(Pool, gauged = TRUE, QMED = QMED, dist = "GEV", UrbAdj = TRUE, CDs = CDs_site)

  testthat::expect_length(result, 4)
  testthat::expect_named(result[[1]], c("RP", "Q", "GF", "lower95", "upper95"))
})

testthat::test_that("PoolEst works when gauged = FALSE and returns 68% intervals", {
  CDs_site <- GetCDs(27081)
  QMED <- median(GetAM(27081)[, 2])
  Pool <- Pool(CDs_site, iug = TRUE, DeUrb = TRUE)

  result <- PoolEst(Pool, gauged = FALSE, QMED = QMED, dist = "GenLog", UrbAdj = FALSE)

  testthat::expect_named(result[[1]], c("RP", "Q", "GF", "lower68", "upper68"))
})

###### PoolSmall ######

testthat::test_that("Check that the small pooling group is the same as in WINFAP", {
  library(UKFE)

  cds_path <- testthat::test_path("testdata", "FEH_Catchment_Descriptors_Scenario4.xml")

  CDs <- CDsXML(cds_path)

  Pool_site <- PoolSmall(CDs)

  expected_output <- data.frame(
    Station = c(
      "74001", "86002", "90003", "4006", "73002",
      "95001", "4005", "58012", "76015", "58006",
      "75004", "75009"
    ),
    SDM = c(
      0.303, 0.394, 0.445, 0.464, 0.469, 0.543, 0.550,
      0.613, 0.640, 0.720, 0.740, 0.763
    ),
    N = c(56, 19, 41, 33, 52, 46, 38, 45, 47, 52, 56, 45),
    QMED = c(120.914, 80.978, 121.753, 84.153, 21.194, 38.449, 101.504, 95.060, 59.677, 88.144, 52.738, 110.942),
    Lcv = c(0.154, 0.092, 0.115, 0.127, 0.174, 0.162, 0.147, 0.151, 0.232, 0.176, 0.279, 0.243),
    LSkew = c(0.246, 0.237, 0.088, 0.007, 0.194, 0.102, 0.143, 0.061, 0.351, 0.134, 0.372, 0.306)
  )

  actual_output <- Pool_site[, c("SDM", "N", "QMED", "Lcv", "LSkew")]
  actual_output <- tibble::rownames_to_column(actual_output, "Station")

  numeric_cols <- c("SDM", "N", "QMED", "Lcv", "LSkew")

  for (col in numeric_cols) {
    testthat::expect_equal(
      expected_output[[col]],
      round(actual_output[[col]], 3),
      info = paste("Mismatch in column:", col)
    )
  }

  testthat::expect_equal(expected_output$Station, actual_output$Station)
})

testthat::test_that("PoolSmall runs with AREA and SAAR directly (CDs = NULL)", {
  pool <- PoolSmall(CDs = NULL, AREA = 100, SAAR = 900)
  testthat::expect_true(is.data.frame(pool))
  testthat::expect_true(all(c("SDM", "N", "QMED", "Lcv", "LSkew") %in% colnames(pool)))
})

##### Integration testing #####

###### DonAdj ######

test_that("Check that GetCDs integrates into DonAdj correctly", {
  CDs.54022 <- GetCDs(54022)
  output <- DonAdj(CDs.54022)

  expect_s3_class(output, "data.frame")
  expect_gte(nrow(output), 1)
  expect_equal(ncol(output), 29)
  expect_named(output, c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "RMED.1H", "RMED.1D", "RMED.2D", "SAAR", "SAAR4170", "SPRHOST", "URBEXT2000",
    "QMED", "QMEDcd", "X", "Y", "QMEDfse", "N", "URBEXT1990", "BFIHOST", "Dists", "a", "QMED.adj"
  ))
  numeric_cols <- setdiff(names(output), "N")
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_false(any(is.na(output)))
  expect_true(all(output$AREA > 0))
  expect_true(all(output$FARL >= 0 & output$FARL <= 1))
  expect_true(all(output$URBEXT2000 >= 0 & output$URBEXT2000 <= 1))
  expect_true(all(output$URBEXT1990 >= 0 & output$URBEXT1990 <= 1))
  expect_true(all(output$QMED >= 0))
  expect_true(all(output$QMED.adj >= 0))
})

test_that("Check that CDsXML integrates into DonAdj correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  output <- DonAdj(CDs.72007)

  expect_s3_class(output, "data.frame")
  expect_gte(nrow(output), 1)
  expect_equal(ncol(output), 29)
  expect_named(output, c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "RMED.1H", "RMED.1D", "RMED.2D", "SAAR", "SAAR4170", "SPRHOST", "URBEXT2000",
    "QMED", "QMEDcd", "X", "Y", "QMEDfse", "N", "URBEXT1990", "BFIHOST", "Dists", "a", "QMED.adj"
  ))
  numeric_cols <- setdiff(names(output), "N")
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_false(any(is.na(output)))
  expect_true(all(output$AREA > 0))
  expect_true(all(output$FARL >= 0 & output$FARL <= 1))
  expect_true(all(output$URBEXT2000 >= 0 & output$URBEXT2000 <= 1))
  expect_true(all(output$URBEXT1990 >= 0 & output$URBEXT1990 <= 1))
  expect_true(all(output$QMED >= 0))
  expect_true(all(output$QMED.adj >= 0))
})

test_that("Check that NRFAData and QMED integrate into DonAdj correctly", {
  CDs.39001 <- NRFAData[rownames(NRFAData) == 39001, ]
  CDs.39001_gt <- GetCDs(39001)
  output <- DonAdj(x = CDs.39001$Easting, y = CDs.39001$Northing, QMEDscd = QMED(CDs.39001_gt))

  expect_s3_class(output, "data.frame")
  expect_gte(nrow(output), 1)
  expect_equal(ncol(output), 29)
  expect_named(output, c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "RMED.1H", "RMED.1D", "RMED.2D", "SAAR", "SAAR4170", "SPRHOST", "URBEXT2000",
    "QMED", "QMEDcd", "X", "Y", "QMEDfse", "N", "URBEXT1990", "BFIHOST", "Dists", "a", "QMED.adj"
  ))
  numeric_cols <- setdiff(names(output), "N")
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_false(any(is.na(output)))
  expect_true(all(output$AREA > 0))
  expect_true(all(output$FARL >= 0 & output$FARL <= 1))
  expect_true(all(output$URBEXT2000 >= 0 & output$URBEXT2000 <= 1))
  expect_true(all(output$URBEXT1990 >= 0 & output$URBEXT1990 <= 1))
  expect_true(all(output$QMED >= 0))
  expect_true(all(output$QMED.adj >= 0))
})

###### GenLogGF ######

# Takes in from Lcv and LSkew

test_that("Check that Lcv and LSkew integrate correctly into GenLogGF and returns the expected result", {
  AM.27051 <- GetAM(27051)

  Lcv.27051 <- Lcv(AM.27051$Flow)
  LSkew.27051 <- LSkew(AM.27051$Flow)

  output <- GenLogGF(Lcv.27051, LSkew.27051, RP = 100)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GenLogAM ######

test_that("Check that GetAM integrates into GenLogAM correctly", {
  AM.27090 <- GetAM(27090)
  output <- GenLogAM(AM.27090$Flow, RP = 50)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMImport integrates into GenLogAM correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- GenLogAM(AM.54906$Flow, RP = 50)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that POTextract integrates into GenLogAM correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- GenLogAM(ThamesPOT$peak)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMSP integrates into GenLogAM correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- GenLogAM(AM_39001)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GenLogPars ######

test_that("Check that GetAM integrates into GenLogPars correctly", {
  AM.27090 <- GetAM(27090)
  output <- GenLogPars(AM.27090$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that AMImport integrates into GenLogPars correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- GenLogPars(AM.54906$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that LMoments integrates into GenLogPars correctly", {
  AM.27090 <- GetAM(27090)

  LPars <- as.numeric(LMoments(AM.27090$Flow))[c(1, 5, 6)]
  output <- GenLogPars(L1 = LPars[1], LCV = LPars[2], LSKEW = LPars[3])

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that POTextract integrates into GenLogPars correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)

  output <- GenLogPars(ThamesPOT$peak)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that AMSP integrates into GenLogPars correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]

  output <- GenLogPars(AM_39001)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

###### GenLogEst ######

test_that("Check that GenLogPars integrates into GenLogEst correctly", {
  AM.27090 <- GetAM(27090)
  Pars <- as.numeric(GenLogPars(AM.27090$Flow))
  output <- GenLogEst(Pars[1], Pars[2], Pars[3], RP = 50)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GenParetoGF ######

# Takes in from Lcv and LSkew

test_that("Check that Lcv and LSkew integrate correctly into GenParetoGF and returns the expected result", {
  AM.27051 <- GetAM(27051)

  Lcv.27051 <- Lcv(AM.27051$Flow)
  LSkew.27051 <- LSkew(AM.27051$Flow)

  output <- GenParetoGF(Lcv.27051, LSkew.27051, RP = 100)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GenParetoPOT ######

test_that("Check that GetAM integrates into GenParetoPOT correctly", {
  AM.27090 <- GetAM(27090)
  output <- GenParetoPOT(AM.27090$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMImport integrates into GenParetoPOT correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- GenParetoPOT(AM.54906$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that POTextract integrates into GenParetoPOT correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- GenParetoPOT(ThamesPOT$peak)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMSP integrates into GenParetoPOT correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- GenParetoPOT(AM_39001)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GenParetoPars ######

test_that("Check that GetAM integrates into GenParetoPars correctly", {
  AM.27090 <- GetAM(27090)
  output <- GenParetoPars(AM.27090$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that AMImport integrates into GenParetoPars correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- GenParetoPars(AM.54906$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that LMoments integrates into GenParetoPars correctly", {
  AM.27090 <- GetAM(27090)

  LPars <- as.numeric(LMoments(AM.27090$Flow))[c(1, 5, 6)]
  output <- GenParetoPars(L1 = LPars[1], LCV = LPars[2], LSKEW = LPars[3])

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that POTextract integrates into GenParetoPars correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)

  output <- GenParetoPars(ThamesPOT$peak)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that AMSP integrates into GenParetoPars correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]

  output <- GenParetoPars(AM_39001)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

###### GenParetoEst ######

test_that("Check that GenParetoPars integrates into GenParetoEst correctly", {
  AM.27090 <- GetAM(27090)
  Pars <- as.numeric(GenParetoPars(AM.27090$Flow))
  output <- GenParetoEst(Pars[1], Pars[2], Pars[3], RP = 50)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GEVGF ######

test_that("Check that Lcv and LSkew integrate correctly into GEVGF and returns the expected result", {
  AM.27051 <- GetAM(27051)

  Lcv.27051 <- Lcv(AM.27051$Flow)
  LSkew.27051 <- LSkew(AM.27051$Flow)

  output <- GEVGF(Lcv.27051, LSkew.27051, RP = 100)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GEVAM ######

test_that("Check that GetAM integrates into GEVAM correctly", {
  AM.27090 <- GetAM(27090)
  output <- GEVAM(AM.27090$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMImport integrates into GEVAM correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- GEVAM(AM.54906$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that POTextract integrates into GEVAM correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- GEVAM(ThamesPOT$peak)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMSP integrates into GEVAM correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- GEVAM(AM_39001)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GEVPars ######

test_that("Check that GetAM integrates into GEVPars correctly", {
  AM.27090 <- GetAM(27090)
  output <- GEVPars(AM.27090$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that AMImport integrates into GEVPars correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- GEVPars(AM.54906$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that LMoments integrates into GEVPars correctly", {
  AM.27090 <- GetAM(27090)

  LPars <- as.numeric(LMoments(AM.27090$Flow))[c(1, 5, 6)]
  output <- GEVPars(L1 = LPars[1], LCV = LPars[2], LSKEW = LPars[3])

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that POTextract integrates into GEVPars correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)

  output <- GEVPars(ThamesPOT$peak)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that AMSP integrates into GEVPars correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]

  output <- GEVPars(AM_39001)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

###### GEVEst ######

test_that("Check that GEVPars integrates into GEVEst correctly", {
  AM.27090 <- GetAM(27090)
  Pars <- as.numeric(GEVPars(AM.27090$Flow))
  output <- GEVEst(Pars[1], Pars[2], Pars[3], RP = 50)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GumbelGF ######

test_that("Check that Lcv integrates correctly into GumbelGF and returns the expected result", {
  AM.27051 <- GetAM(27051)

  Lcv.27051 <- Lcv(AM.27051$Flow)

  output <- GumbelGF(Lcv.27051, RP = 100)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GumbelAM ######

test_that("Check that GetAM integrates into GumbelAM correctly", {
  AM.27090 <- GetAM(27090)
  output <- GumbelAM(AM.27090$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMImport integrates into GumbelAM correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- GumbelAM(AM.54906$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that POTextract integrates into GumbelAM correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- GumbelAM(ThamesPOT$peak)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMSP integrates into GumbelAM correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- GumbelAM(AM_39001)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### GumbelPars ######

test_that("Check that GetAM integrates into GumbelPars correctly", {
  AM.27090 <- GetAM(27090)
  output <- GumbelPars(AM.27090$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
})

test_that("Check that AMImport integrates into GumbelPars correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- GumbelPars(AM.54906$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
})

test_that("Check that LMoments integrates into GumbelPars correctly", {
  AM.27090 <- GetAM(27090)

  LPars <- as.numeric(LMoments(AM.27090$Flow))[c(1, 5, 6)]
  output <- GumbelPars(L1 = LPars[1], LCV = LPars[2])

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
})

test_that("Check that POTextract integrates into GumbelPars correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)

  output <- GumbelPars(ThamesPOT$peak)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
})

test_that("Check that AMSP integrates into GumbelPars correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]

  output <- GumbelPars(AM_39001)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
})

###### GumbelEst ######

test_that("Check that GumbelPars integrates into GumbelEst correctly", {
  AM.27090 <- GetAM(27090)
  Pars <- as.numeric(GumbelPars(AM.27090$Flow))
  output <- GumbelEst(Pars[1], Pars[2], RP = 50)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### H2 ######

test_that("Check that Pool integrates into H2 correctly", {
  CDs.203018 <- GetCDs(203018)
  Pool.203018 <- Pool(CDs.203018)
  output <- H2(Pool.203018)

  expect_type(output, "character")
  expect_length(output, 2)
  expect_true(!is.na(as.numeric(output[1])))
})

test_that("Check that Pool integrates into H2 correctly", {
  CDs.203018 <- GetCDs(203018)
  Pool.203018 <- PoolSmall(CDs.203018)
  output <- H2(Pool.203018)

  expect_type(output, "character")
  expect_length(output, 2)
  expect_true(!is.na(as.numeric(output[1])))
})

###### Kappa3GF ######

test_that("Check that Lcv and LSkew integrate correctly into Kappa3GF and returns the expected result", {
  AM.27051 <- GetAM(27051)

  Lcv.27051 <- Lcv(AM.27051$Flow)
  LSkew.27051 <- LSkew(AM.27051$Flow)

  output <- Kappa3GF(Lcv.27051, LSkew.27051, RP = 100)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### Kappa3AM ######

test_that("Check that GetAM integrates into Kappa3AM correctly", {
  AM.27090 <- GetAM(27090)
  output <- Kappa3AM(AM.27090$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMImport integrates into Kappa3AM correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- Kappa3AM(AM.54906$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that POTextract integrates into Kappa3AM correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- Kappa3AM(ThamesPOT$peak)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMSP integrates into Kappa3AM correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- Kappa3AM(AM_39001)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### Kappa3Pars ######

test_that("Check that GetAM integrates into Kappa3Pars correctly", {
  AM.27090 <- GetAM(27090)
  output <- Kappa3Pars(AM.27090$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that AMImport integrates into Kappa3Pars correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- Kappa3Pars(AM.54906$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that LMoments integrates into Kappa3Pars correctly", {
  AM.27090 <- GetAM(27090)

  LPars <- as.numeric(LMoments(AM.27090$Flow))[c(1, 5, 6)]
  output <- Kappa3Pars(L1 = LPars[1], LCV = LPars[2], LSKEW = LPars[3])

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that POTextract integrates into Kappa3Pars correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)

  output <- Kappa3Pars(ThamesPOT$peak)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

test_that("Check that AMSP integrates into Kappa3Pars correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]

  output <- Kappa3Pars(AM_39001)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_named(output, c("Loc", "Scale", "Shape"))
  expect_type(output$Loc, "double")
  expect_type(output$Scale, "double")
  expect_type(output$Shape, "double")
})

###### Kappa3Est ######

test_that("Check that Kappa3Pars integrates into Kappa3Est correctly", {
  AM.27090 <- GetAM(27090)
  Pars <- as.numeric(Kappa3Pars(AM.27090$Flow))
  output <- Kappa3Est(Pars[1], Pars[2], Pars[3], RP = 50)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### Lcv ######

test_that("Check that GetAM integrates into Lcv correctly", {
  AM.27090 <- GetAM(27090)
  output <- Lcv(AM.27090$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMImport integrates into Lcv correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- Lcv(AM.54906$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that POTextract integrates into Lcv correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- Lcv(ThamesPOT$peak)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMSP integrates into Lcv correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- Lcv(AM_39001)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### LcvUrb ######

test_that("Check that QuickResults and GetCDs integrates into LcvUrb correctly", {
  CDs.53006 <- GetCDs(53006)
  qr_lcv <- QuickResults(CDs.53006)[[2]][1]
  output <- LcvUrb(qr_lcv, CDs.53006$Value[CDs.53006$Descriptor == "URBEXT2000"])

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NRFAData integrates into LcvUrb correctly", {
  nrfa <- NRFAData[which(rownames(NRFAData) == 53006), ]
  output <- LcvUrb(nrfa$Lcv, nrfa$URBEXT2000, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that Lcv and CDsXML integrates into LcvUrb correctly", {
  AM.72007 <- GetAM(72007)
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  org_lcv <- Lcv(AM.72007$Flow)
  output <- LcvUrb(org_lcv, CDs.72007$Value[CDs.72007$Descriptor == "URBEXT2000"], DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that Pool integrates into LcvUrb correctly", {
  CDs.39001 <- GetCDs(39001)
  pool_lcv <- Pool(CDs.39001, iug = TRUE, DeUrb = TRUE)$Lcv[1]
  pool_urb <- Pool(CDs.39001, iug = TRUE, DeUrb = TRUE)$URBEXT2000[1]
  output <- LcvUrb(pool_lcv, pool_urb, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that PoolSmall integrates into LcvUrb correctly", {
  CDs.39001 <- GetCDs(39001)
  pool_lcv <- PoolSmall(CDs.39001, iug = TRUE, DeUrb = TRUE)$Lcv[1]
  pool_urb <- PoolSmall(CDs.39001, iug = TRUE, DeUrb = TRUE)$URBEXT2000[1]
  output <- LcvUrb(pool_lcv, pool_urb, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that PoolEst and DonAdj integrates into LcvUrb correctly", {
  CDs.27083 <- GetCDs(27083)
  Pool.27083 <- Pool(CDs.27083)
  pool_est_lcv <- PoolEst(Pool.27083, QMED = 12, UrbAdj = TRUE, CDs = CDs.27083)[[2]][1]
  donadj_urbext <- DonAdj(CDs.27083)$URBEXT2000[1]
  output <- LcvUrb(pool_est_lcv, donadj_urbext, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that LMoments integrates into LcvUrb correctly", {
  AM.27051 <- GetAM(27051)
  lmoments_lcv <- LMoments(AM.27051$Flow)$Lcv
  output <- LcvUrb(lmoments_lcv, 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NonFloodAdj integrates into LcvUrb correctly", {
  AM.27051 <- GetAM(27051)[, 2]
  nonflood_lcv <- NonFloodAdj(AM.27051)[[1]]$Lcv
  output <- LcvUrb(nonflood_lcv, 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NonFloodAdjPool integrates into LcvUrb correctly", {
  Pool44013 <- Pool(GetCDs(44013))
  PoolNF_lcv <- NonFloodAdjPool(Pool44013)$Lcv[1]
  output <- LcvUrb(PoolNF_lcv, 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that WGaugLcv integrates into LcvUrb correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- LcvUrb(WGaugLcv(Pool.27051), 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that WungLcv integrates into LcvUrb correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- LcvUrb(WungLcv(Pool.27051), 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### LKurt ######

test_that("Check that GetAM integrates into LKurt correctly", {
  AM.27090 <- GetAM(27090)
  output <- LKurt(AM.27090$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMImport integrates into LKurt correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- LKurt(AM.54906$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that POTextract integrates into LKurt correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- LKurt(ThamesPOT$peak)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMSP integrates into LKurt correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- LKurt(AM_39001)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### LMoments ######

test_that("Check that GetAM integrates into LMoments correctly", {
  AM.27090 <- GetAM(27090)
  output <- LMoments(AM.27090$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), 7)
  expect_named(output, c("L1", "L2", "L3", "L4", "Lcv", "LSkew", "LKurt"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
})

test_that("Check that AMImport integrates into LMoments correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- LMoments(AM.54906$Flow)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), 7)
  expect_named(output, c("L1", "L2", "L3", "L4", "Lcv", "LSkew", "LKurt"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
})

test_that("Check that POTextract integrates into LMoments correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- LMoments(ThamesPOT$peak)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), 7)
  expect_named(output, c("L1", "L2", "L3", "L4", "Lcv", "LSkew", "LKurt"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
})

test_that("Check that AMSP integrates into LMoments correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- LMoments(AM_39001)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), 7)
  expect_named(output, c("L1", "L2", "L3", "L4", "Lcv", "LSkew", "LKurt"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
})

###### LSkew ######

test_that("Check that GetAM integrates into LSkew correctly", {
  AM.27090 <- GetAM(27090)
  output <- LSkew(AM.27090$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMImport integrates into LSkew correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- LSkew(AM.54906$Flow)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that POTextract integrates into LSkew correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- LSkew(ThamesPOT$peak)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that AMSP integrates into LSkew correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- LSkew(AM_39001)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### LSkewUrb ######

test_that("Check that QuickResults and GetCDs integrates into LSkewUrb correctly", {
  CDs.53006 <- GetCDs(53006)
  qr_LSkew <- QuickResults(CDs.53006)[[2]][1]
  output <- LSkewUrb(qr_LSkew, CDs.53006$Value[CDs.53006$Descriptor == "URBEXT2000"])

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NRFAData integrates into LSkewUrb correctly", {
  nrfa <- NRFAData[which(rownames(NRFAData) == 53006), ]
  output <- LSkewUrb(nrfa$LSkew, nrfa$URBEXT2000, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that LSkew and CDsXML integrates into LSkewUrb correctly", {
  AM.72007 <- GetAM(72007)
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  org_LSkew <- LSkew(AM.72007$Flow)
  output <- LSkewUrb(org_LSkew, CDs.72007$Value[CDs.72007$Descriptor == "URBEXT2000"], DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that Pool integrates into LSkewUrb correctly", {
  CDs.39001 <- GetCDs(39001)
  pool_LSkew <- Pool(CDs.39001, iug = TRUE, DeUrb = TRUE)$LSkew[1]
  pool_urb <- Pool(CDs.39001, iug = TRUE, DeUrb = TRUE)$URBEXT2000[1]
  output <- LSkewUrb(pool_LSkew, pool_urb, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that PoolSmall integrates into LSkewUrb correctly", {
  CDs.39001 <- GetCDs(39001)
  pool_LSkew <- PoolSmall(CDs.39001, iug = TRUE, DeUrb = TRUE)$LSkew[1]
  pool_urb <- PoolSmall(CDs.39001, iug = TRUE, DeUrb = TRUE)$URBEXT2000[1]
  output <- LSkewUrb(pool_LSkew, pool_urb, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that PoolEst and DonAdj integrates into LSkewUrb correctly", {
  CDs.27083 <- GetCDs(27083)
  Pool.27083 <- Pool(CDs.27083)
  pool_est_LSkew <- PoolEst(Pool.27083, QMED = 12, UrbAdj = TRUE, CDs = CDs.27083)[[2]][1]
  donadj_urbext <- DonAdj(CDs.27083)$URBEXT2000[1]
  output <- LSkewUrb(pool_est_LSkew, donadj_urbext, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that LMoments integrates into LSkewUrb correctly", {
  AM.27051 <- GetAM(27051)
  lmoments_LSkew <- LMoments(AM.27051$Flow)$LSkew
  output <- LSkewUrb(lmoments_LSkew, 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NonFloodAdj integrates into LSkewUrb correctly", {
  AM.27051 <- GetAM(27051)[, 2]
  nonflood_LSkew <- NonFloodAdj(AM.27051)[[1]]$LSkew
  output <- LSkewUrb(nonflood_LSkew, 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NonFloodAdjPool integrates into LSkewUrb correctly", {
  Pool44013 <- Pool(GetCDs(44013))
  PoolNF_LSkew <- NonFloodAdjPool(Pool44013)$LSkew[1]
  output <- LSkewUrb(PoolNF_LSkew, 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that WGaugLSkew integrates into LSkewUrb correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- LSkewUrb(WGaugLSkew(Pool.27051), 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that WungLSkew integrates into LSkewUrb correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- LSkewUrb(WungLSkew(Pool.27051), 0.1138, DeUrb = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### NGRDist ######

test_that("Check that GetCDs integrates into NGRDist correctly", {
  CDs_10001 <- GetCDs(10001)
  CDs_39001 <- GetCDs(39001)

  output <- NGRDist(
    i = c(CDs_10001$Value[CDs_10001$Descriptor == "Easting"], CDs_10001$Value[CDs_10001$Descriptor == "Northing"]),
    j = c(CDs_39001$Value[CDs_39001$Descriptor == "Easting"], CDs_39001$Value[CDs_39001$Descriptor == "Northing"])
  )

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that CDsXML integrates into NGRDist correctly", {
  CDs_72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  CDs_diff <- CDsXML(testthat::test_path("testdata", "FEH_Catchment_Descriptors_Scenario4.xml"))

  output <- NGRDist(
    i = c(CDs_72007$Value[CDs_72007$Descriptor == "Easting"], CDs_72007$Value[CDs_72007$Descriptor == "Northing"]),
    j = c(CDs_diff$Value[CDs_diff$Descriptor == "Easting"], CDs_diff$Value[CDs_diff$Descriptor == "Northing"])
  )

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NRFAData integrates into NGRDist correctly", {
  CDs_10001 <- NRFAData[rownames(NRFAData) == 10001, ]
  CDs_39001 <- NRFAData[rownames(NRFAData) == 39001, ]

  output <- NGRDist(
    i = c(CDs_10001$Easting, CDs_10001$Northing),
    j = c(CDs_39001$Easting, CDs_39001$Northing)
  )

  expect_type(output, "double")
  expect_length(output, 1)
})

###### NonFloodAdj ######

test_that("Check that GetAM integrates into NonFloodAdj correctly", {
  AM.27090 <- GetAM(27090)
  output <- NonFloodAdj(AM.27090$Flow)

  expect_type(output, "list")
  expect_length(output, 2)
  expect_s3_class(output[[1]], "data.frame")
  expect_s3_class(output[[2]], "data.frame")
  expect_equal(nrow(output[[1]]), 1)
  expect_equal(ncol(output[[1]]), 2)
  expect_equal(nrow(output[[2]]), 1)
  expect_equal(ncol(output[[2]]), 3)
  expect_named(output[[1]], c("Lcv", "LSkew"))
  expect_named(output[[2]], c("No.NonFlood", "N", "PercentNonFlood"))
  expect_type(output[[1]]$Lcv, "double")
  expect_type(output[[1]]$LSkew, "double")
  expect_type(output[[2]]$No.NonFlood, "integer")
  expect_type(output[[2]]$N, "integer")
  expect_type(output[[2]]$PercentNonFlood, "double")
  expect_false(any(is.na(output[[1]])))
  expect_false(any(is.na(output[[2]])))
})

test_that("Check that AMImport integrates into NonFloodAdj correctly", {
  AM.54906 <- AMImport(testthat::test_path("testdata", "Multi_reject-054906.am"))
  output <- NonFloodAdj(AM.54906$Flow)

  expect_type(output, "list")
  expect_length(output, 2)
  expect_s3_class(output[[1]], "data.frame")
  expect_s3_class(output[[2]], "data.frame")
  expect_equal(nrow(output[[1]]), 1)
  expect_equal(ncol(output[[1]]), 2)
  expect_equal(nrow(output[[2]]), 1)
  expect_equal(ncol(output[[2]]), 3)
  expect_named(output[[1]], c("Lcv", "LSkew"))
  expect_named(output[[2]], c("No.NonFlood", "N", "PercentNonFlood"))
  expect_type(output[[1]]$Lcv, "double")
  expect_type(output[[1]]$LSkew, "double")
  expect_type(output[[2]]$No.NonFlood, "integer")
  expect_type(output[[2]]$N, "integer")
  expect_type(output[[2]]$PercentNonFlood, "double")
  expect_false(any(is.na(output[[1]])))
  expect_false(any(is.na(output[[2]])))
})

test_that("Check that POTextract integrates into NonFloodAdj correctly", {
  ThamesPOT <- POTextract(ThamesPQ[, c(1, 3)], thresh = 0.90)
  output <- NonFloodAdj(ThamesPOT$peak)

  expect_type(output, "list")
  expect_length(output, 2)
  expect_s3_class(output[[1]], "data.frame")
  expect_s3_class(output[[2]], "data.frame")
  expect_equal(nrow(output[[1]]), 1)
  expect_equal(ncol(output[[1]]), 2)
  expect_equal(nrow(output[[2]]), 1)
  expect_equal(ncol(output[[2]]), 3)
  expect_named(output[[1]], c("Lcv", "LSkew"))
  expect_named(output[[2]], c("No.NonFlood", "N", "PercentNonFlood"))
  expect_type(output[[1]]$Lcv, "double")
  expect_type(output[[1]]$LSkew, "double")
  expect_type(output[[2]]$No.NonFlood, "integer")
  expect_type(output[[2]]$N, "integer")
  expect_type(output[[2]]$PercentNonFlood, "double")
  expect_false(any(is.na(output[[1]])))
  expect_false(any(is.na(output[[2]])))
})

test_that("Check that AMSP integrates into NonFloodAdj correctly", {
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- NonFloodAdj(AM_39001)

  expect_type(output, "list")
  expect_length(output, 2)
  expect_s3_class(output[[1]], "data.frame")
  expect_s3_class(output[[2]], "data.frame")
  expect_equal(nrow(output[[1]]), 1)
  expect_equal(ncol(output[[1]]), 2)
  expect_equal(nrow(output[[2]]), 1)
  expect_equal(ncol(output[[2]]), 3)
  expect_named(output[[1]], c("Lcv", "LSkew"))
  expect_named(output[[2]], c("No.NonFlood", "N", "PercentNonFlood"))
  expect_type(output[[1]]$Lcv, "double")
  expect_type(output[[1]]$LSkew, "double")
  expect_type(output[[2]]$No.NonFlood, "integer")
  expect_type(output[[2]]$N, "integer")
  expect_type(output[[2]]$PercentNonFlood, "double")
  expect_false(any(is.na(output[[1]])))
  expect_false(any(is.na(output[[2]])))
})

###### NonFloodAdjPool ######

test_that("Check that Pool integrates into NonFloodAdjPool correctly", {
  Pool44013 <- Pool(GetCDs(44013))
  output <- NonFloodAdjPool(Pool44013)

  expect_s3_class(output, "data.frame")
  expect_setequal(names(output), c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "SAAR", "SPRHOST", "URBEXT2000", "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2",
    "N", "SDM", "Discordancy", "Discordant?"
  ))
  numeric_cols <- setdiff(names(output), c("N", "Discordant?"))
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_type(output[["Discordant?"]], "logical")
  expect_false(any(is.na(output)))
})

test_that("Check that PoolSmall integrates into NonFloodAdjPool correctly", {
  Pool44013 <- PoolSmall(GetCDs(44013))
  output <- NonFloodAdjPool(Pool44013)

  expect_s3_class(output, "data.frame")
  expect_setequal(names(output), c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "SAAR", "SPRHOST", "URBEXT2000", "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2",
    "N", "SDM", "Discordancy", "Discordant?"
  ))
  numeric_cols <- setdiff(names(output), c("N", "Discordant?"))
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_type(output[["Discordant?"]], "logical")
  expect_false(any(is.na(output)))
})

###### OptimPars ######

test_that("Check that QuickResults integrates into OptimPars correctly", {
  Results <- QuickResults(GetCDs(27051), plot = FALSE)[[1]]
  output <- OptimPars(Results[, 1:2])

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), 3)
  expect_named(output, c("loc", "scale", "shape"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
})

test_that("Check that Uncertainty integrates into OptimPars correctly", {
  Pool.203018 <- Pool(GetCDs(203018), exclude = 203018)
  Results <- Uncertainty(Pool.203018, qmed = QMED(GetCDs(203018)))
  output <- OptimPars(Results[, 1:2])

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), 3)
  expect_named(output, c("loc", "scale", "shape"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
})

test_that("Check that PoolEst integrates into OptimPars correctly", {
  CDs.27083 <- GetCDs(27083)
  Pool.27083 <- Pool(CDs.27083)
  Results <- PoolEst(Pool.27083, QMED = 12, UrbAdj = TRUE, CDs = CDs.27083)[[1]]
  output <- OptimPars(Results[, 1:2])

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), 3)
  expect_named(output, c("loc", "scale", "shape"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
})

###### Pool ######

test_that("Check that GetCDs integrates into Pool correctly", {
  CDs.73005 <- GetCDs(73005)
  output <- Pool(CDs.73005, exclude = c(79005, 71011))

  expect_s3_class(output, "data.frame")
  expect_gt(nrow(output), 1)
  expect_equal(ncol(output), 24)
  expect_setequal(names(output), c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "SAAR", "SPRHOST", "URBEXT2000", "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2",
    "N", "SDM", "Discordancy", "Discordant?"
  ))
  numeric_cols <- c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR",
    "FARL", "FPEXT", "LDP", "PROPWET", "SAAR", "SPRHOST", "URBEXT2000",
    "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2", "SDM", "Discordancy"
  )
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_type(output[["Discordant?"]], "logical")
  expect_false(any(is.na(output)))
})

test_that("Check that CDsXML integrates into Pool correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  output <- Pool(CDs.72007)

  expect_s3_class(output, "data.frame")
  expect_gt(nrow(output), 1)
  expect_equal(ncol(output), 24)
  expect_setequal(names(output), c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "SAAR", "SPRHOST", "URBEXT2000", "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2",
    "N", "SDM", "Discordancy", "Discordant?"
  ))
  numeric_cols <- c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR",
    "FARL", "FPEXT", "LDP", "PROPWET", "SAAR", "SPRHOST", "URBEXT2000",
    "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2", "SDM", "Discordancy"
  )
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_type(output[["Discordant?"]], "logical")
  expect_false(any(is.na(output)))
})

test_that("Check that NRFAData integrates into Pool correctly", {
  CDs_39001 <- NRFAData[rownames(NRFAData) == 39001, ]
  output <- Pool(AREA = CDs_39001$AREA, SAAR = CDs_39001$SAAR, FARL = CDs_39001$FARL, FPEXT = CDs_39001$FPEXT)

  expect_s3_class(output, "data.frame")
  expect_gt(nrow(output), 1)
  expect_equal(ncol(output), 24)
  expect_setequal(names(output), c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "SAAR", "SPRHOST", "URBEXT2000", "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2",
    "N", "SDM", "Discordancy", "Discordant?"
  ))
  numeric_cols <- c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR",
    "FARL", "FPEXT", "LDP", "PROPWET", "SAAR", "SPRHOST", "URBEXT2000",
    "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2", "SDM", "Discordancy"
  )
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_type(output[["Discordant?"]], "logical")
  expect_false(any(is.na(output)))
})

###### PoolEst ######

test_that("Check that Pool, QMED and GetCDs integrates into PoolEst correctly", {
  CDs.27083 <- GetCDs(27083)
  Pool.27083 <- Pool(CDs.27083)
  output <- PoolEst(Pool.27083, QMED = QMED(CDs.27083), UrbAdj = TRUE, CDs = CDs.27083)

  expect_type(output, "list")
  expect_length(output, 4)

  expect_s3_class(output[[1]], "data.frame")
  expect_equal(ncol(output[[1]]), 5)
  expect_named(output[[1]], c("RP", "Q", "GF", "lower68", "upper68"))
  expect_equal(nrow(output[[1]]), 10)
  purrr::walk(output[[1]], ~ expect_type(.x, "double"))

  expect_true(is.matrix(output[[2]]))
  expect_equal(dim(output[[2]]), c(1, 2))
  expect_identical(colnames(output[[2]]), c("PooledLcv", "PooledLSkew"))
  expect_type(output[[2]][, "PooledLcv"], "double")
  expect_type(output[[2]][, "PooledLSkew"], "double")

  expect_s3_class(output[[3]], "data.frame")
  expect_equal(nrow(output[[3]]), 1)
  expect_named(output[[3]], c("loc", "scale", "shape"))
  purrr::walk(output[[3]], ~ expect_type(.x, "double"))

  expect_s3_class(output[[4]], "data.frame")
  expect_equal(nrow(output[[4]]), 1)
  expect_named(output[[4]], c("loc", "scale", "shape"))
  purrr::walk(output[[4]], ~ expect_type(.x, "double"))

  purrr::walk(output, ~ expect_false(any(is.na(.x))))
})

test_that("Check that PoolSmall, NRFAData and CDsXML integrates into PoolEst correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  Pool.72007 <- PoolSmall(CDs.72007)
  output <- PoolEst(Pool.72007, QMED = NRFAData$QMED[rownames(NRFAData) == 72007], CDs = CDs.72007)

  expect_type(output, "list")
  expect_length(output, 4)

  expect_s3_class(output[[1]], "data.frame")
  expect_equal(ncol(output[[1]]), 5)
  expect_named(output[[1]], c("RP", "Q", "GF", "lower68", "upper68"))
  expect_equal(nrow(output[[1]]), 10)
  purrr::walk(output[[1]], ~ expect_type(.x, "double"))

  expect_true(is.matrix(output[[2]]))
  expect_equal(dim(output[[2]]), c(1, 2))
  expect_identical(colnames(output[[2]]), c("PooledLcv", "PooledLSkew"))
  expect_type(output[[2]][, "PooledLcv"], "double")
  expect_type(output[[2]][, "PooledLSkew"], "double")

  expect_s3_class(output[[3]], "data.frame")
  expect_equal(nrow(output[[3]]), 1)
  expect_named(output[[3]], c("loc", "scale", "shape"))
  purrr::walk(output[[3]], ~ expect_type(.x, "double"))

  expect_s3_class(output[[4]], "data.frame")
  expect_equal(nrow(output[[4]]), 1)
  expect_named(output[[4]], c("loc", "scale", "shape"))
  purrr::walk(output[[4]], ~ expect_type(.x, "double"))

  purrr::walk(output, ~ expect_false(any(is.na(.x))))
})

###### PoolSmall ######

test_that("Check that GetCDs integrates into PoolSmall correctly", {
  CDs.73005 <- GetCDs(73005)
  output <- PoolSmall(CDs.73005, exclude = c(79005, 71011))

  expect_s3_class(output, "data.frame")
  expect_gt(nrow(output), 1)
  expect_equal(ncol(output), 24)
  expect_setequal(names(output), c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "SAAR", "SPRHOST", "URBEXT2000", "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2",
    "N", "SDM", "Discordancy", "Discordant?"
  ))
  numeric_cols <- c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR",
    "FARL", "FPEXT", "LDP", "PROPWET", "SAAR", "SPRHOST", "URBEXT2000",
    "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2", "SDM", "Discordancy"
  )
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_type(output[["Discordant?"]], "logical")
  expect_false(any(is.na(output)))
})

test_that("Check that CDsXML integrates into PoolSmall correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  output <- PoolSmall(CDs.72007)

  expect_s3_class(output, "data.frame")
  expect_gt(nrow(output), 1)
  expect_equal(ncol(output), 24)
  expect_setequal(names(output), c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "SAAR", "SPRHOST", "URBEXT2000", "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2",
    "N", "SDM", "Discordancy", "Discordant?"
  ))
  numeric_cols <- c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR",
    "FARL", "FPEXT", "LDP", "PROPWET", "SAAR", "SPRHOST", "URBEXT2000",
    "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2", "SDM", "Discordancy"
  )
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_type(output[["Discordant?"]], "logical")
  expect_false(any(is.na(output)))
})

test_that("Check that NRFAData integrates into PoolSmall correctly", {
  CDs_39001 <- NRFAData[rownames(NRFAData) == 39001, ]
  output <- PoolSmall(AREA = CDs_39001$AREA, SAAR = CDs_39001$SAAR)

  expect_s3_class(output, "data.frame")
  expect_gt(nrow(output), 1)
  expect_equal(ncol(output), 24)
  expect_setequal(names(output), c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP",
    "PROPWET", "SAAR", "SPRHOST", "URBEXT2000", "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2",
    "N", "SDM", "Discordancy", "Discordant?"
  ))
  numeric_cols <- c(
    "AREA", "ALTBAR", "ASPBAR", "ASPVAR", "BFIHOST19", "DPLBAR", "DPSBAR",
    "FARL", "FPEXT", "LDP", "PROPWET", "SAAR", "SPRHOST", "URBEXT2000",
    "QMED", "Lcv", "LSkew", "LKurt", "L1", "L2", "SDM", "Discordancy"
  )
  purrr::walk(output[numeric_cols], ~ expect_type(.x, "double"))
  expect_type(output$N, "integer")
  expect_type(output[["Discordant?"]], "logical")
  expect_false(any(is.na(output)))
})

###### QMED ######

test_that("Check that GetCDs integrates into QMED correctly", {
  CDs.55004 <- GetCDs(55004)
  output <- QMED(CDs.55004)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that CDsXML integrates into QMED correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  output <- QMED(CDs.72007)

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NRFAData integrates into QMED correctly", {
  CDs.39001 <- NRFAData[rownames(NRFAData) == 39001, ]
  output <- QMED(CDs.39001, UrbAdj = TRUE)

  expect_type(output, "double")
  expect_length(output, 1)
})

###### QMEDDonEq ######

test_that("Check that GetCDs and GetAM integrates into QMEDDonEq correctly", {
  CDs.55004 <- GetCDs(55004)
  CDs.39001 <- GetCDs(39001)
  AM.39001 <- GetAM(39001)
  output <- QMEDDonEq(
    AREA = CDs.55004$Value[CDs.55004$Descriptor == "AREA"],
    SAAR = CDs.55004$Value[CDs.55004$Descriptor == "SAAR"],
    FARL = CDs.55004$Value[CDs.55004$Descriptor == "FARL"],
    BFIHOST = CDs.55004$Value[CDs.55004$Descriptor == "BFIHOST19"],
    QMEDgObs = median(AM.39001$Flow),
    QMEDgCds = QMED(CDs.39001, UrbAdj = TRUE),
    xSI = CDs.55004$Value[CDs.55004$Descriptor == "Easting"],
    ySI = CDs.55004$Value[CDs.55004$Descriptor == "Northing"],
    xDon = CDs.39001$Value[CDs.39001$Descriptor == "Easting"],
    yDon = CDs.39001$Value[CDs.39001$Descriptor == "Northing"]
  )

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that CDsXML and AMImport integrates into QMEDDonEq correctly", {
  CDs.test <- CDsXML(testthat::test_path("testdata", "FEH_Catchment_Descriptors_Scenario4.xml"))
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  AM.72007 <- AMImport(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.am"))
  output <- QMEDDonEq(
    AREA = CDs.test$Value[CDs.test$Descriptor == "AREA"],
    SAAR = CDs.test$Value[CDs.test$Descriptor == "SAAR"],
    FARL = CDs.test$Value[CDs.test$Descriptor == "FARL"],
    BFIHOST = CDs.test$Value[CDs.test$Descriptor == "BFIHOST19"],
    QMEDgObs = median(AM.72007$Flow),
    QMEDgCds = QMED(CDs.72007),
    xSI = CDs.test$Value[CDs.test$Descriptor == "Easting"],
    ySI = CDs.test$Value[CDs.test$Descriptor == "Northing"],
    xDon = CDs.72007$Value[CDs.72007$Descriptor == "Easting"],
    yDon = CDs.72007$Value[CDs.72007$Descriptor == "Northing"]
  )

  expect_type(output, "double")
  expect_length(output, 1)
})

test_that("Check that NRFAData and AMSP integrates into QMEDDonEq correctly", {
  CDs.55004 <- NRFAData[rownames(NRFAData) == 55004, ]
  CDs.39001 <- NRFAData[rownames(NRFAData) == 39001, ]
  AM_39001 <- AMSP$Flow[AMSP$id == 39001]
  output <- QMEDDonEq(
    AREA = CDs.55004$AREA,
    SAAR = CDs.55004$SAAR,
    FARL = CDs.55004$FARL,
    BFIHOST = CDs.55004$BFIHOST19,
    QMEDgObs = median(AM_39001),
    QMEDgCds = QMED(CDs.39001, UrbAdj = TRUE),
    xSI = CDs.55004$Easting,
    ySI = CDs.55004$Northing,
    xDon = CDs.39001$Easting,
    yDon = CDs.39001$Northing
  )

  expect_type(output, "double")
  expect_length(output, 1)
})

###### QuickResults ######

test_that("Check that GetCDs integrates into QuickResults correctly", {
  CDs.73005 <- GetCDs(73005)
  output <- QuickResults(CDs.73005)

  expect_type(output, "list")
  expect_length(output, 4)
  expect_s3_class(output[[1]], "data.frame")
  expect_equal(ncol(output[[1]]), 5)
  expect_named(output[[1]], c("RP", "Q", "GF", "lower68", "upper68"))
  expect_equal(nrow(output[[1]]), 10)
  purrr::walk(output[[1]], ~ expect_type(.x, "double"))
  expect_true(is.matrix(output[[2]]))
  expect_equal(dim(output[[2]]), c(1, 2))
  expect_identical(colnames(output[[2]]), c("PooledLcv", "PooledLSkew"))
  expect_type(output[[2]][, "PooledLcv"], "double")
  expect_type(output[[2]][, "PooledLSkew"], "double")
  expect_s3_class(output[[3]], "data.frame")
  expect_equal(nrow(output[[3]]), 1)
  expect_named(output[[3]], c("loc", "scale", "shape"))
  purrr::walk(output[[3]], ~ expect_type(.x, "double"))
  expect_s3_class(output[[4]], "data.frame")
  expect_equal(nrow(output[[4]]), 1)
  expect_named(output[[4]], c("loc", "scale", "shape"))
  purrr::walk(output[[4]], ~ expect_type(.x, "double"))
  purrr::walk(output, ~ expect_false(any(is.na(.x))))
})

test_that("Check that CDsXML integrates into QuickResults correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  output <- QuickResults(CDs.72007)

  expect_type(output, "list")
  expect_length(output, 4)
  expect_s3_class(output[[1]], "data.frame")
  expect_equal(ncol(output[[1]]), 5)
  expect_named(output[[1]], c("RP", "Q", "GF", "lower68", "upper68"))
  expect_equal(nrow(output[[1]]), 10)
  purrr::walk(output[[1]], ~ expect_type(.x, "double"))
  expect_true(is.matrix(output[[2]]))
  expect_equal(dim(output[[2]]), c(1, 2))
  expect_identical(colnames(output[[2]]), c("PooledLcv", "PooledLSkew"))
  expect_type(output[[2]][, "PooledLcv"], "double")
  expect_type(output[[2]][, "PooledLSkew"], "double")
  expect_s3_class(output[[3]], "data.frame")
  expect_equal(nrow(output[[3]]), 1)
  expect_named(output[[3]], c("loc", "scale", "shape"))
  purrr::walk(output[[3]], ~ expect_type(.x, "double"))
  expect_s3_class(output[[4]], "data.frame")
  expect_equal(nrow(output[[4]]), 1)
  expect_named(output[[4]], c("loc", "scale", "shape"))
  purrr::walk(output[[4]], ~ expect_type(.x, "double"))
  purrr::walk(output, ~ expect_false(any(is.na(.x))))
})

###### ReFH ######

test_that("Check that GetCDs integrates into ReFH correctly", {
  CDs.73005 <- GetCDs(73005)
  output <- ReFH(CDs.73005)

  expect_type(output, "list")
  expect_length(output, 2)
  expect_s3_class(output[[1]], "data.frame")
  expect_equal(nrow(output[[1]]), 1)
  expect_equal(ncol(output[[1]]), 8)
  expect_named(output[[1]], c("AREA", "TP", "D", "BR", "BL", "Cmax", "Cini", "BFini"))
  purrr::walk(output[[1]], ~ expect_type(.x, "double"))
  expect_s3_class(output[[2]], "data.frame")
  expect_equal(ncol(output[[2]]), 5)
  expect_named(output[[2]], c("Rain", "NetRain", "Runoff", "Baseflow", "TotalFlow"))
  expect_gte(nrow(output[[2]]), 1)
  purrr::walk(output[[2]], ~ expect_type(.x, "double"))
  expect_true(all(output[[2]]$TotalFlow >= output[[2]]$Baseflow))
  expect_true(all(output[[2]]$TotalFlow >= output[[2]]$Runoff))
})

test_that("Check that CDsXML integrates into ReFH correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  output <- ReFH(CDs.72007)

  expect_type(output, "list")
  expect_length(output, 2)
  expect_s3_class(output[[1]], "data.frame")
  expect_equal(nrow(output[[1]]), 1)
  expect_equal(ncol(output[[1]]), 8)
  expect_named(output[[1]], c("AREA", "TP", "D", "BR", "BL", "Cmax", "Cini", "BFini"))
  purrr::walk(output[[1]], ~ expect_type(.x, "double"))
  expect_s3_class(output[[2]], "data.frame")
  expect_equal(ncol(output[[2]]), 5)
  expect_named(output[[2]], c("Rain", "NetRain", "Runoff", "Baseflow", "TotalFlow"))
  expect_gte(nrow(output[[2]]), 1)
  purrr::walk(output[[2]], ~ expect_type(.x, "double"))
  expect_true(all(output[[2]]$TotalFlow >= output[[2]]$Baseflow))
  expect_true(all(output[[2]]$TotalFlow >= output[[2]]$Runoff))
})

###### SCF ######

test_that("Check that GetCDs integrates into SCF correctly", {
  CDs.73005 <- GetCDs(73005)
  output <- SCF(SAAR = CDs.73005$Value[CDs.73005$Descriptor == "SAAR"], duration = 2)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_length(output, 2)
  expect_named(output, c("SCFSummer", "SCFWinter"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
  expect_true(all(output$SCFSummer >= 0 & output$SCFSummer <= 1))
  expect_true(all(output$SCFWinter >= 0 & output$SCFWinter <= 1))
})

test_that("Check that CDsXML integrates into SCF correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  output <- SCF(SAAR = CDs.72007$Value[CDs.72007$Descriptor == "SAAR"], duration = 10)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_length(output, 2)
  expect_named(output, c("SCFSummer", "SCFWinter"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
  expect_true(all(output$SCFSummer >= 0 & output$SCFSummer <= 1))
  expect_true(all(output$SCFWinter >= 0 & output$SCFWinter <= 1))
})

test_that("Check that NRFAData integrates into SCF correctly", {
  CDs_39001 <- NRFAData[rownames(NRFAData) == 39001, ]
  output <- SCF(SAAR = CDs_39001$SAAR, duration = 5.5)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_length(output, 2)
  expect_named(output, c("SCFSummer", "SCFWinter"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
  expect_true(all(output$SCFSummer >= 0 & output$SCFSummer <= 1))
  expect_true(all(output$SCFWinter >= 0 & output$SCFWinter <= 1))
})

###### UAF ######

test_that("Check that GetCDs integrates into UAF correctly", {
  CDs.73005 <- GetCDs(73005)
  output <- UAF(CDs = CDs.73005)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_length(output, 2)
  expect_named(output, c("PRUAF", "UAF"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
  expect_true(all(output$PRAF >= 0))
  expect_true(all(output$UAF >= 0))
})

test_that("Check that CDsXML integrates into UAF correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  output <- UAF(CDs = CDs.72007)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_length(output, 2)
  expect_named(output, c("PRUAF", "UAF"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
  expect_true(all(output$PRAF >= 0))
  expect_true(all(output$UAF >= 0))
})

test_that("Check that NRFAData integrates into UAF correctly", {
  CDs_39001 <- NRFAData[rownames(NRFAData) == 39001, ]
  output <- UAF(URBEXT2000 = CDs_39001$URBEXT2000, BFIHOST = CDs_39001$BFIHOST19)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 1)
  expect_length(output, 2)
  expect_named(output, c("PRUAF", "UAF"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
  expect_true(all(output$PRAF >= 0))
  expect_true(all(output$UAF >= 0))
})

###### Uncertainty ######

test_that("Check that Pool and QMED integrates into Uncertainty correctly", {
  CDs.203018 <- GetCDs(203018)
  Pool.203018 <- Pool(CDs.203018)
  output <- Uncertainty(Pool.203018, qmed = QMED(CDs.203018))

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 10)
  expect_equal(ncol(output), 4)
  expect_named(output, c("RP", "Central", "Lower", "Upper"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
  expect_true(all(output >= 0))
})

test_that("Check that PoolSmall integrates into Uncertainty correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  Pool.72007 <- PoolSmall(CDs.72007)
  output <- Uncertainty(Pool.72007, Gauged = TRUE)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), 10)
  expect_equal(ncol(output), 4)
  expect_named(output, c("RP", "Central", "Lower", "Upper"))
  purrr::walk(output, ~ expect_type(.x, "double"))
  expect_false(any(is.na(output)))
  expect_true(all(output >= 0))
})

###### WeightsGLcv ######

test_that("Check that Pool integrates into WeightsGLcv correctly", {
  CDs.203018 <- GetCDs(203018)
  Pool.203018 <- Pool(CDs.203018)
  output <- WeightsGLcv(Pool.203018)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(Pool.203018))
  expect_equal(ncol(output), 2)
  expect_named(output, c("Site", "Weight"))
  expect_type(output$Site, "character")
  expect_type(output$Weight, "double")
  expect_false(any(is.na(output)))
  expect_true(all(output$Weight >= 0 & output$Weight <= 1))
})

test_that("Check that PoolSmall integrates into WeightsGLcv correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  Pool.72007 <- PoolSmall(CDs.72007)
  output <- WeightsGLcv(Pool.72007)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(Pool.72007))
  expect_equal(ncol(output), 2)
  expect_named(output, c("Site", "Weight"))
  expect_type(output$Site, "character")
  expect_type(output$Weight, "double")
  expect_false(any(is.na(output)))
  expect_true(all(output$Weight >= 0 & output$Weight <= 1))
})

###### WeightsGLSkew ######

test_that("Check that Pool integrates into WeightsGLSkew correctly", {
  CDs.203018 <- GetCDs(203018)
  Pool.203018 <- Pool(CDs.203018)
  output <- WeightsGLSkew(Pool.203018)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(Pool.203018))
  expect_equal(ncol(output), 2)
  expect_named(output, c("Site", "Weight"))
  expect_type(output$Site, "character")
  expect_type(output$Weight, "double")
  expect_false(any(is.na(output)))
  expect_true(all(output$Weight >= 0 & output$Weight <= 1))
})

test_that("Check that PoolSmall integrates into WeightsGLSkew correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  Pool.72007 <- PoolSmall(CDs.72007)
  output <- WeightsGLSkew(Pool.72007)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(Pool.72007))
  expect_equal(ncol(output), 2)
  expect_named(output, c("Site", "Weight"))
  expect_type(output$Site, "character")
  expect_type(output$Weight, "double")
  expect_false(any(is.na(output)))
  expect_true(all(output$Weight >= 0 & output$Weight <= 1))
})

###### WeightsUnLcv ######

test_that("Check that Pool integrates into WeightsUnLcv correctly", {
  CDs.203018 <- GetCDs(203018)
  Pool.203018 <- Pool(CDs.203018)
  output <- WeightsUnLcv(Pool.203018)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(Pool.203018))
  expect_equal(ncol(output), 2)
  expect_named(output, c("Site", "Weight"))
  expect_type(output$Site, "character")
  expect_type(output$Weight, "double")
  expect_false(any(is.na(output)))
  expect_true(all(output$Weight >= 0 & output$Weight <= 1))
})

test_that("Check that PoolSmall integrates into WeightsUnLcv correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  Pool.72007 <- PoolSmall(CDs.72007)
  output <- WeightsUnLcv(Pool.72007)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(Pool.72007))
  expect_equal(ncol(output), 2)
  expect_named(output, c("Site", "Weight"))
  expect_type(output$Site, "character")
  expect_type(output$Weight, "double")
  expect_false(any(is.na(output)))
  expect_true(all(output$Weight >= 0 & output$Weight <= 1))
})

###### WeightsUnLSkew ######

test_that("Check that Pool integrates into WeightsUnLSkew correctly", {
  CDs.203018 <- GetCDs(203018)
  Pool.203018 <- Pool(CDs.203018)
  output <- WeightsUnLSkew(Pool.203018)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(Pool.203018))
  expect_equal(ncol(output), 2)
  expect_named(output, c("Site", "Weight"))
  expect_type(output$Site, "character")
  expect_type(output$Weight, "double")
  expect_false(any(is.na(output)))
  expect_true(all(output$Weight >= 0 & output$Weight <= 1))
})

test_that("Check that PoolSmall integrates into WeightsUnLSkew correctly", {
  CDs.72007 <- CDsXML(testthat::test_path("testdata", "072007-brock-at-upstream-of-a6.xml"))
  Pool.72007 <- PoolSmall(CDs.72007)
  output <- WeightsUnLSkew(Pool.72007)

  expect_s3_class(output, "data.frame")
  expect_equal(nrow(output), nrow(Pool.72007))
  expect_equal(ncol(output), 2)
  expect_named(output, c("Site", "Weight"))
  expect_type(output$Site, "character")
  expect_type(output$Weight, "double")
  expect_false(any(is.na(output)))
  expect_true(all(output$Weight >= 0 & output$Weight <= 1))
})

###### WGaugLcv ######

test_that("Check that Pool integrates into WGaugLcv correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- WGaugLcv(Pool.27051)

  expect_type(output, "double")
  expect_length(output, 1)
  expect_true(output > 0)
})

test_that("Check that PoolSmall integrates into WGaugLcv correctly", {
  CDs.10001 <- GetCDs(10001)
  Pool.10001 <- PoolSmall(CDs.10001)
  output <- WGaugLcv(Pool.10001)

  expect_type(output, "double")
  expect_length(output, 1)
  expect_true(output > 0)
})

###### WGaugLSkew ######

test_that("Check that Pool integrates into WGaugLSkew correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- WGaugLSkew(Pool.27051)

  expect_type(output, "double")
  expect_length(output, 1)
  expect_true(output > 0)
})

test_that("Check that PoolSmall integrates into WGaugLSkew correctly", {
  CDs.10001 <- GetCDs(10001)
  Pool.10001 <- PoolSmall(CDs.10001)
  output <- WGaugLSkew(Pool.10001)

  expect_type(output, "double")
  expect_length(output, 1)
  expect_true(output > 0)
})

###### WungLcv ######

test_that("Check that Pool integrates into WungLcv correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- WungLcv(Pool.27051)

  expect_type(output, "double")
  expect_length(output, 1)
  expect_true(output > 0)
})

test_that("Check that PoolSmall integrates into WungLcv correctly", {
  CDs.10001 <- GetCDs(10001)
  Pool.10001 <- PoolSmall(CDs.10001)
  output <- WungLcv(Pool.10001)

  expect_type(output, "double")
  expect_length(output, 1)
  expect_true(output > 0)
})

###### WungLSkew ######

test_that("Check that Pool integrates into WungLSkew correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- WungLSkew(Pool.27051)

  expect_type(output, "double")
  expect_length(output, 1)
  expect_true(output > 0)
})

test_that("Check that PoolSmall integrates into WungLSkew correctly", {
  CDs.10001 <- GetCDs(10001)
  Pool.10001 <- PoolSmall(CDs.10001)
  output <- WungLSkew(Pool.10001)

  expect_type(output, "double")
  expect_length(output, 1)
  expect_true(output > 0)
})

###### Zdists ######

test_that("Check that Pool integrates into Zdists correctly", {
  CDs.27051 <- GetCDs(27051)
  Pool.27051 <- Pool(CDs.27051)
  output <- Zdists(Pool.27051)

  expect_type(output, "list")
  expect_length(output, 2)
  expect_s3_class(output[[1]], "data.frame")
  expect_equal(nrow(output[[1]]), 1)
  expect_named(output[[1]], c("GEV", "GenLog", "Gumbel", "Kappa3"))
  purrr::walk(output[[1]], ~ expect_type(.x, "double"))
  expect_type(output[[2]], "character")
  expect_length(output[[2]], 1)
  expect_match(output[[2]], "best fit", ignore.case = TRUE)
  expect_false(any(is.na(output[[1]])))
  expect_false(is.na(output[[2]]))
})

test_that("Check that PoolSmall integrates into Zdists correctly", {
  CDs.10001 <- GetCDs(10001)
  Pool.10001 <- PoolSmall(CDs.10001)
  output <- Zdists(Pool.10001)

  expect_type(output, "list")
  expect_length(output, 2)
  expect_s3_class(output[[1]], "data.frame")
  expect_equal(nrow(output[[1]]), 1)
  expect_named(output[[1]], c("GEV", "GenLog", "Gumbel", "Kappa3"))
  purrr::walk(output[[1]], ~ expect_type(.x, "double"))
  expect_type(output[[2]], "character")
  expect_length(output[[2]], 1)
  expect_match(output[[2]], "best fit", ignore.case = TRUE)
  expect_false(any(is.na(output[[1]])))
  expect_false(is.na(output[[2]]))
})
