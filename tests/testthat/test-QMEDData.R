library(testthat)
library(rnrfa)

###### QMEDData ######
testthat::test_that("Check that the QMEDData results are correct for gauge 39001, except for Area, BFIHOST19, DPSBAR, LDP, QMEDcd, X, Y, QMEDfse and, N as they're either incorrect or not stored", {
  library(UKFE)

  stations <- rnrfa::catalogue()

  stations_39001 <- stations[stations$id == 39001, c(
    "altbar", "aspbar", "aspvar",
    "dplbar", "farl", "mean-flood-plain-extent", "propwet", "rmed-1h",
    "rmed-1d", "rmed-2d", "saar-1961-1990", "saar-1941-1970",
    "sprhost", "urbext-2000", "qmed",
    "urbext-1990", "bfihost"
  )]

  ukfe <- QMEDData[rownames(QMEDData) == 39001, c(
    "ALTBAR", "ASPBAR", "ASPVAR",
    "DPLBAR", "FARL", "FPEXT", "PROPWET",
    "RMED-1H", "RMED-1D", "RMED-2D", "SAAR",
    "SAAR4170", "SPRHOST", "URBEXT2000",
    "QMED", "URBEXT1990", "BFIHOST"
  )]

  rownames(ukfe) <- NULL

  testthat::expect_equal(as.numeric(stations_39001[1, ]), as.numeric(ukfe[1, ]), tolerance = 0.1)
})

testthat::test_that("Check that the QMEDData results are correct for Area, BFIHOST19, DPSBAR, LDP, X, Y, and N for gauge 28115", {
  library(UKFE)

  cds_path <- testthat::test_path("testdata", "028115-maun-at-mansfield-the-dykes.xml")

  cds <- CDsXML(cds_path)

  common_descriptors <- c("AREA", "BFIHOST19", "DPSBAR", "LDP", "X", "Y")

  ukfe <- QMEDData[
    rownames(QMEDData) == 28115,
    common_descriptors
  ]


  for (desc in common_descriptors) {
    expected <- ukfe$Value[ukfe$Descriptor == desc]
    actual <- cds[[desc]]
    testthat::expect_equal(actual, expected)
  }
})

testthat::test_that("Check that the QMEDData catchment descriptors are the same as those extracted using GetCDs for gauge 39001", {
  library(UKFE)

  expected_output <- GetCDs(39001)
  QMEDfse_expect <- expected_output$Value[expected_output$Descriptor == "QMEDfse"]
  QMED_expect <- median(GetAM(39001)$Flow)
  QMEDcd_expect <- QMED(GetCDs(39001))

  actual_output <- QMEDData[rownames(QMEDData) == 39001, ]
  QMEDfse_actual <- actual_output$QMEDfse
  QMED_actual <- actual_output$QMED
  QMEDcd_actual <- actual_output$QMEDcd
  actual_output <- actual_output[, !names(actual_output) %in% c("QMED", "QMEDcd", "QMEDfse")]

  actual_output <- unlist(actual_output, use.names = TRUE)
  expected_output <- setNames(expected_output$Value, expected_output$Descriptor)

  names(actual_output)[names(actual_output) == "X"] <- "Easting"
  names(actual_output)[names(actual_output) == "Y"] <- "Northing"

  expected_output <- expected_output[sort(names(expected_output))]
  actual_output <- actual_output[sort(names(actual_output))]

  expected_output <- expected_output[!names(expected_output) %in% "QMEDfse"]

  testthat::expect_equal(actual_output, expected_output)
  testthat::expect_equal(QMEDfse_expect, QMEDfse_actual, tolerance = 0.01) # Due to simulation, tolerance is required
  testthat::expect_equal(QMED_expect, QMED_actual)
  testthat::expect_equal(QMEDcd_expect, QMEDcd_actual)
})
