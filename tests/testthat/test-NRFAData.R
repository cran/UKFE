library(testthat)
library(rnrfa)

###### NRFAData ######

testthat::test_that("Check that the NRFAData results are correct for gauge 39001, except for Area, BFIHOST19, X, Y, Lcv, LSkew, LKurt, L1, L2, and, N as they're either incorrect or not stored", {
  library(UKFE)

  stations <- rnrfa::catalogue()

  stations_39001 <- stations[stations$id == 39001, c(
    "altbar", "aspbar", "aspvar",
    "dplbar", "dpsbar", "farl", "mean-flood-plain-extent", "ldp", "propwet", "rmed-1h",
    "rmed-1d", "rmed-2d", "saar-1961-1990", "saar-1941-1970",
    "sprhost", "urbext-2000", "qmed"
  )]

  ukfe <- NRFAData[rownames(NRFAData) == 39001, c(
    "ALTBAR", "ASPBAR", "ASPVAR",
    "DPLBAR", "DPSBAR", "FARL", "FPEXT", "LDP", "PROPWET",
    "RMED-1H", "RMED-1D", "RMED-2D", "SAAR",
    "SAAR4170", "SPRHOST", "URBEXT2000",
    "QMED"
  )]

  rownames(ukfe) <- NULL

  testthat::expect_equal(as.numeric(stations_39001[1, ]), as.numeric(ukfe[1, ]), tolerance = 0.01) # Allow 1% tolerance for dp differences and some differences due to area not being updated on NRFA website
})

testthat::test_that("Check that the NRFAData results are correct for Area, bfihost19, easting, and northing for gauge 28115", {
  library(UKFE)

  cds_path <- testthat::test_path("testdata", "028115-maun-at-mansfield-the-dykes.xml")

  cds <- CDsXML(cds_path)

  common_descriptors <- c("AREA", "BFIHOST19", "Easting", "Northing")

  ukfe <- NRFAData[
    rownames(NRFAData) == 28115,
    common_descriptors
  ]


  for (desc in common_descriptors) {
    expected <- ukfe$Value[ukfe$Descriptor == desc]
    actual <- cds[[desc]]
    testthat::expect_equal(actual, expected)
  }
})

testthat::test_that("Check that the L-moments and length of data are correct for gauge 39001", {
  library(UKFE)

  common_descriptors <- c("Lcv", "LSkew", "LKurt", "L1", "L2", "N")

  actual_output <- NRFAData[
    rownames(NRFAData) == 39001,
    common_descriptors
  ]

  rownames(actual_output) <- NULL

  am_39001 <- GetAM(39001)

  lmoms_39001 <- LMoments(am_39001$Flow)

  lmoms_39001$L3 <- NULL
  lmoms_39001$L4 <- NULL

  expected_output <- data.frame(lmoms_39001, N = nrow(am_39001))

  expected_output <- expected_output[, c(3, 4, 5, 1, 2, 6)]

  testthat::expect_equal(actual_output, expected_output)
})
