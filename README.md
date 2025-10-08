# Introduction

The primary purpose of the UKFE R package is to implement the regional flood frequency methods outlined by the Flood Estimation Handbook (FEH; available at <https://www.ceh.ac.uk/data/software-models/flood-estimation-handbook>) and associated updates. However, there are numerous other functions of use to the hydrologist.

The UKFE package was initially developed by Anthony Hammond in 2020, and he has been updating it regularly since then with additional functionality and in response to new published methods and changes to best practice. 

In 2025, the Environment Agency commissioned JBA Consulting to undertake testing of the package, to write this documentation and training material and to assess future needs. Changes to the package for v1.0.0 were a result of this testing and funded by the Environment Agency. The training material is available at <https://agqhammond.github.io/ukfe-training-materials/>.

# Contents

The 'Changelog' tab summarises updates in each version of the UKFE.

Under 'Articles', there are quick guides to the following topics:

* Basic R concepts and resources (R quick start guide).

* Data inputs. 

*	Data visualisation.

*	FEH statistical analysis.

* ReFH analysis.

*	Troubleshooting common errors.

There is also a 'References' article, which contains the references of books, user guides or papers cited throughout the documentation.

There is no assumption that readers of these guides are experienced 'R' users, but there is an assumption of familiarity with the application of the FEH methods. More detailed step-by-step guidance can be found in the training material. There is an introduction to some R concepts that may be useful in the 'R quick start guide' article.

Note that some of the information in the vignettes is based on that of the FloodHydroStats website (<https://www.floodhydrostats.com/ukfe-package-feh-quick-guide>).

# Hints and tips

For more information about the functions discussed in the guides and further functions of the package, all the functions are detailed in the 'References' tab (with links to their help files) and in a PDF via the following link: <https://cran.r-project.org/package=UKFE/UKFE.pdf>

Details can also be viewed in the package documentation within the 'R' environment. To get help with individual functions, you can type `help(FunctionName)` or `?FunctionName`. If using RStudio, you can click on the 'Packages' tab in the bottom right pane (assuming the default layout), search for UKFE, and then click on it. You will then see a list of functions; each can be clicked for more details. To get a list of the function names, type `ls("package:UKFE")`.

Note that for any functions that take file paths, on Windows operating systems, the backslashes will need to be changed to forward slashes, or the file path will need to be stated as follows: `r"{my\file\path}"`.

Many of the examples throughout this documentation have the outputs from running the code displayed below the code.

# Installation guide

It is recommended that the UKFE R package be used in RStudio (an integrated development environment for R). You will first need to install R from <https://cran.r-project.org/> and then RStudio can be downloaded from <https://posit.co/download/rstudio-desktop/>.

Once RStudio is open, the package must be installed and then loaded to access its functions.

Note that installing the package only needs to be done once (although it will need redoing if the UKFE package is updated or if you update your version of R). Once you have installed the package, it will be saved in your package library. 

The package must be loaded from the library at the start of each new R session to make its functions available.

The following commands can be entered into the console to carry out these steps. 

## Install from CRAN 

Install the UKFE package: `install.packages("UKFE")`

## Install from GitHub

First, install the `remotes` package if you do not already have it installed: `install.packages("remotes")`

Then, install UKFE directly from GitHub: `remotes::install_github("agqhammond/UKFE")`

You may be prompted to update package dependencies — choosing option "1: All" is recommended.

## Load the package

Once installed, load it into your R session: `library(UKFE)`

You can also check the installed version with: `packageVersion("UKFE")`

# Data

Acknowledgement: Data from the UK National River Flow Archive.

This package is based on data from the National River Flow Archive (NRFA). There is a pre-processing script that converts new releases of the NRFA Peak Flow Dataset into data frames suitable for use within UKFE.

Note that the unit tests include a small subset of data from the NRFA Peak Flow Dataset, consisting of XML and AM files. This data sample can only be used for test purposes. Please see the NRFA website (<https://nrfa.ceh.ac.uk/>) for station metadata, the download of the full peak flows dataset (<https://nrfa.ceh.ac.uk/data/peak-flow-dataset>), and the data licence (<https://eidc.ceh.ac.uk/licences/nrfa-data-terms-and-conditions-for-api-access-to-time-series-data-and-metadata/>).

# Feedback

If you have any feedback or suggestions, then please contact Anthony Hammond at [floodhydrostats@gmail.com](mailto:floodhydrostats@gmail.com) or contribute to the GitHub discussion page (<https://github.com/agqhammond/UKFE/issues>). You can submit edits and additions to the package through GitHub.
