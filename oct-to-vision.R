################################################################
# HarvardX Data Science Capstone Course
# Data Science: Capstone - R Script for Choose Your Own 
# Author: Dror Lichtenberg
# Start Date: 05 September 2020
# Developed using R version 3.5.3 (2019-03-11) -- "Great Truth"
#################################################################

# Section 1: Prerequisites
## 1.a Ensure required R packages are installed
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org", dependencies = TRUE) # %<>% operator ...
if(!require(Amelia)) install.packages("Amelia", repos = "http://cran.us.r-project.org", dependencies = TRUE) # missmap

## 1.b Global settings and helper functions
getOcularHypertension <- function(strInput, whichEye) {
  # strInput is in format "RE-[:digit:]{1,2}\nLE-[:digit:]{1,2}"
  # whichEye is either "RE" for right-eye or "LE" for left-eye
  if (is.null(strInput) | strInput == "") 
    NA
  else {
    entry <- ifelse(whichEye=="RE", 1, 2)
    section <- str_split(strInput, "\\n", simplify = TRUE)[1, entry]
    as.numeric(unlist(strsplit(section, "-"))[2])
  }
}

vGetREHypertension<- function(vStrInput) {
  unlist(lapply(vStrInput, getOcularHypertension, whichEye="RE"))
}

vGetLEHypertension<- function(vStrInput) {
  unlist(lapply(vStrInput, getOcularHypertension, whichEye="LE"))
}

getLastAcuity <- function(strInput, whichEye) {
  # strInput is in complex format. Find the number in last 6/<number>
  # whichEye is either "RE" for right-eye or "LE" for left-eye
  if (is.null(strInput) | strInput == "") 
    NA
  else {
    entry <- ifelse(whichEye=="RE", 1, 2)
    section <- str_split(strInput, "\\n", simplify = TRUE)[1, entry]
    snellenDenominator <- as.numeric(unlist(str_match(section, ".*6/([0-9]+\\.?[0-9]*).*"))[2])
    logMAR <- log10(snellenDenominator/6)
    logMAR
  }
}

vGetREAcuity <- function(vStrInput) {
  unlist(lapply(vStrInput, getLastAcuity, whichEye="RE"))
}

vGetLEAcuity <- function(vStrInput) {
  unlist(lapply(vStrInput, getLastAcuity, whichEye="LE"))
}


## 1.c Create sub-directories for rda files and images (to be used in report)
if (!file.exists("./rdas")){dir.create("./rdas")}
if (!file.exists("./figs")){dir.create("./figs")}

# Section 2: Load, clean, Summarize and split data
## 2.a Load <TODO Place data on GitHub and read from it>
patients_data <- read.csv("./data/patients_data.csv", stringsAsFactors = FALSE)

visit_summary_data <- read.csv("./data/visit_summary_data.csv", stringsAsFactors = FALSE)
visit_summary_data %<>% mutate(REACC = vGetREAcuity(ACUITIES), 
                               LEACC = vGetLEAcuity(ACUITIES),
                               REHT = vGetREHypertension(PATDATA.F5614), 
                               LEHT = vGetLEHypertension(PATDATA.F5614))
missmap(visit_summary_data[,5:8], col = c("red", "lightgreen"), rank.order = FALSE)

# now need the OCT data ..

