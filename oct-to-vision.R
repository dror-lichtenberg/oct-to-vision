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
getOcularTension <- function(strInput, whichEye) {
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

vGetRETension<- function(vStrInput) {
  unlist(lapply(vStrInput, getOcularTension, whichEye="RE"))
}

vGetLETension<- function(vStrInput) {
  unlist(lapply(vStrInput, getOcularTension, whichEye="LE"))
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
patients_data <- read.csv("https://raw.githubusercontent.com/dror-lichtenberg/oct-to-vision/main/data/patients_data.csv", stringsAsFactors = FALSE)
patients_data$GENDER <- as.factor(patients_data$GENDER)
patients_data$DATE_OF_BIRTH <- as.Date(patients_data$DATE_OF_BIRTH,format="%d/%m/%Y")


visit_summary_data <- read.csv("https://raw.githubusercontent.com/dror-lichtenberg/oct-to-vision/main/data/visit_summary_data.csv", stringsAsFactors = FALSE)
visit_summary_data %<>% mutate(REACC = vGetREAcuity(ACUITIES), 
                               LEACC = vGetLEAcuity(ACUITIES),
                               REHT = vGetRETension(PATDATA.F5614), 
                               LEHT = vGetLETension(PATDATA.F5614))
missmap(visit_summary_data[,5:8], col = c("red", "lightgreen"), rank.order = FALSE)

#  OCT data ..
oct_data <- read.csv("https://raw.githubusercontent.com/dror-lichtenberg/oct-to-vision/main/data/oct_data.csv", stringsAsFactors = FALSE)
oct_data <- oct_data %>% mutate(GENDER = ifelse(SEX=="M", "Male", "Female")) %>% filter(OBF_ID != "") %>% select(-SEX, -X)
oct_data$DATE_OF_BIRTH <- as.Date(oct_data$DATE_OF_BIRTH,format="%d/%m/%Y")
oct_data$GENDER <- as.factor(oct_data$GENDER)
oct_data$OBF_ID <- trimws(oct_data$OBF_ID)

oct_patients_data <- oct_data %>% 
                      group_by(OBF_ID) %>% 
                      mutate(NUMBER_OF_SCANS = n()) %>% 
                      select(OBF_ID, DATE_OF_BIRTH, GENDER, NUMBER_OF_SCANS) %>% 
                      ungroup() %>% distinct()

patients_join <- left_join(oct_patients_data, patients_data, by = "OBF_ID", suffix = c("_OCT", "_CRM")) %>%
                      mutate(SAME_GENDER = (GENDER_OCT == GENDER_CRM), SAME_B_DATE = (DATE_OF_BIRTH_OCT == DATE_OF_BIRTH_CRM))
data_to_validate <- patients_join %>% filter(!SAME_GENDER | !SAME_B_DATE)
