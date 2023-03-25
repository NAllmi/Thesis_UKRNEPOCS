#############################################################
# Technical University Munich
# Chair of Digital Governance
# Master Replication File for Thesis
# Natalia Allmi
# February 2023
#############################################################

# clear environment
# -------------------------------
rm(list = ls())

# Please set your user name and custom path here:
# -------------------------------

if (Sys.getenv("USERNAME") == "natal"){
  setwd("C:/Users/natal/OneDrive/Documents/Documents/Work/Thesis/replication/")}


# If needed, install, and then load the following packages:
# -------------------------------

library(haven)
library(countrycode)
library(tidyverse)
library(plyr)
library(ggplot2)
library(dplyr)
#library(ggcorrplot)
library(glmmTMB)
library(DHARMa)
library(regclass)
library(ordinal)
library(xtable)
library(car)
library(lmtest)
library(estimatr)
#library(GGally)
#library(gridExtra)

# Run the following files
# -------------------------------

# load and prepare data
source("data_preprocessing.R")

 


## END

