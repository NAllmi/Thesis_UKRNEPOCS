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
  setwd("C:/Users/natal/OneDrive/Documents/GitHub/Thesis_UKRNEPOCS/")}


# If needed, install, and then load the following packages:
# -------------------------------

library(haven)
library(countrycode)
library(tidyverse)
library(plyr)
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(DHARMa)
library(xtable)
library(car)
library(lmtest)
library(estimatr)
library(sandwich)
library(performance)
library(corrplot)
library(psych)
library(GPArotation)
library(MuMIn)
library(MASS)
library(broom)
library(broom.helpers)
library(GGally)
library(broom.mixed)
library(jtools)
library(fastDummies)
library(huxtable)
library(pheatmap)
#library(ModelMetrics)
# Run the following files
# -------------------------------



# load and prepare data
source("data_preprocessing.R")

#models rq1
source("rq1_models.R")

#models rq2
source("rq2_glm_models.R")

#clear everything
rm(list = ls(all.names = TRUE))
#run efa and save output images
source("efa.R")

## END

