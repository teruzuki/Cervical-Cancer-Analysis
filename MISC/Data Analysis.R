library(devtools)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(cowplot)

#Importing Data
setwd("C:/Users/Yuefu/Documents/GitHub/Independent Project")
masterData <- read.csv(file = "risk_factors_cervical_cancer.csv")
head(masterData)

#Cleaning Data
