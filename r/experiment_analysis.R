library(readr)
library(ggplot2)
library(ez)
library(pbkrtest)
library(nlme)
library(plyr)

setwd("/Users/Valeria/Documents/Experimental Design and Analysis/ExperimentalDesign_ClassProject/r")

visualvariable_data <- read_delim("lens_experiment.csv", ";", trim_ws = TRUE)

#summary(visualvariable_data)

source("helper_functions.R")

##Linear Regresion Analysis
visualvariable_summary <- summarySE(visualvariable_data, measurevar="PointingTime", groupvars=c("Participant", "Lens","ID"))

VV1_data <- visualvariable_summary[visualvariable_data$Lens == "FL",]
linearmodel <- lm(VV1_data$PointingTime ~ VV1_data$ID)
