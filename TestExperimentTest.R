library(readr)

library(ggplot2)

library(ez) # requires having installed package "ez"

source("helper_functions.R")

setwd("/Users/rankovae")
TestExperimentTest <- read_delim("all_logs.csv"  , trim_ws = TRUE)
spec(TestExperimentTest)


TestExperimentTest$VV <- factor(TestExperimentTest$VV)
TestExperimentTest$OC <- factor(TestExperimentTest$OC)


levels(TestExperimentTest$VV)
TestExperimentTest$VV <- ordered(TestExperimentTest$VV,
                         levels = c("VV1", "VV2", "VV1VV2"))



VV1_Data <- TestExperimentTest[TestExperimentTest$VV == "VV1", ]

ezANOVA( data=VV1_Data,dv=.(visualSearchTime), wid=.(Participant), within =. (OC), detailed=TRUE)


VV2_Data <- TestExperimentTest[TestExperimentTest$VV == "VV2", ]
ezANOVA( data=VV2_Data,dv=.(visualSearchTime), wid=.(Participant), within =. (OC), detailed=TRUE)
VV1VV2_Data <- TestExperimentTest[TestExperimentTest$VV == "VV1VV2", ]

ezANOVA( data=TestExperimentTest,dv=.(visualSearchTime), wid=.(Participant), within =. (VV), detailed=TRUE)
pairwise.t.test(TestExperimentTest$visualSearchTime,TestExperimentTest$VV,pairwise=TRUE)

TukeyHSD(model, conf.level=.95)

ggplot(TestExperimentTest, aes(x=VV, y=visualSearchTime)) + # map chart attributes to data
  geom_point(shape=1) + # add a point layer (one circle per data point)
  geom_bar(stat = "identity") # add a regression line layer







