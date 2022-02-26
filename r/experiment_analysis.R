library(readr)
library(ggplot2)
library(ez)
library(pbkrtest)
library(nlme)
library(plyr)

setwd("/Users/Valeria/Documents/Experimental Design and Analysis/ExperimentalDesign_ClassProject/r")

vv_file <- read_delim("all_logs.csv", ",", trim_ws = TRUE)

# Removing practice runs
vv_experiment <- vv_file[vv_file$Practice=="FALSE",]
summary(vv_experiment)

vv_experiment$VV <- factor(vv_experiment$VV)
# RE-LEVELS data so that "Small" is first --> used later for plotting
vv_experiment$OC <- factor(vv_experiment$OC, levels = c("Small", "Medium", "Large"))
summary(vv_experiment)

# Creating a dataset for separate variables
vv1 <- vv_experiment[vv_experiment$VV=="VV1",]
vv2 <- vv_experiment[vv_experiment$VV=="VV2",]
vv1vv2 <- vv_experiment[vv_experiment$VV=="VV1VV2",]


summary(vv1)
summary(vv2)
summary(vv1vv2)

# Linear model
vv1_linear_model <- lm(vv1$visualSearchTime ~ vv1$Participant)
vv2_linear_model <- lm(vv2$visualSearchTime ~ vv2$Participant)
vv1vv2_linear_model <- lm(vv1vv2$visualSearchTime ~ vv1vv2$Participant)

vv1_linear_model <- lm(vv1$visualSearchTime ~ vv1$OC)
vv2_linear_model <- lm(vv2$visualSearchTime ~ vv2$OC)
vv1vv2_linear_model <- lm(vv1vv2$visualSearchTime ~ vv1vv2$OC)

summary(vv1_linear_model)
summary(vv2_linear_model)
summary(vv1vv2_linear_model)

library(ggplot2)

# PLOT VISUAL SEARCH TIME based on VV
ggplot(vv_experiment, aes(x=visualSearchTime, color=VV)) + theme(text = element_text(size=30), legend.position = "top") +
  geom_density()

ggsave("visualSearchTime_VV.png", plot = last_plot(), path = "./plots")

# PLOT VISUAL SEARCH TIME depending on OC
ggplot(vv1, aes(y=visualSearchTime, x=OC)) + geom_density( color="red") + theme(text = element_text(size=30), legend.position = "top") + ggtitle("VV1: Style")
ggsave("VV1_geom_density.png",plot = last_plot(), path = "./plots")
ggplot(vv2, aes(y=visualSearchTime, x=OC)) + geom_density( color="#009900") + theme(text = element_text(size=30), legend.position = "top") + ggtitle("VV2: Thickness")
ggsave("VV2_geom_density.png",plot = last_plot(), path = "./plots")
ggplot(vv1vv2, aes(y=visualSearchTime, x=OC), size=20) + geom_density( color="blue") + theme(text = element_text(size=30), legend.position = "top") + ggtitle("VV1VV2: ThicknessStyle")
ggsave("VV1VV2_geom_density.png",plot = last_plot(), path = "./plots")

# A bit nicer graph to look at :) 
ggplot(vv1, aes(y=visualSearchTime, x=OC)) + geom_point( color="red") + theme(text = element_text(size=30), legend.position = "top") + ggtitle("VV1: Style")
ggsave("VV1_geom_point.png",plot = last_plot(), path = "./plots")
ggplot(vv2, aes(y=visualSearchTime, x=OC)) + geom_point(color="#009900") + theme(text = element_text(size=30), legend.position = "top") + ggtitle("VV2: Thickness")
ggsave("VV2_geom_point.png",plot = last_plot(), path = "./plots")
ggplot(vv1vv2, aes(y=visualSearchTime, x=OC)) + geom_point( color="blue") + theme(text = element_text(size=30), legend.position = "top") + ggtitle("VV1VV2: ThicknessStyle")
ggsave("VV1VV2_geom_point.png",plot = last_plot(), path = "./plots")

# OPTIONAL: AGGREGATING REPLICATIONS
source("helper_functions.R")

vv_experiment_summary <- summarySEwithin(vv_experiment, measurevar="visualSearchTime", withinvars=c("VV", "OC"), idvar="Participant")
vv_experiment_summary$OC <- as.double(vv_experiment_summary$OC)

vv1_summary <- vv_experiment_summary[vv_experiment_summary$VV=="Style",]
vv2_summary <- vv_experiment_summary[vv_experiment_summary$VV=="Thickness",]
vv1vv2_summary <- vv_experiment_summary[vv_experiment_summary$VV=="ThicknessStyle",]

vv1_summary_linear <- lm(vv1$visualSearchTime ~ vv1$OC)
vv2_summary_linear <- lm(vv2$visualSearchTime ~ vv2$OC)
vv1vv2_summary_linear <- lm(vv1vv2$visualSearchTime ~ vv1vv2$OC)

summary(vv1_summary_linear)
summary(vv2_summary_linear)
summary(vv1vv2_summary_linear)

# PLOTTING LINEAR REGRESSION
# vv_experiment
vv_experiment$OC <- as.double(vv_experiment$OC)
ggplot(vv_experiment, aes(x=OC, y=visualSearchTime, color=VV)) + theme(text = element_text(size=30), legend.position = "top") + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE)

ggsave("linear_regression_VV_OC.png",plot = last_plot(), path = "./plots")

# OPTIONAL: vv_experiment_summary
ggplot(vv_experiment_summary, aes(x=OC, y=visualSearchTime, color=VV)) + theme(text = element_text(size=30), legend.position = "top") + geom_point(shape=1) + geom_smooth(method=lm, se=FALSE)
ggsave("aggregated_linear_regression_VV_OC.png",plot = last_plot(), path = "./plots")

# ANOVA TEST
library(ez)
# H0: There is no change between different object counts
# H1: VV1 (Style) is preattentive
# One-way repeated measures anova in case of a within-subject design
ezANOVA(data=vv1, dv=.(visualSearchTime), wid=.(Participant), within =.(OC), detailed=TRUE)
# p=0.2301203862
# p>0.05 -> VV1 is preattentive

# H0: There is no change between different object counts
# H2: VV2 (Thickness) is preattentive
# One-way repeated measures anova in case of a within-subject design
ezANOVA(data=vv2, dv=.(visualSearchTime), wid=.(Participant), within =.(OC), detailed=TRUE)
# p=0.0185909819 
# p<0.05 -> VV2 is NOT preattentive

# LINEAR RELATIONSHIP:
# p=0.001182 (line 36: summary(vv2_linear_model))

# Can't do the H3 because VV2 isn't preattentive
# H3: VV1VV2 (ThicknessStyle) are less preattentive when used together than VV1 and VV2 in isolation
# ezANOVA(data=vv_experiment, dv=.(visualSearchTime), wid=.(Participant), within =.(VV), detailed=TRUE)