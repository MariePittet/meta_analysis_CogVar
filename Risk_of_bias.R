# This script was made to assess the risk of bias in Pittet et al. (in prep). The analysis focuses on biases that could
# have impacted the score differences between pathological groups and control groups in social cognition tests. The package
# used to visualize the analysis is Robvis (McGuinness & Higgins, 2020) 
# For more information, see: https://cran.r-project.org/web/packages/robvis/vignettes/Introduction_to_robvis.html
# Marie Pittet, May 2023, marie.pittet@chuv.ch


# Environment -------------------------------------------------------------

# Make sure to install robvis through these commands. Otherwise expect bugs in label orientation.
install.packages("devtools")
devtools::install_github("mcguinlu/robvis")
library(robvis)
require(tidyverse)

# Reading the csv file containing bias information for all studies
df <- read.csv("Risk_of_bias_assessment.csv", header = TRUE)


# Biases in emotion recognition tests -------------------------------------

# Selecting the columns pertaining to emotion recognition. Adding an unused weight column with all 1 for format recognition 
df_emo<- df[,c(1,5:11)]
df_emo<- df_emo[df_emo$Tool_type == 1,]
df_emo<- df_emo %>% select(-Tool_type)
df_emo$Weights<- 1

# Visualizing bias: emotions
summary_bias_emo<- rob_summary(df_emo, tool = "Generic")
summary_bias_emo
traffic_bias_emo<- rob_traffic_light(df_emo, tool = "Generic", psize=6)
traffic_bias_emo


# Biases in Theory of Mind (ToM) tests -----------------------------------

#Selecting columns rows pertaining to Theory of Mind tasks. Adding unused weight vector
df_ToM<- df[,c(1,5:11)]
df_ToM<- df_ToM[df_ToM$Tool_type == 2,]
df_ToM<- df_ToM %>% select(-Tool_type)
df_ToM$Weights<- 1

# Visualizing bias: ToM
summary_bias_ToM<- rob_summary(df_ToM, tool = "Generic")
summary_bias_ToM
traffic_bias_ToM<- rob_traffic_light(df_ToM, tool = "Generic", psize=6)
traffic_bias_ToM


# Biases in social cognition batteries (bat) ------------------------------------

#Selecting columns rows pertaining to Theory of Mind tasks
df_bat<- df[,c(1,5:11)]
df_bat<- df_bat[df_bat$Tool_type == 3,]
df_bat<- df_bat %>% select(-Tool_type)
df_bat$Weights<- 1

# Visualizing bias: batteries
summary_bias_bat<- rob_summary(df_bat, tool = "Generic")
summary_bias_bat
traffic_bias_bat<- rob_traffic_light(df_bat, tool = "Generic", psize=6)
traffic_bias_bat

# Overall summary ---------------------------------------------------------
traffic_bias_overall<- rob_traffic_light(df[,c(1,6:11)], tool = "Generic", psize=5)
traffic_bias_overall

library(RColorBrewer)
library(prettyGraphs)

my_colours<- c(brewer.pal(6, "Set2")[c(1,6,2,3,4)]) # specifying colors manually
my_colours<- add.alpha(col=my_colours,alpha=0.6)
summary_bias_overall<- rob_summary(df[,c(1,6:11)], tool = "Generic", colour = my_colours)
summary_bias_overall
