#### This script was created to create a forest plot for a meta-analysis (Marie Pittet, CHUV, Lausanne, Fall 2022)-------------------------------------------------

# Setting up the environment, uncomment package installation if needed ----------------------------------------------
install.packages("metaviz")
library(metaviz)
install.packages("MBESS")
library(MBESS)
install.packages("grid")
library(grid)
install.packages("forestploter")
library(forestploter)
install.packages("esc")
library(esc)
install.packages("ggplotify")
library(ggplotify)
install.packages("patchwork")
library(patchwork)
install.packages("gridExtra")
library(gridExtra)
install.packages("grid")
library(grid)
install.packages("ggpubr")
library(ggpubr)
install.packages("meta")
library(meta)
install.packages("metafor")
library(metafor)
library(psych)
library(reshape)
library(ggpattern)
library(tidyverse)
library(RColorBrewer)

# Setting up the data frames -----------------------------------------------

#Load data
df<- read.csv2("Studies_list.csv", header = T, sep=",")
df<- df[1:88,]
df$Population[83]<- "mTBI"
df$Population[88]<- "various neurol. conditions"
#Reorder in a relevant way for tool-wise display
df<-df[order(df$Population_type, df$Tool),]


#Create a data frame for the Forest plot and assigning a population_type=6 to mixed populations
df_forest<- df[!is.na(df$Cs),] # Removing lines with no control group
df_forest[is.na(df_forest$Population_type),3]<- 6


#Apply Hedges g correction for small samples
total_groupsize<- df_forest$Cs+df_forest$Patho
hedges_g<- hedges_g(df_forest$Effect_size, total_groupsize)
df_forest<- data.frame(df_forest,hedges_g)

#Naming population type levels
df_forest$Population_type<- factor(df_forest$Population_type, levels = c(1,2,3,4,5,6),labels = c("SZ", "ASD", "M&A", "NDD", "ABI", "Mixed"))


# Forest plots -------------------------------------------------------------

# separate studies with covariate-adjusted hedges g from others
df_forest1<- df_forest[(df_forest$g_ANCOVA ==""),] # no covariate analyses
df_forest2<- df_forest[!(df_forest$g_ANCOVA ==""),] # covariate analyses


# compute standard error and confidence interval for studies without covariate analyses
df_forest1$Cs<- as.numeric(df_forest1$Cs)
df_forest1$Patho<- as.numeric(df_forest1$Patho)

# Yes, I am very aware that this is the least elegant code to have ever existed but, hey, it works.
for (i in 1:nrow(df_forest1)){
  ci<- hedges_g(cohen.d.ci(df_forest1$Effect_size[i], df_forest1$Cs[i]+df_forest1$Patho[i],df_forest1$Cs[i],df_forest1$Patho[i]),df_forest1$Cs[i]+df_forest1$Patho[i])
  df_forest1$g_lower[i]<- ci[1]
  df_forest1$g_upper[i]<- ci[3] 
}

# Merge things again
df_forest<- rbind(df_forest1,df_forest2)
df_forest$g_lower<- as.numeric(df_forest$g_lower)
df_forest$g_upper<- as.numeric(df_forest$g_upper)


for (i in 1:nrow(df_forest)){   # replacing unadjusted hedges g values with covariate adjusted values 
  
  if(df_forest$g_ANCOVA[i]!= ""){
    df_forest$hedges_g[i]<- df_forest$g_ANCOVA[i] 
  }
}

df_forest$hedges_g<- as.numeric(df_forest$hedges_g)

for (i in 1:nrow(df_forest)){ # computing confidence interval
  df_forest$"SMD [95% CI]"[i]<- sprintf("%.2f [%.2f;%.2f]", df_forest$hedges_g[i],df_forest$g_lower[i],df_forest$g_upper[i])
  df_forest$g_se[i]<- (df_forest$g_upper[i] - df_forest$hedges_g[i]) / 1.96
}


df_forest<-df_forest[order(df_forest$Population_type, df_forest$Tool),]

# Visualization
df_forest_emorecog<- df_forest[df_forest$Tool_type==1,]

# Emotion recognition
m.gen_emo <- metagen(TE = hedges_g,
                 seTE = g_se,
                 lower = g_lower,
                 upper = g_upper,
                 studlab = Study,
                 data = df_forest_emorecog,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 subgroup = Population_type,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Emotion recognition")
forest_emo<- forest(m.gen_emo,
                 layout ="meta",
                 clip = c(-1.5, 4),
                 xlim = c(-1.5,4),
                 type.subgroup = "square",
                 leftcols = (c("Study","Population","Tool")),
                 leftlabs = (c("Study","Condition", "Test")),
                 rightcols= NULL,
                 col.square = "black", 
                 col.square.lines = "black",
                 col.diamond.random = "gray",
                 col.predict = "black",
                 subgroup.name = c("Population type"),
                 print.Q = TRUE)

forest_emo

update(m.gen_emo, 
      subgroup = Population_type, 
      tau.common = FALSE)


# ToM
df_forest_ToM<- df_forest[df_forest$Tool_type==2,]
m.gen_ToM <- metagen(TE = hedges_g,
                    lower = g_lower,
                    upper = g_upper,
                    seTE = g_se,
                    studlab = Study,
                    data = df_forest_ToM,
                    subgroup = Population_type,
                    sm = "SMD",
                    fixed = FALSE,
                    random = TRUE,
                    method.tau = "REML",
                    hakn = TRUE,
                    title = "Theory of Mind")
forest_ToM<- forest(m.gen_ToM,
                    layout ="meta",
                    leftcols = (c("Study","Population","Tool")),
                    leftlabs = (c("Study","Condition", "Test")),
                    rightcols= NULL,
                    clip = c(-1.5, 4),
                    xlim = c(-1.5,4),
                    type.subgroup = "square",
                    col.square = "black", 
                    col.square.lines = "black",
                    col.diamond.random = "gray",
                    col.predict = "black",
                    subgroup.name = c("Population type"),
                    print.Q = TRUE)

forest_ToM

update(m.gen_ToM, 
            subgroup = Population_type, 
            tau.common = FALSE)


# Batteries
df_forest_batteries<- df_forest[df_forest$Tool_type==3,]
m.gen_bat <- metagen(TE = hedges_g,
                     seTE = g_se,
                     lower = g_lower,
                     upper = g_upper,
                     studlab = Study,
                     data = df_forest_batteries,
                     sm = "SMD",
                     subgroup = Population_type,
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     hakn = TRUE,
                     title = "Batteries")
forest_bat<- forest(m.gen_bat,
                    layout ="meta",
                    leftcols = (c("Study","Population","Tool")),
                    leftlabs = (c("Study","Condition", "Test")),
                    clip = c(-1.5, 4),
                    xlim = c(-1.5,4),
                    type.subgroup = "square",
                    rightcols= NULL,
                    col.square = "black", 
                    col.square.lines = "black",
                    col.diamond.random = "gray",
                    col.predict = "black",
                    subgroup.name = c("Population type"),
                    print.Q = TRUE)

forest_bat

update(m.gen_bat, 
      subgroup = Population_type, 
      tau.common = FALSE)



# Funnel plots ------------------------------------------------------------

col.contour = c("white","orange", "red")

# Generate funnel plot (we do not include study labels here)
res <- rma(hedges_g,g_se, data=df_forest) 


funnel_plot<- funnel(res)
funnel(res, refline=0, level=c(90, 95, 99),
       shade=c("white", "gray55", "gray75"),
       legend=FALSE,
       xlab="Effect size (Hedges' g)", 
       ylab = "Precision (SE)")
title(main="Publication bias: Funnel plot" )


m.gen_overall <- metagen(TE = hedges_g,
                     lower = g_lower,
                     upper = g_upper,
                     seTE = g_se,
                     studlab = Study,
                     data = df_forest,
                     sm = "SMD",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     hakn = TRUE,
                     title = "Overall")
metabias(m.gen_overall, method.bias = "linreg")


# Non-Independence testing ------------------------------------------------


df_forest_emorecog$Study_ID <- as.numeric(factor(df_forest_emorecog$Study))
df_forest_ToM$Study_ID <- as.numeric(factor(df_forest_ToM$Study))
df_forest_batteries$Study_ID <- as.numeric(factor(df_forest_batteries$Study))

# Emotion recognition - three level model (testing if Study_ID is significant)
m3l_emo <- metafor::rma.mv(hedges_g, 
                           V = g_se^2,
                           random = ~ 1 | Study_ID / Tool,
                           data = df_forest_emorecog,
                           method = "REML")
summary(m3l_emo)

# ToM - three level model
m3l_ToM <- metafor::rma.mv(hedges_g,
                           V = g_se^2,
                           random = ~ 1 | Study_ID / Tool,
                           data = df_forest_ToM,
                           method = "REML")
summary(m3l_ToM)

# Batteries - three level model
m3l_bat <- metafor::rma.mv(hedges_g,
                  V = g_se^2,
                  random = ~ 1 | Study_ID / Tool,
                  data = df_forest_batteries,
                  method = "REML")
summary(m3l_bat)

# Comparison two-level vs three-level models (likelihood ratio test)

m2l_emo <- metafor::rma.mv(hedges_g,
                  V = g_se^2,
                  random = ~ 1 | Study_ID,
                  data = df_forest_emorecog,
                  method = "REML")

anova(m2l_emo, m3l_emo) 

# ToM
m2l_ToM <- metafor::rma.mv(hedges_g,
                           V = g_se^2,
                           random = ~ 1 | Study_ID,
                           data = df_forest_ToM,
                           method = "REML")
anova(m2l_ToM, m3l_ToM)

# Batteries
m2l_bat <- metafor::rma.mv(hedges_g,
                           V = g_se^2,
                           random = ~ 1 | Study_ID,
                           data = df_forest_batteries,
                           method = "REML")
anova(m2l_bat, m3l_bat)