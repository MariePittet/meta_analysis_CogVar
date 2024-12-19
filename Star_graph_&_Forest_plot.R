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


# Forest plot -------------------------------------------------------------

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
                 leftlabs = (c("Study","Condition", "Test", "Condition vs controls")),
                 rightcols= NULL,
                 col.square = "black", 
                 col.square.lines = "black",
                 col.diamond.random = "gray",
                 col.predict = "black",
                 subgroup.name = c("Population type"))

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
                    leftlabs = (c("Study","Condition", "Test", "Condition vs controls")),
                    rightcols= NULL,
                    clip = c(-1.5, 4),
                    xlim = c(-1.5,4),
                    type.subgroup = "square",
                    col.square = "black", 
                    col.square.lines = "black",
                    col.diamond.random = "gray",
                    col.predict = "black",
                    subgroup.name = c("Population type"))

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
                    subgroup.name = c("Population type"))

forest_bat

update(m.gen_bat, 
      subgroup = Population_type, 
      tau.common = FALSE)



# Funnel plots (to investigate publication bias)
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
legend(x = "topright", inset = c(0.1, 0.2),
       legend = c("p > .10", "p < .10", "p  < .05", "p < .01", "Studies"))


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


# Horizontal bar graph ----------------------------------------------------
df_bar<- read.csv2("studies_list_validation_status.csv", header = T, sep=",")
df_bar[55,1]<- "oMDD"
df_bar[56,1]<- "yMDD"

# for emotion recognition tests 
df_bar_emo<- df_bar[df_bar$Tool_type == 1,]
df_bar_emo <- df_bar_emo[order(df_bar_emo$Population_type,-df_bar_emo$Criteria.fulfilled),]

df1<- data.frame(Population = df_bar_emo$Population, Population_type=df_bar_emo$Population_type, x = df_bar_emo$Tool, y = df_bar_emo$Criteria.tested)
df2<- data.frame(Population = df_bar_emo$Population, Population_type=df_bar_emo$Population_type, x = df_bar_emo$Tool, y = df_bar_emo$Criteria.fulfilled)

df1$Population_type<- factor(df1$Population_type, levels = c(1,2,3,4,5,6),labels = c("Schizophrenia", "Autism", "Mood disorders", "Neurodegenerative diseases", "Acquired brain injury", "Mixed populations"))
df2$Population_type<- factor(df2$Population_type, levels = c(1,2,3,4,5,6),labels = c("Schizophrenia", "Autism", "Mood disorders", "Neurodegenerative diseases", "Acquired brain injury", "Mixed populations"))
df1$x<- as.factor(df1$x)
df2$x<- as.factor(df2$x)
df1$y<- as.factor(df1$y)
df2$y<- as.factor(df2$y)
df1$Population<- as.factor(df1$Population)
df2$Population<- as.factor(df2$Population)

my_colors<- c(brewer.pal(6, "Set2")[c(1,2,4,5)]) # specifyng colors manually to match other graphs

p1 <-  ggplot(df2,aes(fct_rev(fct_inorder(interaction(factor(x), Population))),y, fill = Population_type)) +
  geom_bar(stat="identity", position=position_dodge(width = 1), alpha=1) +
  scale_fill_manual(values=my_colors) +
  scale_x_discrete(labels = rev(df2$x)) +
  coord_flip() + 
  labs(title = "Validation status of emotion recognition tasks",
       y = "Number of criteria tested (gray) / fulfilled (colored)",
       x = "Test",
       fill = "Population type") + 
  geom_bar(data = df1, stat="identity", position=position_dodge(width= 1), alpha=0.3, fill = "gray") +
  theme_classic2() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y =element_text(size=12,hjust=1, vjust = 0.5, angle = 0)) +
  theme(plot.title =element_text(size=14,hjust=0.5, vjust = 0.5, angle = 0)) +
  theme(legend.position = "None")+
  geom_text(aes(label = Population),hjust=1.2, vjust=0.5, fontface="italic", colour = "white")
p1

# for ToM tasks
df_bar_ToM<- df_bar[df_bar$Tool_type == 2,]
df_bar_ToM <- df_bar_ToM[order(df_bar_ToM$Population_type,-df_bar_ToM$Criteria.fulfilled),]

df1<- data.frame(Population = df_bar_ToM$Population, Population_type=df_bar_ToM$Population_type, x = df_bar_ToM$Tool, y = df_bar_ToM$Criteria.tested)
df2<- data.frame(Population = df_bar_ToM$Population, Population_type=df_bar_ToM$Population_type, x = df_bar_ToM$Tool, y = df_bar_ToM$Criteria.fulfilled)

df1$Population_type<- factor(df1$Population_type, levels = c(1,2,3,4,5,6),labels = c("Schizophrenia", "Autism", "Mood disorders", "Neurodegenerative diseases", "Acquired brain injury", "Mixed populations"))
df2$Population_type<- factor(df2$Population_type, levels = c(1,2,3,4,5,6),labels = c("Schizophrenia", "Autism", "Mood disorders", "Neurodegenerative diseases", "Acquired brain injury", "Mixed populations"))
df1$x<- as.factor(df1$x)
df2$x<- as.factor(df2$x)
df1$Population<- as.factor(df1$Population)
df2$Population<- as.factor(df2$Population)

p2 <-  ggplot(df2,aes(fct_rev(fct_inorder(interaction(x, Population))),y, fill = Population_type)) +
  geom_bar(stat="identity", position=position_dodge(width = 1), alpha=1) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = rev(df2$x)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_flip() + 
  labs(title = "Validation status of ToM tasks",
       y = "Number of criteria tested (gray) / fulfilled (colored)",
       x = "Test",
       fill = "Population type") + 
  geom_bar(data = df1, stat="identity", position=position_dodge(width= 1), alpha=0.3, fill = "gray") +
  theme_classic2() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y =element_text(size=12,hjust=1, vjust = 0.5, angle = 0)) +
  theme(plot.title =element_text(size=14,hjust=0.5, vjust = 0.5, angle = 0)) +
  geom_text(aes(label = Population),hjust=1.2, vjust=0.5, size= 4, fontface="italic", colour = "white")
p2


# for batteries
df_bar_bat<- df_bar[df_bar$Tool_type == 3,]
df_bar_bat <- df_bar_bat[order(df_bar_bat$Population_type,-df_bar_bat$Criteria.fulfilled),]

df1<- data.frame(Population = df_bar_bat$Population, Population_type=df_bar_bat$Population_type, x = df_bar_bat$Tool, y = df_bar_bat$Criteria.tested)
df2<- data.frame(Population = df_bar_bat$Population, Population_type=df_bar_bat$Population_type, x = df_bar_bat$Tool, y = df_bar_bat$Criteria.fulfilled)

df1$Population_type<- factor(df1$Population_type, levels = c(1,2,3,4,5,6),labels = c("Schizophrenia", "Autism", "Mood disorders", "Neurodegenerative diseases", "Acquired brain injury", "Mixed populations"))
df2$Population_type<- factor(df2$Population_type, levels = c(1,2,3,4,5,6),labels = c("Schizophrenia", "Autism", "Mood disorders", "Neurodegenerative diseases", "Acquired brain injury", "Mixed populations"))
df1$x<- as.factor(df1$x)
df2$x<- as.factor(df2$x)
df1$Population<- as.factor(df1$Population)
df2$Population<- as.factor(df2$Population)

p3 <-  ggplot(df2,aes(fct_rev(fct_inorder(interaction(x, Population))),y, fill = Population_type)) +
  geom_bar(stat="identity", position=position_dodge(width = 1), alpha=1) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_discrete(labels = rev(df2$x)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_flip() + 
  labs(title = "Validation status of batteries",
       y = "Number of criteria tested (gray) / fulfilled (colored)",
       x = "Test",
       fill = "Population type") + 
  geom_bar(data = df1, stat="identity", position=position_dodge(width= 1), alpha=0.3, fill = "gray") +
  theme_classic2() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y =element_text(size=12,hjust=1, vjust = 0.5, angle = 0)) +
  theme(plot.title =element_text(size=14,hjust=0.5, vjust = 0.5, angle = 0)) +
  geom_text(aes(label = Population),hjust=1.2, vjust=0.5, size= 4, fontface="italic", colour = "white")
p3

# Table --------------------------------------------------------------

# Arranging data frames 
df_stars_emo<- df[!is.na(df$Stars), ]
df_stars_emo<- df_stars_emo[df_stars_emo$Tool_type==1,]
df_stars_emo<- df_stars_emo[order(-df_stars_emo$Stars,df_stars_emo$Population_type),c(14,4,2,1)]
names(df_stars_emo)[names(df_stars_emo) == "Stars"] <- "Validation"

df_stars_ToM<- df[!is.na(df$Stars), ]
df_stars_ToM<- df_stars_ToM[df_stars_ToM$Tool_type==2,]
df_stars_ToM<- df_stars_ToM[order(-df_stars_ToM$Stars,df_stars_ToM$Population_type),c(14,4,2,1)]
names(df_stars_ToM)[names(df_stars_ToM) == "Stars"] <- "Validation"

df_stars_bat<- df[!is.na(df$Stars), ]
df_stars_bat<- df_stars_bat[df_stars_bat$Tool_type==3,]
df_stars_bat<- df_stars_bat[order(-df_stars_bat$Stars,df_stars_bat$Population_type),c(14,4,2,1)]
names(df_stars_bat)[names(df_stars_bat) == "Stars"] <- "Validation"


# Setting up visualization themes
tm1<- ttheme(base_size=8,
             padding=unit(c(1.5, 1.5), "mm"))
my_palette<- terrain.colors(8)
my_palette<- my_palette[2:8]

# Setting up a common interpretation key
lgnd = data.frame(  
  "Sum of validation criteria" = c("5", "4", "3", "2","1", "0"),  
  "Validation status" = c("Strong", "Good", "Fair", "Questionable", "Insufficient", "Absent"),
  check.names = FALSE)

lgnd_table<- ggtexttable(lgnd, theme = ttheme("blank", padding=unit(c(1.5, 1.5), "mm"),base_size=8),rows= NULL)
lgnd_table <- table_cell_bg(lgnd_table, row = 2, column = 1, fill = my_palette[1], alpha = 0.3, col="white", linewidth = 2)
lgnd_table <- table_cell_bg(lgnd_table, row = 3, column = 1, fill = my_palette[2], alpha = 0.3, col="white", linewidth = 2)
lgnd_table <- table_cell_bg(lgnd_table, row = 4, column = 1, fill = my_palette[3], alpha = 0.3, col="white", linewidth = 2)
lgnd_table <- table_cell_bg(lgnd_table, row = 5, column = 1, fill = my_palette[4], alpha = 0.3, col="white", linewidth = 2)
lgnd_table <- table_cell_bg(lgnd_table, row = 6, column = 1, fill = my_palette[5], alpha = 0.3, col="white", linewidth = 2)
lgnd_table <- table_cell_bg(lgnd_table, row = 7, column = 1, fill = my_palette[6], alpha = 0.3, col="white", linewidth = 2)

# Visualization: emotion recognition tasks

p_emo<-  ggtexttable(df_stars_emo, theme = tm1, rows=NULL)

p_emo <- table_cell_bg(p_emo, row = 2:5, column = 1, fill = my_palette[2], alpha = 0.3, col="white", linewidth = 2)
p_emo <- table_cell_bg(p_emo, row = 6:12, column = 1, fill = my_palette[3], alpha = 0.3, col="white", linewidth = 2)
p_emo <- table_cell_bg(p_emo, row = 13:18, column = 1, fill = my_palette[4], alpha = 0.3, col="white", linewidth = 2)
p_emo <- table_cell_bg(p_emo, row = 19:22, column = 1, fill = my_palette[5], alpha = 0.3, col="white", linewidth = 2)


# Visualization: ToM tasks

p_ToM<-  ggtexttable(df_stars_ToM, theme = tm1, rows=NULL)

p_ToM <- table_cell_bg(p_ToM, row = 2:6, column = 1, fill = my_palette[1], alpha = 0.3, col="white", linewidth = 2)
p_ToM <- table_cell_bg(p_ToM, row = 7:15, column = 1, fill = my_palette[2], alpha = 0.3, col="white", linewidth = 2)
p_ToM <- table_cell_bg(p_ToM, row = 16:24, column = 1, fill = my_palette[3], alpha = 0.3, col="white", linewidth = 2)
p_ToM <- table_cell_bg(p_ToM, row = 25:33, column = 1, fill = my_palette[4], alpha = 0.3, col="white", linewidth = 2)
p_ToM <- table_cell_bg(p_ToM, row = 34:41, column = 1, fill = my_palette[5], alpha = 0.3, col="white", linewidth = 2)
p_ToM <- table_cell_bg(p_ToM, row = 42, column = 1, fill = my_palette[6], alpha = 0.3, col="white", linewidth = 2)


# Visualization: batteries

p_bat<-  ggtexttable(df_stars_bat, theme = tm1, rows=NULL)

p_bat <- table_cell_bg(p_bat, row = 2, column = 1, fill = my_palette[1], alpha = 0.3, col="white", linewidth = 2)
p_bat <- table_cell_bg(p_bat, row = 3:4, column = 1, fill = my_palette[2], alpha = 0.3, col="white", linewidth = 2)
p_bat <- table_cell_bg(p_bat, row = 5:10, column = 1, fill = my_palette[3], alpha = 0.3, col="white", linewidth = 2)
p_bat <- table_cell_bg(p_bat, row = 11:16, column = 1, fill = my_palette[4], alpha = 0.3, col="white", linewidth = 2)
p_bat <- table_cell_bg(p_bat, row = 17:19, column = 1, fill = my_palette[5], alpha = 0.3, col="white", linewidth = 2)

#Arranging the graphs
valid_graph<- ggarrange(p_emo,
                        p_ToM,
                        p_bat,
                        #hjust = -1, vjust = 1.5,
                        ncol = 2, nrow = 2)
valid_graph


# New graph ---------------------------------------------------------------
df<- read.csv2("Studies_list_validation_status2.csv", header = T, sep=",")
df<- df[,2:12]

# Computing psychometric criteria tested
Content_validity_t <- ifelse(!is.na(df$Content.Validity),1,0)
Convergent_validity_t<- ifelse(!is.na(df$Convergent.validity),1,0)
Knowngroup_validity_t<-ifelse(!is.na(df$Known.group.validity),1,0)
SensSpecAUC_t<- ifelse(!is.na(df$Sens.Spec.AUC),1,0)
Testretest_reliability_t<- ifelse(!is.na(df$Test.retest.reliability),1,0)
Interrater_reliability_t<- ifelse(!is.na(df$Inter.rater.reliability),1,0)
Internal_consistency_t<- ifelse(!is.na(df$Internal.consistency),1,0)

# Computing psychometric variables fulfilled
Content_validity_f <- ifelse(is.na(df$Content.Validity),0,df$Content.Validity)
Convergent_validity_f<- ifelse(is.na(df$Convergent.validity),0,df$Convergent.validity)
Knowngroup_validity_f<-ifelse(is.na(df$Known.group.validity),0,df$Known.group.validity)
SensSpecAUC_f<- ifelse(is.na(df$Sens.Spec.AUC),0,df$Sens.Spec.AUC)
Testretest_reliability_f<- ifelse(is.na(df$Test.retest.reliability),0,df$Test.retest.reliability)
Interrater_reliability_f<- ifelse(is.na(df$Inter.rater.reliability),0,df$Inter.rater.reliability)
Internal_consistency_f<- ifelse(is.na(df$Internal.consistency),0,df$Internal.consistency)

df<- data.frame(df, Content_validity_t,Content_validity_f, Convergent_validity_t,Convergent_validity_f,
                Knowngroup_validity_t,Knowngroup_validity_f, SensSpecAUC_t,SensSpecAUC_f,Testretest_reliability_t,
                Testretest_reliability_f,Interrater_reliability_t,Interrater_reliability_f,Internal_consistency_t,
                Internal_consistency_f)

# Summing criteria for same test in same group
library(tidyverse)

df$Population<- as.factor(df$Population)
df$Population_type<- factor(df$Population_type, levels = c(1:6), labels = c("SZ", "ASD", "Mood and Anxiety", "Neurodegenerative", "ABI", "Others"))
df$Tool_type<- factor(df$Tool_type, levels = c(1,2,3), labels = c("emotion recognition", "ToM", "Batteries"))

df1<- df %>%
  group_by(Tool,Population, Tool_type) %>%
  summarise(across(where(is.numeric), sum))

df2<- df1[,c(1:3,11:24)]

# Reformatting for plot
df3<- df2[rep(seq_len(nrow(df2)), each = 7),1:3]
Psych_crit<- rep(c(1:7),74)
df3<- data.frame(df3, Psych_crit)

df2_t<- df2[,c(4,6,8,10,12,14,16)]
df2_f<- df2[,c(5,7,9,11,13,15,17)]

values<- c()
for (i in 1:nrow(df2_t)){
  new_values<- c(unlist(df2_t[i,], use.names= FALSE))
  values<- append(values, new_values)
}

df3_t<- data.frame(df3,values)

values<- c()
for (i in 1:nrow(df2_f)){
  new_values<- c(unlist(df2_f[i,], use.names= FALSE))
  values<- append(values, new_values)
}

df3_f<- data.frame(df3,values)

df3_t_emo<- df3_t[df3_t$Tool_type=="emotion recognition",]
df3_f_emo<- df3_f[df3_f$Tool_type=="emotion recognition",]
df3_t_emo$Tool<- as.factor(df3_t_emo$Tool)
df3_t_emo$Psych_crit<- as.factor(df3_t_emo$Psych_crit)
df3_f_emo$Tool<- as.factor(df3_f_emo$Tool)
df3_f_emo$Psych_crit<- as.factor(df3_f_emo$Psych_crit)

# Plotting 
ggplot(df3_f_emo,aes(fct_rev(fct_inorder(interaction(Tool, Population))),values, fill = fct_rev(Psych_crit))) +
  geom_bar(stat="identity", position=position_stack(vjust = 1), alpha=1) +
  scale_fill_brewer(palette = "Set2") +
  #scale_x_discrete(labels = rev(df2$x)) +
  scale_y_continuous(expand=c(0,0)) +
  coord_flip() + 
  labs(title = "Validation status of emotion recognition tests",
       y = "Number of criteria tested (gray) / fulfilled (colored)",
       x = "Test",
       fill = "Psychometric criteria") + 
  #geom_bar(data = df3_t_emo, stat="identity", position = position_nudge(x=0,y=0), alpha=0.3, fill= "gray") +
  theme_classic2() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "right") +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y =element_text(size=12,hjust=1, vjust = 0.5, angle = 0)) +
  theme(plot.title =element_text(size=14,hjust=0.5, vjust = 0.5, angle = 0)) +
  geom_text(aes(label = Population),hjust=1.2, vjust=0.5, size= 4, fontface="italic", colour = "white")