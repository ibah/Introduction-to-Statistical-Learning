# ISLR - Ch.9 - Support Vector Machines

# 337
# 354
# Heart data
# Figure 8.6 shows an example on the Heart data set. These data contain a binary outcome HD for 303 patients who presented with chest pain. An outcome value of Yes indicates the presence of heart disease based on an angiographic test, while No means no heart disease. There are 13 predictors including Age, Sex, Chol (a cholesterol measurement), and other heart and lung function measurements. Cross-validation results in a tree with six terminal nodes.

getwd()
setwd("D:/data/Dropbox/cooperation/_R/Intro_to_Stat_Learning/ISLR-chapters")
dir()
input_dir <- "D:/data/Dropbox/cooperation/_R/Intro_to_Stat_Learning/ISLR-input"
dir(input_dir)
df <- read.csv(paste0(input_dir, '/Heart.csv'))
head(df); tail(df)
df$X <- NULL
summary(df)
