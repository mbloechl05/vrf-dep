## ===============================================================
## MIDUS: Data analysis: Descriptive stats
## (contact maria.bloechl@uni-leipzig.de in case of questions)
## ==============================================================

# -----------------
# 0) Preparations 
# -----------------

# clean work space
rm(list = ls()) 

# load packages 
library(dplyr)
library(Hmisc)
library(VennDiagram)
library(ggplot2)

# load preprocessed data 
setwd("C:/Users/Maria/Desktop/learn/0_PhD/Projects/vascular_depression_trajectories/2_MIDUS")
load("data/processed/midus_proc_data.RData")


# ------------------------------------------
# 1) Baseline descriptives sample (wave 1)
# ------------------------------------------

# conti. vars (Age, BMI, baseline depression)
describe(data[,c("A1PAGE_M2", "A1SBMI",
                 "A1SS7", "A1PRSEX", "A1PB1", "A1SA9S", "A1SA9X", "A1PA43")]) # binary variables only for missingness

# binary variables
tables <- lapply(data[,c("A1SS7", "A1PRSEX", "A1PB1", "A1SA9S", "A1SA9X", "A1PA43", "n_rf")], table)
tables
lapply(tables, prop.table)


# --------------------------
# 2) Correlation matrices
# --------------------------

# select variables for corr matrix
vars <- data[,c("A1PAGE_M2", "A1PRSEX", "A1SS7",  "A1PB1", "A1SA9S", "A1SBMI", "A1SA9X", "A1PA43")]  

# create corr matrix (rounded to 2 dec places)
rcorr(as.matrix(vars))


# -------------------------------------------------
# 3) Co-occurences vascular risk factors (wave 1)
# -------------------------------------------------

# create seperte data frame with risk factor variables
rf <- data[,c("A1SA9X","A1SA9S","A1PA43", "A1SBMI.b")] # put in here variables from data frame (BMI as binary)

## this is a custom function to plot a venn diagram for the vascular risk factors
# create function that allows to analyse co-morbidities
cm <- function(riskfact) {
  names(rf) <- c(1, 2, 3, 4) # 1 = diab, 2 = hyp, 3 = smok, 4 = obes
  for (i in 1:length(riskfact)) {
    rf <- subset(rf, rf[riskfact[i]] == 1)
  }
  nrow(rf)
}

# Venn diagramm co-occurence of risk factors
grid.newpage()
draw.quad.venn(
  cm(1), cm(2), cm(3), cm(4), 
  cm(c(1,2)), cm(c(1,3)), cm(c(1,4)), cm(c(2,3)), cm(c(2,4)), cm(c(3,4)), 
  cm(c(1,2,3)), cm(c(1,2,4)), cm(c(1,3,4)), cm(c(2,3,4)), cm(c(1,2,3,4)), 
  category = c("Diabetes", "Hypertension", "Current smoking", "BMI > 30"), 
  lty = 3, col = "white", fill = c("#AC95BF", "#AC95BF", "#AC95BF", "#AC95BF"), 
  fontfamily = "sans", cat.fontfamily = "sans", alpha=c(0.4, 0.4,0.4,0.4))

#  c("#a2d1ec", "#a8bef7", "#aab0ef", "#d7c7f9"), 


