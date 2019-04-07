## ===============================================================
## ELSA: Data analysis descriptive stats
## (contact maria.bloechl@uni-leipzig.de in case of questions)
## ==============================================================

# -----------------
# 0) Preparations 
# -----------------

# clean work space
rm(list = ls()) 

# load packages 
library(psych)
library(Hmisc)
library(dplyr)
library(VennDiagram)
library(ggplot2)

# load preprocessed data 
load("data/processed/elsa_proc_data.RData")


# ------------------------------------------
# 1) Baseline descriptives sample (wave 2)
# ------------------------------------------

# cont. vars (age, blood pressure, BMI, cholesterol, hdl)
psych::describe(data[,c("w2_dhager", "w2_bmival", "w2_meansys", "w2_chol_f", "w2_hdl_f", "w2_ldl_f",  
                        "w2_DhSex", "w2_fqethnr", "w0_educ", "hyp", "diab", "w2_HESka", "n_rf")]) #binary vars only included to get valid n

# binary vars (gender, ethnicity, diabetes, current smoking) 
tables <- lapply(data[,c("w2_DhSex", "w2_fqethnr", "w0_educ", "hyp", "diab", "w2_HESka", "n_rf")], table)
tables
lapply(tables, prop.table)


# --------------------------
# 2) Correlation matrices
# --------------------------

# select variables for corr matrix
vars <- data[,c("w2_dhager","w2_DhSex", "w2_fqethnr", "w0_educ", "hyp",
                "w2_bmival", "diab",  "w2_HESka", "w2_meansys", "w2_chol_f", "w2_hdl_f", "w2_ldl_f")]  

# create corr matrix (rounded to 2 dec places)
rcorr(as.matrix(vars))


# -------------------------------------------------
# 3) Co-occurences vascular risk factors (wave 1)
# -------------------------------------------------

# create seperte data frame with risk factor variables
rf  <- data[,c("hyp","diab","w2_HESka", "w2_bmi.b")] # BMI as binary

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
  category = c("Hypertension", "Diabetes", "Current smoking", "BMI > 30"), 
  lty = 3, col = "white", fill = c("#a2d1ec", "#a8bef7", "#aab0ef", "#d7c7f9"), 
  fontfamily = "sans", cat.fontfamily = "sans", alpha=c(0.4, 0.4,0.4,0.4))

