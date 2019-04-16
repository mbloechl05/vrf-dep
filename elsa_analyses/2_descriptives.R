# ===============================================================
# ELSA: Descriptive stats
# (contact maria.bloechl@gmail.com in case of questions)
# ==============================================================

# clear work space
rm(list = ls()) 

# load packages 
library(psych)
library(Hmisc)
library(dplyr)

# load preprocessed data 
load("data/elsa/processed/elsa_proc_data.RData")


# ---------------------------
# 1) Baseline descriptives
# ---------------------------

# descriptive stats for continuous variables (binary vars only included to get valid Ns)
psych::describe(data[,c("w2_dhager", "w2_bmival", "w2_meansys", "w2_chol_f", "w2_hdl_f", "w2_ldl_f",  
                        "w2_DhSex", "w2_fqethnr", "w0_educ", "hyp", "diab", "w2_HESka", "n_rf")]) 

# descriptive stats for dichotomous variables
tables <- lapply(data[,c("w2_DhSex", "w2_fqethnr", "w0_educ", "hyp", "diab", "w2_HESka", "n_rf")], table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies


# ------------------------------------------------
# 2) Supplement: Zero-order correlation matrices
# ------------------------------------------------

# select variables for corr matrix
vars <- data[,c("w2_dhager","w2_DhSex", "w2_fqethnr", "w0_educ", "hyp",
                "w2_bmival", "diab",  "w2_HESka", "w2_meansys", "w2_chol_f", "w2_hdl_f", "w2_ldl_f", 
                "w2_deprmean", "w3_deprmean", "w4_deprmean", "w5_deprmean", "w6_deprmean", "w7_deprmean")]  

# create corr matrix (rounded to 2 dec places)
rcorr(as.matrix(vars))


