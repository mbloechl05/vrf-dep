# ===============================================================
# MIDUS: Data analysis: Descriptive stats
# (contact maria.bloechl@gmail.com in case of questions)
# ==============================================================

# clean work space
rm(list = ls()) 

# load packages 
library(dplyr)
library(Hmisc)
library(VennDiagram)
library(ggplot2)

# load preprocessed data 
load("data/midus/processed/midus_proc_data.RData")


# ---------------------------
# 1) Baseline descriptives 
# ---------------------------

# descriptive stats for continuous variables (binary vars only included to get valid Ns)
describe(data[,c("A1PAGE_M2", "A1SBMI",
                 "A1SS7", "A1PRSEX", "A1PB1", "A1SA9S", "A1SA9X", "A1PA43")])

# descriptive stats for dichotomous variables
tables <- lapply(data[,c("A1SS7", "A1PRSEX", "A1PB1", "A1SA9S", "A1SA9X", "A1PA43", "n_rf")], table)
tables
lapply(tables, prop.table)


# -------------------------------------------------
# 2) Supplement: Zero-order correlation matrices
# -------------------------------------------------

# select variables for corr matrix
vars <- data[,c("A1PAGE_M2", "A1PRSEX", "A1SS7",  "A1PB1", "A1SA9S", "A1SBMI", "A1SA9X", "A1PA43", 
                "w1_deprmean", "w2_deprmean", "w3_deprmean")]  

# create corr matrix (rounded to 2 dec places)
rcorr(as.matrix(vars))



