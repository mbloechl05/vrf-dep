# ===============================================================
# Cardiovascular Risk and Trajectories of Depressive Symptoms
# Script 11: Descriptive Statistics
# ==============================================================

# clear work space
rm(list = ls()) 

# load packages 
library(psych)
library(Hmisc)
library(dplyr)
library(car) # for recode()
library(ggplot2)
library(effsize) # for cohen.d()

# load preprocessed data 
load("data/elsa/processed/elsa_proc_data.RData")


# ---------------------------
# 1) Baseline descriptives
# ---------------------------

# descriptive stats for continuous variables (binary vars only included to get 
# valid Ns)
psych::describe(data[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"  , "w2_hypt"  , 
                        "w2_obese"  , "w2_diab"   , "w2_smok"   , "w2_hchol", "w2_cvrisk", 
                        "w2_aff_sum", "w2_som_sum", "w2_dep_sum", "w2_adl"
                        )]) 

# descriptive stats for dichotomous variables
tables <- lapply(data[,c("w2_sex" , "w2_ethn" , "w0_edu"   , "w2_hypt"   , "w2_diab"   , 
                         "w2_smok", "w2_obese", "w2_cvrisk", "w2_aff_sum", "w2_som_sum", 
                         "w2_dep_sum")], 
                 table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies


# ------------------------------------------------
# 2) Zero-order correlation matrices
# ------------------------------------------------

# select variables for corr matrix
vars <- data[,c("w2_dhager","w2_sex", "w2_eth", "w0_edu", "hyp",
                "w2_bmi.b", "diab", "chol", "w2_HESka", "adl", "n_rf",
                "w2_affsum", "w3_affsum", "w4_affsum", "w5_affsum", "w6_affsum", "w7_affsum", 
                "w2_somsum", "w3_somsum", "w4_somsum", "w5_somsum", "w6_somsum", "w7_somsum")]  

# create corr matrix (rounded to 2 dec places)
rcorr(as.matrix(vars))


# --------------------------------
# 3) Check selective drop-out
# --------------------------------

# How many people participated in each wave?
waves <- data[,c("wave2", "wave2n","wave3", "wave4", "wave5", "wave6", "wave7")]
sapply(waves, function(x) table(x))

# How many waves did people participate on average?
data$n_waves <- rowSums(data[,c("wave2", "wave3", "wave4", "wave5", "wave6", "wave7")])
psych::describe(data$n_waves)
tab <- table(data$n_waves)
tab # absolute frequencies
prop.table(tab) # relative frequencies


# 3.1) Longitudinal drop-out
# ----------------------------

# Descriptive stats of people with less than 5 waves
loss <- subset(data, n_waves < 5) 

psych::describe(loss[c("w2_sex", "w2_dhager", "w0_edu", "w2_eth", "w2_HESka", "adl",
                       "w2_affsum", "w2_somsum", "w2_depsum", "n_rf", "w2_bmi.b", "hyp", 
                       "diab", "chol")])

# Descriptive stats of poeple with 5 or more waves
comp <- subset(data, n_waves >= 5) 

psych:: describe(comp[c("w2_sex", "w2_dhager", "w0_edu", "w2_eth", "w2_HESka", "adl",
                        "w2_affsum", "w2_somsum", "w2_depsum", "n_rf", "w2_bmi.b", "hyp", 
                        "diab", "chol")])

# Calculate Cohens ds for comparisons
cohen.d(loss$w2_dhager, comp$w2_dhager, pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_sex,    comp$w2_sex,    pooled = T, paired = F, na.rm = T)
cohen.d(loss$w0_edu,    comp$w0_edu,    pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_eth,    comp$w2_eth,    pooled = T, paired = F, na.rm = T)
cohen.d(loss$adl,       comp$adl,       pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_depsum, comp$w2_depsum, pooled = T, paired = F, na.rm = T)
cohen.d(loss$n_rf,      comp$n_rf,      pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_HESka,  comp$w2_HESka,  pooled = T, paired = F, na.rm = T)
cohen.d(loss$hyp,       comp$hyp,       pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_bmival, comp$w2_bmival, pooled = T, paired = F, na.rm = T)
cohen.d(loss$diab,      comp$diab,      pooled = T, paired = F, na.rm = T)
cohen.d(loss$chol,      comp$chol,      pooled = T, paired = F, na.rm = T)


