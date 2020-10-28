# ==========================================================
# Age, Vascular Risk Factors and Depressed Mood
# Script 1: Descriptive Statistics
# ==========================================================

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
psych::describe(data[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"    , "w2_hypt"   , 
                        "w2_obese"  , "w2_diab"   , "w2_smok"   , "w2_hchol"  , "w2_cvrisk" , 
                        "w2_aff_sum", "w3_aff_sum", "w4_aff_sum", "w5_aff_sum", "w6_aff_sum",
                        "w7_aff_sum", "w2_adl"    , "w2_cvrisk")]) 

# descriptive stats for dichotomous variables
tables <- lapply(data[,c("w2_sex" , "w2_eth"  , "w0_edu"   , "w2_hypt"  , "w2_diab"   , 
                         "w2_smok", "w2_obese", "w2_hchol" , "w2_cvrisk")], table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies

# absolute and relative frequency of depressiove symptoms >2
clin_dep <- cumsum(tables$w2_dep_sum[4:9])
clin_dep

100/nrow(data)*clin_dep[6]


# ------------------------------------------------
# 2) Zero-order correlation matrices
# ------------------------------------------------

# select variables for corr matrix
vars <- data[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"    , "w2_hypt", 
                "w2_obese"  , "w2_diab"   , "w2_smok"   , "w2_hchol"  , "w2_cvrisk" , "w2_adl",
                "w2_aff_sum", "w3_aff_sum", "w4_aff_sum", "w5_aff_sum", "w6_aff_sum", "w7_aff_sum")]  

# create corr matrix (rounded to 2 dec places)
zo_corr <- rcorr(as.matrix(vars))
zo_corr

# cor between age and vrf
cor.test(data$w2_cvrisk, data$w2_age)


# ---------------------------
# 3) Check attrition rates
# ---------------------------

# 3.1) How many people participated in each wave?
# -------------------------------------------------

# select all waves variables
waves <- data[,c("wave_2_c", "wave_2_n","wave_3_c", "wave_4_c", "wave_5_c", 
                 "wave_6_c", "wave_7_c")]

# get info about n per wave
sapply(waves, function(x) table(x))


# 3.2) How many waves did people participate on average?
# --------------------------------------------------------

# create variable indicating number of waves people participated in 
data$n_waves <- rowSums(data[,c("wave_2_c", "wave_3_c", "wave_4_c", "wave_5_c", 
                                "wave_6_c", "wave_7_c")])

# get mean and sd of number of waves
psych::describe(data$n_waves)

# get asolute and relative frquencies for each wave
tab <- table(data$n_waves)
tab # absolute frequencies
prop.table(tab) # relative frequencies


# 3.3) Was drop-out selective?
# -------------------------------

# descriptive stats of people with less than 5 waves
loss <- subset(data, n_waves < 5) 

psych::describe(loss[c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"  , "w2_hypt"  , 
                       "w2_obese"  , "w2_diab"   , "w2_smok"   , "w2_hchol", "w2_cvrisk", 
                       "w2_aff_sum", "w2_dep_sum", "w2_adl")])

# Descriptive stats of poeple with 5 or more waves
comp <- subset(data, n_waves >= 5) 

psych:: describe(comp[c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"  , "w2_hypt"  , 
                        "w2_obese"  , "w2_diab"   , "w2_smok"   , "w2_hchol", "w2_cvrisk", 
                        "w2_aff_sum", "w2_dep_sum", "w2_adl")])

# Calculate Cohens ds for comparisons
cohen.d(loss$w2_age,     comp$w2_age,     pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_sex,     comp$w2_sex,     pooled = T, paired = F, na.rm = T)
cohen.d(loss$w0_edu,     comp$w0_edu,     pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_eth,     comp$w2_eth,     pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_smok,    comp$w2_smok,    pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_hypt,    comp$w2_hypt,    pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_obese,   comp$w2_obese,   pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_diab,    comp$diab,       pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_hchol,   comp$hchol,      pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_cvrisk,  comp$w2_cvrisk,  pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_dep_sum, comp$w2_dep_sum, pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_adl,     comp$w2_adl,     pooled = T, paired = F, na.rm = T)


