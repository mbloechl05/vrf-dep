# ==========================================================
# Age, Vascular Risk Factors and Depressed Mood
# Script 1: Descriptive Statistics
# ==========================================================

# clear work space
rm(list = ls()) 

# load packages 
library(psych)
library(segmented)
library(Hmisc)
library(dplyr)
library(ggplot2)
library(effsize) # for cohen.d()

# load preprocessed data 
load("data/elsa/processed/elsa_proc_data.RData")


# ---------------------------
# 1) Baseline descriptives
# ---------------------------

# 1.1) Total sample
# ----------------------

# descriptive stats for continuous variables (binary vars only included to get 
# valid Ns)
psych::describe(data[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"    , 
                        "w2_hypt"   , "w2_obese"  , "w2_diab"   , "w2_smok"   , 
                        "w2_hchol"  , "w2_cvrisk" , "w2_aff_sum", "w3_aff_sum", 
                        "w4_aff_sum", "w5_aff_sum", "w6_aff_sum", "w7_aff_sum", 
                        "w2_adl")]) 

# descriptive stats for dichotomous variables
tables <- lapply(data[,c("w2_sex" , "w2_eth"  , "w0_edu"  , "w2_hypt"  , 
                         "w2_diab", "w2_smok" , "w2_obese", "w2_hchol" , 
                         "w2_cvrisk", "w2_aff_sum")], table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies


# 1.2) Per age group 
# --------------------

data$w2_age_gr <- case_when((data$w2_age <  60)                      ~ 1, 
                            (data$w2_age >= 60) & (data$w2_age < 70) ~ 2, 
                            (data$w2_age >= 70) & (data$w2_age < 80) ~ 3,
                            (data$w2_age >= 80)                      ~ 4)


# 50-59 years 
# -------------

data_50s <- subset(data, data$w2_age_gr == 1)

psych::describe(data_50s[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"    , 
                            "w2_hypt"   , "w2_obese"  , "w2_diab"   , "w2_smok"   , 
                            "w2_hchol"  , "w2_cvrisk" , "w2_aff_sum", "w3_aff_sum", 
                            "w4_aff_sum", "w5_aff_sum", "w6_aff_sum", "w7_aff_sum", 
                            "w2_adl")]) 

tables <- lapply(data_50s[,c("w2_sex" , "w2_eth"  , "w0_edu"  , "w2_hypt"  , 
                             "w2_diab", "w2_smok" , "w2_obese", "w2_hchol" , 
                             "w2_cvrisk")], table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies


# 60-69 years
# ------------

data_60s <- subset(data, data$w2_age_gr == 2)

psych::describe(data_60s[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"    , 
                            "w2_hypt"   , "w2_obese"  , "w2_diab"   , "w2_smok"   , 
                            "w2_hchol"  , "w2_cvrisk" , "w2_aff_sum", "w3_aff_sum", 
                            "w4_aff_sum", "w5_aff_sum", "w6_aff_sum", "w7_aff_sum", 
                            "w2_adl")]) 

tables <- lapply(data_60s[,c("w2_sex" , "w2_eth"  , "w0_edu"  , "w2_hypt"  , 
                             "w2_diab", "w2_smok" , "w2_obese", "w2_hchol" , 
                             "w2_cvrisk")], table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies


# 70-79 years
# ------------

data_70s <- subset(data, data$w2_age_gr == 3)

psych::describe(data_70s[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"    , 
                            "w2_hypt"   , "w2_obese"  , "w2_diab"   , "w2_smok"   , 
                            "w2_hchol"  , "w2_cvrisk" , "w2_aff_sum", "w3_aff_sum", 
                            "w4_aff_sum", "w5_aff_sum", "w6_aff_sum", "w7_aff_sum", 
                            "w2_adl")]) 

tables <- lapply(data_70s[,c("w2_sex" , "w2_eth"  , "w0_edu"  , "w2_hypt"  , 
                             "w2_diab", "w2_smok" , "w2_obese", "w2_hchol" , 
                             "w2_cvrisk")], table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies


# > 80 years
# ------------

data_80s <- subset(data, data$w2_age_gr == 4)

psych::describe(data_80s[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"    , 
                            "w2_hypt"   , "w2_obese"  , "w2_diab"   , "w2_smok"   , 
                            "w2_hchol"  , "w2_cvrisk" , "w2_aff_sum", "w3_aff_sum", 
                            "w4_aff_sum", "w5_aff_sum", "w6_aff_sum", "w7_aff_sum", 
                            "w2_adl")]) 

tables <- lapply(data_80s[,c("w2_sex" , "w2_eth"  , "w0_edu"  , "w2_hypt"  , 
                             "w2_diab", "w2_smok" , "w2_obese", "w2_hchol" , 
                             "w2_cvrisk")], table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies


# ------------------------------------------------
# 2) Zero-order correlation matrices
# ------------------------------------------------

# select variables for corr matrix
vars <- data[,c("w2_age"    , "w2_sex"    , "w2_eth"    , "w0_edu"    , 
                "w2_hypt"   , "w2_obese"  , "w2_diab"   , "w2_smok"   , 
                "w2_hchol"  , "w2_cvrisk" , "w2_adl"    , "w2_aff_sum", 
                "w3_aff_sum", "w4_aff_sum", "w5_aff_sum", "w6_aff_sum", 
                "w7_aff_sum")]  

# create corr matrix (rounded to 2 dec places)
zo_corr <- rcorr(as.matrix(vars))
zo_corr

# cor between age and vrf
cor.test(data$w2_cvrisk, data$w2_age)

# linear regression 
fit.lin <- lm(w2_cvrisk ~ w2_age, data = data)
summary(fit.lin)

# spline regression 
fit.seg <- segmented(fit.lin, seg.Z = ~ w2_age, psi = list(w2_age = 75))
summary(fit.seg)

# plot 
ggplot(data, aes(x = w2_age, y = w2_cvrisk)) + 
  geom_jitter(colour = "grey80", alpha = 0.5, width = 0.5) +
  geom_smooth(colour = "plum4", fill = "plum4") + 
  scale_x_continuous(name = "Age (in years)") + 
  scale_y_continuous(name = "Vascular risk factor burden", 
                     breaks = c(0,1,2,3,4,5)) +
  theme_tufte(base_size = 14) + 
  theme(axis.text  = element_text(colour = "black", size = 15), 
        axis.title = element_text(colour = "black", size = 15), 
        axis.line.x = element_line(color="black"), 
        axis.line.y = element_line(color="black"))


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

psych::describe(loss[c("w2_age"  , "w2_sex"   , "w2_eth"    , "w0_edu"    ,
                       "w2_hypt" , "w2_obese" , "w2_diab"   , "w2_smok"   , 
                       "w2_hchol", "w2_cvrisk", "w2_aff_sum", "w2_dep_sum", 
                       "w2_adl")])

# Descriptive stats of poeple with 5 or more waves
comp <- subset(data, n_waves >= 5) 

psych:: describe(comp[c("w2_age"  , "w2_sex"   , "w2_eth"    , "w0_edu"    ,
                        "w2_hypt" , "w2_obese" , "w2_diab"   , "w2_smok"   , 
                        "w2_hchol", "w2_cvrisk", "w2_aff_sum", "w2_dep_sum", 
                        "w2_adl")])

# Calculate Cohens ds for comparisons
cohen.d(loss$w2_age,     comp$w2_age,     pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_sex,     comp$w2_sex,     pooled = T, paired = F, na.rm = T)
cohen.d(loss$w0_edu,     comp$w0_edu,     pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_eth,     comp$w2_eth,     pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_cvrisk,  comp$w2_cvrisk,  pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_dep_sum, comp$w2_dep_sum, pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_adl,     comp$w2_adl,     pooled = T, paired = F, na.rm = T)


