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
psych::describe(data[,c("w2_dhager", "w2_bmival", "w2_meansys", "w2_meandia", 
                        "w2_fglu_f", "w2_sex", "w2_fqethnr", "w0_edu", "hyp", 
                        "diab", "chol", "w2_HESka", "n_rf")]) 

# descriptive stats for dichotomous variables
tables <- lapply(data[,c("w2_sex", "w2_fqethnr", "w0_edu", "hyp", "diab", 
                         "w2_HESka", "n_rf")], table)
tables # absolute frequencies
lapply(tables, prop.table) # relative frequencies


# ------------------------------------------------
# 2) Supplement: Zero-order correlation matrices
# ------------------------------------------------

# select variables for corr matrix
vars <- data[,c("w2_dhager","w2_sex", "w2_eth", "w0_edu", "hyp",
                "w2_bmival", "diab", "chol", "w2_HESka", "w2_deprmean", "w3_deprmean", 
                "w4_deprmean", "w5_deprmean", "w6_deprmean", "w7_deprmean")]  

# create corr matrix (rounded to 2 dec places)
rcorr(as.matrix(vars))


# -------------------------
# 2) Descriptives by sex
# -------------------------

# 2.1) Descriptive statistics
# -----------------------------

describeBy(data[,c("w2_dhager", "w0_edu", "w2_eth", 
                    "hyp", "diab", "chol", "w2_HESka", "w2_bmival", "n_rf", 
                    "w2_bmi.b", "w2_meansys", "w2_meandia",    
                    "w2_deprmean", "w3_deprmean", "w4_deprmean", 
                    "w5_deprmean", "w6_deprmean", "w7_deprmean")], 
            group = data$w2_sex) # 0 = female


# 2.2) Plot number of risk factors
# ----------------------------------

# first some data wrangling
table_nrf <- table(data$n_rf, data$w2_sex)
data_nrf  <- as.data.frame(prop.table(table_nrf, 2))
names(data_nrf) <- c("n_rf", "sex", "freq")

# finally some plot
ggplot(data = data_nrf, aes(x = n_rf, y = freq, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge())

plot(data$n_rf, data$w2_dhager)
cor.test(data$n_rf, data$w2_dhager)


# 2.3) Plot single risk factors
# -------------------------------

# first some data wrangling
srf_desc <- describeBy(data[,c("hyp", "diab", "chol", "w2_HESka", "w2_bmi.b")], 
                       group = data$w2_sex) # 0 = female
freq <- c(srf_desc$`0`$mean, srf_desc$`1`$mean)
srf  <- c(srf_desc$`0`$vars, srf_desc$`1`$vars) # 1=hypt, 2=diab, 3=chol, 4=smok, 5=obes
sex  <- c(0,0,0,0,0,1,1,1,1,1)
data_srf <- as.data.frame(cbind(freq, srf, sex))
data_srf$srf <- car::recode(data_srf$srf, "1='Hypertension'; 2='Diabetes'; 
                                           3='Hypercholesterimia';
                                           4='Smoking'; 5='Obesity'")

# finally some plot
ggplot(data_srf, aes(freq, srf)) +
  geom_point(aes(color = sex))


# --------------------------------
# 3) Check selective drop-out
# --------------------------------

# First set missing values indicating that participants did not participate in 
# a specific wave to 0
data$wave0[is.na(data$wave0)] <- 0
data$wave2[is.na(data$wave2)] <- 0
data$wave3[is.na(data$wave3)] <- 0
data$wave4[is.na(data$wave4)] <- 0
data$wave5[is.na(data$wave5)] <- 0
data$wave6[is.na(data$wave6)] <- 0
data$wave7[is.na(data$wave7)] <- 0

# Do the same for nurse data
data$wave2n[is.na(data$wave2n)] <- 0

# How many people participated in each wave?
waves <- data[,c("wave2", "wave2n","wave3", "wave4", "wave5", "wave6", "wave7")]
sapply(waves, function(x) table(x))

# How many waves did people participate on average?
data$n_waves <- rowSums(data[,c("wave2", "wave3", "wave4", "wave5", "wave6", "wave7")])
psych::describe(data$n_waves)
table(data$n_waves)


# 3.1) Nurse visit drop-out
# ----------------------------

# Descriptive stats of poeple with nurse visit at wave 2
n2_yes <- subset(data, wave2n == 1) 

psych::describe(n2_yes[c("w2_sex", "w2_dhager", "w0_educ", "w2_eth", 
                         "w2_deprmean", "w2_HESka")])

# Descriptive stats of poeple with nurse visit at wave 2
n2_no <- subset(data, wave2n == 0) 

psych::describe(n2_no[c("w2_sex", "w2_dhager", "w0_educ", "w2_eth", 
                        "w2_deprmean", "w2_HESka")])

# Calculate Cohens d for comparisons
cohen.d(n2_yes$w2_dhager,   n2_no$w2_dhager,   pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$w2_sex,      n2_no$w2_sex,      pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$w0_edu,      n2_no$w0_edu,      pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$w2_eth,      n2_no$w2_eth,      pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$w2_deprmean, n2_no$w2_deprmean, pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$w2_HESka,    n2_no$w2_HESka,    pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$hyp,         n2_no$hyp,         pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$diab,        n2_no$diab,        pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$chol,        n2_no$chol,        pooled = T, paired = F, na.rm = T)
cohen.d(n2_yes$n_rf,        n2_no$n_rf,        pooled = T, paired = F, na.rm = T)


# 3.2) Longitudinal drop-out
# ----------------------------

# Descriptive stats of people with less than 4 waves
loss <- subset(data, n_waves < 3) 

psych::describe(loss[c("w2_sex", "w2_dhager", "w2_eth", "w0_edu",
                       "w2_deprmean", "w2_HESka", "w2_bmival", "hyp", "diab", "chol")])

# Descriptive stats of poeple with complete follow-up
comp <- subset(data, n_waves >= 3) 

psych:: describe(comp[c("w2_sex", "w2_dhager", "w2_eth", "w0_edu", 
                        "w2_deprmean", "w2_HESka", "w2_bmival", "hyp", "diab", "chol")])

# Calculate Cohens ds for comparisons
cohen.d(loss$w2_dhager,   comp$w2_dhager,   pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_sex,      comp$w2_sex,      pooled = T, paired = F, na.rm = T)
cohen.d(loss$w0_edu,      comp$w0_edu,      pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_eth,      comp$w2_eth,      pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_deprmean, comp$w2_deprmean, pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_HESka,    comp$w2_HESka,    pooled = T, paired = F, na.rm = T)
cohen.d(loss$hyp,         comp$hyp,         pooled = T, paired = F, na.rm = T)
cohen.d(loss$w2_bmival,   comp$w2_bmival,   pooled = T, paired = F, na.rm = T)
cohen.d(loss$diab,        comp$diab,        pooled = T, paired = F, na.rm = T)
cohen.d(loss$chol,        comp$chol,        pooled = T, paired = F, na.rm = T)
cohen.d(loss$n_rf,        comp$n_rf,        pooled = T, paired = F, na.rm = T)




ggplot(data, aes(x = w2_dhager, y = n_rf)) + 
  geom_point() +
  geom_smooth(method = lm)

cor(data$w2_dhager, data$n_rf, use = "complete.obs")
