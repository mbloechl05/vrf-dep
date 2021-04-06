# ==========================================================
# Cardiovascular Risk and Trajectories of Depressed Mood
# Script 4: Supplementary Analyses
# ==========================================================

# clean work space
rm(list = ls())

# load packages 
library(lavaan)

# Load pre-processed data
load("data/elsa/processed/elsa_proc_data.RData")

# ---------------------------------
# 1) Analyses Manifest Variables
# ----------------------------------

# define model
model_fiml <- '

# intercept and slope with fixed coefficients
i =~ 1*w2_aff_sum + 1*w3_aff_sum + 1*w4_aff_sum + 1*w5_aff_sum + 1*w6_aff_sum + 1*w7_aff_sum
s =~ 0*w2_aff_sum + 1*w3_aff_sum + 2*w4_aff_sum + 3*w5_aff_sum + 4*w6_aff_sum + 5*w7_aff_sum

# regressions
i ~ w2_age_c + w2_sex + w0_edu + w2_eth + w2_adl + w2_pred + w2_pred_age
s ~ w2_age_c + w2_sex + w0_edu + w2_eth + w2_adl + w2_pred + w2_pred_age
'

# 1.1) Fit model with data used in LGM (no missing cov data)
# ------------------------------------------------------------

# set vascular risk variable for placeholder
data$w2_pred     <- data$w2_cvrisk_c
data$w2_pred_age <- data$w2_cvrisk_c*data$w2_age_c 

# fit model
fit_fiml_1 <- growth(model_fiml, data = data, missing = "fiml", estimator = "MLR")

# summarise fit results
summary(fit_fiml_1,  fit.measures = T, standardized = T, ci = T)


# 1.2) Fit model with data with missing covariate values
# ------------------------------------------------------------

# set vascular risk variable for placeholder
data_miss_cov$w2_pred     <- data_miss_cov$w2_cvrisk_c
data_miss_cov$w2_pred_age <- data_miss_cov$w2_cvrisk_c*data_miss_cov$w2_age_c 

# fit model
fit_fiml_2 <- growth(model_fiml, data = data_miss_cov, 
                     missing = "fiml.x", estimator = "MLR")

# summarise fit results
summary(fit_fiml_2,  fit.measures = T, standardized = T, ci = T)


# 1.3) Fit model including sampling weights
# --------------------------------------------

# set vascular risk variable for placeholder
data$w2_pred     <- data$w2_cvrisk_c
data$w2_pred_age <- data$w2_cvrisk_c*data$w2_age_c 

# fit model
fit_fiml_3 <- growth(model_fiml, data = data, missing = "fiml", estimator = "MLR", 
                     sampling.weights = "w2_w2wgt")

# summarise fit results
summary(fit_fiml_3,  fit.measures = T, standardized = T, ci = T)


