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
# 1) Analyses using PML 
# ----------------------------------

model_2_aff <- '

# measurement models with constrained factor loadings
w2_dep =~ 1*w2_psceda + l1*w2_pscedd + l2*w2_pscede + l3*w2_pscedf + l4*w2_pscedg
w3_dep =~ 1*w3_psceda + l1*w3_pscedd + l2*w3_pscede + l3*w3_pscedf + l4*w3_pscedg
w4_dep =~ 1*w4_psceda + l1*w4_pscedd + l2*w4_pscede + l3*w4_pscedf + l4*w4_pscedg
w5_dep =~ 1*w5_psceda + l1*w5_pscedd + l2*w5_pscede + l3*w5_pscedf + l4*w5_pscedg
w6_dep =~ 1*w6_psceda + l1*w6_pscedd + l2*w6_pscede + l3*w6_pscedf + l4*w6_pscedg
w7_dep =~ 1*w7_psceda + l1*w7_pscedd + l2*w7_pscede + l3*w7_pscedf + l4*w7_pscedg

# define tresholds
w2_psceda | 0*t1
w3_psceda | 0*t1
w4_psceda | 0*t1
w5_psceda | 0*t1
w6_psceda | 0*t1
w7_psceda | 0*t1

w2_pscedd | c*t1
w3_pscedd | c*t1
w4_pscedd | c*t1
w5_pscedd | c*t1
w6_pscedd | c*t1
w7_pscedd | c*t1

w2_pscede | d*t1
w3_pscede | d*t1
w4_pscede | d*t1
w5_pscede | d*t1
w6_pscede | d*t1
w7_pscede | d*t1

w2_pscedf | b*t1
w3_pscedf | b*t1
w4_pscedf | b*t1
w5_pscedf | b*t1
w6_pscedf | b*t1
w7_pscedf | b*t1

w2_pscedg | e*t1
w3_pscedg | e*t1
w4_pscedg | e*t1
w5_pscedg | e*t1
w6_pscedg | e*t1
w7_pscedg | e*t1

# define latent factor means
w2_dep ~ 0*1
w3_dep ~ 0*1
w4_dep ~ 0*1
w5_dep ~ 0*1
w6_dep ~ 0*1
w7_dep ~ 0*1

# define unique variance terms
w2_psceda ~~ 1*w2_psceda
w3_psceda ~~ 1*w3_psceda
w4_psceda ~~ 1*w4_psceda
w5_psceda ~~ 1*w5_psceda
w6_psceda ~~ 1*w6_psceda
w7_psceda ~~ 1*w7_psceda

w2_pscedd ~~ 1*w2_pscedd
w3_pscedd ~~ 1*w3_pscedd
w4_pscedd ~~ 1*w4_pscedd
w5_pscedd ~~ 1*w5_pscedd
w6_pscedd ~~ 1*w6_pscedd
w7_pscedd ~~ 1*w7_pscedd

w2_pscede ~~ 1*w2_pscede
w3_pscede ~~ 1*w3_pscede
w4_pscede ~~ 1*w4_pscede
w5_pscede ~~ 1*w5_pscede
w6_pscede ~~ 1*w6_pscede
w7_pscede ~~ 1*w7_pscede

w2_pscedf ~~ 1*w2_pscedf
w3_pscedf ~~ 1*w3_pscedf
w4_pscedf ~~ 1*w4_pscedf
w5_pscedf ~~ 1*w5_pscedf
w6_pscedf ~~ 1*w6_pscedf
w7_pscedf ~~ 1*w7_pscedf

w2_pscedg ~~ 1*w2_pscedg
w3_pscedg ~~ 1*w3_pscedg
w4_pscedg ~~ 1*w4_pscedg
w5_pscedg ~~ 1*w5_pscedg
w6_pscedg ~~ 1*w6_pscedg
w7_pscedg ~~ 1*w7_pscedg

# residual correlations
w2_psceda ~~ w3_psceda + w4_psceda + w5_psceda + w6_psceda + w7_psceda
w3_psceda ~~ w4_psceda + w5_psceda + w6_psceda + w7_psceda
w4_psceda ~~ w5_psceda + w6_psceda + w7_psceda
w5_psceda ~~ w6_psceda + w7_psceda
w6_psceda ~~ w7_psceda

w2_pscedd ~~ w3_pscedd + w4_pscedd + w5_pscedd + w6_pscedd + w7_pscedd
w3_pscedd ~~ w4_pscedd + w5_pscedd + w6_pscedd + w7_pscedd
w4_pscedd ~~ w5_pscedd + w6_pscedd + w7_pscedd
w5_pscedd ~~ w6_pscedd + w7_pscedd
w6_pscedd ~~ w7_pscedd

w2_pscede ~~ w3_pscede + w4_pscede + w5_pscede + w6_pscede + w7_pscede
w3_pscede ~~ w4_pscede + w5_pscede + w6_pscede + w7_pscede
w4_pscede ~~ w5_pscede + w6_pscede + w7_pscede
w5_pscede ~~ w6_pscede + w7_pscede
w6_pscede ~~ w7_pscede

w2_pscedf ~~ w3_pscedf + w4_pscedf + w5_pscedf + w6_pscedf + w7_pscedf
w3_pscedf ~~ w4_pscedf + w5_pscedf + w6_pscedf + w7_pscedf
w4_pscedf ~~ w5_pscedf + w6_pscedf + w7_pscedf
w5_pscedf ~~ w6_pscedf + w7_pscedf
w6_pscedf ~~ w7_pscedf

w2_pscedg ~~ w3_pscedg + w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
w3_pscedg ~~ w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
w4_pscedg ~~ w5_pscedg + w6_pscedg + w7_pscedg
w5_pscedg ~~ w6_pscedg + w7_pscedg
w6_pscedg ~~ w7_pscedg

# intercept and slope with fixed coefficients
i =~ 1*w2_dep + 1*w3_dep + 1*w4_dep + 1*w5_dep + 1*w6_dep + 1*w7_dep
s =~ 0*w2_dep + 1*w3_dep + 2*w4_dep + 3*w5_dep + 4*w6_dep + 5*w7_dep

# intercept and slope variances and covariance
i ~~ i
s ~~ s
i ~~ s

# intercept and slope means
i ~ 1
s ~ 1

# regressions
i ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_pred + w2_adl + w2_pred_age
s ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_pred + w2_adl + w2_pred_age
'

# set vascular risk variable for placeholder
data$w2_pred     <- data$w2_cvrisk_c
data$w2_pred_age <- data$w2_cvrisk_c*data$w2_age_c 

# fit model
fit_2_aff <- 
  sem(model_2_aff, data = data, 
      ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg",
                  "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg",
                  "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg",
                  "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg",
                  "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg",
                  "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"),
      missing = "pairwise", estimator = "PML", se = "robust")

# summarise fit results
summary(fit_2_aff,  fit.measures = T, standardized = T, ci = T)



# ---------------------------------
# 2) Analyses Manifest Variables
# ----------------------------------

# define model
model_fiml <- '

# intercept and slope with fixed coefficients
i =~ 1*w2_aff_sum + 1*w3_aff_sum + 1*w4_aff_sum + 1*w5_aff_sum + 1*w6_aff_sum + 1*w7_aff_sum
s =~ 0*w2_aff_sum + 1*w3_aff_sum + 2*w4_aff_sum + 3*w5_aff_sum + 4*w6_aff_sum + 5*w7_aff_sum

# regressions
i ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_pred + w2_adl + w2_pred_age
s ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_pred + w2_adl + w2_pred_age
'

# 2.1) Fit model with data used in LGM (no missing cov data)
# ------------------------------------------------------------

# set vascular risk variable for placeholder
data$w2_pred     <- data$w2_cvrisk_c
data$w2_pred_age <- data$w2_cvrisk_c*data$w2_age_c 

# fit model
fit_fiml_1 <- growth(model_fiml, data = data, missing = "fiml", estimator = "MLR")

# summarise fit results
summary(fit_fiml_1,  fit.measures = T, standardized = T, ci = T)


# 2.2) Fit model with data with missing covariate values
# ------------------------------------------------------------

# set vascular risk variable for placeholder
data_miss_cov$w2_pred     <- data_miss_cov$w2_cvrisk_c
data_miss_cov$w2_pred_age <- data_miss_cov$w2_cvrisk_c*data_miss_cov$w2_age_c 

# fit model
fit_fiml_2 <- growth(model_fiml, data = data_miss_cov, 
                     missing = "fiml.x", estimator = "MLR")

# summarise fit results
summary(fit_fiml_2,  fit.measures = T, standardized = T, ci = T)


