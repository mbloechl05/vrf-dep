# ================================================================
# Cardiovascular Risk and Trajectories of Depressive Symptoms
# Script 22: Measurement Invariance for Somatic Symptoms
# ================================================================

# clean work space
rm(list = ls())

# load packages 
library(lavaan)

# Load pre-processed data
load("data/elsa/processed/elsa_proc_data.RData")


# --------------------
# 1) Baseline model
# --------------------

# define model
model_baseline_som <- '

# measurement models without constrained factor loadings
w2_som =~ 1*w2_pscedb + w2_pscedc + w2_pscedh
w3_som =~ 1*w3_pscedb + w3_pscedc + w3_pscedh
w4_som =~ 1*w4_pscedb + w4_pscedc + w4_pscedh
w5_som =~ 1*w5_pscedb + w5_pscedc + w5_pscedh
w6_som =~ 1*w6_pscedb + w6_pscedc + w6_pscedh
w7_som =~ 1*w7_pscedb + w7_pscedc + w7_pscedh

# define tresholds
w2_pscedb | a*t1
w3_pscedb | a*t1
w4_pscedb | a*t1
w5_pscedb | a*t1
w6_pscedb | a*t1
w7_pscedb | a*t1

w2_pscedc | b*t1
w3_pscedc | b*t1
w4_pscedc | b*t1
w5_pscedc | b*t1
w6_pscedc | b*t1
w7_pscedc | b*t1

w2_pscedh | c*t1
w3_pscedh | c*t1
w4_pscedh | c*t1
w5_pscedh | c*t1
w6_pscedh | c*t1
w7_pscedh | c*t1

# define latent factor means
w2_som ~ 0*1
w3_som ~ 1
w4_som ~ 1
w5_som ~ 1
w6_som ~ 1
w7_som ~ 1

# define unique variance terms
w2_pscedb ~~ 1*w2_pscedb
w3_pscedb ~~ 1*w3_pscedb
w4_pscedb ~~ 1*w4_pscedb
w5_pscedb ~~ 1*w5_pscedb
w6_pscedb ~~ 1*w6_pscedb
w7_pscedb ~~ 1*w7_pscedb

w2_pscedc ~~ 1*w2_pscedc
w3_pscedc ~~ w3_pscedc
w4_pscedc ~~ w4_pscedc
w5_pscedc ~~ w5_pscedc
w6_pscedc ~~ w6_pscedc
w7_pscedc ~~ w7_pscedc

w2_pscedh ~~ 1*w2_pscedh
w3_pscedh ~~ w3_pscedh
w4_pscedh ~~ w4_pscedh
w5_pscedh ~~ w5_pscedh
w6_pscedh ~~ w6_pscedh
w7_pscedh ~~ w7_pscedh

# residual correlations
w2_pscedb ~~ w3_pscedb + w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w3_pscedb ~~ w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w4_pscedb ~~ w5_pscedb + w6_pscedb + w7_pscedb
w5_pscedb ~~ w6_pscedb + w7_pscedb
w6_pscedb ~~ w7_pscedb

w2_pscedc ~~ w3_pscedc + w4_pscedc + w5_pscedc + w6_pscedc + w7_pscedc
w3_pscedc ~~ w4_pscedc + w5_pscedc + w6_pscedc + w7_pscedc
w4_pscedc ~~ w5_pscedc + w6_pscedc + w7_pscedc
w5_pscedc ~~ w6_pscedc + w7_pscedc
w6_pscedc ~~ w7_pscedc

w2_pscedh ~~ w3_pscedh + w4_pscedh + w5_pscedh + w6_pscedh + w7_pscedh
w3_pscedh ~~ w4_pscedh + w5_pscedh + w6_pscedh + w7_pscedh
w4_pscedh ~~ w5_pscedh + w6_pscedh + w7_pscedh
w5_pscedh ~~ w6_pscedh + w7_pscedh
w6_pscedh ~~ w7_pscedh
'

# fit model
fit_baseline_som <- cfa(model_baseline_som, data = data,
                        ordered = c("w2_pscedb","w2_pscedc","w2_pscedh",
                                    "w3_pscedb","w3_pscedc","w3_pscedh",
                                    "w4_pscedb","w4_pscedc","w4_pscedh",
                                    "w5_pscedb","w5_pscedc","w5_pscedh",
                                    "w6_pscedb","w6_pscedc","w6_pscedh",
                                    "w7_pscedb","w7_pscedc","w7_pscedh"),
                        missing = "pairwise", estimator = "WLSMV",
                        parameterization = "theta" )

# summarise fit results
summary(fit_baseline_som, fit.measures = T)


# -------------------------------
# 2.) Loading invariance model
# -------------------------------

# define model
model_loading_som <- '

# measurement models with constrained factor loadings
w2_som =~ 1*w2_pscedb + l1*w2_pscedc + l2*w2_pscedh
w3_som =~ 1*w3_pscedb + l1*w3_pscedc + l2*w3_pscedh
w4_som =~ 1*w4_pscedb + l1*w4_pscedc + l2*w4_pscedh
w5_som =~ 1*w5_pscedb + l1*w5_pscedc + l2*w5_pscedh
w6_som =~ 1*w6_pscedb + l1*w6_pscedc + l2*w6_pscedh
w7_som =~ 1*w7_pscedb + l1*w7_pscedc + l2*w7_pscedh

# define tresholds
w2_pscedb | a*t1
w3_pscedb | a*t1
w4_pscedb | a*t1
w5_pscedb | a*t1
w6_pscedb | a*t1
w7_pscedb | a*t1

w2_pscedc | b*t1
w3_pscedc | b*t1
w4_pscedc | b*t1
w5_pscedc | b*t1
w6_pscedc | b*t1
w7_pscedc | b*t1

w2_pscedh | c*t1
w3_pscedh | c*t1
w4_pscedh | c*t1
w5_pscedh | c*t1
w6_pscedh | c*t1
w7_pscedh | c*t1

# define latent factor means
w2_som ~ 0*1
w3_som ~ 1
w4_som ~ 1
w5_som ~ 1
w6_som ~ 1
w7_som ~ 1

# define unique variance terms
w2_pscedb ~~ 1*w2_pscedb
w3_pscedb ~~ 1*w3_pscedb
w4_pscedb ~~ 1*w4_pscedb
w5_pscedb ~~ 1*w5_pscedb
w6_pscedb ~~ 1*w6_pscedb
w7_pscedb ~~ 1*w7_pscedb

w2_pscedc ~~ 1*w2_pscedc
w3_pscedc ~~ w3_pscedc
w4_pscedc ~~ w4_pscedc
w5_pscedc ~~ w5_pscedc
w6_pscedc ~~ w6_pscedc
w7_pscedc ~~ w7_pscedc

w2_pscedh ~~ 1*w2_pscedh
w3_pscedh ~~ w3_pscedh
w4_pscedh ~~ w4_pscedh
w5_pscedh ~~ w5_pscedh
w6_pscedh ~~ w6_pscedh
w7_pscedh ~~ w7_pscedh

# residual correlations
w2_pscedb ~~ w3_pscedb + w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w3_pscedb ~~ w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w4_pscedb ~~ w5_pscedb + w6_pscedb + w7_pscedb
w5_pscedb ~~ w6_pscedb + w7_pscedb
w6_pscedb ~~ w7_pscedb

w2_pscedc ~~ w3_pscedc + w4_pscedc + w5_pscedc + w6_pscedc + w7_pscedc
w3_pscedc ~~ w4_pscedc + w5_pscedc + w6_pscedc + w7_pscedc
w4_pscedc ~~ w5_pscedc + w6_pscedc + w7_pscedc
w5_pscedc ~~ w6_pscedc + w7_pscedc
w6_pscedc ~~ w7_pscedc

w2_pscedh ~~ w3_pscedh + w4_pscedh + w5_pscedh + w6_pscedh + w7_pscedh
w3_pscedh ~~ w4_pscedh + w5_pscedh + w6_pscedh + w7_pscedh
w4_pscedh ~~ w5_pscedh + w6_pscedh + w7_pscedh
w5_pscedh ~~ w6_pscedh + w7_pscedh
w6_pscedh ~~ w7_pscedh
'

# fit model
fit_loading_som <- cfa(model_loading_som, data = data,
                       ordered = c("w2_pscedb","w2_pscedc","w2_pscedh",
                                   "w3_pscedb","w3_pscedc","w3_pscedh",
                                   "w4_pscedb","w4_pscedc","w4_pscedh",
                                   "w5_pscedb","w5_pscedc","w5_pscedh",
                                   "w6_pscedb","w6_pscedc","w6_pscedh",
                                   "w7_pscedb","w7_pscedc","w7_pscedh"),
                       missing = "pairwise", estimator = "WLSMV",
                       parameterization = "theta" )

# summarise fit results
summary(fit_loading_som, fit.measures = T)


# 2.3) Unique factor invariance model
# -------------------------------------

# define model
model_factor_som <- '

# measurement models wit constrained factor loadings
w2_som =~ 1*w2_pscedb + l1*w2_pscedc + l2*w2_pscedh
w3_som =~ 1*w3_pscedb + l1*w3_pscedc + l2*w3_pscedh
w4_som =~ 1*w4_pscedb + l1*w4_pscedc + l2*w4_pscedh
w5_som =~ 1*w5_pscedb + l1*w5_pscedc + l2*w5_pscedh
w6_som =~ 1*w6_pscedb + l1*w6_pscedc + l2*w6_pscedh
w7_som =~ 1*w7_pscedb + l1*w7_pscedc + l2*w7_pscedh

# define tresholds
w2_pscedb | a*t1
w3_pscedb | a*t1
w4_pscedb | a*t1
w5_pscedb | a*t1
w6_pscedb | a*t1
w7_pscedb | a*t1

w2_pscedc | b*t1
w3_pscedc | b*t1
w4_pscedc | b*t1
w5_pscedc | b*t1
w6_pscedc | b*t1
w7_pscedc | b*t1

w2_pscedh | c*t1
w3_pscedh | c*t1
w4_pscedh | c*t1
w5_pscedh | c*t1
w6_pscedh | c*t1
w7_pscedh | c*t1

# define latent factor means
w2_som ~ 0*1
w3_som ~ 1
w4_som ~ 1
w5_som ~ 1
w6_som ~ 1
w7_som ~ 1

# define unique variance terms
w2_pscedb ~~ 1*w2_pscedb
w3_pscedb ~~ 1*w3_pscedb
w4_pscedb ~~ 1*w4_pscedb
w5_pscedb ~~ 1*w5_pscedb
w6_pscedb ~~ 1*w6_pscedb
w7_pscedb ~~ 1*w7_pscedb

w2_pscedc ~~ 1*w2_pscedc
w3_pscedc ~~ 1*w3_pscedc
w4_pscedc ~~ 1*w4_pscedc
w5_pscedc ~~ 1*w5_pscedc
w6_pscedc ~~ 1*w6_pscedc
w7_pscedc ~~ 1*w7_pscedc

w2_pscedh ~~ 1*w2_pscedh
w3_pscedh ~~ 1*w3_pscedh
w4_pscedh ~~ 1*w4_pscedh
w5_pscedh ~~ 1*w5_pscedh
w6_pscedh ~~ 1*w6_pscedh
w7_pscedh ~~ 1*w7_pscedh

# residual correlations
w2_pscedb ~~ w3_pscedb + w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w3_pscedb ~~ w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w4_pscedb ~~ w5_pscedb + w6_pscedb + w7_pscedb
w5_pscedb ~~ w6_pscedb + w7_pscedb
w6_pscedb ~~ w7_pscedb

w2_pscedc ~~ w3_pscedc + w4_pscedc + w5_pscedc + w6_pscedc + w7_pscedc
w3_pscedc ~~ w4_pscedc + w5_pscedc + w6_pscedc + w7_pscedc
w4_pscedc ~~ w5_pscedc + w6_pscedc + w7_pscedc
w5_pscedc ~~ w6_pscedc + w7_pscedc
w6_pscedc ~~ w7_pscedc

w2_pscedh ~~ w3_pscedh + w4_pscedh + w5_pscedh + w6_pscedh + w7_pscedh
w3_pscedh ~~ w4_pscedh + w5_pscedh + w6_pscedh + w7_pscedh
w4_pscedh ~~ w5_pscedh + w6_pscedh + w7_pscedh
w5_pscedh ~~ w6_pscedh + w7_pscedh
w6_pscedh ~~ w7_pscedh
'

# fit model
fit_factor_som <- cfa(model_factor_som, data = data,
                      ordered = c("w2_pscedb","w2_pscedc","w2_pscedh",
                                  "w3_pscedb","w3_pscedc","w3_pscedh",
                                  "w4_pscedb","w4_pscedc","w4_pscedh",
                                  "w5_pscedb","w5_pscedc","w5_pscedh",
                                  "w6_pscedb","w6_pscedc","w6_pscedh",
                                  "w7_pscedb","w7_pscedc","w7_pscedh"),
                      missing = "pairwise", estimator = "WLSMV",
                      parameterization = "theta" )

# summarise fit results
summary(fit_factor_som, fit.measures = T)


# 2.4) Model comparisons
# -------------------------

# show fit measures for each model
fitMeasures(fit_baseline_som, c("rmsea.scaled", "cfi.scaled", "srmr"))
fitMeasures(fit_loading_som,  c("rmsea.scaled", "cfi.scaled", "srmr"))
fitMeasures(fit_factor_som,   c("rmsea.scaled", "cfi.scaled", "srmr"))

# perform model comparison
lavTestLRT(fit_baseline_som, fit_loading_som, fit_factor_som, method = "satorra.bentler.2010")

