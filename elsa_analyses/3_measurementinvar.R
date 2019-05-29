# ==============================================================
# ELSA: Testing measurement invariance
# (contact maria.bloechl@gmail.com in case of questions)
# ==============================================================

# clean work space
rm(list = ls())

# load packages 
library(lavaan)

# Load pre-processed data
load("data/elsa/processed/elsa_proc_data.RData")


# ---------------------
# 1) Baseline model 
# ---------------------

# define model 
model.baseline <- '

# measurement models without constrained factor loadings

W2_DEP =~ 1*w2_psceda + w2_pscedb + w2_pscedd + w2_pscede + w2_pscedg
W3_DEP =~ 1*w3_psceda + w3_pscedb + w3_pscedd + w3_pscede + w3_pscedg
W4_DEP =~ 1*w4_psceda + w4_pscedb + w4_pscedd + w4_pscede + w4_pscedg
W5_DEP =~ 1*w5_psceda + w5_pscedb + w5_pscedd + w5_pscede + w5_pscedg
W6_DEP =~ 1*w6_psceda + w6_pscedb + w6_pscedd + w6_pscede + w6_pscedg
W7_DEP =~ 1*w7_psceda + w7_pscedb + w7_pscedd + w7_pscede + w7_pscedg

# define tresholds
w2_psceda | a*t1
w3_psceda | a*t1
w4_psceda | a*t1
w5_psceda | a*t1
w6_psceda | a*t1
w7_psceda | a*t1

w2_pscedb | b*t1
w3_pscedb | b*t1
w4_pscedb | b*t1
w5_pscedb | b*t1
w6_pscedb | b*t1
w7_pscedb | b*t1

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

w2_pscedg | e*t1
w3_pscedg | e*t1
w4_pscedg | e*t1
w5_pscedg | e*t1
w6_pscedg | e*t1
w7_pscedg | e*t1

# define latent factor means
W2_DEP ~ 0*1
W3_DEP ~ 1
W4_DEP ~ 1
W5_DEP ~ 1
W6_DEP ~ 1
W7_DEP ~ 1

# define unique variance terms
w2_psceda ~~ 1*w2_psceda
w3_psceda ~~ 1*w3_psceda
w4_psceda ~~ 1*w4_psceda
w5_psceda ~~ 1*w5_psceda
w6_psceda ~~ 1*w6_psceda
w7_psceda ~~ 1*w7_psceda

w2_pscedb ~~ 1*w2_pscedb
w3_pscedb ~~ w3_pscedb
w4_pscedb ~~ w4_pscedb
w5_pscedb ~~ w5_pscedb
w6_pscedb ~~ w6_pscedb
w7_pscedb ~~ w7_pscedb

w2_pscedd ~~ 1*w2_pscedd
w3_pscedd ~~ w3_pscedd
w4_pscedd ~~ w4_pscedd
w5_pscedd ~~ w5_pscedd
w6_pscedd ~~ w6_pscedd
w7_pscedd ~~ w7_pscedd

w2_pscede ~~ 1*w2_pscede
w3_pscede ~~ w3_pscede
w4_pscede ~~ w4_pscede
w5_pscede ~~ w5_pscede
w6_pscede ~~ w6_pscede
w7_pscede ~~ w7_pscede

w2_pscedg ~~ 1*w2_pscedg
w3_pscedg ~~ w3_pscedg
w4_pscedg ~~ w4_pscedg
w5_pscedg ~~ w5_pscedg
w6_pscedg ~~ w6_pscedg
w7_pscedg ~~ w7_pscedg

# residual correlations
w2_psceda ~~ w3_psceda + w4_psceda + w5_psceda + w6_psceda + w7_psceda
w3_psceda ~~ w4_psceda + w5_psceda + w6_psceda + w7_psceda
w4_psceda ~~ w5_psceda + w6_psceda + w7_psceda
w5_psceda ~~ w6_psceda + w7_psceda
w6_psceda ~~ w7_psceda

w2_pscedb ~~ w3_pscedb + w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w3_pscedb ~~ w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w4_pscedb ~~ w5_pscedb + w6_pscedb + w7_pscedb
w5_pscedb ~~ w6_pscedb + w7_pscedb
w6_pscedb ~~ w7_pscedb

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

w2_pscedg ~~ w3_pscedg + w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
w3_pscedg ~~ w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
w4_pscedg ~~ w5_pscedg + w6_pscedg + w7_pscedg
w5_pscedg ~~ w6_pscedg + w7_pscedg
w6_pscedg ~~ w7_pscedg
'

# fit model
fit.baseline <- cfa(model.baseline, data = data, 
                    ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
                                "w2_pscede","w2_pscedg", 
                                "w3_psceda","w3_pscedb","w3_pscedd",
                                "w3_pscede","w3_pscedg", 
                                "w4_psceda","w4_pscedb","w4_pscedd",
                                "w4_pscede","w4_pscedg", 
                                "w5_psceda","w5_pscedb","w5_pscedd",
                                "w5_pscede","w5_pscedg", 
                                "w6_psceda","w6_pscedb","w6_pscedd",
                                "w6_pscede","w6_pscedg", 
                                "w7_psceda","w7_pscedb","w7_pscedd",
                                "w7_pscede","w7_pscedg"), 
                    missing = "pairwise", estimator = "WLSMV", 
                    parameterization = "theta" )

# fit results
summary(fit.baseline, fit.measures = T)    


# --------------------------------
# 2.) Loading invariance model
# --------------------------------

# define model
model.weak <- '

# measurement models with constrained factor loadings
W2_DEP =~ 1*w2_psceda + l1*w2_pscedb + l2*w2_pscedd + l3*w2_pscede + l4*w2_pscedg
W3_DEP =~ 1*w3_psceda + l1*w3_pscedb + l2*w3_pscedd + l3*w3_pscede + l4*w3_pscedg
W4_DEP =~ 1*w4_psceda + l1*w4_pscedb + l2*w4_pscedd + l3*w4_pscede + l4*w4_pscedg
W5_DEP =~ 1*w5_psceda + l1*w5_pscedb + l2*w5_pscedd + l3*w5_pscede + l4*w5_pscedg
W6_DEP =~ 1*w6_psceda + l1*w6_pscedb + l2*w6_pscedd + l3*w6_pscede + l4*w6_pscedg
W7_DEP =~ 1*w7_psceda + l1*w7_pscedb + l2*w7_pscedd + l3*w7_pscede + l4*w7_pscedg

# define tresholds
w2_psceda | a*t1
w3_psceda | a*t1
w4_psceda | a*t1
w5_psceda | a*t1
w6_psceda | a*t1
w7_psceda | a*t1

w2_pscedb | b*t1
w3_pscedb | b*t1
w4_pscedb | b*t1
w5_pscedb | b*t1
w6_pscedb | b*t1
w7_pscedb | b*t1

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

w2_pscedg | e*t1
w3_pscedg | e*t1
w4_pscedg | e*t1
w5_pscedg | e*t1
w6_pscedg | e*t1
w7_pscedg | e*t1

# define latent factor means
W2_DEP ~ 0*1
W3_DEP ~ 1
W4_DEP ~ 1
W5_DEP ~ 1
W6_DEP ~ 1
W7_DEP ~ 1

# define unique variance terms
w2_psceda ~~ 1*w2_psceda
w3_psceda ~~ 1*w3_psceda
w4_psceda ~~ 1*w4_psceda
w5_psceda ~~ 1*w5_psceda
w6_psceda ~~ 1*w6_psceda
w7_psceda ~~ 1*w7_psceda

w2_pscedb ~~ 1*w2_pscedb
w3_pscedb ~~ w3_pscedb
w4_pscedb ~~ w4_pscedb
w5_pscedb ~~ w5_pscedb
w6_pscedb ~~ w6_pscedb
w7_pscedb ~~ w7_pscedb

w2_pscedd ~~ 1*w2_pscedd
w3_pscedd ~~ w3_pscedd
w4_pscedd ~~ w4_pscedd
w5_pscedd ~~ w5_pscedd
w6_pscedd ~~ w6_pscedd
w7_pscedd ~~ w7_pscedd

w2_pscede ~~ 1*w2_pscede
w3_pscede ~~ w3_pscede
w4_pscede ~~ w4_pscede
w5_pscede ~~ w5_pscede
w6_pscede ~~ w6_pscede
w7_pscede ~~ w7_pscede

w2_pscedg ~~ 1*w2_pscedg
w3_pscedg ~~ w3_pscedg
w4_pscedg ~~ w4_pscedg
w5_pscedg ~~ w5_pscedg
w6_pscedg ~~ w6_pscedg
w7_pscedg ~~ w7_pscedg

# residual correlations
w2_psceda ~~ w3_psceda + w4_psceda + w5_psceda + w6_psceda + w7_psceda
w3_psceda ~~ w4_psceda + w5_psceda + w6_psceda + w7_psceda
w4_psceda ~~ w5_psceda + w6_psceda + w7_psceda
w5_psceda ~~ w6_psceda + w7_psceda
w6_psceda ~~ w7_psceda

w2_pscedb ~~ w3_pscedb + w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w3_pscedb ~~ w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w4_pscedb ~~ w5_pscedb + w6_pscedb + w7_pscedb
w5_pscedb ~~ w6_pscedb + w7_pscedb
w6_pscedb ~~ w7_pscedb

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

w2_pscedg ~~ w3_pscedg + w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
w3_pscedg ~~ w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
w4_pscedg ~~ w5_pscedg + w6_pscedg + w7_pscedg
w5_pscedg ~~ w6_pscedg + w7_pscedg
w6_pscedg ~~ w7_pscedg
'

# fit model
fit.weak <- cfa(model.weak, data = data, 
                ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
                            "w2_pscede","w2_pscedg", 
                            "w3_psceda","w3_pscedb","w3_pscedd",
                            "w3_pscede","w3_pscedg", 
                            "w4_psceda","w4_pscedb","w4_pscedd",
                            "w4_pscede","w4_pscedg", 
                            "w5_psceda","w5_pscedb","w5_pscedd",
                            "w5_pscede","w5_pscedg", 
                            "w6_psceda","w6_pscedb","w6_pscedd",
                            "w6_pscede","w6_pscedg", 
                            "w7_psceda","w7_pscedb","w7_pscedd",
                            "w7_pscede","w7_pscedg"), 
                missing = "pairwise", estimator = "WLSMV", 
                parameterization = "theta" )

# fit results
summary(fit.weak, fit.measures = T)     


# -------------------------------------
# 3) Unique factor invariance model
# -------------------------------------

# define model
model.strict <- '

# measurement models wit constrained factor loadings
W2_DEP =~ 1*w2_psceda + l1*w2_pscedb + l2*w2_pscedd + l3*w2_pscede + l4*w2_pscedg
W3_DEP =~ 1*w3_psceda + l1*w3_pscedb + l2*w3_pscedd + l3*w3_pscede + l4*w3_pscedg
W4_DEP =~ 1*w4_psceda + l1*w4_pscedb + l2*w4_pscedd + l3*w4_pscede + l4*w4_pscedg
W5_DEP =~ 1*w5_psceda + l1*w5_pscedb + l2*w5_pscedd + l3*w5_pscede + l4*w5_pscedg
W6_DEP =~ 1*w6_psceda + l1*w6_pscedb + l2*w6_pscedd + l3*w6_pscede + l4*w6_pscedg
W7_DEP =~ 1*w7_psceda + l1*w7_pscedb + l2*w7_pscedd + l3*w7_pscede + l4*w7_pscedg

# define tresholds
w2_psceda | a*t1
w3_psceda | a*t1
w4_psceda | a*t1
w5_psceda | a*t1
w6_psceda | a*t1
w7_psceda | a*t1

w2_pscedb | b*t1
w3_pscedb | b*t1
w4_pscedb | b*t1
w5_pscedb | b*t1
w6_pscedb | b*t1
w7_pscedb | b*t1

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

w2_pscedg | e*t1
w3_pscedg | e*t1
w4_pscedg | e*t1
w5_pscedg | e*t1
w6_pscedg | e*t1
w7_pscedg | e*t1

# define latent factor means
W2_DEP ~ 0*1
W3_DEP ~ 1
W4_DEP ~ 1
W5_DEP ~ 1
W6_DEP ~ 1
W7_DEP ~ 1

# define unique variance terms
w2_psceda ~~ 1*w2_psceda
w3_psceda ~~ 1*w3_psceda
w4_psceda ~~ 1*w4_psceda
w5_psceda ~~ 1*w5_psceda
w6_psceda ~~ 1*w6_psceda
w7_psceda ~~ 1*w7_psceda

w2_pscedb ~~ 1*w2_pscedb
w3_pscedb ~~ 1*w3_pscedb
w4_pscedb ~~ 1*w4_pscedb
w5_pscedb ~~ 1*w5_pscedb
w6_pscedb ~~ 1*w6_pscedb
w7_pscedb ~~ 1*w7_pscedb

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

w2_pscedb ~~ w3_pscedb + w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w3_pscedb ~~ w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
w4_pscedb ~~ w5_pscedb + w6_pscedb + w7_pscedb
w5_pscedb ~~ w6_pscedb + w7_pscedb
w6_pscedb ~~ w7_pscedb

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

w2_pscedg ~~ w3_pscedg + w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
w3_pscedg ~~ w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
w4_pscedg ~~ w5_pscedg + w6_pscedg + w7_pscedg
w5_pscedg ~~ w6_pscedg + w7_pscedg
w6_pscedg ~~ w7_pscedg
'

# fit model
fit.strict <- cfa(model.strict, data = data, 
                  ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
                              "w2_pscede","w2_pscedg", 
                              "w3_psceda","w3_pscedb","w3_pscedd",
                              "w3_pscede","w3_pscedg", 
                              "w4_psceda","w4_pscedb","w4_pscedd",
                              "w4_pscede","w4_pscedg", 
                              "w5_psceda","w5_pscedb","w5_pscedd",
                              "w5_pscede","w5_pscedg", 
                              "w6_psceda","w6_pscedb","w6_pscedd",
                              "w6_pscede","w6_pscedg", 
                              "w7_psceda","w7_pscedb","w7_pscedd",
                              "w7_pscede","w7_pscedg"), 
                  missing = "pairwise", estimator = "WLSMV", 
                  parameterization = "theta" )

# fit results
summary(fit.strict, fit.measures = T)   


# -------------------------
# 4) Model comparisons 
# -------------------------

# fit measures for each model
fitMeasures(fit.baseline, c("rmsea.robust", "cfi.scaled", "srmr.scaled"))
fitMeasures(fit.weak,     c("rmsea.scaled", "cfi.scaled", "srmr.scaled"))
fitMeasures(fit.strict,   c("rmsea.scaled", "cfi.scaled", "srmr.scaled"))

# model comparison
anova(fit.baseline, fit.weak, fit.strict)
