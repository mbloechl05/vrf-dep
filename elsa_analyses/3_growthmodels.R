## ===============================================================
## ELSA: Data analysis vascular risk factors
## (contact maria.bloechl@uni-leipzig.de in case of questions)
## ==============================================================

# -------------------------
# 0) Preparations 
# -------------------------

# clean work space
rm(list = ls())

# load packages 
library(lavaan)
library(semTools)
library(ggplot2)
#library(DescTools)
#library(cobalt)

# load preprocessed data 
load("data/processed/elsa_proc_data.RData")


# 1.a) baseline model (see Liu et al., 2017)

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

summary(fit.baseline, fit.measures = T)    


# 1.b) loading invariance model:

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

summary(fit.weak, fit.measures = T)     


# 1.c) unique factor invariance model:

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

summary(fit.strict, fit.measures = T)   


# 1.d) model comparisons 

fitMeasures(fit.baseline, c("rmsea", "cfi", "srmr"))
fitMeasures(fit.weak,     c("rmsea", "cfi", "srmr"))
fitMeasures(fit.strict,   c("rmsea", "cfi", "srmr"))
anova(fit.baseline, fit.weak, fit.strict)


# -------------------------------
# 2) Latent growth models (LGMs)
# -------------------------------

# Variable transformations

### center age variable
data$w2_dhager.c <- scale(data$w2_dhager, center = T, scale = F) # center age variable

# 2.b) Conditional LGM with covariates (no predictors)

m.uc <- '

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
W3_DEP ~ 0*1
W4_DEP ~ 0*1
W5_DEP ~ 0*1
W6_DEP ~ 0*1
W7_DEP ~ 0*1

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

# intercept and slope with fixed coefficients
i =~ 1*W2_DEP + 1*W3_DEP + 1*W4_DEP + 1*W5_DEP + 1*W6_DEP + 1*W7_DEP
s =~ 0*W2_DEP + 1*W3_DEP + 2*W4_DEP + 3*W5_DEP + 4*W6_DEP + 5*W7_DEP

i ~~ i
s ~~ s
i ~~ s

i ~ 0*1
s ~ 1
'

fit.uc <- sem(m.uc, data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
summary(fit.uc,  fit.measures = T, standardized = T)

# 2.b) Conditional LGM with covariates (no predictors)

m.cov <- '

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
W3_DEP ~ 0*1
W4_DEP ~ 0*1
W5_DEP ~ 0*1
W6_DEP ~ 0*1
W7_DEP ~ 0*1

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

# intercept and slope with fixed coefficients
i =~ 1*W2_DEP + 1*W3_DEP + 1*W4_DEP + 1*W5_DEP + 1*W6_DEP + 1*W7_DEP
s =~ 0*W2_DEP + 1*W3_DEP + 2*W4_DEP + 3*W5_DEP + 4*W6_DEP + 5*W7_DEP

i ~~ i
s ~~ s
i ~~ s

i ~ 0*1
s ~ 1

# regressions
i ~ w2_dhager.c + w2_DhSex + w2_fqethnr + w0_educ 
s ~ w2_dhager.c + w2_DhSex + w2_fqethnr + w0_educ
'

fit.cov <- sem(m.cov, data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
summary(fit.cov,  fit.measures = T, standardized = T)


# 2.c) Conditional LGMs with VRFs as preditors 

### The following model is an extension of the previous model with one additional predictor (pred), 
### In the modelpred is used as a place holder to subsequently introduce VRFs in the model.

m.pred <- '

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
W3_DEP ~ 0*1
W4_DEP ~ 0*1
W5_DEP ~ 0*1
W6_DEP ~ 0*1
W7_DEP ~ 0*1

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

# intercept and slope with fixed coefficients
i =~ 1*W2_DEP + 1*W3_DEP + 1*W4_DEP + 1*W5_DEP + 1*W6_DEP + 1*W7_DEP
s =~ 0*W2_DEP + 1*W3_DEP + 2*W4_DEP + 3*W5_DEP + 4*W6_DEP + 5*W7_DEP

i ~~ i
s ~~ s
i ~~ s

i ~ 0*1
s ~ 1

# regressions
i ~ w2_dhager.c + w2_DhSex + w2_fqethnr + w0_educ + pred
s ~ w2_dhager.c + w2_DhSex + w2_fqethnr + w0_educ + pred
'

### Self-reported hypertension
data$pred <- data$hyp # set hypertension for placeholder
fit.hyp <- sem(m.pred, data = data, 
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
               parameterization = "theta")
summary(fit.hyp,  fit.measures = T, standardized = T)

### Smoking  

data$pred <- data$w2_HESka # set smoking for placeholder
fit.smo <- sem(m.pred, data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
               parameterization = "theta")
summary(fit.smo,  fit.measures = T, standardized = T)


### BMI 

data$pred <- data$w2_bmival # set BMI for placeholder
fit.bmi <- sem(m.pred, data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
               parameterization = "theta")
summary(fit.bmi,  fit.measures = T, standardized = T)


### Diabetes

data$pred <- data$diab # set diabetes for placeholder
fit.dia <- sem(m.pred, data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
               parameterization = "theta")
summary(fit.dia,  fit.measures = T, standardized = T)


### additionally in ELSA:

### Systolic blood pressure  

data$pred <- data$w2_meansys # set hypertension for placeholder
fit.sbp <- sem(m.pred, data = data, 
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
               parameterization = "theta")
summary(fit.sbp,  fit.measures = T, standardized = T)


### Total chlesterol

data$pred <- data$w2_chol_f # set cholesterol for placeholder
fit.cho <- sem(m.pred, data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
               parameterization = "theta")
summary(fit.cho,  fit.measures = T, standardized = T)


### HDL

data$pred <- data$w2_hdl_f # set hdl for placeholder
fit.hdl <- sem(m.pred,data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
               parameterization = "theta")
summary(fit.hdl,  fit.measures = T, standardized = T)


### LDL

data$pred <- data$w2_ldl_f # set hdl for placeholder
fit.ldl <- sem(m.pred,data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
               parameterization = "theta")
summary(fit.ldl,  fit.measures = T, standardized = T)


### Multiple risk factors (accumulated)

data$pred <- data$n_rf # set accumulated for placeholder
fit.mul <- sem(m.pred,data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
                parameterization = "theta")
summary(fit.mul,  fit.measures=T, standardized = T)


### All risk factors simultaneously

m.all <- '

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
W3_DEP ~ 0*1
W4_DEP ~ 0*1
W5_DEP ~ 0*1
W6_DEP ~ 0*1
W7_DEP ~ 0*1

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

# intercept and slope with fixed coefficients
i =~ 1*W2_DEP + 1*W3_DEP + 1*W4_DEP + 1*W5_DEP + 1*W6_DEP + 1*W7_DEP
s =~ 0*W2_DEP + 1*W3_DEP + 2*W4_DEP + 3*W5_DEP + 4*W6_DEP + 5*W7_DEP

i ~~ i
s ~~ s
i ~~ s

i ~ 0*1
s ~ 1

# regressions
i ~ w2_dhager.c + w2_DhSex + w2_fqethnr + w0_educ + hyp + w2_HESka + w2_bmival + diab
s ~ w2_dhager.c + w2_DhSex + w2_fqethnr + w0_educ + hyp + w2_HESka + w2_bmival + diab
'

fit.all <- sem(m.all, data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedd",
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
               parameterization = "theta")
summary(fit.all,  fit.measures = T, standardized = T)



# -------------
# plots LGMs
# -------------

# Figure 2: single risk factors

### code function to extract parameters for each risk factor from the simple and the full model
get_parameters <- function(fit.simple, i.simple, s.simple, i.all, s.all){
  beta         <- standardizedSolution(fit.simple)$est.std[c(i.simple, s.simple)]
  ci.l         <- standardizedSolution(fit.simple)$ci.lower[c(i.simple, s.simple)]
  ci.u         <- standardizedSolution(fit.simple)$ci.upper[c(i.simple, s.simple)]
  coeff.simple <- data.frame(beta, ci.l, ci.u)
  beta         <- standardizedSolution(fit.all)$est.std[c(i.all, s.all)]
  ci.l         <- standardizedSolution(fit.all)$ci.lower[c(i.all, s.all)]
  ci.u         <- standardizedSolution(fit.all)$ci.upper[c(i.all, s.all)]
  coeff.all    <- data.frame(beta, ci.l, ci.u)
  
  coeff           <- rbind(coeff.simple, coeff.all)
  coeff$parameter <- c("Intercept", "Slope", "Intercept", "Slope")
  coeff$model     <- c("1", "1", "2", "2") #1 = Simple, 2 = All
  
  return(coeff)
}

### create plotting function
get_plot <- function(coeffs, title) {
  ggplot(coeffs, aes(x = parameter, y = beta, ymin = ci.l, ymax = ci.u)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.4) +
    geom_errorbar(aes(color = model), width = 0.1, size = 1, position = position_dodge(0.3)) +
    geom_point(aes(color = model), size = 3.5, position = position_dodge(0.3))+
    ggtitle(title) +
    labs(x = "", y = "Standardised beta (95% CI)") +
    scale_y_continuous(limits = c(-0.3, 0.6)) +
    theme_classic(base_size = 15) +
    theme(axis.text  = element_text(colour = "black", size = 15), 
          axis.title = element_text(colour = "black", size = 15, face = "bold"), 
          plot.title = element_text(hjust = 0.5, face = "bold"), 
          legend.position = "none") +
    scale_color_manual(values=c('#18345C','#AC95BF'), 
                       name="",
                       breaks=c("1", "2"),
                       labels=c("One predictor", "All predictors"))
  ggsave(paste("output/", title, ".pdf", sep = ""), width = 4, height = 4)
}

### now make and saveplots for risk factors

### Hypertension
coeff.hyp <- get_parameters(fit.simple = fit.hyp, i.simple = 193, s.simple = 198, i.all = 193, s.all = 201)
get_plot(coeffs = coeff.hyp, title = "Hypertension")

### Smoking
coeff.smo <- get_parameters(fit.simple = fit.smo, i.simple = 193, s.simple = 198, i.all = 194, s.all = 202)
get_plot(coeffs = coeff.smo, title = "Current smoking") 

### BMI
coeff.bmi <- get_parameters(fit.simple = fit.bmi, i.simple = 193, s.simple = 198, i.all = 195, s.all = 203)
get_plot(coeffs = coeff.bmi, title = "Body mass index")

### Diabetes
coeff.dia <- get_parameters(fit.simple = fit.dia, i.simple = 193, s.simple = 198, i.all = 196, s.all = 204)
get_plot(coeffs = coeff.dia, title = "Diabetes") 



# Figure 3: multiple risk factors 

### code function to extract parameters for risk factor from the simple model
get_parameters_s  <- function(fit.simple, i.simple, s.simple, i.all, s.all){
  beta            <- standardizedSolution(fit.simple)$est.std[c(i.simple, s.simple)]
  ci.l            <- standardizedSolution(fit.simple)$ci.lower[c(i.simple, s.simple)]
  ci.u            <- standardizedSolution(fit.simple)$ci.upper[c(i.simple, s.simple)]
  coeff.simple    <- data.frame(beta, ci.l, ci.u)
  coeff.simple$parameter <- c("Intercept", "Slope")
  return(coeff.simple)
}

# plot function with only simple model results
get_plot_s <- function(coeffs, title) {
  ggplot(coeffs, aes(x = parameter, y = beta, ymin = ci.l, ymax = ci.u)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.4) +
    geom_errorbar(width = 0.1, size = 1, colour = '#18345C') +
    geom_point(size = 3.5, colour = '#18345C')+
    ggtitle(title) +
    labs(x = "", y = "Standardised beta (95% CI)") +
    scale_y_continuous(limits = c(-0.4, 0.6)) +
    theme_classic(base_size = 15) +
    theme(axis.text  = element_text(colour = "black", size = 15), 
          axis.title = element_text(colour = "black", size = 15, face = "bold"), 
          plot.title = element_text(hjust = 0.5, face = "bold"), 
          legend.position = "none")
  ggsave(paste("output/", title, ".pdf", sep = ""), dpi = 600, width = 4, height = 4)
}

### now make and save plot 

### Multiple risk factors
coeff.mul <- get_parameters_s(fit.simple = fit.mul, i.simple = 193, s.simple = 198)
get_plot_s(coeffs = coeff.mul, title = "")



# Supplementary: additional figures for ELSA only

### Systolic blood pressure
coeff.sbp <- get_parameters_s(fit.simple = fit.sbp, i.simple = 193, s.simple = 198)
get_plot_s(coeffs = coeff.sbp, title = "Systolic blood pressure")

### Total cholesterol
coeff.cho <- get_parameters_s(fit.simple = fit.cho, i.simple = 193, s.simple = 198)
get_plot_s(coeffs = coeff.cho, title = "Total cholesterol")

### HDL cholesterol
coeff.hdl <- get_parameters_s(fit.simple = fit.hdl, i.simple = 193, s.simple = 198)
get_plot_s(coeffs = coeff.hdl, title = "HDL cholesterol")

### LDL cholesterol
coeff.ldl <- get_parameters_s(fit.simple = fit.ldl, i.simple = 193, s.simple = 198)
get_plot_s(coeffs = coeff.ldl, title = "LDL cholesterol")


