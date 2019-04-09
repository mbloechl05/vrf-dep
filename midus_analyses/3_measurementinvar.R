# ==============================================================
# MIDUS: Testing measurement invariance
# (contact maria.bloechl@gmail.com in case of questions)
# ==============================================================

# clean work space
rm(list = ls())

# load packages 
library(lme4)
library(lmerTest)
library(lavaan)
library(semTools)
library(psych)
library(ggplot2)
library(dplyr)
library(plyr)
library(DescTools)
library(cobalt)

# load preprocessed data 
load("data/midus/processed/midus_proc_data.RData")


# -----------------------------------------
# 1) Configural measurement invariance 
# -----------------------------------------

# define model
model.configural <- '

# measurement models without constrained factor loadings
W1_DEP =~ 1*A1SA13A + A1SA13B + A1SA13D + A1SA13E + A1SA13F
W2_DEP =~ 1*B1SA24A + B1SA24B + B1SA24D + B1SA24E + B1SA24F
W3_DEP =~ 1*C1SA20A + C1SA20B + C1SA20D + C1SA20E + C1SA20F

# residual correlations
A1SA13A ~~ B1SA24A 
B1SA24A ~~ C1SA20A
A1SA13B ~~ B1SA24B
B1SA24B ~~ C1SA20B
A1SA13D ~~ B1SA24D 
B1SA24D ~~ C1SA20D
A1SA13E ~~ B1SA24E 
B1SA24E ~~ C1SA20E
A1SA13F ~~ B1SA24F 
B1SA24F ~~ C1SA20F
'

# fit model 
fit_configural <- cfa(model.configural, data = data, missing = 'fiml')

# fit results
summary(fit_configural, fit.measures = T) 


# -----------------------------------
# 2) weak measurement invariance 
# -----------------------------------

# define model 
model.weak <- '

# measurement models with constrained factor loadings
W1_DEP =~ 1*A1SA13A + l1*A1SA13B + l2*A1SA13D + l3*A1SA13E + l4*A1SA13F
W2_DEP =~ 1*B1SA24A + l1*B1SA24B + l2*B1SA24D + l3*B1SA24E + l4*B1SA24F
W3_DEP =~ 1*C1SA20A + l1*C1SA20B + l2*C1SA20D + l3*C1SA20E + l4*C1SA20F

# residual correlations
A1SA13A ~~ B1SA24A 
B1SA24A ~~ C1SA20A
A1SA13B ~~ B1SA24B
B1SA24B ~~ C1SA20B
A1SA13D ~~ B1SA24D 
B1SA24D ~~ C1SA20D
A1SA13E ~~ B1SA24E 
B1SA24E ~~ C1SA20E
A1SA13F ~~ B1SA24F 
B1SA24F ~~ C1SA20F
'

# fit model
fit_weak <- cfa(model.weak, data = data, missing='fiml')

# fit results
summary(fit_weak,  fit.measures=T) 

# ------------------------------------
# 3) strong measurement invariance 
# ------------------------------------

# define model
model_strong <- '

# measurement models with constrained factor loadings
W1_DEP =~ 1*A1SA13A + l1*A1SA13B + l2*A1SA13D + l3*A1SA13E + l4*A1SA13F
W2_DEP =~ 1*B1SA24A + l1*B1SA24B + l2*B1SA24D + l3*B1SA24E + l4*B1SA24F
W3_DEP =~ 1*C1SA20A + l1*C1SA20B + l2*C1SA20D + l3*C1SA20E + l4*C1SA20F

# indicator intercepts constrained equal
A1SA13A ~ ai*1
B1SA24A ~ ai*1
C1SA20A ~ ai*1

A1SA13B ~ bi*1
B1SA24B ~ bi*1
C1SA20B ~ bi*1

A1SA13D ~ di*1
B1SA24D ~ di*1
C1SA20D ~ di*1

A1SA13E ~ ei*1
B1SA24E ~ ei*1
C1SA20E ~ ei*1

A1SA13F ~ di*1
B1SA24F ~ di*1
C1SA20F ~ di*1

# residual correlations allowed
A1SA13A ~~ B1SA24A + C1SA20A
B1SA24A ~~ C1SA20A
A1SA13B ~~ B1SA24B + C1SA20B
B1SA24B ~~ C1SA20B
A1SA13D ~~ B1SA24D + C1SA20D
B1SA24D ~~ C1SA20D
A1SA13E ~~ B1SA24E + C1SA20E
B1SA24E ~~ C1SA20E
A1SA13F ~~ B1SA24F + C1SA20F
B1SA24F ~~ C1SA20F
'

# fit model
fit_strong <- cfa(model_strong, data = data, missing = 'fiml')

# fit results
summary(fit_strong, fit.measures = T) # = same as model above but with additional constraints of intercepts of indicators


# ------------------------------------
# 4) strict measurement invariance 
# ------------------------------------

# define model
model_strict <- '

# measurement models with constrained factor loadings
W1_DEP =~ 1*A1SA13A + l1*A1SA13B + l2*A1SA13D + l3*A1SA13E + l4*A1SA13F
W2_DEP =~ 1*B1SA24A + l1*B1SA24B + l2*B1SA24D + l3*B1SA24E + l4*B1SA24F
W3_DEP =~ 1*C1SA20A + l1*C1SA20B + l2*C1SA20D + l3*C1SA20E + l4*C1SA20F

# indicator intercepts constrained equal
A1SA13A ~ ai*1
B1SA24A ~ ai*1
C1SA20A ~ ai*1

A1SA13B ~ bi*1
B1SA24B ~ bi*1
C1SA20B ~ bi*1

A1SA13D ~ di*1
B1SA24D ~ di*1
C1SA20D ~ di*1

A1SA13E ~ ei*1
B1SA24E ~ ei*1
C1SA20E ~ ei*1

A1SA13F ~ fi*1
B1SA24F ~ fi*1
C1SA20F ~ fi*1

# residual correlations allowed
A1SA13A ~~ ac*B1SA24A + ac*C1SA20A
B1SA24A ~~ ac*C1SA20A
A1SA13B ~~ bc*B1SA24B + bc*C1SA20B
B1SA24B ~~ bc*C1SA20B
A1SA13D ~~ dc*B1SA24D + dc*C1SA20D
B1SA24D ~~ dc*C1SA20D
A1SA13E ~~ ec*B1SA24E + ec*C1SA20E
B1SA24E ~~ ec*C1SA20E
A1SA13F ~~ fc*B1SA24F + fc*C1SA20F
B1SA24F ~~ fc*C1SA20F

# residual variances constrained
A1SA13A ~~ ae*A1SA13A
B1SA24A ~~ ae*B1SA24A
C1SA20A ~~ ae*C1SA20A

A1SA13B ~~ be*A1SA13B
B1SA24B ~~ be*B1SA24B
C1SA20B ~~ be*C1SA20B

A1SA13D ~~ de*A1SA13D
B1SA24D ~~ de*B1SA24D
C1SA20D ~~ de*C1SA20D 

A1SA13E ~~ ee*A1SA13E
B1SA24E ~~ ee*B1SA24E
C1SA20E ~~ ee*C1SA20E

A1SA13F ~~ fe*A1SA13F
B1SA24F ~~ fe*B1SA24F
C1SA20F ~~ fe*C1SA20F
'

# fit model
fit_strict <- cfa(model_strict, data = data, missing = 'fiml')

# fit results
summary(fit_strict, fit.measures = T) 


# -------------------------
# 5) Model comparisons 
# -------------------------

# fit measures for each model
fitMeasures(fit_configural, c("rmsea", "cfi", "srmr"))
fitMeasures(fit_weak,       c("rmsea", "cfi", "srmr"))
fitMeasures(fit_strong,     c("rmsea", "cfi", "srmr"))
fitMeasures(fit_strict,     c("rmsea", "cfi", "srmr"))

# model test
anova(fit_configural, fit_weak, fit_strong, fit_strict)

