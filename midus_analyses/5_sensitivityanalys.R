# ===============================================================
# MIDUS: Sensitivity analyses
# (contact maria.bloechl@gmail.com in case of questions)
# ===============================================================

# clean work space
rm(list = ls())

# load packages 
library(lavaan)

# load preprocessed data 
load("data/midus/processed/midus_proc_data.RData")


# -----------------------------------------------------
# 1) Conditional LGM with interaction effect of age  
# -----------------------------------------------------

# define generic model
m.pred.interact <- '

# measurement models with constrained factor loadings
W1_DEP =~ 1*A1SA13A + l1*A1SA13B + l2*A1SA13D + l3*A1SA13E + l4*A1SA13F
W2_DEP =~ 1*B1SA24A + l1*B1SA24B + l2*B1SA24D + l3*B1SA24E + l4*B1SA24F
W3_DEP =~ 1*C1SA20A + l1*C1SA20B + l2*C1SA20D + l3*C1SA20E + l4*C1SA20F

# define latent factor means
W1_DEP ~ 0*1
W2_DEP ~ 0*1
W3_DEP ~ 0*1

# indicator intercepts constrained equal
A1SA13A ~ 0*1
B1SA24A ~ 0*1
C1SA20A ~ 0*1

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

# intercept and slope with fixed coefficients
i =~ 1*W1_DEP + 1*W2_DEP + 1*W3_DEP
s =~ 0*W1_DEP + 1*W2_DEP + 2*W3_DEP

# intercept and slope var, covar
i ~~ i
s ~~ s
i ~~ s

# intercept and slope means
i ~ 1
s ~ 1

# regressions
i ~ A1PAGE_M2.c + A1PRSEX.n + A1SS7.n + A1PB1.n + pred + pred:A1PAGE_M2.c
s ~ A1PAGE_M2.c + A1PRSEX.n + A1SS7.n + A1PB1.n + pred + pred:A1PAGE_M2.c
'


# 3.1) Hypertension
# set hypertension variable for placeholder
data$pred <- data$A1SA9S.n 

# fit model
fit.hyp.interact   <- sem(m.pred.interact, data = data, missing = 'fiml')

# fit results
summary(fit.hyp.interact,  fit.measures = T, standardized = T, ci = T)


# 3.2) Smoking  
# set smoking for placeholder
data$pred <- data$A1PA43.n 

# fit model
fit.smo.interact   <- sem(m.pred.interact, data = data, missing = 'fiml')

# fit results
summary(fit.smo.interact,  fit.measures = T, standardized = T, ci = T)


# 3.3) BMI 
# set BMI for placeholder
data$pred <- data$A1SBMI 

# fit model
fit.bmi.interact   <- sem(m.pred.interact, data = data, missing = 'fiml')

# fit results
summary(fit.bmi.interact,  fit.measures = T, standardized = T, ci = T)


# 3.4) Diabetes
# set diabetes for placeholder
data$pred <- data$A1SA9X.n 

# fit model
fit.dia.interact <- sem(m.pred.interact, data = data, missing = 'fiml')

# fit results
summary(fit.dia.interact,  fit.measures = T, standardized = T, ci = T)


# 3.9) Multiple risk factors (accumulated)
# set number of risk factors for placeholder
data$pred <- data$n_rf 

# fit model
fit.mul.interact   <- sem(m.pred.interact, data = data, missing = 'fiml')

# fit results
summary(fit.mul.interact,  fit.measures = T, standardized = T, ci = T)

