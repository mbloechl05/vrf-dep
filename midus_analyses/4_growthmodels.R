# ===============================================================
# MIDUS: Fitting latent growth models (LGMs)
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


# --------------------------------------------------------
# 1) Unconditional LGM without covariates or predictors
# --------------------------------------------------------

# define model
m.uc <- '

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

# define latent factor means
W1_DEP ~ 0*1
W2_DEP ~ 0*1
W3_DEP ~ 0*1

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

# intercept and slope with fixed coefficients
i =~ 1*W1_DEP + 1*W2_DEP + 1*W3_DEP
s =~ 0*W1_DEP + 1*W2_DEP + 2*W3_DEP

# intercept and slope var, covar
i ~~ i
s ~~ s
i ~~ s

# intercept and slope means
i ~ 0*1
s ~ 1
'

# fit model
fit.uc <- sem(m.uc, data = data, missing = 'fiml')

# fit results
summary(fit.uc,  fit.measures = T, standardized = T)


# --------------------------------------------------------
# 2) Conditional LGM with covariates (but no predictors)
# --------------------------------------------------------

# define model
m.cov <- '

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

# define latent factor means
W1_DEP ~ 0*1
W2_DEP ~ 0*1
W3_DEP ~ 0*1

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

# intercept and slope with fixed coefficients
i =~ 1*W1_DEP + 1*W2_DEP + 1*W3_DEP
s =~ 0*W1_DEP + 1*W2_DEP + 2*W3_DEP

# intercept and slope var, covar
i ~~ i
s ~~ s
i ~~ s

# intercept and slope means
i ~ 0*1
s ~ 1

# regressions
i ~ A1PAGE_M2.c + A1PRSEX.n + A1SS7.n + A1PB1.n 
s ~ A1PAGE_M2.c + A1PRSEX.n + A1SS7.n + A1PB1.n 
'

# fit model 
fit.cov <- sem(m.cov, data = data, missing = 'fiml')

# fit results
summary(fit.cov,  fit.measures = T, standardized = T)


# ----------------------------------------------------------
# 3) Conditional LGMs with single vascular risk factors 
# ----------------------------------------------------------

# The following model is an extension of the previous model with one additional predictor ("pred"). 
# It is coded as a generic model. That is "pred" is used as a place holder for vascular risk factors
# that will be introduced successively.

# define generic model
m.pred <- '

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

# intercept and slope with fixed coefficients
i =~ 1*W1_DEP + 1*W2_DEP + 1*W3_DEP
s =~ 0*W1_DEP + 1*W2_DEP + 2*W3_DEP

i ~~ i
s ~~ s
i ~~ s

i ~ 0*1
s ~ 1

W1_DEP ~ 0*1
W2_DEP ~ 0*1
W3_DEP ~ 0*1

# regressions
i ~ A1PAGE_M2.c + A1PRSEX.n + A1SS7.n + A1PB1.n + pred
s ~ A1PAGE_M2.c + A1PRSEX.n + A1SS7.n + A1PB1.n + pred
'


# 3.1) Hypertension
# set hypertension variable for placeholder
data$pred <- data$A1SA9S.n 

# fit model
fit.hyp   <- sem(m.pred, data = data, missing = 'fiml')

# fit results
summary(fit.hyp,  fit.measures = T, standardized = T)


# 3.2) Smoking  
# set smoking for placeholder
data$pred <- data$A1PA43.n 

# fit model
fit.smo   <- sem(m.pred, data = data, missing = 'fiml')

# fit results
summary(fit.smo,  fit.measures = T, standardized = T)


# 3.3) BMI 
# set BMI for placeholder
data$pred <- data$A1SBMI 

# fit model
fit.bmi   <- sem(m.pred, data = data, missing = 'fiml')

# fit results
summary(fit.bmi,  fit.measures = T, standardized = T)


# 3.4) Diabetes
# set diabetes for placeholder
data$pred <- data$A1SA9X.n 

# fit model
fit.dia <- sem(m.pred, data = data, missing = 'fiml')

# fit results
summary(fit.dia,  fit.measures = T, standardized = T)


# 3.9) Multiple risk factors (accumulated)
# set number of risk factors for placeholder
data$pred <- data$n_rf 

# fit model
fit.mul   <- sem(m.pred, data = data, missing = 'fiml')

# fit results
summary(fit.mul,  fit.measures = T, standardized = T)


# ----------------------------------------------------------
# 4) Conditional LGM with all risk factors simultaneously
# ----------------------------------------------------------

# define model
m.all <- '

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

# intercept and slope with fixed coefficients
i =~ 1*W1_DEP + 1*W2_DEP + 1*W3_DEP
s =~ 0*W1_DEP + 1*W2_DEP + 2*W3_DEP

i ~~ i
s ~~ s
i ~~ s

i ~ 0*1
s ~ 1

W1_DEP ~ 0*1
W2_DEP ~ 0*1
W3_DEP ~ 0*1

# regressions
i ~ A1PAGE_M2.c + A1PRSEX.n + A1SS7.n + A1PB1.n + A1PA43.n + A1SA9S.n + A1SBMI + A1SA9X.n
s ~ A1PAGE_M2.c + A1PRSEX.n + A1SS7.n + A1PB1.n + A1PA43.n + A1SA9S.n + A1SBMI + A1SA9X.n
'

# fit model
fit.all <- sem(m.all, data = data, missing = 'fiml')

# fit results
summary(fit.all,  fit.measures = T, standardized = T)


# --------------------
# 5) Create figures
# --------------------

# 5.1) Figure 2 A-D: Single risk factors

# create get_parameters() function to extract parameters from the simple and the full model
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

# create get_plot() function to plot models with both model results
# note that plot is automatically saved using ggsave 
get_plot <- function(coeffs, title) {
  ggplot(coeffs, aes(x = parameter, y = beta, ymin = ci.l, ymax = ci.u)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.4) +    
    geom_errorbar(aes(color = model), width = 0.1, size = 1, position = position_dodge(0.3)) +
    geom_point(aes(color = model), size = 3.5, position = position_dodge(0.3), alpha = 1)+
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
  ggsave(paste("output/midus/", title, ".pdf", sep = ""), width = 4, height = 4)
}

# now create and save plots using functions
# hypertension
coeff.hyp <- get_parameters(fit.simple = fit.hyp, i.simple = 79, s.simple = 84, i.all = 80, s.all = 88)
get_plot(coeffs = coeff.hyp, title = "Hypertension")

# smoking
coeff.smo <- get_parameters(fit.simple = fit.smo, i.simple = 79, s.simple = 84, i.all = 79, s.all = 87)
get_plot(coeffs = coeff.smo, title = "Current smoking") # A1PA43.n

# bmi
coeff.bmi <- get_parameters(fit.simple = fit.bmi, i.simple = 79, s.simple = 84, i.all = 81, s.all = 89)
get_plot(coeffs = coeff.bmi, title = "Body mass index")

# diabetes
coeff.dia <- get_parameters(fit.simple = fit.dia, i.simple = 79, s.simple = 84, i.all = 82, s.all = 90)
get_plot(coeffs = coeff.dia, title = "Diabetes") # A1SA9X.n


# 5.2) Figure 2 E: Multiple risk factors 

# create get_parameters_s() function to extract parameters from the simple model
get_parameters_s  <- function(fit.simple, i.simple, s.simple, i.all, s.all){
  beta            <- standardizedSolution(fit.simple)$est.std[c(i.simple, s.simple)]
  ci.l            <- standardizedSolution(fit.simple)$ci.lower[c(i.simple, s.simple)]
  ci.u            <- standardizedSolution(fit.simple)$ci.upper[c(i.simple, s.simple)]
  coeff.simple    <- data.frame(beta, ci.l, ci.u)
  coeff.simple$parameter <- c("Intercept", "Slope")
  return(coeff.simple)
}

# create get_plot_s() function to plot models with one model result
# note that plot is automatically saved using ggsave 
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
  ggsave(paste("output/midus", title, ".pdf", sep = ""), dpi = 600, width = 4, height = 4)
}

# now create and save plot using functions
coeff.mul <- get_parameters_s(fit.simple = fit.mul, i.simple = 79, s.simple = 84)
get_plot_s(coeffs = coeff.mul, title = "Multiple risk factors")
