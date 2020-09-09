# ==========================================================
# Cardiovascular Risk and Trajectories of Depressed Mood
# Script 2: Measurement Invariance 
# ==========================================================

# clean work space
rm(list = ls())

# load packages 
library(lavaan)

# Load pre-processed data
load("data/elsa/processed/elsa_proc_data.RData")


# ------------------------
# 1) Baseline Model
# ------------------------

# define model 
model_baseline_aff <- '

# measurement models without constrained factor loadings

w2_dep =~ 1*w2_psceda + w2_pscedd + w2_pscede + w2_pscedf + w2_pscedg
w3_dep =~ 1*w3_psceda + w3_pscedd + w3_pscede + w3_pscedf + w3_pscedg
w4_dep =~ 1*w4_psceda + w4_pscedd + w4_pscede + w4_pscedf + w4_pscedg
w5_dep =~ 1*w5_psceda + w5_pscedd + w5_pscede + w5_pscedf + w5_pscedg
w6_dep =~ 1*w6_psceda + w6_pscedd + w6_pscede + w6_pscedf + w6_pscedg
w7_dep =~ 1*w7_psceda + w7_pscedd + w7_pscede + w7_pscedf + w7_pscedg

# define tresholds
w2_psceda | a*t1
w3_psceda | a*t1
w4_psceda | a*t1
w5_psceda | a*t1
w6_psceda | a*t1
w7_psceda | a*t1

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
w3_dep ~ 1
w4_dep ~ 1
w5_dep ~ 1
w6_dep ~ 1
w7_dep ~ 1

# define unique variance terms
w2_psceda ~~ 1*w2_psceda
w3_psceda ~~ 1*w3_psceda
w4_psceda ~~ 1*w4_psceda
w5_psceda ~~ 1*w5_psceda
w6_psceda ~~ 1*w6_psceda
w7_psceda ~~ 1*w7_psceda

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

w2_pscedf ~~ 1*w2_pscedf
w3_pscedf ~~ w3_pscedf
w4_pscedf ~~ w4_pscedf
w5_pscedf ~~ w5_pscedf
w6_pscedf ~~ w6_pscedf
w7_pscedf ~~ w7_pscedf

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
'

# fit model
fit_baseline_aff <- 
  cfa(model_baseline_aff, data = data, 
      ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                  "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                  "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                  "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                  "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                  "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
      missing = "pairwise", estimator = "WLSMV", 
      parameterization = "theta" )

# summarise fit results
summary(fit_baseline_aff, fit.measures = T)    


# -----------------------------
# 2) Loading Invariance Model
# -----------------------------

# define model
model_loading_aff <- '

# measurement models with constrained factor loadings
w2_dep =~ 1*w2_psceda + l1*w2_pscedd + l2*w2_pscede + l3*w2_pscedf + l4*w2_pscedg
w3_dep =~ 1*w3_psceda + l1*w3_pscedd + l2*w3_pscede + l3*w3_pscedf + l4*w3_pscedg
w4_dep =~ 1*w4_psceda + l1*w4_pscedd + l2*w4_pscede + l3*w4_pscedf + l4*w4_pscedg
w5_dep =~ 1*w5_psceda + l1*w5_pscedd + l2*w5_pscede + l3*w5_pscedf + l4*w5_pscedg
w6_dep =~ 1*w6_psceda + l1*w6_pscedd + l2*w6_pscede + l3*w6_pscedf + l4*w6_pscedg
w7_dep =~ 1*w7_psceda + l1*w7_pscedd + l2*w7_pscede + l3*w7_pscedf + l4*w7_pscedg

# define tresholds
w2_psceda | a*t1
w3_psceda | a*t1
w4_psceda | a*t1
w5_psceda | a*t1
w6_psceda | a*t1
w7_psceda | a*t1

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
w3_dep ~ 1
w4_dep ~ 1
w5_dep ~ 1
w6_dep ~ 1
w7_dep ~ 1

# define unique variance terms
w2_psceda ~~ 1*w2_psceda
w3_psceda ~~ 1*w3_psceda
w4_psceda ~~ 1*w4_psceda
w5_psceda ~~ 1*w5_psceda
w6_psceda ~~ 1*w6_psceda
w7_psceda ~~ 1*w7_psceda

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

w2_pscedf ~~ 1*w2_pscedf
w3_pscedf ~~ w3_pscedf
w4_pscedf ~~ w4_pscedf
w5_pscedf ~~ w5_pscedf
w6_pscedf ~~ w6_pscedf
w7_pscedf ~~ w7_pscedf

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
'

# fit model
fit_loading_aff <- 
  cfa(model_loading_aff, data = data, 
      ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                  "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                  "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                  "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                  "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                  "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"),
      missing = "pairwise", estimator = "WLSMV", 
      parameterization = "theta" )

# summarise fit results
summary(fit_loading_aff, fit.measures = T)     


# ------------------------------------
# 3) Unique Factor Invariance Model
# -------------------------------------

# define model
model_ufactor_aff <- '

# measurement models with constrained factor loadings
w2_dep =~ 1*w2_psceda + l1*w2_pscedd + l2*w2_pscede + l3*w2_pscedf + l4*w2_pscedg
w3_dep =~ 1*w3_psceda + l1*w3_pscedd + l2*w3_pscede + l3*w3_pscedf + l4*w3_pscedg
w4_dep =~ 1*w4_psceda + l1*w4_pscedd + l2*w4_pscede + l3*w4_pscedf + l4*w4_pscedg
w5_dep =~ 1*w5_psceda + l1*w5_pscedd + l2*w5_pscede + l3*w5_pscedf + l4*w5_pscedg
w6_dep =~ 1*w6_psceda + l1*w6_pscedd + l2*w6_pscede + l3*w6_pscedf + l4*w6_pscedg
w7_dep =~ 1*w7_psceda + l1*w7_pscedd + l2*w7_pscede + l3*w7_pscedf + l4*w7_pscedg

# define tresholds
w2_psceda | a*t1
w3_psceda | a*t1
w4_psceda | a*t1
w5_psceda | a*t1
w6_psceda | a*t1
w7_psceda | a*t1

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
w3_dep ~ 1
w4_dep ~ 1
w5_dep ~ 1
w6_dep ~ 1
w7_dep ~ 1

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
'

# fit model
fit_ufactor_aff <- 
  cfa(model_ufactor_aff, data = data, 
      ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                  "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                  "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                  "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                  "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                  "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"),
      missing = "pairwise", estimator = "WLSMV", 
      parameterization = "theta" )

# summarise fit results
summary(fit_ufactor_aff, fit.measures = T)   


# -----------------------
# 4) Model comparisons 
# -----------------------

# show fit measures for each model
fitMeasures(fit_baseline_aff, c("rmsea.scaled", "cfi.scaled", "srmr"))
fitMeasures(fit_loading_aff,  c("rmsea.scaled", "cfi.scaled", "srmr"))
fitMeasures(fit_ufactor_aff,  c("rmsea.scaled", "cfi.scaled", "srmr"))

# perform model comparison
anova(fit_baseline_aff, fit_loading_aff, fit_ufactor_aff)


