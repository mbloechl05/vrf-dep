# ================================================================
# Cardiovascular Risk and Trajectories of Depressive Symptoms
# Script 31: Latent Growth Models for Affective Symptoms
# ================================================================

# clean work space
rm(list = ls())

# load packages 
library(lavaan)
library(semTools)
library(ggplot2)
library(lattice)

# Load pre-processed data
load("data/elsa/processed/elsa_proc_data.RData")


# --------------------------------------------
# 0) Set up generic plot function and models
# --------------------------------------------

# Please note that this part of the script is generic in a sense that
# the plot function and model definition will be applied to all 
# analysed outcomes. Hence, this part of the script (part 0) *has to be* 
# run before conducting analyses.

# Create basic plot function
# -----------------------------

# This function randonmly selects n participants and plots their 
# values over 4 time points. First, the data are being transformed 
# to a dataframe with a long format.

n <- 500 # set number of participants to 500

plot_long <- function(n, id, w2_var, w3_var, w4_var, w5_var, w6_var, w7_var) {
  data_plot <- data[sample(nrow(data), n), c(id, w2_var, w4_var, w6_var, w7_var)]
  data_plot <- melt(data_plot, id.vars=c(id)) # wide to long format
  ggplot(data = data_plot, aes(x = variable, y = value, group = idauniq)) + 
    geom_point(size = 0.5) +
    geom_line(alpha = 0.3)
}

# Mean depressive symptoms plot
plot_long(n, "idauniq", "w2_aff_sum", "w3_aff_sum", "w4_aff_sum", 
          "w5_aff_sum", "w6_aff_sum", "w7_aff_sum")

# Create panel plot
data_plot <- data[sample(nrow(data), 500), 
                  c("idauniq", "w2_aff_sum", "w3_aff_sum", "w4_aff_sum", "w5_aff_sum", 
                    "w6_aff_sum", "w7_aff_sum")]
data_plot <- melt(data_plot, id.vars=c("idauniq")) # wide to long format

jpeg("plot_affect_time.jpeg", width = 500, height = 5000)
xyplot(value ~ variable | idauniq, data = data_plot,
       #prepanel = function(x, y) prepanel.loess(x, y, family = "gaussian"),
       xlab = "Time", ylab = "Affective Symptoms",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family = "gaussian") 
         },
       as.table = T)
dev.off()


# --------------------------------------
# 1) Unconditional LGM (no predictors)
# --------------------------------------

# Model 1a: Unconditional LGM, linear fit
# -----------------------------------------------

# define model
model_1a_aff <- '

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
'

# fit model
fit_1a_aff <- sem(model_1a_aff, data = data, 
                  ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                              "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                              "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                              "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                              "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                              "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                  missing = "pairwise", estimator = "WLSMV", 
                  parameterization = "theta" )

# summarise fit results
summary(fit_1a_aff,  fit.measures = T, standardized = T)


# Model 1b: Unconditional LGM, non-linear fit
# ----------------------------------------------

model_1b_aff <- '

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
s =~ 0*w2_dep + w*w3_dep + x*w4_dep + y*w5_dep + z*w6_dep + 1*w7_dep

# intercept and slope variances and covariance
i ~~ i
s ~~ s
i ~~ s

# intercept and slope means
i ~ 1
s ~ 1
'

# fit model
fit_1b_aff <- sem(model_1b_aff, data = data, 
                  ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                              "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                              "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                              "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                              "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                              "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                  missing = "pairwise", estimator = "WLSMV", 
                  parameterization = "theta" )

# summarise fit results
summary(fit_1b_aff,  fit.measures = T, standardized = T)

# compare linear against non-linear fit
lavTestLRT(fit_1a_aff, fit_1b_aff)


# --------------------------------------
# 2) Conditional LGM (with covariates)
# --------------------------------------

# Note that in the following models, 'pred' is used as a place holder for the 
# cardiovascular risk variable. Later in this script, we will also use this model to 
# model single risk factors.

# Model 2a: Conditional LGM with demographic covariates + cv risk  
# -----------------------------------------------------------------------

# define model
model_2a_aff <- '

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
i ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_pred + w2_pred_age
s ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_pred + w2_pred_age
'

# set cardiovascular risk variable for placeholders
data$w2_pred     <- data$w2_cvrisk_c
data$w2_pred_age <- data$w2_cvrisk_c*data$w2_age_c 

# fit model
fit_2a_aff <- sem(model_2a_aff, data = data, 
                  ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                              "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                              "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                              "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                              "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                              "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"),
                  missing = "pairwise", estimator = "WLSMV", 
                  parameterization = "theta")

# summarise fit results
summary(fit_2a_aff,  fit.measures = T, standardized = T, ci = T)


# 2.2) Model 2b: Conditional LGM with demographic covariates + cv risk + adl 
# ---------------------------------------------------------------------------

# define model
model_2b_aff <- '

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
i ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_adl + w2_pred + w2_pred_age 
s ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_adl + w2_pred + w2_pred_age
'

# set cardiovascular risk variable for placeholder
data$w2_pred     <- data$w2_cvrisk_c
data$w2_pred_age <- data$w2_cvrisk_c*data$w2_age_c 

# fit model
fit_2b_aff <- sem(model_2b_aff, data = data, 
                  ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                              "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                              "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                              "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                              "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                              "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                  missing = "pairwise", estimator = "WLSMV", 
                  parameterization = "theta")

# summarise fit results
summary(fit_2b_aff,  fit.measures = T, standardized = T, ci = T)


# --------------------
# 3) Create figures
# --------------------







# 
# # 5.1) Figure 2 F-I: Single risk factors
# 
# # create get_parameters() function to extract parameters from the simple and the full model
# get_parameters <- function(fit.simple, i.simple, s.simple, i.all, s.all){
#   beta         <- standardizedSolution(fit.simple)$est.std[c(i.simple, s.simple)]
#   ci.l         <- standardizedSolution(fit.simple)$ci.lower[c(i.simple, s.simple)]
#   ci.u         <- standardizedSolution(fit.simple)$ci.upper[c(i.simple, s.simple)]
#   coeff.simple <- data.frame(beta, ci.l, ci.u)
#   beta         <- standardizedSolution(fit.all)$est.std[c(i.all, s.all)]
#   ci.l         <- standardizedSolution(fit.all)$ci.lower[c(i.all, s.all)]
#   ci.u         <- standardizedSolution(fit.all)$ci.upper[c(i.all, s.all)]
#   coeff.all    <- data.frame(beta, ci.l, ci.u)
#   
#   coeff           <- rbind(coeff.simple, coeff.all)
#   coeff$parameter <- c("Intercept", "Slope", "Intercept", "Slope")
#   coeff$model     <- c("1", "1", "2", "2") #1 = Simple, 2 = All
#   
#   return(coeff)
# }
# 
# # create get_plot() function to plot models with both model results
# # note that plot is automatically saved using ggsave 
# get_plot <- function(coeffs, title) {
#   ggplot(coeffs, aes(x = parameter, y = beta, ymin = ci.l, ymax = ci.u)) +
#     geom_hline(yintercept = 0, linetype = "dashed", size = 0.4) +
#     geom_errorbar(aes(color = model), width = 0.2, size = 1.1, 
#                   position = position_dodge(0.3)) +
#     geom_point(aes(color = model), size = 3.8, position = position_dodge(0.3))+
#     ggtitle(title) +
#     labs(x = "", y = "Standard. beta (95% CI)") +
#     scale_y_continuous(limits = c(-0.3, 0.6)) +
#     theme_classic(base_size = 19) +
#     theme(axis.text  = element_text(colour = "black", size = 19), 
#           axis.title = element_text(colour = "black", size = 19), 
#           plot.title = element_text(hjust = 0.5, face = "bold", size = 19), 
#           legend.position = "none") +
#     scale_color_manual(values=c('#18345C','#AC95BF'), 
#                        name="",
#                        breaks=c("1", "2"),
#                        labels=c("One predictor", "All predictors"))
# }
# 
# 
# # now create and save plots using functions
# # hypertension
# coeff.hyp <- get_parameters(fit.simple = fit.hyp, i.simple = 193, s.simple = 198, 
#                             i.all = 193, s.all = 201)
# p1 <- get_plot(coeffs = coeff.hyp, title = "Hypertension")
# 
# # diabetes
# coeff.dia <- get_parameters(fit.simple = fit.dia, i.simple = 193, s.simple = 198, 
#                             i.all = 196, s.all = 204)
# p2<- get_plot(coeffs = coeff.dia, title = "Diabetes") 
# 
# # smoking
# coeff.smo <- get_parameters(fit.simple = fit.smo, i.simple = 193, s.simple = 198, 
#                             i.all = 194, s.all = 202)
# p3 <- get_plot(coeffs = coeff.smo, title = "Current smoking") 
# 
# # bmi
# coeff.bmi <- get_parameters(fit.simple = fit.bmi, i.simple = 193, s.simple = 198, 
#                             i.all = 195, s.all = 203)
# p4 <- get_plot(coeffs = coeff.bmi, title = "Body mass index")
# 
# 
# # 5.2) Figure 2J: Multiple risk factors 
# 
# # create get_parameters_s() function to extract parameters from the simple model
# get_parameters_s  <- function(fit.simple, i.simple, s.simple, i.all, s.all){
#   beta            <- standardizedSolution(fit.simple)$est.std[c(i.simple, s.simple)]
#   ci.l            <- standardizedSolution(fit.simple)$ci.lower[c(i.simple, s.simple)]
#   ci.u            <- standardizedSolution(fit.simple)$ci.upper[c(i.simple, s.simple)]
#   coeff.simple    <- data.frame(beta, ci.l, ci.u)
#   coeff.simple$parameter <- c("Intercept", "Slope")
#   return(coeff.simple)
# }
# 
# # create get_plot_s() function to plot models with one model result
# # note that plot is automatically saved using ggsave 
# get_plot_s <- function(coeffs, title) {
#   ggplot(coeffs, aes(x = parameter, y = beta, ymin = ci.l, ymax = ci.u)) +
#     geom_hline(yintercept = 0, linetype = "dashed", size = 0.4) +
#     geom_errorbar(width = 0.1, size = 1.1, colour = '#18345C') +
#     geom_point(size = 3.8, colour = '#18345C')+
#     ggtitle(title) +
#     labs(x = "", y = "Standard. beta (95% CI)") +
#     scale_y_continuous(limits = c(-0.4, 0.6)) +
#     theme_classic(base_size = 19) +
#     theme(axis.text  = element_text(colour = "black", size = 19), 
#           axis.title = element_text(colour = "black", size = 19), 
#           plot.title = element_text(hjust = 0.5, face = "bold", size = 19), 
#           legend.position = "none")
# }
# 
# 
# # now create and save plot using functions
# coeff.mul <- get_parameters_s(fit.simple = fit.mul, i.simple = 193, s.simple = 198)
# p5 <- get_plot_s(coeffs = coeff.mul, title = "Multiple risk factors")
# 
# 
# # 5.3) Combine and save plots
# ggarrange(p1, p2, p3, p4, p5,
#           labels = c("F", "G", "H", "I", "J"),
#           font.label = list(size = 26),
#           hjust = -0.2, vjust = 1,
#           ncol = 5, nrow = 1)
# 
# ggsave("output/elsa/combined_elsa.png", dpi = 600, width = 20, height = 4)




# --------------------------------------------------------------------------
# 4) Supplement: Conditional LGM (with single risk factors as covariates)
# --------------------------------------------------------------------------

# 4.1) Hypertension
# --------------------

# set hypertension variable for placeholder
data$w2_pred     <- data$w2_hypt
data$w2_pred_age <- data$w2_hypt*data$w2_age_c 

# model 2a: with hypertension as predictor
fit_hypt_2a_aff <- sem(model_2a_aff, data = data, 
                       ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                   "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                   "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                   "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                   "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                   "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                       missing = "pairwise", estimator = "WLSMV", 
                       parameterization = "theta")
summary(fit_hypt_2a_aff, fit.measures = T, standardized = T, ci = T)

# model 2b: with hypertension as predictor
fit_hypt_2b_aff <- sem(model_2b_aff, data = data, 
                       ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                   "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                   "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                   "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                   "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                   "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                       missing = "pairwise", estimator = "WLSMV", 
                       parameterization = "theta")
summary(fit_hypt_2b_aff, fit.measures = T, standardized = T, ci = T)


# 4.2) Diabetes
# ---------------

# set diabetes variable for placeholder
data$w2_pred     <- data$w2_diab
data$w2_pred_age <- data$w2_diab*data$w2_age_c 

# model 2a: with diabetes as predictor
fit_diab_2a_aff <- sem(model_2a_aff, data = data, 
                       ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                   "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                   "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                   "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                   "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                   "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                       missing = "pairwise", estimator = "WLSMV", 
                       parameterization = "theta")
summary(fit_diab_2a_aff, fit.measures = T, standardized = T, ci = T)

# model 2b: with diabetes as predictor
fit_diab_2b_aff <- sem(model_2b_aff, data = data, 
                       ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                   "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                   "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                   "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                   "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                   "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                       missing = "pairwise", estimator = "WLSMV", 
                       parameterization = "theta")
summary(fit_diab_2b_aff, fit.measures = T, standardized = T, ci = T)


# 4.4) Smoking  
# --------------

# set smoking variable for placeholder
data$w2_pred     <- data$w2_smok
data$w2_pred_age <- data$w2_smok*data$w2_age_c 

# model 2a: with smoking as predictor
fit_smok_2a_aff <- sem(model_2a_aff, data = data, 
                       ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                   "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                   "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                   "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                   "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                   "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                       missing = "pairwise", estimator = "WLSMV", 
                       parameterization = "theta")
summary(fit_smok_2a_aff, fit.measures = T, standardized = T, ci = T)

# model 2b: with smoking as predictor
fit_smok_2b_aff <- sem(model_2b_aff, data = data, 
                       ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                   "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                   "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                   "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                   "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                   "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                       missing = "pairwise", estimator = "WLSMV", 
                       parameterization = "theta")
summary(fit_smok_2b_aff, fit.measures = T, standardized = T, ci = T)


# 4.5) Obesity 
# ---------------

# set obesity variable for placeholder
data$w2_pred     <- data$w2_obese
data$w2_pred_age <- data$w2_obese*data$w2_age_c 

# model 2a: with obesity as predictor
fit_obese_2a_aff <- sem(model_2a_aff, data = data, 
                        ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                    "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                    "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                    "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                    "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                    "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                        missing = "pairwise", estimator = "WLSMV", 
                        parameterization = "theta")
summary(fit_obese_2a_aff,  fit.measures = T, standardized = T, ci = T) 

# model 2b: with obesity as predictor
fit_obese_2b_aff <- sem(model_2b_aff, data = data, 
                        ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                    "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                    "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                    "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                    "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                    "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                        missing = "pairwise", estimator = "WLSMV", 
                        parameterization = "theta")
summary(fit_obese_2b_aff, fit.measures = T, standardized = T, ci = T) 


# 4.6) Hypercholesterolemia 
# --------------------------

# set cholesterol variable for placeholder
data$w2_pred     <- data$w2_hchol
data$w2_pred_age <- data$w2_hchol*data$w2_age_c 

# model 2a: with cholesterol as predictor
fit_hchol_2a_aff <- sem(model_2a_aff, data = data, 
                        ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                    "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                    "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                    "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                    "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                    "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                        missing = "pairwise", estimator = "WLSMV", 
                        parameterization = "theta")
summary(fit_hchol_2a_aff, fit.measures = T, standardized = T, ci = T) 

# model 2b: with cholesterol as predictor
fit_hchol_2b_aff <- sem(model_2b_aff, data = data, 
                        ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                    "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                    "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                    "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                    "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                    "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                        missing = "pairwise", estimator = "WLSMV", 
                        parameterization = "theta")
summary(fit_hchol_2b_aff, fit.measures = T, standardized = T, ci = T) 

