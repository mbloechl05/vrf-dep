# =========================================================
# Cardiovascular Risk and Trajectories of Depressed Mood
# Script 3: Latent Growth Models 
# =========================================================

# clean work space
rm(list = ls())

# load packages 
library(lavaan)
library(semTools)
library(ggplot2)
library(ggthemes)
library(lattice)
library(tidyr) # for drop_na()
library(lemon) # for facet_wrap_grid()
library(reshape) # for melt()
library(car) # for recode()

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
summary(fit_1a_aff,  fit.measures = T, standardized = T, ci = T)


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

# define model
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
i ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_adl_c + w2_pred + w2_pred_age 
s ~ w2_age_c + w2_sex + w2_eth + w0_edu + w2_adl_c + w2_pred + w2_pred_age
'

# set cardiovascular risk variable for placeholder
data$w2_pred     <- data$w2_cvrisk_c
data$w2_pred_age <- data$w2_cvrisk_c*data$w2_age_c 

# fit model
fit_2_aff <- sem(model_2_aff, data = data, 
                  ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                              "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                              "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                              "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                              "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                              "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                  missing = "pairwise", estimator = "WLSMV", 
                  parameterization = "theta")

# summarise fit results
summary(fit_2_aff,  fit.measures = T, standardized = T, ci = T)


# --------------------
# 3) Create figures
# --------------------

# 3.1) First, have to create data matrix of predicted mean trajectories
# -----------------------------------------------------------------------

# create age and vrf variable
age <- c(-15.62, -5.62, 4.38, 14.38, 
         -15.62, -5.62, 4.38, 14.38, 
#         -15.62, -5.62, 4.38, 14.38, 
         -15.62, -5.62, 4.38, 14.38) # age is cenetered so 0 = 65.62y
vrf <- c(-1.46, -1.46, -1.46, -1.46, 
#         -0.46, -0.46, -0.46, -0.46,
          0.54,  0.54,  0.54,  0.54, 
#          1.54,  1.54,  1.54,  1.54, 
#          2.54,  2.54,  2.54,  2.54, 
          3.54,  3.54,  3.54,  3.54) # vrf variable is centered so 0 = 1.46

# put variables into one data frame
plot_frame <- data.frame(age, vrf)

# create interaction variable just fyi
plot_frame$age_vrf <- plot_frame$age*plot_frame$vrf

# now add time information by duplicating data frames and adding time variables
plot_frame_2 <- plot_frame
plot_frame_3 <- plot_frame
plot_frame_4 <- plot_frame
plot_frame_5 <- plot_frame
plot_frame_6 <- plot_frame
plot_frame_7 <- plot_frame
plot_frame_2$time <- 2
plot_frame_3$time <- 3
plot_frame_4$time <- 4
plot_frame_5$time <- 5
plot_frame_6$time <- 6
plot_frame_7$time <- 7

# recombine information again into one data frame
plot_frame <- rbind(plot_frame_2, plot_frame_3, plot_frame_4, plot_frame_5, 
                    plot_frame_6, plot_frame_7)

# calculate predicted values for affective symptoms
plot_frame$y <- (-1.842 - 0.001*plot_frame$age + 0.076*plot_frame$vrf + 
                   -0.007*plot_frame$vrf*plot_frame$age) + 
                ((-0.072 + 0.005*plot_frame$age + 0.004*plot_frame$vrf)*plot_frame$time)

# make some variables factors
plot_frame$age <- as.factor(plot_frame$age)
plot_frame$vrf <- as.factor(plot_frame$vrf)

# change names for facet labels
age_labs <- c("50 years", "60 years", "70 years", "80 years")
names(age_labs) <- c("-15.62", "-5.62", "4.38", "14.38")


# 3.2) Now extract and add individual predicted trajectories 
# ------------------------------------------------------------

# to plot predicted trajectories for indiv, first drop data with missing values
data_pred <- data %>% 
  drop_na(c(w2_age_c, w2_sex, w2_eth, w0_edu, w2_cvrisk_c))

# and merge with predicted values
pred_2_aff <- predict(fit_2_aff)
data_pred  <- cbind(data_pred, pred_2_aff)

# extract values for people aged 52, 60, 70, and 80
data_pred_50 <- subset(data_pred, w2_age == 52, select=c(w2_age_c, w2_sex, i, s))
data_pred_60 <- subset(data_pred, w2_age == 60, select=c(w2_age_c, w2_sex, i, s))
data_pred_70 <- subset(data_pred, w2_age == 70, select=c(w2_age_c, w2_sex, i, s))
data_pred_80 <- subset(data_pred, w2_age == 80, select=c(w2_age_c, w2_sex, i, s))

# rename columns
names(data_pred_50)[names(data_pred_50) == "w2_age_c"] <- "age"
names(data_pred_60)[names(data_pred_60) == "w2_age_c"] <- "age"
names(data_pred_70)[names(data_pred_70) == "w2_age_c"] <- "age"
names(data_pred_80)[names(data_pred_80) == "w2_age_c"] <- "age"

# recode age variable
data_pred_50$age <- -15.62
data_pred_60$age <-  -5.62
data_pred_70$age <-   4.38
data_pred_80$age <-  14.38

# create id variable
data_pred_50$id <- c(1:57)
data_pred_60$id <- c(1:240)
data_pred_70$id <- c(1:184)
data_pred_80$id <- c(1:79)

# create new values over time
data_pred_50$s3 <- data_pred_50$i + 1*data_pred_50$s
data_pred_50$s4 <- data_pred_50$i + 2*data_pred_50$s
data_pred_50$s5 <- data_pred_50$i + 3*data_pred_50$s
data_pred_50$s6 <- data_pred_50$i + 4*data_pred_50$s
data_pred_50$s7 <- data_pred_50$i + 5*data_pred_50$s

data_pred_60$s3 <- data_pred_60$i + 1*data_pred_60$s
data_pred_60$s4 <- data_pred_60$i + 2*data_pred_60$s
data_pred_60$s5 <- data_pred_60$i + 3*data_pred_60$s
data_pred_60$s6 <- data_pred_60$i + 4*data_pred_60$s
data_pred_60$s7 <- data_pred_60$i + 5*data_pred_60$s

data_pred_70$s3 <- data_pred_70$i + 1*data_pred_70$s
data_pred_70$s4 <- data_pred_70$i + 2*data_pred_70$s
data_pred_70$s5 <- data_pred_70$i + 3*data_pred_70$s
data_pred_70$s6 <- data_pred_70$i + 4*data_pred_70$s
data_pred_70$s7 <- data_pred_70$i + 5*data_pred_70$s

data_pred_80$s3 <- data_pred_80$i + 1*data_pred_80$s
data_pred_80$s4 <- data_pred_80$i + 2*data_pred_80$s
data_pred_80$s5 <- data_pred_80$i + 3*data_pred_80$s
data_pred_80$s6 <- data_pred_80$i + 4*data_pred_80$s
data_pred_80$s7 <- data_pred_80$i + 5*data_pred_80$s

# delete slope column
data_pred_50$s <- NULL
data_pred_60$s <- NULL
data_pred_70$s <- NULL
data_pred_80$s <- NULL

# now melt data frames
data_pred_50 <- reshape2::melt(data_pred_50[c(1:50),], id = c("id", "age", "w2_sex"))
data_pred_60 <- reshape2::melt(data_pred_60[c(1:50),], id = c("id", "age", "w2_sex"))
data_pred_70 <- reshape2::melt(data_pred_70[c(1:50),], id = c("id", "age", "w2_sex"))
data_pred_80 <- reshape2::melt(data_pred_80[c(1:50),], id = c("id", "age", "w2_sex"))

# rename columns
names(data_pred_50) <- c("id", "age", "sex", "time", "y")
names(data_pred_60) <- c("id", "age", "sex","time", "y")
names(data_pred_70) <- c("id", "age", "sex", "time", "y")
names(data_pred_80) <- c("id", "age", "sex", "time", "y")

# now create new variables
data_pred_50$vrf     <- rep(NA)
data_pred_50$age_vrf <- rep(NA)
data_pred_60$vrf     <- rep(NA)
data_pred_60$age_vrf <- rep(NA)
data_pred_70$vrf     <- rep(NA)
data_pred_70$age_vrf <- rep(NA)
data_pred_80$vrf     <- rep(NA)
data_pred_80$age_vrf <- rep(NA)

# recode time variable
data_pred_50$time <- as.numeric(data_pred_50$time)
data_pred_60$time <- as.numeric(data_pred_60$time)
data_pred_70$time <- as.numeric(data_pred_70$time)
data_pred_80$time <- as.numeric(data_pred_80$time)

# add one to each value to get correct time mapping
data_pred_50$time <- data_pred_50$time + 1
data_pred_60$time <- data_pred_60$time + 1
data_pred_70$time <- data_pred_70$time + 1
data_pred_80$time <- data_pred_80$time + 1

# reorder columns
data_pred_50 <- data_pred_50[, c("id", "age", "sex", "vrf", "age_vrf", "time", "y")]
data_pred_60 <- data_pred_60[, c("id", "age", "sex", "vrf", "age_vrf", "time", "y")]
data_pred_70 <- data_pred_70[, c("id", "age", "sex", "vrf", "age_vrf", "time", "y")]
data_pred_80 <- data_pred_80[, c("id", "age", "sex", "vrf", "age_vrf", "time", "y")]

# now merge all data frames
plot_frame_l <- rbind(data_pred_50, data_pred_60, data_pred_70, data_pred_80)

# make some variables factors
plot_frame_l$age <- as.factor(plot_frame_l$age)


# 3.3) Finally, create plot
# ---------------------------

# text_10 <- data.frame(
#   label = c("10th", "10th", "10th", "10th"),
#   age   = factor(c(-15.62, -5.62, 4.38, 14.38)),
#   y     = c(-1.2, -0.4, 0.4, 1.4),
#   x     = c(7, 7, 7, 7)
# )


ggplot() +
  geom_line(aes(time, y, group = id), data = plot_frame_l, colour = "grey80", size = 0.2, alpha = 0.4) +
  geom_line(aes(time, y, linetype = vrf), data = plot_frame, size = 0.6, colour = '#254D60') +
#  geom_line(aes(time, y, group = id), data = perc_frame, linetype = 2) +
  facet_rep_grid(.~ age, 
                 labeller = labeller(age = age_labs)) + 
#  geom_text(data = text_10, mapping = aes(x = x, y = y, label = label), 
#            size = 3.6, family = "serif") +
  scale_x_continuous(name = "Years since baseline", breaks = c(2,3,4,5,6,7), 
                     labels = c(0,2,4,6,8,10), limits = c(1.7, 7.3),
                     expand = c(0,0)) + 
  scale_y_continuous(name = "Depressed mood", limits = c(-4.4,2), 
                     expand = c(0,0)) +
  geom_segment(aes_all(c('x', 'y', 'xend', 'yend')),
               data = data.frame(x = c(1.7, 2), xend = c(1.7, 7), 
                                 y = c(-4,-4.4), yend = c(2, -4.4))) +
  theme_tufte(base_size = 14) + 
  scale_linetype_manual(values= c(3, 2, 1), 
                        name = "",
                        breaks = c("-1.46", "0.54", "3.54"),
                        labels = c("0 Vascular risk factors", "2 Vascular risk factors", 
                                   "5 Vascular risk factors")) +
  theme(axis.text  = element_text(colour = "black", size = 15), 
        axis.title = element_text(colour = "black", size = 15), 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
        strip.text.x = element_text(size = 15, face = "bold"),
        strip.background = element_rect(color = NA, fill = NA),
        legend.position = c(0.92,0.11), 
        legend.background = element_rect(fill = "white", color = NA, size = 0.25), 
        legend.title = element_text(size = 1), 
        legend.text = element_text(size = 13), 
        legend.key.width = unit(1, "cm"), 
        legend.key.height = unit(0.4, "cm"))

ggsave("figures/interact.jpg", width = 16, height = 5, dpi = 600)
