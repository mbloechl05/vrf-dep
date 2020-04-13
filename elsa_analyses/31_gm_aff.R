# =================================================================
# ELSA: Fitting latent growth models (LGMs) for affective symptoms
# (contact maria.bloechl@gmail.com in case of questions)
# =================================================================

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
# run before conducting analyses on the outcomes (parts 1-3).

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
plot_long(n, "idauniq", "w2_affmean", "w3_affmean", "w4_affmean", 
          "w5_affmean", "w6_affmean", "w7_affmean")

# Create panel plot
data_plot <- data[sample(nrow(data), 500), 
                  c("idauniq", "w2_affmean", "w3_affmean", "w4_affmean", "w5_affmean", 
                    "w6_affmean", "w7_affmean")]
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


# ----------------------
# 1) Unconditional LGM 
# ----------------------

# 1.1) define model with a linear fit
# -------------------------------------

m.uc <- '

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
fit.uc.aff <- sem(m.uc, data = data, 
                  ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                              "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                              "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                              "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                              "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                              "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                  missing = "pairwise", estimator = "WLSMV", 
                  parameterization = "theta" )

# summarise fit results
summary(fit.uc.aff,  fit.measures = T, standardized = T)


# 1.2) define model with a non-linear fit
# ------------------------------------------

m.uc.2 <- '

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
s =~ 0*w2_dep + a*w3_dep + b*w4_dep + c*w5_dep + d*w6_dep + 5*w7_dep

# intercept and slope variances and covariance
i ~~ i
s ~~ s
i ~~ s

# intercept and slope means
i ~ 1
s ~ 1
'

# fit model
fit.uc.aff.2 <- sem(m.uc.2, data = data, 
                  ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                              "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                              "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                              "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                              "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                              "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                  missing = "pairwise", estimator = "WLSMV", 
                  parameterization = "theta" )

# summarise fit results
summary(fit.uc.aff.2,  fit.measures = T, standardized = T)
plot(lavPredict(fit.uc.aff.2))

# compare against linear fit
anova(fit.uc.aff, fit.uc.aff.2)


# ----------------------------------------------------------------
# 2) Conditional LGM with demographic covariates (no predictors)
# ----------------------------------------------------------------

# define model
m.cov <- '

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
i ~ w2_age_c + w2_sex + w2_eth + w0_edu
s ~ w2_age_c + w2_sex + w2_eth + w0_edu
'

# fit model (non-linear fit, spline model)
fit.cov.aff <- sem(m.cov, data = data, 
                   ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                               "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                               "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                               "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                               "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                               "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                   missing = "pairwise", estimator = "WLSMV", 
                   parameterization = "theta" )

# summarise fit results
summary(fit.cov.aff,  fit.measures = T, standardized = T, ci = T)


# -------------------------------------------------
# 3) Conditional LGMs with vascular risk factors 
# -------------------------------------------------

# The following model is an extension of the previous model with one additional 
# predictor ("pred"). It is coded as a generic model. That is "pred" is used as 
# a place holder for vascular risk factors, which are introduced individually.

# define generic model
m.pred <- '

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
i ~ w2_age_c + w2_sex + w2_eth + w0_edu + pred + int.a
s ~ w2_age_c + w2_sex + w2_eth + w0_edu + pred + int.a
'


# 3.1) Multiple risk factors (accumulated)
# -------------------------------------------

# set number of risk factors for placeholder
data$pred  <- data$n_rf_c
data$int.a <- data$pred*data$w2_age_c 

# fit model
fit.mul.aff <- sem(m.pred,data = data, 
                   ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                               "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                               "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                               "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                               "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                               "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"),
                   missing = "pairwise", estimator = "WLSMV", 
                   parameterization = "theta")

# summarise fit results
summary(fit.mul.aff,  fit.measures = T, standardized = T, ci = T)


# 3.2) Hypertension
# --------------------

# set hypertension variable for placeholder
data$pred  <- data$hyp
data$int.a <- data$pred*data$w2_age_c 
data$int.s <- data$pred*data$w2_sex

# fit model 
fit.hyp.aff <- sem(m.pred, data = data, 
                   ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                               "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                               "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                               "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                               "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                               "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                   missing = "pairwise", estimator = "WLSMV", 
                   parameterization = "theta")

# summarise fit results
summary(fit.hyp.aff,  fit.measures = T, standardized = T, ci = T)


# 3.3) Diabetes
# ---------------

# set diabetes for placeholder
data$pred  <- data$diab
data$int.a <- data$pred*data$w2_age_c 
data$int.s <- data$pred*data$w2_sex

# fit model
fit.dia.aff <- sem(m.pred, data = data, 
                   ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                               "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                               "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                               "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                               "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                               "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                   missing = "pairwise", estimator = "WLSMV", 
                   parameterization = "theta")

# summarise fit results
summary(fit.dia.aff,  fit.measures = T, standardized = T, ci = T)


# 3.4) Smoking  
# --------------

# set smoking for placeholder
data$pred  <- data$w2_HESka 
data$int.a <- data$pred*data$w2_age_c 
data$int.s <- data$pred*data$w2_sex

# fit model
fit.smo.aff <- sem(m.pred, data = data, 
                   ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                               "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                               "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                               "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                               "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                               "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                   missing = "pairwise", estimator = "WLSMV", 
                   parameterization = "theta")

# summarise fit results
summary(fit.smo.aff,  fit.measures = T, standardized = T, ci = T)


# 3.5) BMI 
# -----------

# set BMI for placeholder
data$pred  <- data$w2_bmival 
data$int.a <- data$pred*data$w2_age_c 
data$int.s <- data$pred*data$w2_sex

# fit model
fit.bmi.aff <- sem(m.pred, data = data, 
                   ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                               "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                               "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                               "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                               "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                               "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                   missing = "pairwise", estimator = "WLSMV", 
                   parameterization = "theta")

# summarise fit results
summary(fit.bmi.aff,  fit.measures = T, standardized = T, ci = T) 


# 3.6) Hypercholesterolemia 
# --------------------------

# set cholesterol for placeholder
data$pred  <- data$chol 
data$int.a <- data$pred*data$w2_age_c 
data$int.s <- data$pred*data$w2_sex

# fit model
fit.chol.aff <- sem(m.pred, data = data, 
                    ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                                "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                                "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                                "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                                "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                                "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                    missing = "pairwise", estimator = "WLSMV", 
                    parameterization = "theta")

# summarise fit results
summary(fit.chol.aff,  fit.measures = T, standardized = T, ci = T) 


# ---------------------------------------------------------------------
# 4) Conditional LGMs with vascular risk factors, controlling for ADL 
# ---------------------------------------------------------------------

# The following model is an extension of the previous model with one additional 
# predictor ("pred"). It is coded as a generic model. That is "pred" is used as 
# a place holder for vascular risk factors, which are introduced individually.

# define generic model
m.adl <- '

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
i ~ w2_age_c + w2_sex + w2_eth + w0_edu + adl + pred + int.a 
s ~ w2_age_c + w2_sex + w2_eth + w0_edu + adl + pred + int.a
'


# 4.1) Multiple risk factors (accumulated)
# -------------------------------------------

# set number of risk factors for placeholder
data$pred  <- data$n_rf
data$int.a <- data$pred*data$w2_age_c 
data$int.s <- data$pred*data$w2_sex

# fit model
fit.adl.aff <- sem(m.adl, data = data, 
                   ordered = c("w2_psceda","w2_pscedd","w2_pscede","w2_pscedf","w2_pscedg", 
                               "w3_psceda","w3_pscedd","w3_pscede","w3_pscedf","w3_pscedg", 
                               "w4_psceda","w4_pscedd","w4_pscede","w4_pscedf","w4_pscedg", 
                               "w5_psceda","w5_pscedd","w5_pscede","w5_pscedf","w5_pscedg", 
                               "w6_psceda","w6_pscedd","w6_pscede","w6_pscedf","w6_pscedg", 
                               "w7_psceda","w7_pscedd","w7_pscede","w7_pscedf","w7_pscedg"), 
                   missing = "pairwise", estimator = "WLSMV", 
                   parameterization = "theta")

# summarise fit results
summary(fit.adl.aff,  fit.measures = T, standardized = T, ci = T)


# # 3.2) Hypertension
# # --------------------
# 
# # set hypertension variable for placeholder
# data$pred  <- data$hyp
# data$int.a <- data$pred*data$w2_age_c 
# data$int.s <- data$pred*data$w2_sex
# 
# # fit model 
# fit.hyp <- sem(m.pred, data = data, 
#                ordered = c("w2_psceda","w2_pscedb","w2_pscedd", "w2_pscede","w2_pscedg", 
#                            "w3_psceda","w3_pscedb","w3_pscedd", "w3_pscede","w3_pscedg",
#                            "w4_psceda","w4_pscedb","w4_pscedd", "w4_pscede","w4_pscedg", 
#                            "w5_psceda","w5_pscedb","w5_pscedd", "w5_pscede","w5_pscedg", 
#                            "w6_psceda","w6_pscedb","w6_pscedd", "w6_pscede","w6_pscedg", 
#                            "w7_psceda","w7_pscedb","w7_pscedd", "w7_pscede","w7_pscedg"), 
#                missing = "pairwise", estimator = "WLSMV", 
#                parameterization = "theta")
# 
# # summarise fit results
# summary(fit.hyp,  fit.measures = T, standardized = T, ci = T)
# 
# 
# # 3.3) Diabetes
# # ---------------
# 
# # set diabetes for placeholder
# data$pred  <- data$diab
# data$int.a <- data$pred*data$w2_age_c 
# data$int.s <- data$pred*data$w2_sex
# 
# # fit model
# fit.dia <- sem(m.pred, data = data, 
#                ordered = c("w2_psceda","w2_pscedb","w2_pscedd", "w2_pscede","w2_pscedg", 
#                            "w3_psceda","w3_pscedb","w3_pscedd", "w3_pscede","w3_pscedg",
#                            "w4_psceda","w4_pscedb","w4_pscedd", "w4_pscede","w4_pscedg", 
#                            "w5_psceda","w5_pscedb","w5_pscedd", "w5_pscede","w5_pscedg", 
#                            "w6_psceda","w6_pscedb","w6_pscedd", "w6_pscede","w6_pscedg", 
#                            "w7_psceda","w7_pscedb","w7_pscedd", "w7_pscede","w7_pscedg"), 
#                missing = "pairwise", estimator = "WLSMV", 
#                parameterization = "theta")
# 
# # summarise fit results
# summary(fit.dia,  fit.measures = T, standardized = T, ci = T)
# 
# 
# # 3.4) Smoking  
# # --------------
# 
# # set smoking for placeholder
# data$pred  <- data$w2_HESka 
# data$int.a <- data$pred*data$w2_age_c 
# data$int.s <- data$pred*data$w2_sex
# 
# # fit model
# fit.smo <- sem(m.pred, data = data, 
#                ordered = c("w2_psceda","w2_pscedb","w2_pscedd", "w2_pscede","w2_pscedg", 
#                            "w3_psceda","w3_pscedb","w3_pscedd", "w3_pscede","w3_pscedg",
#                            "w4_psceda","w4_pscedb","w4_pscedd", "w4_pscede","w4_pscedg", 
#                            "w5_psceda","w5_pscedb","w5_pscedd", "w5_pscede","w5_pscedg", 
#                            "w6_psceda","w6_pscedb","w6_pscedd", "w6_pscede","w6_pscedg", 
#                            "w7_psceda","w7_pscedb","w7_pscedd", "w7_pscede","w7_pscedg"), 
#                missing = "pairwise", estimator = "WLSMV", 
#                parameterization = "theta")
# 
# # summarise fit results
# summary(fit.smo,  fit.measures = T, standardized = T, ci = T)
# 
# 
# # 3.5) BMI 
# # -----------
# 
# # set BMI for placeholder
# data$pred  <- data$w2_bmival 
# data$int.a <- data$pred*data$w2_age_c 
# data$int.s <- data$pred*data$w2_sex
# 
# # fit model
# fit.bmi <- sem(m.pred, data = data, 
#                ordered = c("w2_psceda","w2_pscedb","w2_pscedd", "w2_pscede","w2_pscedg", 
#                            "w3_psceda","w3_pscedb","w3_pscedd", "w3_pscede","w3_pscedg",
#                            "w4_psceda","w4_pscedb","w4_pscedd", "w4_pscede","w4_pscedg", 
#                            "w5_psceda","w5_pscedb","w5_pscedd", "w5_pscede","w5_pscedg", 
#                            "w6_psceda","w6_pscedb","w6_pscedd", "w6_pscede","w6_pscedg", 
#                            "w7_psceda","w7_pscedb","w7_pscedd", "w7_pscede","w7_pscedg"), 
#                missing = "pairwise", estimator = "WLSMV", 
#                parameterization = "theta")
# 
# # summarise fit results
# summary(fit.bmi,  fit.measures = T, standardized = T, ci = T) 
# 
# 
# # 3.6) Hypercholesterolemia 
# # --------------------------
# 
# # set cholesterol for placeholder
# data$pred  <- data$chol 
# data$int.a <- data$pred*data$w2_age_c 
# data$int.s <- data$pred*data$w2_sex
# 
# # fit model
# fit.chol <- sem(m.pred, data = data, 
#                 ordered = c("w2_psceda","w2_pscedb","w2_pscedd", "w2_pscede","w2_pscedg", 
#                             "w3_psceda","w3_pscedb","w3_pscedd", "w3_pscede","w3_pscedg",
#                             "w4_psceda","w4_pscedb","w4_pscedd", "w4_pscede","w4_pscedg", 
#                             "w5_psceda","w5_pscedb","w5_pscedd", "w5_pscede","w5_pscedg", 
#                             "w6_psceda","w6_pscedb","w6_pscedd", "w6_pscede","w6_pscedg", 
#                             "w7_psceda","w7_pscedb","w7_pscedd", "w7_pscede","w7_pscedg"), 
#                 missing = "pairwise", estimator = "WLSMV", 
#                 parameterization = "theta")
# 
# # summarise fit results
# summary(fit.chol,  fit.measures = T, standardized = T, ci = T)


# # ----------------------------------------------------------
# # 4) Conditional LGM with all risk factors simultaneously
# # ----------------------------------------------------------
# 
# # define model
# m.all <- '
# 
# # measurement models wit constrained factor loadings
# W2_DEP =~ 1*w2_psceda + l1*w2_pscedb + l2*w2_pscedd + l3*w2_pscede + l4*w2_pscedg
# W3_DEP =~ 1*w3_psceda + l1*w3_pscedb + l2*w3_pscedd + l3*w3_pscede + l4*w3_pscedg
# W4_DEP =~ 1*w4_psceda + l1*w4_pscedb + l2*w4_pscedd + l3*w4_pscede + l4*w4_pscedg
# W5_DEP =~ 1*w5_psceda + l1*w5_pscedb + l2*w5_pscedd + l3*w5_pscede + l4*w5_pscedg
# W6_DEP =~ 1*w6_psceda + l1*w6_pscedb + l2*w6_pscedd + l3*w6_pscede + l4*w6_pscedg
# W7_DEP =~ 1*w7_psceda + l1*w7_pscedb + l2*w7_pscedd + l3*w7_pscede + l4*w7_pscedg
# 
# # define tresholds
# w2_psceda | 0*t1
# w3_psceda | 0*t1
# w4_psceda | 0*t1
# w5_psceda | 0*t1
# w6_psceda | 0*t1
# w7_psceda | 0*t1
# 
# w2_pscedb | b*t1
# w3_pscedb | b*t1
# w4_pscedb | b*t1
# w5_pscedb | b*t1
# w6_pscedb | b*t1
# w7_pscedb | b*t1
# 
# w2_pscedd | c*t1
# w3_pscedd | c*t1
# w4_pscedd | c*t1
# w5_pscedd | c*t1
# w6_pscedd | c*t1
# w7_pscedd | c*t1
# 
# w2_pscede | d*t1
# w3_pscede | d*t1
# w4_pscede | d*t1
# w5_pscede | d*t1
# w6_pscede | d*t1
# w7_pscede | d*t1
# 
# w2_pscedg | e*t1
# w3_pscedg | e*t1
# w4_pscedg | e*t1
# w5_pscedg | e*t1
# w6_pscedg | e*t1
# w7_pscedg | e*t1
# 
# # define latent factor means
# W2_DEP ~ 0*1
# W3_DEP ~ 0*1
# W4_DEP ~ 0*1
# W5_DEP ~ 0*1
# W6_DEP ~ 0*1
# W7_DEP ~ 0*1
# 
# # define unique variance terms
# w2_psceda ~~ 1*w2_psceda
# w3_psceda ~~ 1*w3_psceda
# w4_psceda ~~ 1*w4_psceda
# w5_psceda ~~ 1*w5_psceda
# w6_psceda ~~ 1*w6_psceda
# w7_psceda ~~ 1*w7_psceda
# 
# w2_pscedb ~~ 1*w2_pscedb
# w3_pscedb ~~ 1*w3_pscedb
# w4_pscedb ~~ 1*w4_pscedb
# w5_pscedb ~~ 1*w5_pscedb
# w6_pscedb ~~ 1*w6_pscedb
# w7_pscedb ~~ 1*w7_pscedb
# 
# w2_pscedd ~~ 1*w2_pscedd
# w3_pscedd ~~ 1*w3_pscedd
# w4_pscedd ~~ 1*w4_pscedd
# w5_pscedd ~~ 1*w5_pscedd
# w6_pscedd ~~ 1*w6_pscedd
# w7_pscedd ~~ 1*w7_pscedd
# 
# w2_pscede ~~ 1*w2_pscede
# w3_pscede ~~ 1*w3_pscede
# w4_pscede ~~ 1*w4_pscede
# w5_pscede ~~ 1*w5_pscede
# w6_pscede ~~ 1*w6_pscede
# w7_pscede ~~ 1*w7_pscede
# 
# w2_pscedg ~~ 1*w2_pscedg
# w3_pscedg ~~ 1*w3_pscedg
# w4_pscedg ~~ 1*w4_pscedg
# w5_pscedg ~~ 1*w5_pscedg
# w6_pscedg ~~ 1*w6_pscedg
# w7_pscedg ~~ 1*w7_pscedg
# 
# # residual correlations
# w2_psceda ~~ w3_psceda + w4_psceda + w5_psceda + w6_psceda + w7_psceda
# w3_psceda ~~ w4_psceda + w5_psceda + w6_psceda + w7_psceda
# w4_psceda ~~ w5_psceda + w6_psceda + w7_psceda
# w5_psceda ~~ w6_psceda + w7_psceda
# w6_psceda ~~ w7_psceda
# 
# w2_pscedb ~~ w3_pscedb + w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
# w3_pscedb ~~ w4_pscedb + w5_pscedb + w6_pscedb + w7_pscedb
# w4_pscedb ~~ w5_pscedb + w6_pscedb + w7_pscedb
# w5_pscedb ~~ w6_pscedb + w7_pscedb
# w6_pscedb ~~ w7_pscedb
# 
# w2_pscedd ~~ w3_pscedd + w4_pscedd + w5_pscedd + w6_pscedd + w7_pscedd
# w3_pscedd ~~ w4_pscedd + w5_pscedd + w6_pscedd + w7_pscedd
# w4_pscedd ~~ w5_pscedd + w6_pscedd + w7_pscedd
# w5_pscedd ~~ w6_pscedd + w7_pscedd
# w6_pscedd ~~ w7_pscedd
# 
# w2_pscede ~~ w3_pscede + w4_pscede + w5_pscede + w6_pscede + w7_pscede
# w3_pscede ~~ w4_pscede + w5_pscede + w6_pscede + w7_pscede
# w4_pscede ~~ w5_pscede + w6_pscede + w7_pscede
# w5_pscede ~~ w6_pscede + w7_pscede
# w6_pscede ~~ w7_pscede
# 
# w2_pscedg ~~ w3_pscedg + w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
# w3_pscedg ~~ w4_pscedg + w5_pscedg + w6_pscedg + w7_pscedg
# w4_pscedg ~~ w5_pscedg + w6_pscedg + w7_pscedg
# w5_pscedg ~~ w6_pscedg + w7_pscedg
# w6_pscedg ~~ w7_pscedg
# 
# # intercept and slope with fixed coefficients
# i =~ 1*W2_DEP + 1*W3_DEP + 1*W4_DEP + 1*W5_DEP + 1*W6_DEP + 1*W7_DEP
# s =~ 0*W2_DEP + 1*W3_DEP + 2*W4_DEP + 3*W5_DEP + 4*W6_DEP + 5*W7_DEP
# 
# # intercept and slope variances and covariance
# i ~~ i
# s ~~ s
# i ~~ s
# 
# # intercept and slope means
# i ~ 1
# s ~ 1
# 
# # regressions
# i ~ w2_age_c + w2_sex + w2_eth + w0_edu + hyp + w2_HESka + w2_bmival + diab + chol
# s ~ w2_age_c + w2_sex + w2_eth + w0_edu + hyp + w2_HESka + w2_bmival + diab + chol
# '
# 
# # fit model
# fit.all <- sem(m.all, data = data, 
#                ordered = c("w2_psceda","w2_pscedb","w2_pscedd", "w2_pscede","w2_pscedg", 
#                            "w3_psceda","w3_pscedb","w3_pscedd", "w3_pscede","w3_pscedg",
#                            "w4_psceda","w4_pscedb","w4_pscedd", "w4_pscede","w4_pscedg", 
#                            "w5_psceda","w5_pscedb","w5_pscedd", "w5_pscede","w5_pscedg", 
#                            "w6_psceda","w6_pscedb","w6_pscedd", "w6_pscede","w6_pscedg", 
#                            "w7_psceda","w7_pscedb","w7_pscedd", "w7_pscede","w7_pscedg"), 
#                missing = "pairwise", estimator = "WLSMV", 
#                parameterization = "theta")
# 
# # summarise fit results
# summary(fit.all,  fit.measures = T, standardized = T, ci = T)
# 
# 
# # --------------------
# # 5) Create figures
# # --------------------
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

