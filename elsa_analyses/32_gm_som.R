# ================================================================
# Cardiovascular Risk and Trajectories of Depressive Symptoms
# Script 32: Latent Growth Models for Affective Symptoms
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
plot_long(n, "idauniq", "w2_som_sum", "w3_som_sum", "w4_som_sum", 
          "w5_som_sum", "w6_som_sum", "w7_som_sum")

# Create panel plot
data_plot <- data[sample(nrow(data), 500), 
                  c("idauniq", "w2_som_sum", "w3_som_sum", "w4_som_sum", "w5_som_sum", 
                    "w6_som_sum", "w7_som_sum")]
data_plot <- melt(data_plot, id.vars=c("idauniq")) # wide to long format

jpeg("plot_somatic_time.jpeg", width = 500, height = 5000)
xyplot(value ~ variable | idauniq, data = data_plot,
       #prepanel = function(x, y) prepanel.loess(x, y, family = "gaussian"),
       xlab = "Time", ylab = "Somatic Symptoms",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family = "gaussian") 
       },
       as.table = T)
dev.off()




