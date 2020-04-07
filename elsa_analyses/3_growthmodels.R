# subset data into men and women
wom <- subset(data, w2_DhSex == 0)
men <- subset(data, w2_DhSex == 1)

# define generic growth model with predictor
model <- '

# intercept and slope with fixed coefficients
i  =~ 1*w2_deprmean + 1*w3_deprmean + 1*w4_deprmean + 1*w5_deprmean + 1*w6_deprmean + 1*w7_deprmean
s  =~ 0*w2_deprmean + 1*w3_deprmean + 2*w4_deprmean + 3*w5_deprmean + 4*w6_deprmean + 5*w7_deprmean

# variance and covariance
i ~~ i
s ~~ s
i ~~ s

# observed variable means 
w2_deprmean ~ 0*1
w3_deprmean ~ 0*1
w4_deprmean ~ 0*1
w5_deprmean ~ 0*1
w6_deprmean ~ 0*1
w7_deprmean ~ 0*1

# latent variable means
i ~ 1
s ~ 1

# regressions
i ~ w2_dhager.c + w2_fqethnr + w0_educ + n_rf
s ~ w2_dhager.c + w2_fqethnr + w0_educ + n_rf
'


# ----------------
# 1. hypertension
# ----------------

data$pred <- data$hyp

# 1.a men
fit.1a <- sem(model, data = men, missing = "FIML", estimator = "MLR")
summary(fit.1a, fit.measures = T, standardized = T)

# 1.b women
fit.1b <- sem(model, data = wom, missing = "FIML", estimator = "MLR")
summary(fit.1b, fit.measures = T, standardized = T)


# ------------
# 2. diabetes
# ------------

data$pred <- data$diab

# 2.a men
fit.2a <- sem(model, data = men, missing = "FIML", estimator = "MLR")
summary(fit.2a, fit.measures = T, standardized = T)

# 2.b women
fit.2b <- sem(model, data = wom, missing = "FIML", estimator = "MLR")
summary(fit.2b, fit.measures = T, standardized = T)


# ------------
# 3. smoking
# ------------

data$pred <- data$w2_HESka

# 3.a men
fit.3a <- sem(model, data = men, missing = "FIML", estimator = "MLR")
summary(fit.3a,  fit.measures = T, standardized = T)

# 3.b women
fit.3b <- sem(model, data = wom, missing = "FIML", estimator = "MLR")
summary(fit.3b, fit.measures = T, standardized = T)


# ------------
# 4. BMI
# ------------

data$pred <- data$w2_bmival

# 4.a men
fit.4a <- sem(model, data = men, missing = "FIML", estimator = "MLR")
summary(fit.4a,  fit.measures = T, standardized = T)

# 4.b women
fit.4b <- sem(model, data = wom, missing = "FIML", estimator = "MLR")
summary(fit.4b,  fit.measures = T, standardized = T)


# ---------------------------
# 5. Number of risk factors
# --------------------------

data$pred <- data$n_rf

# 5.a men
fit.5a <- sem(model, data = men, missing = "FIML", estimator = "MLR")
summary(fit.5a,  fit.measures = T, standardized = T)

# 4.b women
fit.5b <- sem(model, data = wom, missing = "FIML", estimator = "MLR")
summary(fit.5b,  fit.measures = T, standardized = T)


# -----------------------
# Plot predicted values 
# -----------------------

df2 <- as.data.frame(predict(fit.1a))
hist(df2$i)
hist(df2$s)

# ----------------------
# Plot the interaction
# ----------------------

# get effects of interest (women)
eff.w.low.0 <- c(0, 0, 0.1170, 0.011)
eff.w.low.1 <- c(0, 2, 0.1176, 0.011)
eff.w.low.2 <- c(0, 4, 0.1182, 0.011)
eff.w.low.3 <- c(0, 6, 0.1188, 0.011)
eff.w.low.4 <- c(0, 8, 0.1194, 0.011)
eff.w.low.5 <- c(0,10, 0.1200, 0.011)

eff.w.hig.0 <- c(4, 0, 0.1600, 0.011)
eff.w.hig.1 <- c(4, 2, 0.1566, 0.011)
eff.w.hig.2 <- c(4, 4, 0.1526, 0.011)
eff.w.hig.3 <- c(4, 6, 0.1486, 0.011)
eff.w.hig.4 <- c(4, 8, 0.1446, 0.011)
eff.w.hig.5 <- c(4,10, 0.1406, 0.011)

df <- rbind(eff.w.low.0, eff.w.low.1, eff.w.low.2, eff.w.low.3, eff.w.low.4, eff.w.low.5,
            eff.w.hig.0, eff.w.hig.1, eff.w.hig.2, eff.w.hig.3, eff.w.hig.4, eff.w.hig.5)

df <- as.data.frame(df)

names(df) <- c("n_rf", "time", "eff", "se")

df$n_rf <- as.factor(df$n_rf)





# Plot
ggplot(df, aes(x = time, y = eff, 
                    color = n_rf, group = n_rf)) + 
  geom_point(size=0.1) + 
  geom_line(size=1.2) +
  geom_ribbon(aes(ymin = eff-se, ymax = eff+se, fill = n_rf), 
              alpha = 0.3, color = NA) + 
  labs(x= "Years followed-up",
       y="Depressed mood", color= "Number of vascular risk factors", 
       fill = "Number of vascular risk factors") + 
  ylim(0, 0.3) +
  scale_fill_manual(values=c("#722769", "#7DC2BF")) +
  scale_colour_manual(values=c("#722769", "#7DC2BF")) +
  scale_x_continuous(limits = c(0,10.3), expand = c(0, 0), breaks = c(0,2,4,6,8,10)) +
  theme_classic() + 
  theme(text=element_text(size=20), legend.position = "none")
