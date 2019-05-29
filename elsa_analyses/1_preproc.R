# ===============================================================
# ELSA: Data preprocessing 
# (contact maria.bloechl@gmail.com in case of questions)
# ==============================================================

# clean work space
rm(list = ls()) 

# load packages
library(plyr)       # for rbind.fill
library(data.table) # for setnames 
library(dplyr)      # for reaname_at
library(lavaan)     # for cfa models

# ----------------------------------------------------
# 1) Read in raw data, select variables, and merge 
# ----------------------------------------------------

# load raw data from wave 0 (for basic demographic information)
wave01 <- read.table("data/elsa/raw/tab/wave_0_1998_data.tab", sep = "\t", header = T)
wave02 <- read.table("data/elsa/raw/tab/wave_0_1999_data.tab", sep = "\t", header = T)
wave03 <- read.table("data/elsa/raw/tab/wave_0_2001_data.tab", sep = "\t", header = T)

# combine data from wave 0 
wave0 <- rbind.fill(wave01, wave02, wave03)
rm(list = c("wave01", "wave02", "wave03"))

# load data from all other waves (starting from wave 2)
wave2  <- read.table("data/elsa/raw/tab/wave_2_core_data_v4.tab",  sep = "\t", header = T)
wave2n <- read.table("data/elsa/raw/tab/wave_2_nurse_data_v2.tab", sep = "\t", header = T) # nurse data
wave3  <- read.table("data/elsa/raw/tab/wave_3_elsa_data_v4.tab",  sep = "\t", header = T, fill = T) 
wave4  <- read.table("data/elsa/raw/tab/wave_4_elsa_data_v3.tab",  sep = "\t", header = T)
wave5  <- read.table("data/elsa/raw/tab/wave_5_elsa_data_v4.tab",  sep = "\t", header = T)
wave6  <- read.table("data/elsa/raw/tab/wave_6_elsa_data_v2.tab",  sep = "\t", header = T)
wave7  <- read.table("data/elsa/raw/tab/wave_7_elsa_data.tab"   ,  sep = "\t", header = T)

# select relevant variables from waves (see codebook for explanations)
wave0_s  <- wave0[,c("idauniq", "ager", "topqual2")]

wave2_s  <- wave2[,c("idauniq" , "DhSex"   , "dhager"  , "fqethnr" , "hedia01" , 
                     "hedia02" , "hedia03" , "hedia04" , "hedia05" , "hedia06" , 
                     "hedia07" , "hedia08" , "hedia09" , "behdia01", "behdia02", 
                     "behdia03", "behdia04", "behdia05", "behdia06", "behdia07", 
                     "Hemda"   , "HESka"   , "PScedA"  , "PScedB"  , "PScedC"  , 
                     "PScedD"  , "PScedE"  , "PScedF"  , "PScedG"  , "PScedH")]

wave2n_s <- wave2n[,c("idauniq", 
                      "confage",
                      "bpconst", 
                      "bprespc", 
                      "bsoutc", 
                      "fasteli", 
                      "chol", 
                      "hdl", 
                      "ldl", 
                      "fglu", 
                      "bmi", 
                      "bmival", 
                      "whval", 
                      "sys1", 
                      "sys2", 
                      "sys3", 
                      "dias1", 
                      "dias2", 
                      "dias3")]

wave3_s  <- wave3[,c("idauniq", 
                     "indager", 
                     "psceda", 
                     "pscedb", 
                     "pscedc", 
                     "pscedd", 
                     "pscede", 
                     "pscedf", 
                     "pscedg", 
                     "pscedh")]

wave4_s  <- wave4[,c("idauniq", 
                     "indager", 
                     "psceda", 
                     "pscedb", 
                     "pscedc", 
                     "pscedd", 
                     "pscede", 
                     "pscedf", 
                     "pscedg", 
                     "pscedh")]

wave5_s  <- wave5[,c("idauniq", 
                     "indager", 
                     "psceda", 
                     "pscedb", 
                     "pscedc", 
                     "pscedd", 
                     "pscede", "pscedf", "pscedg", "pscedh")]

wave6_s  <- wave6[,c("idauniq", "indager", "PScedA", "PScedB", "PScedC", 
                     "PScedD", "PScedE", "PScedF", "PScedG", "PScedH")]

wave7_s  <- wave7[,c("idauniq", "indager", "PScedA", "PScedB", "PScedC", 
                     "PScedD", "PScedE", "PScedF", "PScedG", "PScedH")]

# harmonise naming of depression variables across waves 
setnames(wave2_s, old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", 
                          "PScedF", "PScedG", "PScedH"), 
                  new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", 
                          "pscedf", "pscedg", "pscedh"))

setnames(wave6_s, old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", 
                          "PScedF", "PScedG", "PScedH"), 
                  new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", 
                          "pscedf", "pscedg", "pscedh"))

setnames(wave7_s, old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", 
                          "PScedF", "PScedG", "PScedH"), 
                  new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", 
                          "pscedf", "pscedg", "pscedh"))

# add pre-fix to variables to indicate the from which wave they are from (except idauniq)
wave0_s  <- wave0_s  %>% rename_at(vars(-idauniq), ~ paste0("w0_",.))
wave2_s  <- wave2_s  %>% rename_at(vars(-idauniq), ~ paste0("w2_",.))
wave2n_s <- wave2n_s %>% rename_at(vars(-idauniq), ~ paste0("w2_",.))
wave3_s  <- wave3_s  %>% rename_at(vars(-idauniq), ~ paste0("w3_",.))
wave4_s  <- wave4_s  %>% rename_at(vars(-idauniq), ~ paste0("w4_",.))
wave5_s  <- wave5_s  %>% rename_at(vars(-idauniq), ~ paste0("w5_",.))
wave6_s  <- wave6_s  %>% rename_at(vars(-idauniq), ~ paste0("w6_",.))
wave7_s  <- wave7_s  %>% rename_at(vars(-idauniq), ~ paste0("w7_",.))

# create variables indicating whether people participated in that wave (1 = yes)
wave0_s$wave0 <- 1
wave2_s$wave2 <- 1
wave3_s$wave3 <- 1
wave4_s$wave4 <- 1
wave5_s$wave5 <- 1
wave6_s$wave6 <- 1
wave7_s$wave7 <- 1

# merge data from waves 2-7 into one dataframe (wide format)
# but only keep people with data from wave 2 (i.e. exclude top-ups)
data = merge(wave2_s, wave2n_s, all   = T, by = "idauniq", sort = T)
data = merge(data,    wave3_s,  all.x = T, by = "idauniq", sort = T)
data = merge(data,    wave4_s,  all.x = T, by = "idauniq", sort = T)
data = merge(data,    wave5_s,  all.x = T, by = "idauniq", sort = T)
data = merge(data,    wave6_s,  all.x = T, by = "idauniq", sort = T)
data = merge(data,    wave7_s,  all.x = T, by = "idauniq", sort = T)

# add wave 0 data 
data = merge(data, wave0_s, all = F, all.x = T, by = "idauniq", sort = T)


# ------------------------
# 2) Recode variables
# ------------------------

# 2.1) NAs
# recode different negative values for missing data to NAs
data <- data %>% mutate_all(funs(na_if(., -11)))
data <- data %>% mutate_all(funs(na_if(., -10)))
data <- data %>% mutate_all(funs(na_if(., -9)))
data <- data %>% mutate_all(funs(na_if(., -8)))
data <- data %>% mutate_all(funs(na_if(., -7)))
data <- data %>% mutate_all(funs(na_if(., -6)))
data <- data %>% mutate_all(funs(na_if(., -5)))
data <- data %>% mutate_all(funs(na_if(., -4)))
data <- data %>% mutate_all(funs(na_if(., -3)))
data <- data %>% mutate_all(funs(na_if(., -2)))
data <- data %>% mutate_all(funs(na_if(., -1)))

# 2.2) Age
# recode variable because 109 adults aged > 89 have age coded as 99 
# 99 --> set these as missings
data$w2_dhager[data$w2_dhager == 99] <- NA

# center age variable for analyses
data$w2_dhager.c <- scale(data$w2_dhager, center = T, scale = F) # center age 

# 2.3) Dichotomous variables
# recode all dichotomous variables to dummies (0 = no, 1 = yes)
for (i in names(data[,c(grep("psced", colnames(data)), 
                        grep("w2_DhSex", colnames(data)), # 0 = female 
                        grep("w2_HESka", colnames(data)), # current smoking
                        grep("w2_Hemda", colnames(data)) 
                        )])) {
  data[[i]][data[[i]] == 2] <- 0}

# 2.4) Ethnicity
# recode to dummy (0 = "white", 1 = "non-white")
data$w2_fqethnr <- ifelse(data$w2_fqethnr == 1, 0, 1)

# 2.5) Education
# recode to dummy (0 = lower education, 1 = higher education)
data$w0_topqual2[data$w0_topqual2 == 6] <- NA # recode foreign / other qual to NA
data$w0_topqual2[data$w0_topqual2 == 8] <- NA # recode full-time students to NA
data$w0_educ <- ifelse(data$w0_topqual2 == 1, 1, 0) 

# 2.6) Hypertension
# create hypertension variable from CVD-variables at wave 1
data$w1_hypt <- ifelse(data$w2_behdia01 == 1, 1, 0)
data$w1_hypt[data$w2_behdia02 == 1   | data$w2_behdia03 == 1 | 
               data$w2_behdia04 == 1 | data$w2_behdia05 == 1 | 
               data$w2_behdia06 == 1 | data$w2_behdia07 == 1 |
               data$w2_behdia08 == 1 | data$w2_behdia09 == 1] <- 1 

# create hypertension variable from CVD-variables at wave 2 (newly diagn. hypertension)
data$w2_hypt <- ifelse(data$w2_hedia01 == 1, 1, 0)
data$w2_hypt[data$w2_hedia02 == 1   | data$w2_hedia03 == 1 | 
               data$w2_hedia04 == 1 | data$w2_hedia05 == 1 | 
               data$w2_hedia06 == 1 | data$w2_hedia07 == 1 |
               data$w2_hedia08 == 1 | data$w2_hedia09 == 1] <- 1 

# create variable that indicates whether participants ever reported having hypertension in wave 1 or 2
# (0 = no, 1 = yes)
data$hyp <- ifelse(data$w1_hypt == 1, 1, 0)
data$hyp[data$w2_hypt == 1] <- 1

# 2.7) Diabetes
# create diabetes variable from CVD-variables at wave 1
data$w1_diab <- ifelse(data$w2_behdia01 == 7, 1, 0)
data$w1_diab[data$w2_behdia02 == 7 | data$w2_behdia03 == 7 | data$w2_behdia04 == 7 |
               data$w2_behdia05 == 7 | data$w2_behdia06 == 7 | data$w2_behdia07 == 7 |
               data$w2_behdia08 == 7 | data$w2_behdia09 == 7] <- 1 

# create diabetes var from CVD-variables at wave 2 (newly diagn. diabetes)
data$w2_diab <- ifelse(data$w2_hedia01 == 7, 1, 0)
data$w2_diab[data$w2_hedia02 == 7 | data$w2_hedia03 == 7 | data$w2_hedia04 == 7 |
               data$w2_hedia05 == 7 | data$w2_hedia06 == 7 | data$w2_hedia07 == 7 |
               data$w2_hedia08 == 7 | data$w2_hedia09 == 7] <- 1 

# create variable that indicates whether participants ever reported having diabetes in wave 1 or 2
# (0 = no, 1 = yes)
data$diab <- ifelse(data$w1_diab == 1, 1, 0)
data$diab[data$w2_diab == 1] <- 1

# 2.8) Number of risk factors
# create a variable that indicates obesity (BMI >= 30)
data$w2_bmi.b <- ifelse(data$w2_bmi >=30,1,0)

# create variable that indicates the number of reported risk factors
rf         <- data[,c("hyp","diab","w2_HESka", "w2_bmi.b")]
data$n_rf  <- rowSums(rf)

# 2.9) Systolic and diastolic blood pressure
# average all three blood pressure values from wave 2 
data$w2_meansys  <- rowMeans(data[,c("w2_sys1",  "w2_sys2",  "w2_sys3" )], na.rm = T)
data$w2_meandias <- rowMeans(data[,c("w2_dias1", "w2_dias2", "w2_dias3")], na.rm = T)

# 2.10) Blood glucose variables
# Only select participants with valid fasting blood samples 
data$w2_fglu_f <- ifelse(data$w2_fasteli == 2, NA, data$w2_fglu) # total cholesterol


# -----------------
# 3) CESD-Scale
# -----------------

# 3.1) Unidimensionality 
# CFA Full 8-item scale
depr.1.m <- 
  'depr.1 =~ w2_psceda + w2_pscedb + w2_pscedc + w2_pscedd + w2_pscede + 
             w2_pscedf + w2_pscedg + w2_pscedh'
depr.1.f <- cfa(model = depr.1.m, data = data, ordered = c("w2_psceda","w2_pscedb","w2_pscedc", 
                                                           "w2_pscedd","w2_pscede","w2_pscedf", 
                                                           "w2_pscedg", "w2_pscedh"))
summary(depr.1.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.1.f, sort = TRUE, minimum.value = 100) # modification indices

# CFA reduced 5-item scale
depr.1.m <- 'depr.1 =~ w2_psceda + w2_pscedb + w2_pscedd + w2_pscede + w2_pscedg'
depr.1.f <- cfa(model = depr.1.m, data = data, ordered = c("w2_psceda","w2_pscedb", "w2_pscedd", 
                                                           "w2_pscede", "w2_pscedg"))
summary(depr.1.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.1.f, sort = TRUE, minimum.value = 100) # modification indices


# 3.2) Internal consistency
# item d has to be reversed for calculations of alpha
data$w2_pscedd_r <- 1 - data$w2_pscedd
data$w3_pscedd_r <- 1 - data$w3_pscedd
data$w4_pscedd_r <- 1 - data$w4_pscedd
data$w5_pscedd_r <- 1 - data$w5_pscedd
data$w6_pscedd_r <- 1 - data$w6_pscedd
data$w7_pscedd_r <- 1 - data$w7_pscedd

# select items CES-D items for each wave 
depr.items.2 <- data[,c("w2_psceda", "w2_pscedb", "w2_pscedd_r", "w2_pscede", "w2_pscedg")] # wave 2
depr.items.3 <- data[,c("w3_psceda", "w3_pscedb", "w3_pscedd_r", "w3_pscede", "w3_pscedg")] # wave 3
depr.items.4 <- data[,c("w4_psceda", "w4_pscedb", "w4_pscedd_r", "w4_pscede", "w4_pscedg")] # wave 4
depr.items.5 <- data[,c("w5_psceda", "w5_pscedb", "w5_pscedd_r", "w5_pscede", "w5_pscedg")] # wave 5
depr.items.6 <- data[,c("w6_psceda", "w6_pscedb", "w6_pscedd_r", "w6_pscede", "w6_pscedg")] # wave 6
depr.items.7 <- data[,c("w7_psceda", "w7_pscedb", "w7_pscedd_r", "w7_pscede", "w7_pscedg")] # wave 7

# calculate Cronbach's alpha 
# note: reduces to Kuder-Richardson formula for dichotomous items (implemented in alpha())
psych::alpha(depr.items.2) # wave 2
psych::alpha(depr.items.3) # wave 3
psych::alpha(depr.items.4) # wave 4
psych::alpha(depr.items.5) # wave 5
psych::alpha(depr.items.6) # wave 6
psych::alpha(depr.items.7) # wave 7

# 3.3) Calculate means (i.e. proportion of yes answers) for descriptive stats
data$w2_deprmean <- rowMeans(depr.items.2)
data$w3_deprmean <- rowMeans(depr.items.3)
data$w4_deprmean <- rowMeans(depr.items.4)
data$w5_deprmean <- rowMeans(depr.items.5)
data$w6_deprmean <- rowMeans(depr.items.6)
data$w7_deprmean <- rowMeans(depr.items.7)


# ---------------------------
# 4) Save preprocessed data
# ---------------------------

save(data, file = "data/elsa/processed/elsa_proc_data.RData")

