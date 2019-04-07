## ===============================================================
## ELSA: Data preprocessing of data set for 
## (contact maria.bloechl@uni-leipzig.de in case of questions)
## ==============================================================

# Delimited data files can be downloaded from the ... website (login necessary)
# data wave 1:
# data wave 2:
# data wave 3: 

# -------------------------
# 0) Preparations 
# -------------------------

# clean work space
rm(list = ls()) 

# load packages
library(plyr)       # for rbind.fill
library(data.table) # for setnames 
library(dplyr)      # for reaname_at
library(lavaan)     # for cfa models


# --------------------------------------
# 1) Get raw data and select variables
# --------------------------------------

# proper codebooks: C:\Users\Maria\Desktop\learn\0_PhD\Projects\PP-stroke\ELSA\data\raw\mrdoc\allissue
# these files: wave_0_1998_data_ukda_data_dictionary

# load raw data from wave 0 (for basic demographic information)
wave01 <- read.table("data/raw/tab/wave_0_1998_data.tab", sep = "\t", header = T)
wave02 <- read.table("data/raw/tab/wave_0_1999_data.tab", sep = "\t", header = T)
wave03 <- read.table("data/raw/tab/wave_0_2001_data.tab", sep = "\t", header = T)
# combine data from wave 0 
wave0 <- rbind.fill(wave01, wave02, wave03)
rm(list = c("wave01", "wave02", "wave03"))

# load data from wave 2 onwards
wave2  <- read.table("data/raw/tab/wave_2_core_data_v4.tab",      sep = "\t", header = T)
wave2n <- read.table("data/raw/tab/wave_2_nurse_data_v2.tab",     sep = "\t", header = T) # nurse data
wave3  <- read.table("data/raw/tab/wave_3_elsa_data_v4.tab",      sep = "\t", header = T, fill = T) # error?
wave4  <- read.table("data/raw/tab/wave_4_elsa_data_v3.tab",      sep = "\t", header = T)
wave5  <- read.table("data/raw/tab/wave_5_elsa_data_v4.tab",      sep = "\t", header = T)
wave6  <- read.table("data/raw/tab/wave_6_elsa_data_v2.tab",      sep = "\t", header = T)
wave7  <- read.table("data/raw/tab/wave_7_elsa_data.tab"   ,      sep = "\t", header = T)

# select relevant variables from different waves
wave0_s  <- wave0[,c("idauniq", "ager", "educend", "topqual2", "topqual3")]

wave2_s  <- wave2[,c("idauniq", "DhSex",   "dhager", "fqethnr", "FqEnd",
                     "hedia01", "hedia02", "hedia03", "hedia04", "hedia05", "hedia06", "hedia07", "hedia08", "hedia09", 
                     "hedias1", "HeDiaS7",
                     "behdia01", "behdia02", "behdia03", "behdia04", "behdia05", "behdia06", "behdia07",
                     "Hemda", "bheagb", "HeYRb", "HeAgd", "HeIns", 
                     "HeMdb", "HeAge", "HeSmk", "HESka", 
                     "PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH")]

wave2n_s <- wave2n[,c("idauniq", "w2wtnur", "w2wtbld", "confage", "bpconst", "bprespc", "bsoutc", "fasteli",
                      "chol", "hdl", "ldl", "fglu", "bmi", "bmival", "whval", "sys1", "sys2", "sys3")]

wave3_s  <- wave3[,c("idauniq","indager",
                     "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh")]

wave4_s  <- wave4[,c("idauniq", "indager",
                     "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh")]

wave5_s  <- wave5[,c("idauniq", "indager",
                     "psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh")]

wave6_s  <- wave6[,c("idauniq", "indager",
                     "PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH")]

wave7_s  <- wave7[,c("idauniq", "indager",
                     "PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH")]

# before merging dataframes have to harmonise naming of depression variables across waves 
setnames(wave2_s, old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH"), 
                  new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh"))

setnames(wave6_s, old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH"), 
                  new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh"))

setnames(wave7_s, old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", "PScedF", "PScedG", "PScedH"), 
                  new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", "pscedf", "pscedg", "pscedh"))

# and add indicator for the respective wave to each variable (except idauniq)
wave0_s  <- wave0_s  %>% rename_at(vars(-idauniq), ~ paste0("w0_",.))
wave2_s  <- wave2_s  %>% rename_at(vars(-idauniq), ~ paste0("w2_",.))
wave2n_s <- wave2n_s %>% rename_at(vars(-idauniq), ~ paste0("w2_",.))
wave3_s  <- wave3_s  %>% rename_at(vars(-idauniq), ~ paste0("w3_",.))
wave4_s  <- wave4_s  %>% rename_at(vars(-idauniq), ~ paste0("w4_",.))
wave5_s  <- wave5_s  %>% rename_at(vars(-idauniq), ~ paste0("w5_",.))
wave6_s  <- wave6_s  %>% rename_at(vars(-idauniq), ~ paste0("w6_",.))
wave7_s  <- wave7_s  %>% rename_at(vars(-idauniq), ~ paste0("w7_",.))

# Create variables indicating whether people participated in wave: YES
wave0_s$wave0 <- 1
wave2_s$wave2 <- 1
wave3_s$wave3 <- 1
wave4_s$wave4 <- 1
wave5_s$wave5 <- 1
wave6_s$wave6 <- 1
wave7_s$wave7 <- 1

# merge datafrom wave 2-7 into one dataframe (wide format; only keep people with data from vave 2, no top-ups)
data = merge(wave2_s, wave2n_s, all   = T, by = "idauniq", sort = T)
data = merge(data,    wave3_s,  all.x = T, by = "idauniq", sort = T)
data = merge(data,    wave4_s,  all.x = T, by = "idauniq", sort = T)
data = merge(data,    wave5_s,  all.x = T, by = "idauniq", sort = T)
data = merge(data,    wave6_s,  all.x = T, by = "idauniq", sort = T)
data = merge(data,    wave7_s,  all.x = T, by = "idauniq", sort = T)

# and then add wave 0 cases that have data (from these waves)
data = merge(data, wave0_s, all = F, all.x = T, by = "idauniq", sort = T)

# ------------------------
# 2) Recode variables
# ------------------------

# recode vallues for missing data (-9, -8, -1) as NA
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

# recode age variable; 109 adults aged > 89 have age coded as 99; these are set as missings
data$w2_dhager[data$w2_dhager==99] <- NA

# recode all binary variables to 0 (no) and 1 (yes); before 1 (yes), 2 (no)
for (i in names(data[,c(grep("psced", colnames(data)), 
                        grep("w2_DhSex", colnames(data)), # 0 = female (same as midus)
                        grep("w2_HESka", colnames(data)) # current smoking
                        )])) {
  data[[i]][data[[i]]==2] <- 0}

# recode ethnicity so "white" is coded as 0 and "non-white" as 1
data$w2_fqethnr <- ifelse(data$w2_fqethnr == 1, 0, 1)

# education variable (higher education? (yes / no))
data$w0_topqual2[data$w0_topqual2 == 6] <- NA # recode foreign / other qual to NA
data$w0_topqual2[data$w0_topqual2 == 8] <- NA # recode full-time students to NA
data$w0_educ <- ifelse(data$w0_topqual2 == 1, 1, 0) 

# create hypertension variable from CVD-variables at wave 2 (newly diagn. hypertension)
data$w2_hypt <- ifelse(data$w2_hedia01 == 1, 1, 0)
data$w2_hypt[data$w2_hedia02 == 1 | data$w2_hedia03 == 1 | data$w2_hedia04 == 1 |
               data$w2_hedia05 == 1 | data$w2_hedia06 == 1 | data$w2_hedia07 == 1 |
               data$w2_hedia08 == 1 | data$w2_hedia09 == 1] <- 1 

# create hypertension variable from CVD-variables at wave 1
data$w1_hypt <- ifelse(data$w2_behdia01 == 1, 1, 0)
data$w1_hypt[data$w2_behdia02 == 1 | data$w2_behdia03 == 1 | data$w2_behdia04 == 1 |
               data$w2_behdia05 == 1 | data$w2_behdia06 == 1 | data$w2_behdia07 == 1 |
               data$w2_behdia08 == 1 | data$w2_behdia09 == 1] <- 1 

# ever reported hypertension in wave 1 or 2?
data$hyp <- ifelse(data$w1_hypt == 1, 1, 0)
data$hyp[data$w2_hypt == 1] <- 1

# create diabetes var from CVD-variables at wave 2 (newly diagn. diabetes)
data$w2_diab <- ifelse(data$w2_hedia01 == 7, 1, 0)
data$w2_diab[data$w2_hedia02 == 7 | data$w2_hedia03 == 7 | data$w2_hedia04 == 7 |
               data$w2_hedia05 == 7 | data$w2_hedia06 == 7 | data$w2_hedia07 == 7 |
               data$w2_hedia08 == 7 | data$w2_hedia09 == 7] <- 1 

# create diabetes variable from CVD-variables at wave 1
data$w1_diab <- ifelse(data$w2_behdia01 == 7, 1, 0)
data$w1_diab[data$w2_behdia02 == 7 | data$w2_behdia03 == 7 | data$w2_behdia04 == 7 |
               data$w2_behdia05 == 7 | data$w2_behdia06 == 7 | data$w2_behdia07 == 7 |
               data$w2_behdia08 == 7 | data$w2_behdia09 == 7] <- 1 

# ever reported diabetes in wave 1 or 2?
data$diab <- ifelse(data$w1_diab == 1, 1, 0)
data$diab[data$w2_diab == 1] <- 1

# create variable that indicates the number of reported risk factors
data$w2_bmi.b <- ifelse(data$w2_bmi >=30,1,0)
rf         <- data[,c("hyp","diab","w2_HESka", "w2_bmi.b")]
data$n_rf  <- rowSums(rf)

# average blood pressure values from wave 2 (three measurements taken)
data$w2_meansys <- rowMeans(data[,c("w2_sys1", "w2_sys2", "w2_sys3")], na.rm = T)

# only select valid fasting blood samples 
data$w2_chol_f <- ifelse(data$w2_fasteli == 2, NA, data$w2_chol) # total cholesterol
data$w2_hdl_f  <- ifelse(data$w2_fasteli == 2, NA, data$w2_hdl ) # hdl cholesterol
data$w2_ldl_f  <- ifelse(data$w2_fasteli == 2, NA, data$w2_ldl ) # ldl cholesterol


# ----------------------------------------------
# 3) Unidimensionality: CESD-Scale
# ----------------------------------------------

# 3.a.) CFA models: all items 

### wave 1 (all items)
depr.1.m <- 'depr.1 =~ w2_psceda + w2_pscedb + w2_pscedc + w2_pscedd + w2_pscede + w2_pscedf + w2_pscedg + w2_pscedh'
depr.1.f <- cfa(model = depr.1.m, data = data, ordered = c("w2_psceda","w2_pscedb", "w2_pscedc", "w2_pscedd", 
                                                           "w2_pscede", "w2_pscedf", "w2_pscedg", "w2_pscedh"))
summary(depr.1.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.1.f, sort = TRUE, minimum.value = 100) # modification indices

### wave 1 (reduced item set)
depr.1.m <- 'depr.1 =~ w2_psceda + w2_pscedb + w2_pscedd + w2_pscede + w2_pscedg'
depr.1.f <- cfa(model = depr.1.m, data = data, ordered = c("w2_psceda","w2_pscedb", "w2_pscedd", "w2_pscede", "w2_pscedg"))
summary(depr.1.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.1.f, sort = TRUE, minimum.value = 100) # modification indices

# http://lavaan.ugent.be/tutorial/cat.html


# -----------------------------------------
# 4) Reliability: depressed affect scale
# -----------------------------------------

# to calculate cronbach's alpha according to the Kuder-Richardson formula, item d has to be reversed
data$w2_pscedd_r <- 1 - data$w2_pscedd
data$w3_pscedd_r <- 1 - data$w3_pscedd
data$w4_pscedd_r <- 1 - data$w4_pscedd
data$w5_pscedd_r <- 1 - data$w5_pscedd
data$w6_pscedd_r <- 1 - data$w6_pscedd

# select items depressed affect for each wave 
depr.items.2 <- data[,c("w2_psceda", "w2_pscedb", "w2_pscedd_r", "w2_pscede", "w2_pscedg")] # wave 2
depr.items.3 <- data[,c("w3_psceda", "w3_pscedb", "w3_pscedd_r", "w3_pscede", "w3_pscedg")] # wave 3
depr.items.4 <- data[,c("w4_psceda", "w4_pscedb", "w4_pscedd_r", "w4_pscede", "w4_pscedg")] # wave 4
depr.items.5 <- data[,c("w5_psceda", "w5_pscedb", "w5_pscedd_r", "w5_pscede", "w5_pscedg")] # wave 5
depr.items.6 <- data[,c("w6_psceda", "w6_pscedb", "w6_pscedd_r", "w6_pscede", "w6_pscedg")] # wave 6

# cronbachs alpha reduces to Kuder-Richardson formula for dichotomous items (automatically in function alpha)
psych::alpha(depr.items.2) # wave 2
psych::alpha(depr.items.3) # wave 3
psych::alpha(depr.items.4) # wave 4
psych::alpha(depr.items.5) # wave 5
psych::alpha(depr.items.6) # wave 6


# ---------------------------
# 5) Save preprocessed data
# ---------------------------

save(data, file = "data/processed/elsa_proc_data.RData")

