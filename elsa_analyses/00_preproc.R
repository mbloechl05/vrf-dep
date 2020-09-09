# ==========================================================
# Cardiovascular Risk and Trajectories of Depressed Mood
# Script 0: Data Preprocessing
# ==========================================================

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
wave_01 <- read.table("data/elsa/raw/tab/wave_0_1998_data.tab", sep = "\t", header = T)
wave_02 <- read.table("data/elsa/raw/tab/wave_0_1999_data.tab", sep = "\t", header = T)
wave_03 <- read.table("data/elsa/raw/tab/wave_0_2001_data.tab", sep = "\t", header = T)

# combine data from wave 0 
wave_0_c <- rbind.fill(wave_01, wave_02, wave_03)
rm(list = c("wave_01", "wave_02", "wave_03"))

# load data from all other waves (starting from wave 2)
wave_2_c <- read.table("data/elsa/raw/tab/wave_2_core_data_v4.tab",  sep = "\t", header = T)
wave_2_n <- read.table("data/elsa/raw/tab/wave_2_nurse_data_v2.tab", sep = "\t", header = T) # nurse data
wave_3_c <- read.table("data/elsa/raw/tab/wave_3_elsa_data_v4.tab",  sep = "\t", header = T, fill = T) 
wave_4_c <- read.table("data/elsa/raw/tab/wave_4_elsa_data_v3.tab",  sep = "\t", header = T)
wave_5_c <- read.table("data/elsa/raw/tab/wave_5_elsa_data_v4.tab",  sep = "\t", header = T)
wave_6_c <- read.table("data/elsa/raw/tab/wave_6_elsa_data_v2.tab",  sep = "\t", header = T)
wave_7_c <- read.table("data/elsa/raw/tab/wave_7_elsa_data.tab"   ,  sep = "\t", header = T)

# select relevant variables from waves (see codebook for explanations)
wave_0_c  <- wave_0_c[,c("idauniq", "ager", "topqual2")]

wave_2_c  <- wave_2_c[,c( # demographic variables
                         "idauniq" , "DhSex"   , "dhager"  , "fqethnr" , "sampsta" , 
                         "hedia01" , "hedia02" , "hedia03" , "hedia04" , "hedia05" , 
                         "hedia06" , "hedia07" , "hedia08" , "hedia09" , 
                         "behdia01", "behdia02", "behdia03", "behdia04", "behdia05", 
                         "behdia06", "behdia07", 
                         "Hemda"   , "HESka"   , "HeSmk"   , "HeSkf"   ,
                         "bhesmk"  , "bheska"  , 
                         "PScedA"  , "PScedB"  , "PScedC"  , "PScedD"  , "PScedE"  , 
                         "PScedF"  , "PScedG"  , "PScedH"  ,
                         "hedib01" , "hedib02" , "hedib03" , "hedib04" , 
                         "hedibw1" , "HeDibW2" , "HeDibW3" , "HeDibW4" , "HeDibW5" , 
                         "HeDibW6" , "HeDibW7" , "HeDibW8" , "HeDibW9" , 
                         "heada01" , "heada02" , "heada03" , "heada04" , "heada05" , 
                         "heada06" , "heada07" , "heada08" , "heada09" , "heada10")]


wave_2_n <- wave_2_n[,c("idauniq", "confage", 
                        "sys1"   , "sys2"   , "sys3", "sysval", "dias1", "dias2", "dias3", 
                        "fasteli", "fglu"   , "chol", "ldl"   , "bmival")]

wave_3_c  <- wave_3_c[,c("idauniq", "indager", "psceda", "pscedb", "pscedc", 
                         "pscedd" , "pscede" , "pscedf", "pscedg", "pscedh")]

wave_4_c  <- wave_4_c[,c("idauniq", "indager", "psceda", "pscedb", "pscedc", 
                         "pscedd" , "pscede" , "pscedf", "pscedg", "pscedh")]

wave_5_c  <- wave_5_c[,c("idauniq", "indager", "psceda", "pscedb", "pscedc", 
                         "pscedd" , "pscede" , "pscedf", "pscedg", "pscedh")]

wave_6_c  <- wave_6_c[,c("idauniq", "indager", "PScedA", "PScedB", "PScedC", 
                         "PScedD" , "PScedE" , "PScedF", "PScedG", "PScedH")]

wave_7_c  <- wave_7_c[,c("idauniq", "indager", "PScedA", "PScedB", "PScedC", 
                         "PScedD" , "PScedE" , "PScedF", "PScedG", "PScedH")]

# harmonise naming of depression variables across waves 
setnames(wave_2_c, 
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", 
                           "PScedF", "PScedG", "PScedH"), 
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", 
                 "pscedf", "pscedg", "pscedh"))

setnames(wave_6_c, 
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", 
                           "PScedF", "PScedG", "PScedH"), 
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", 
                 "pscedf", "pscedg", "pscedh"))

setnames(wave_7_c, 
         old = c("PScedA", "PScedB", "PScedC", "PScedD", "PScedE", 
                           "PScedF", "PScedG", "PScedH"), 
         new = c("psceda", "pscedb", "pscedc", "pscedd", "pscede", 
                 "pscedf", "pscedg", "pscedh"))

# add pre-fix to variables to indicate the which wave they are from (except idauniq)
wave_0_c <- wave_0_c %>% rename_at(vars(-idauniq), ~ paste0("w0_",.))
wave_2_c <- wave_2_c %>% rename_at(vars(-idauniq), ~ paste0("w2_",.))
wave_2_n <- wave_2_n %>% rename_at(vars(-idauniq), ~ paste0("w2_",.))
wave_3_c <- wave_3_c %>% rename_at(vars(-idauniq), ~ paste0("w3_",.))
wave_4_c <- wave_4_c %>% rename_at(vars(-idauniq), ~ paste0("w4_",.))
wave_5_c <- wave_5_c %>% rename_at(vars(-idauniq), ~ paste0("w5_",.))
wave_6_c <- wave_6_c %>% rename_at(vars(-idauniq), ~ paste0("w6_",.))
wave_7_c <- wave_7_c %>% rename_at(vars(-idauniq), ~ paste0("w7_",.))

# create variables indicating whether people participated in that wave (1 = yes)
wave_0_c$wave_0_c <- 1
wave_2_c$wave_2_c <- 1
wave_2_n$wave_2_n <- 1
wave_3_c$wave_3_c <- 1
wave_4_c$wave_4_c <- 1
wave_5_c$wave_5_c <- 1
wave_6_c$wave_6_c <- 1
wave_7_c$wave_7_c <- 1

# merge data from waves 2-7 into one dataframe (wide format)
# but only keep people with data from wave 2 (i.e. exclude top-ups)
data = merge(wave_2_c, wave_2_n, all   = T, by = "idauniq", sort = T)
data = merge(data,     wave_3_c, all.x = T, by = "idauniq", sort = T)
data = merge(data,     wave_4_c, all.x = T, by = "idauniq", sort = T)
data = merge(data,     wave_5_c, all.x = T, by = "idauniq", sort = T)
data = merge(data,     wave_6_c, all.x = T, by = "idauniq", sort = T)
data = merge(data,     wave_7_c, all.x = T, by = "idauniq", sort = T)

# add wave 0 data 
data = merge(data, wave_0_c, all = F, all.x = T, by = "idauniq", sort = T)

# set missing values indicating that participants did not participate in a wave to 0
data$wave_0_c[is.na(data$wave_0_c)] <- 0
data$wave_2_c[is.na(data$wave_2_c)] <- 0
data$wave_2_n[is.na(data$wave_2_n)] <- 0
data$wave_3_c[is.na(data$wave_3_c)] <- 0
data$wave_4_c[is.na(data$wave_4_c)] <- 0
data$wave_5_c[is.na(data$wave_5_c)] <- 0
data$wave_6_c[is.na(data$wave_6_c)] <- 0
data$wave_7_c[is.na(data$wave_7_c)] <- 0


# ------------------------------
# 2) Exclusion of participants
# ------------------------------

# 2.1) Only include core members 
# --------------------------------

table(data$w2_sampsta) # N = 460
data <- subset(data, w2_sampsta == 1)


# 2.2) Exclude participants with manifest CVD: STROKE
# -----------------------------------------------------

# create stroke variable from CVD-variables at wave 1
data$w1_stroke <- ifelse(data$w2_behdia01 == 8, 1, 0)
data$w1_stroke[data$w2_behdia02 == 8] <- 1
data$w1_stroke[data$w2_behdia03 == 8] <- 1
data$w1_stroke[data$w2_behdia04 == 8] <- 1
data$w1_stroke[data$w2_behdia05 == 8] <- 1
data$w1_stroke[data$w2_behdia06 == 8] <- 1
data$w1_stroke[data$w2_behdia07 == 8] <- 1

# create stroke variable from CVD-variables at wave 2 (newly diagn.)
data$w2_stroke <- ifelse(data$w2_hedia01 == 8, 1, 0)
data$w2_stroke[data$w2_hedia02 == 8] <- 1
data$w2_stroke[data$w2_hedia03 == 8] <- 1
data$w2_stroke[data$w2_hedia04 == 8] <- 1
data$w2_stroke[data$w2_hedia05 == 8] <- 1
data$w2_stroke[data$w2_hedia06 == 8] <- 1
data$w2_stroke[data$w2_hedia07 == 8] <- 1
data$w2_stroke[data$w2_hedia08 == 8] <- 1
data$w2_stroke[data$w2_hedia09 == 8] <- 1

# create variable indiating whether participants ever reported having 
# stroke in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_stroke <- ifelse(data$w1_stroke == 1, 1, 0)
data$w1_w2_stroke[data$w2_stroke == 1] <- 1

# check how many?
table(data$w1_w2_stroke) # N = 460

# exclude those
data <- subset(data, w1_w2_stroke == 0)


# 2.3) Exclude participants with manifest CVD: HEART ATTACK
# -----------------------------------------------------------

# create heart attack variable from CVD-variables at wave 1
data$w1_heartattack <- ifelse(data$w2_behdia01 == 3, 1, 0)
data$w1_heartattack[data$w2_behdia02 == 3] <- 1
data$w1_heartattack[data$w2_behdia03 == 3] <- 1
data$w1_heartattack[data$w2_behdia04 == 3] <- 1
data$w1_heartattack[data$w2_behdia05 == 3] <- 1
data$w1_heartattack[data$w2_behdia06 == 3] <- 1
data$w1_heartattack[data$w2_behdia07 == 3] <- 1

# create heart attack variable from CVD-variables at wave 2 (newly diagn.)
data$w2_heartattack <- ifelse(data$w2_hedia01 == 3, 1, 0)
data$w2_heartattack[data$w2_hedia02 == 3] <- 1
data$w2_heartattack[data$w2_hedia03 == 3] <- 1
data$w2_heartattack[data$w2_hedia04 == 3] <- 1
data$w2_heartattack[data$w2_hedia05 == 3] <- 1
data$w2_heartattack[data$w2_hedia06 == 3] <- 1
data$w2_heartattack[data$w2_hedia07 == 3] <- 1
data$w2_heartattack[data$w2_hedia08 == 3] <- 1
data$w2_heartattack[data$w2_hedia09 == 3] <- 1

# create variable indiating whether participants ever reported having 
# heart attack in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_heartattack <- ifelse(data$w1_heartattack == 1, 1, 0)
data$w1_w2_heartattack[data$w2_heartattack == 1] <- 1

# check how many?
table(data$w1_w2_heartattack) # N = 469

# exclude those
data <- subset(data, w1_w2_heartattack == 0)


# 2.4) Exclude participants with manifest CVD: CONGESTIVE HEART FAILURE
# ----------------------------------------------------------------------

# create heart attack variable from CVD-variables at wave 1
data$w1_heartfailure <- ifelse(data$w2_behdia01 == 4, 1, 0)
data$w1_heartfailure[data$w2_behdia02 == 4] <- 1
data$w1_heartfailure[data$w2_behdia03 == 4] <- 1
data$w1_heartfailure[data$w2_behdia04 == 4] <- 1
data$w1_heartfailure[data$w2_behdia05 == 4] <- 1
data$w1_heartfailure[data$w2_behdia06 == 4] <- 1
data$w1_heartfailure[data$w2_behdia07 == 4] <- 1

# create heart attack variable from CVD-variables at wave 2 (newly diagn.)
data$w2_heartfailure <- ifelse(data$w2_hedia01 == 4, 1, 0)
data$w2_heartfailure[data$w2_hedia02 == 4] <- 1
data$w2_heartfailure[data$w2_hedia03 == 4] <- 1
data$w2_heartfailure[data$w2_hedia04 == 4] <- 1
data$w2_heartfailure[data$w2_hedia05 == 4] <- 1
data$w2_heartfailure[data$w2_hedia06 == 4] <- 1
data$w2_heartfailure[data$w2_hedia07 == 4] <- 1
data$w2_heartfailure[data$w2_hedia08 == 4] <- 1
data$w2_heartfailure[data$w2_hedia09 == 4] <- 1

# create variable indiating whether participants ever reported having 
# heart attack in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_heartfailure <- ifelse(data$w1_heartfailure == 1, 1, 0)
data$w1_w2_heartfailure[data$w2_heartfailure == 1] <- 1

# check how many?
table(data$w1_w2_heartfailure) # N = 27

# exclude those
data <- subset(data, w1_w2_heartfailure == 0)


# 2.5) Exclude participants with Parkinson's disease
# ---------------------------------------------------

# create PD variable from wave 1
data$w1_parkinson <- ifelse(data$w2_HeDibW6 == 1, 1, 0)

# create heart attack variable from CVD-variables at wave 2 (newly diagn.)
data$w2_parkinson <- ifelse(data$w2_hedib01 == 6, 1, 0)
data$w2_parkinson[data$w2_hedib02 == 6] <- 1
data$w2_parkinson[data$w2_hedib03 == 6] <- 1
data$w2_parkinson[data$w2_hedib04 == 6] <- 1

# create variable indiating whether participants ever reported having 
# heart attack in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_parkinson <- ifelse(data$w1_parkinson == 1, 1, 0)
data$w1_w2_parkinson[data$w2_parkinson == 1] <- 1

# check how many?
table(data$w1_w2_parkinson) # N = 12

# exclude those
data <- subset(data, w1_w2_parkinson == 0)


# 2.6) Exclude participants with Alzheimer's disease
# -----------------------------------------------------

# create PD variable from wave 1
data$w1_alzheimer <- ifelse(data$w2_HeDibW8 == 1, 1, 0)

# create heart attack variable from CVD-variables at wave 2 (newly diagn.)
data$w2_alzheimer <- ifelse(data$w2_hedib01 == 8, 1, 0)
data$w2_alzheimer[data$w2_hedib02 == 8] <- 1
data$w2_alzheimer[data$w2_hedib03 == 8] <- 1
data$w2_alzheimer[data$w2_hedib04 == 8] <- 1

# create variable indiating whether participants ever reported having 
# heart attack in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_alzheimer <- ifelse(data$w1_alzheimer == 1, 1, 0)
data$w1_w2_alzheimer[data$w2_alzheimer == 1] <- 1

# check how many?
table(data$w1_w2_alzheimer) # N = 8

# exclude those
data <- subset(data, w1_w2_alzheimer == 0)


# 2.7) Exclude participants with Dementia
# -----------------------------------------

# create PD variable from wave 1
data$w1_dementia <- ifelse(data$w2_HeDibW9 == 1, 1, 0)

# create heart attack variable from CVD-variables at wave 2 (newly diagn.)
data$w2_dementia  <- ifelse(data$w2_hedib01 == 9, 1, 0)
data$w2_dementia[data$w2_hedib02 == 9] <- 1
data$w2_dementia[data$w2_hedib03 == 9] <- 1
data$w2_dementia[data$w2_hedib04 == 9] <- 1

# create variable indiating whether participants ever reported having 
# heart attack in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_dementia <- ifelse(data$w1_dementia == 1, 1, 0)
data$w1_w2_dementia[data$w2_dementia == 1] <- 1

# check how many?
table(data$w1_w2_dementia) # N = 30

# exclude those
data <- subset(data, w1_w2_dementia == 0)


# 2.8) Exclude participants without nurse visit data
# -----------------------------------------------------

# check how many? 
table(data$wave_2_n, useNA = "always")

# Descriptive stats of poeple with nurse visit at wave 2
data <- subset(data, wave_2_n == 1) 


# -------------------
# 3) Data wrangling
# -------------------

# 3.1) Recode general missing values to NAs
# -------------------------------------------

# recode different negative values for missing data to NAs
data <- data %>% mutate_all(funs(na_if(., -11)))
data <- data %>% mutate_all(funs(na_if(., -10)))
data <- data %>% mutate_all(funs(na_if(., -9 )))
data <- data %>% mutate_all(funs(na_if(., -8 )))
data <- data %>% mutate_all(funs(na_if(., -7 )))
data <- data %>% mutate_all(funs(na_if(., -6 )))
data <- data %>% mutate_all(funs(na_if(., -5 )))
data <- data %>% mutate_all(funs(na_if(., -4 )))
data <- data %>% mutate_all(funs(na_if(., -3 )))
data <- data %>% mutate_all(funs(na_if(., -2 )))
data <- data %>% mutate_all(funs(na_if(., -1 )))


# 3.2) Age
# ----------

# recode variable because 109 adults aged > 89 have age coded as 99 
# 99 --> set these as missings
data$w2_dhager[data$w2_dhager == 99] <- NA

# rename variable for analyses
names(data)[names(data) == 'w2_dhager'] <- 'w2_age'

# center age variable for analyses
data$w2_age_c <- scale(data$w2_age, center = T, scale = F) # center age 



# 3.3) Sex
# ------------

# recode sex into dummy (0 = female, 1 = male) 
data$w2_sex <- ifelse(data$w2_DhSex == 2, 0, 1)


# 3.4) Ethnicity
# ----------------

# recode to dummy (0 = "white", 1 = "non-white")
data$w2_eth <- ifelse(data$w2_fqethnr == 1, 0, 1)


# 3.5) Education
# ---------------

# recode to dummy (0 = lower education, 1 = higher education)
data$w0_topqual2[data$w0_topqual2 == 6] <- NA # recode foreign / other qual to NA
data$w0_topqual2[data$w0_topqual2 == 8] <- NA # recode full-time students to NA
data$w0_edu <- ifelse(data$w0_topqual2 == 1, 1, 0) 


# 3.6) Activities of daily living
# ---------------------------------

# Select only relevant variables from each wave
adl <- data[,c("idauniq",    "w2_heada01", "w2_heada02", "w2_heada03", "w2_heada04", 
               "w2_heada05", "w2_heada06", "w2_heada07", "w2_heada08", "w2_heada09", 
               "w2_heada10")]

# Get adl data from wave 2 (10 items)
adl_items <- select(adl, contains("heada"))

# Code 96 to 0 and all others to 1
# i.e. if people report any endorsement of a disability, the item is coded as 1
adl_items <- adl_items %>% mutate_all(funs(dplyr::recode(., '96' = 0, 
                                      '1'  = 1, 
                                      '2'  = 1, 
                                      '3'  = 1, 
                                      '4'  = 1, 
                                      '5'  = 1,
                                      '6'  = 1, 
                                      '7'  = 1, 
                                      '8'  = 1, 
                                      '9'  = 1, 
                                      '10' = 1, 
                                      '11' = 1, 
                                      '12' = 1,
                                      '13' = 1)))

# Calculate sum scores items
adl$w2_adl <- rowSums(adl_items, na.rm = T)

# Have to code people with al NAs to NA per hand here
adl[is.na(adl_items$heada01)   & 
      is.na(adl_items$heada02) & 
      is.na(adl_items$heada03) & 
      is.na(adl_items$heada04) & 
      is.na(adl_items$heada05) & 
      is.na(adl_items$heada06) & 
      is.na(adl_items$heada07) & 
      is.na(adl_items$heada08) & 
      is.na(adl_items$heada09) & 
      is.na(adl_items$heada10), "w2_adl"] <- NA

# Check how many people report ADLs
table(adl$w2_adl, useNA = "always")

# Merge data 
data <- merge(data, adl, all.x = T, by = "idauniq", sort = T)

# center variable for analyses
data$w2_adl_c <- scale(data$w2_adl, center = T, scale = F) 


# --------------------------
# 4) Vascular risk factors
# --------------------------

# 4.1) Smoking 
# --------------

# recode all dichotomous variables to dummies (0 = no, 1 = yes)
for (i in names(data[,c(grep("w2_HESka", colnames(data)), # current smoking (wave 2)
                        grep("w2_bheska",colnames(data)), # current smoking (wave 1)
                        grep("w2_bhesmk",colnames(data))  # ever smoked (wave 1)
)])) {
  data[[i]][data[[i]] == 2] <- 0}

# rename smoking variable 
data$w2_smok <- data$w2_HESka


# 4.2) Hypertension / Blood pressure
# --------------------------------------------

# average all three blood pressure values from wave 2 
data$w2_mean_sys <- rowMeans(data[,c("w2_sys1" , "w2_sys2" , "w2_sys3" )], na.rm = F)
data$w2_mean_dia <- rowMeans(data[,c("w2_dias1", "w2_dias2", "w2_dias3")], na.rm = F)

# create hypertension variable from CVD-variables at wave 1
data$w1_hypertension <- ifelse(data$w2_behdia01 == 1, 1, 0)
data$w1_hypertension[data$w2_behdia02 == 1] <- 1
data$w1_hypertension[data$w2_behdia03 == 1] <- 1
data$w1_hypertension[data$w2_behdia04 == 1] <- 1
data$w1_hypertension[data$w2_behdia05 == 1] <- 1
data$w1_hypertension[data$w2_behdia06 == 1] <- 1
data$w1_hypertension[data$w2_behdia07 == 1] <- 1

# create hypertension variable from CVD-variables at wave 2 
# (newly diagn. hypertension)
data$w2_hypertension <- ifelse(data$w2_hedia01 == 1, 1, 0)
data$w2_hypertension[data$w2_hedia02 == 1] <- 1
data$w2_hypertension[data$w2_hedia03 == 1] <- 1
data$w2_hypertension[data$w2_hedia04 == 1] <- 1
data$w2_hypertension[data$w2_hedia05 == 1] <- 1
data$w2_hypertension[data$w2_hedia06 == 1] <- 1
data$w2_hypertension[data$w2_hedia07 == 1] <- 1
data$w2_hypertension[data$w2_hedia08 == 1] <- 1
data$w2_hypertension[data$w2_hedia09 == 1] <- 1

# create variable that indicating whether participants ever reported having 
# hypertension in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_hypertension <- ifelse(data$w1_hypertension == 1, 1, 0)
data$w1_w2_hypertension[data$w2_hypertension == 1] <- 1

# finally, code variable that includes the measurement of blood pressure in the definition of hypertension
data$w2_n_hypertension <- ifelse(data$w2_mean_sys >= 130 & data$w2_mean_dia >= 80, 1, 0) # error?!
data$w1_w2_hypertension[data$w2_n_hypertension == 1] <- 1

# rename variable for analyses
data$w2_hypt <- data$w1_w2_hypertension


# 4.3) Diabetes / Blood glucose
# --------------------------------

# Only select participants with valid fasting blood samples 
data$w2_fglu_fasting <- ifelse(data$w2_fasteli == 2, NA, data$w2_fglu) 

# create diabetes variable from CVD-variables at wave 1
data$w1_diabetes <- ifelse(data$w2_behdia01 == 7, 1, 0)
data$w1_diabetes[data$w2_behdia02 == 7] <- 1
data$w1_diabetes[data$w2_behdia03 == 7] <- 1
data$w1_diabetes[data$w2_behdia04 == 7] <- 1
data$w1_diabetes[data$w2_behdia05 == 7] <- 1
data$w1_diabetes[data$w2_behdia06 == 7] <- 1
data$w1_diabetes[data$w2_behdia07 == 7] <- 1

# create diabetes var from CVD-variables at wave 2 (newly diagn. diabetes)
data$w2_diabetes <- ifelse(data$w2_hedia01 == 7, 1, 0)
data$w2_diabetes[data$w2_hedia02 == 7] <- 1
data$w2_diabetes[data$w2_hedia03 == 7] <- 1
data$w2_diabetes[data$w2_hedia04 == 7] <- 1
data$w2_diabetes[data$w2_hedia05 == 7] <- 1
data$w2_diabetes[data$w2_hedia06 == 7] <- 1
data$w2_diabetes[data$w2_hedia07 == 7] <- 1
data$w2_diabetes[data$w2_hedia08 == 7] <- 1
data$w2_diabetes[data$w2_hedia09 == 7] <- 1

# create variable indiating whether participants ever reported having 
# diabetes in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_diabetes <- ifelse(data$w1_diabetes == 1, 1, 0)
data$w1_w2_diabetes[data$w2_diabetes == 1] <- 1

# finally create variable that includes the measurement of blood glucose in the definition of diabetes
data$w2_n_diabetes <- ifelse(data$w2_fglu_fasting >= 7, 1, 0)
data$w1_w2_diabetes[data$w2_n_diabetes == 1] <- 1

# rename variable for analyses
data$w2_diab <- data$w1_w2_diabetes


# 4.4) Body mass index
# -----------------------

# create a variable that indicates obesity (BMI >= 30)
data$w2_obese <- ifelse(data$w2_bmival >= 30, 1, 0)


# 4.5) Hypercholesterolemia / cholesterol
# ---------------------------------------

# create hyperchol variable from CVD-variables at wave 1
data$w1_hypercholest <- ifelse(data$w2_behdia01 == 9, 1, 0)
data$w1_hypercholest[data$w2_behdia02 == 9]  <- 1
data$w1_hypercholest[data$w2_behdia03 == 9] <- 1
data$w1_hypercholest[data$w2_behdia04 == 9] <- 1
data$w1_hypercholest[data$w2_behdia05 == 9] <- 1
data$w1_hypercholest[data$w2_behdia06 == 9] <- 1
data$w1_hypercholest[data$w2_behdia07 == 9] <- 1

# create diabetes var from CVD-variables at wave 2 (newly diagn. high chol)
data$w2_hypercholest <- ifelse(data$w2_hedia01 == 9, 1, 0)
data$w2_hypercholest[data$w2_hedia02 == 9] <- 1
data$w2_hypercholest[data$w2_hedia03 == 9] <- 1
data$w2_hypercholest[data$w2_hedia04 == 9] <- 1
data$w2_hypercholest[data$w2_hedia05 == 9] <- 1
data$w2_hypercholest[data$w2_hedia06 == 9] <- 1
data$w2_hypercholest[data$w2_hedia07 == 9] <- 1
data$w2_hypercholest[data$w2_hedia08 == 9] <- 1
data$w2_hypercholest[data$w2_hedia09 == 9] <- 1

# create variable indiating whether participants ever reported having 
# high chol in wave 1 or 2 (0 = no, 1 = yes)
data$w1_w2_hypercholest <- ifelse(data$w1_hypercholest == 1, 1, 0)
data$w1_w2_hypercholest[data$w2_hypercholest == 1] <- 1

# finally create variable that includes the measurement of cholesterol
#data$w2_n_hypercholest  <- ifelse(data$w2_hypercholest >= 6.1, 1, 0)
#data$w2_n_hypercholest  <- ifelse(data$w2_chol >= 6.1, 1, 0)
data$w2_n_hypercholest <- ifelse(data$w2_ldl >= 4.1, 1, 0)
#data$w1_w2_hypercholest[data$w2_n_hypercholest == 1] <- 1
data$w1_w2_hypercholest[data$w2_n_hypercholest == 1] <- 1

# rename variable for analyses
data$w2_hchol <- data$w1_w2_hypercholest


# 4.6) Number of risk factors
# -------------------------------

# create variable that indicates the number of reported risk factors
risk_factors   <- data[,c("w2_hypt","w2_diab","w2_smok", "w2_obese", "w2_hchol")]
data$w2_cvrisk <- rowSums(risk_factors)

# center variable
data$w2_cvrisk_c <- scale(data$w2_cvrisk, center = T, scale = F) 


# -------------------------
# 5) Depressive symptoms
# -------------------------

# 5.0) Recode items to dummies (0 = no, 1 = yes)
for (i in names(data[,c(grep("psced", colnames(data)))])) {
  data[[i]][data[[i]] == 2] <- 0}

# item d has to be reversed for calculations of alpha
data$w2_pscedd_r <- 1 - data$w2_pscedd
data$w3_pscedd_r <- 1 - data$w3_pscedd
data$w4_pscedd_r <- 1 - data$w4_pscedd
data$w5_pscedd_r <- 1 - data$w5_pscedd
data$w6_pscedd_r <- 1 - data$w6_pscedd
data$w7_pscedd_r <- 1 - data$w7_pscedd

# item f has to be reversed for calculations of alpha
data$w2_pscedf_r <- 1 - data$w2_pscedf
data$w3_pscedf_r <- 1 - data$w3_pscedf
data$w4_pscedf_r <- 1 - data$w4_pscedf
data$w5_pscedf_r <- 1 - data$w5_pscedf
data$w6_pscedf_r <- 1 - data$w6_pscedf
data$w7_pscedf_r <- 1 - data$w7_pscedf


# 5.1) Measurement model 
# -------------------------

# One-factor model: affective symptoms (5 symptoms)

### define model
aff_m <- 'aff =~ w2_psceda + w2_pscedd + w2_pscede + w2_pscedf + w2_pscedg'

### fit model
aff_f <- cfa(model = aff_m, data = data, 
             ordered = c("w2_psceda", "w2_pscedd", "w2_pscede", "w2_pscedf", "w2_pscedg"), 
             missing = "pairwise", estimator = "WLSMV", 
             parameterization = "theta")

### show model results
summary(aff_f, fit.measures = TRUE, standardized = TRUE)
modindices(aff_f, sort = TRUE, minimum.value = 100) 


# 5.2) Internal consistency
# ---------------------------

# select affective items CES-D items for each wave 
w2_aff_items <- data[,c("w2_psceda", "w2_pscedd_r", "w2_pscede", "w2_pscedf_r", "w2_pscedg")] 
w3_aff_items <- data[,c("w3_psceda", "w3_pscedd_r", "w3_pscede", "w3_pscedf_r", "w3_pscedg")] 
w4_aff_items <- data[,c("w4_psceda", "w4_pscedd_r", "w4_pscede", "w4_pscedf_r", "w4_pscedg")] 
w5_aff_items <- data[,c("w5_psceda", "w5_pscedd_r", "w5_pscede", "w5_pscedf_r", "w5_pscedg")] 
w6_aff_items <- data[,c("w6_psceda", "w6_pscedd_r", "w6_pscede", "w6_pscedf_r", "w6_pscedg")]
w7_aff_items <- data[,c("w7_psceda", "w7_pscedd_r", "w7_pscede", "w7_pscedf_r", "w7_pscedg")]

# calculate Cronbach's alpha 
# note: reduces to Kuder-Richardson formula for dichotomous items (implemented in alpha())
psych::alpha(w2_aff_items) 
psych::alpha(w3_aff_items) 
psych::alpha(w4_aff_items)
psych::alpha(w5_aff_items)
psych::alpha(w6_aff_items)
psych::alpha(w7_aff_items)


# 5.3) Calculate sum scores
# --------------------------------------
# i.e. proportion of yes answers for descriptive stats

# 5.3.1) Affective symptoms
data$w2_aff_sum <- rowSums(w2_aff_items)
data$w3_aff_sum <- rowSums(w3_aff_items)
data$w4_aff_sum <- rowSums(w4_aff_items)
data$w5_aff_sum <- rowSums(w5_aff_items)
data$w6_aff_sum <- rowSums(w6_aff_items)
data$w7_aff_sum <- rowSums(w7_aff_items)

# 5.3.2) Overall score 
# select all CES-D items for each wave 
w2_dep_items <- data[,c("w2_psceda", "w2_pscedb", "w2_pscedc", "w2_pscedd_r", "w2_pscede", "w2_pscedf_r", "w2_pscedg", "w2_pscedh")] # wave 2
w3_dep_items <- data[,c("w3_psceda", "w3_pscedb", "w3_pscedc", "w3_pscedd_r", "w3_pscede", "w3_pscedf_r", "w3_pscedg", "w3_pscedh")] # wave 3
w4_dep_items <- data[,c("w4_psceda", "w4_pscedb", "w4_pscedc", "w4_pscedd_r", "w4_pscede", "w4_pscedf_r", "w4_pscedg", "w4_pscedh")] # wave 4
w5_dep_items <- data[,c("w5_psceda", "w5_pscedb", "w5_pscedc", "w5_pscedd_r", "w5_pscede", "w5_pscedf_r", "w5_pscedg", "w5_pscedh")] # wave 5
w6_dep_items <- data[,c("w6_psceda", "w6_pscedb", "w6_pscedc", "w6_pscedd_r", "w6_pscede", "w6_pscedf_r", "w6_pscedg", "w6_pscedh")] # wave 6
w7_dep_items <- data[,c("w7_psceda", "w7_pscedb", "w7_pscedc", "w7_pscedd_r", "w7_pscede", "w7_pscedf_r", "w7_pscedg", "w7_pscedh")] # wave 7

# Calculate Score
data$w2_dep_sum <- rowSums(w2_dep_items)
data$w3_dep_sum <- rowSums(w3_dep_items)
data$w4_dep_sum <- rowSums(w4_dep_items)
data$w5_dep_sum <- rowSums(w5_dep_items)
data$w6_dep_sum <- rowSums(w6_dep_items)
data$w7_dep_sum <- rowSums(w7_dep_items)


# ---------------------------
# 6) Save preprocessed data
# ---------------------------

save(data, file = "data/elsa/processed/elsa_proc_data.RData")

