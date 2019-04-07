## ===============================================================
## MIDUS: Data preprocessing of data set for 
## (contact maria.bloechl@uni-leipzig.de in case of questions)
## ==============================================================

# Delimited data files can be downloaded from the ICPR website (login necessary)
# data wave 1: https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/2760
# data wave 2: https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/4652 
# data wave 3: https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36346 

# -------------------------
# 0) Preparations 
# -------------------------

# clean work space
rm(list = ls()) 

# load packages
library(dplyr)
library(lavaan)
library(psych)


# --------------------------------------
# 1) Get raw data and select variables
# --------------------------------------

# load raw data all waves
load("data/raw/02760-0001-Data.rda") # wave 1
load("data/raw/04652-0001-Data.rda") # wave 2
load("data/raw/36346-0001-Data.rda") # wave 3


# select only relevant varibales (see codebooks for variable labels)
wave1_s <- da02760.0001[,c("M2ID", "A1PAGE_M2", "A1SS7", "A1PRSEX", "A1PB1", 
                           "A1PA4", "A1PA5", "A1PA11BC", "A1SA9X", "A1PA40", "A1SBMI",
                           "A1PDEPAF", "A1PANHED", "A1PDEPRE",
                           "A1PA36", "A1SA9Y", "A1SA9S", "A1SA10A", "A1SA10C", 
                           "A1SA13A", "A1SA13B", "A1SA13C", "A1SA13D", "A1SA13E", "A1SA13F", 
                           "A1SA15A", "A1SA15B", "A1SA15C", "A1SA15D", "A1SA15E", "A1SA15F",
                           "A1SA9Z", "A1SA10K",
                           "A1PA31", "A1PA33", 
                           "A1SF1A", "A1SF1B", "A1SF1C", "A1SF1D", "A1SF1E", "A1SF1F", "A1SF1G", "A1SF1H", "A1SF1I", 
                           "A1SF1J", "A1SF1K", "A1SF1L", "A1SF1M", "A1SF1N", "A1SF1O", "A1SF1P", "A1SF1Q", "A1SF1R", 
                           "A1PA43", "A1PA41")]

wave2_s <- da04652.0001[,c("M2ID", "B1PAGE_M2", "B1PB1", "B1PF7A", "B1PB19", "B1PB30",
                           "B1PA1", "B1PA2", "B1PA7BC", "B1SA11X", "B1PA38A", "B1SBMI", 
                           "B1PDEPAF", "B1PANHED", "B1PDEPRE",
                           "B1SA11S", "B1PA12", "B1SA12A", "B1SA12C",
                           "B1PA26", "B1PA8", "B1PA6D", "B1SA11Y", 
                           "B1SA24A", "B1SA24B", "B1SA24C", "B1SA24D", "B1SA24E", "B1SA24F", 
                           "B1SA26A", "B1SA26B", "B1SA26C", "B1SA26D", "B1SA26E", "B1SA26F",
                           "B1SA11Z", "B1PA6A", "B1SA12K",
                           "B1SA28A", "B1SA28B", "B1SA28C", "B1SA28E", "B1SA28F", "B1SA28G", "B1SA28H", "B1SA28I","B1SA28J", 
                           "B1SE1X", "B1SE1F", "B1SE1OO", "B1SE1H", "B1SE1DD", "B1SE1J", "B1SE1E", "B1SE1B", "B1SE1T", 
                           "B1SE1QQ", "B1SE1AA", "B1SE1I", "B1SE1BB", "B1SE1GG", "B1SE1M", "B1SE1HH", "B1SE1S", "B1SE1KK", 
                           "B1PA39", "B1PA37")]

wave3_s <- da36346.0001[,c("M2ID", "C1PRAGE", "C1PB1", 
                           "C1PA1", "C1PA2", "C1PA7BC", "C1SA11X", "C1PA38A", "C1SBMI", 
                           "C1PDEPAF", "C1PANHED", "C1PDEPRE",
                           "C1PA26", "C1PA8","C1SA11Y",
                           "C1SA20A", "C1SA20B", "C1SA20C", "C1SA20D", "C1SA20E", "C1SA20F", 
                           "C1SA22A", "C1SA22B", "C1SA22C", "C1SA22D", "C1SA22E", "C1SA22F",
                           "C1SA11Z", "C1PA6A", 
                           "C1SA24A", "C1SA24B", "C1SA24C", "C1SA24E", "C1SA24F", "C1SA24G", "C1SA24H", "C1SA24I", "C1SA24J", 
                           "C1SNEGAF", "C1SPOSAF", "C1SA12K", 
                           "C1SE1X", "C1SE1F", "C1SE1OO", "C1SE1H", "C1SE1DD", "C1SE1J", "C1SE1E", "C1SE1B", "C1SE1T", 
                           "C1SE1QQ", "C1SE1AA", "C1SE1I", "C1SE1BB", "C1SE1GG", "C1SE1M", "C1SE1HH", "C1SE1S", "C1SE1KK", 
                           "C1PA39", "C1PA37")]

# Create variables indicating whether people participated in wave: YES
wave1_s$wave1 <- 1
wave2_s$wave2 <- 1
wave3_s$wave3 <- 1

# We have to code missing values from wave 2 for smoking to "NO", because question was actually inapplicable
# to some participants who responded that they never smoked (A1PA41) or never smoked regularly (A1PA40).
wave1_s$A1PA43[wave1_s$A1PA41 == 96]         <- "(2) NO"
wave1_s$A1PA43[wave1_s$A1PA40 == "(2) NO"]   <- "(2) NO"

# merge data from all waves into one dataframe (wide format)
data = merge(wave1_s, wave2_s, all = T, by = "M2ID", sort = T)
data = merge(data,    wave3_s, all = T, by = "M2ID", sort = T )


# ------------------------
# 2) Recode variables
# ------------------------

# Variables indicating participation in specific wave: NO
data$wave1[is.na(data$wave1)] <- 0
data$wave2[is.na(data$wave2)] <- 0
data$wave3[is.na(data$wave3)] <- 0

# gender 
data$A1PRSEX  <- recode_factor(data$A1PRSEX, `(1) MALE` = 1, `(2) FEMALE` = 0)

# ethnicity (decided to code as binary variable since most participnats identified themself as "white")
data$A1SS7    <- ifelse(data$A1SS7 == "(1) WHITE",  0, 1) 

table(data$A1SS7, exclude = NULL) # about 900 missings on this variable
table(data$A1SS7, data$B1PF7A, exclude = NULL) # > 350 people with missing in wave 1 have data in wave 2
data$B1PF7A <- ifelse(data$B1PF7A == "(1) WHITE", 1,
                      ifelse(data$B1PF7A == "(7) DON'T KNOW", NA, 
                             ifelse(data$B1PF7A == "(8) REFUSED", NA, 0)
                             )
                      )

# top up ethnicity variable with wave 2 data to replace missing values from wave 1 (racial identiication is assumed to be relatively stable)
for(i in 1:nrow(data)){
       if (is.na(data$A1SS7[i]) == T){
         data$A1SS7[i] <- data$B1PF7A[i]}} 
  
# education 
data$A1PB1    <- as.integer(data$A1PB1) 
data$A1PB1    <- ifelse(data$A1PB1 <= 5, 0, 1) # now indicates higher education level

data$B1PB1    <- as.integer(data$B1PB1) 
data$B1PB1    <- ifelse(data$B1PB1 <= 5, 0, 1) # now indicates higher education level


# creating a variable indicating: married or living wit someone, wave 2
data$B1PB19 <- ifelse(data$B1PB19 == "(1) MARRIED", 1, 0) # now indicates married or not
data$B1PB30 <- ifelse(data$B1PB30 == "(1) YES", 1, 0) # variable cohabitation

data$livsome <- ifelse(data$B1PB19 == 1 | data$B1PB30 == 1, 1,0)

# recode all binary variables
binary.vars <- data[,c("A1PA11BC", "A1SA9S", "A1SA9X", "A1PA40", "A1SA10A", "A1SA10C", "A1SA10K", 
                       "A1PA33", "A1PA36", "A1PA43", "A1SA9Y", "A1SA9Z", 
                       
                       "B1PA6A", "B1PA8", "B1PA26", "B1PA6D", "B1SA11X", "B1SA11S", "B1SA12K", 
                       "B1PA12", "B1SA12A", "B1PA39", "B1SA12C", "B1SA11Y", "B1SA11Z", "B1PA38A", 
                       "B1PA7BC",
                       
                       "C1SA11X", "C1PA38A", "C1SA11Y", "C1PA6A", "C1SA11Z", "C1PA8", "C1PA26", "C1PA7BC", 
                       "C1PA39")]

for (i in names(binary.vars)) {
  data[[i]] <- recode_factor(data[[i]],`(1) YES` = 1, `(2) NO` = 0)}

# recode factors to numeric for modelling in lavaan (doesn't like factors)
data$A1PRSEX.n   <- as.integer(as.character(data$A1PRSEX))
data$A1SS7.n     <- as.integer(as.character(data$A1SS7)) # ethnicity
data$A1PB1.n     <- as.integer(as.character(data$A1PB1)) # education
data$A1SA9S.n    <- as.integer(as.character(data$A1SA9S)) # hypertension
data$A1PA43.n    <- as.integer(as.character(data$A1PA43)) # smoking
data$A1SA9X.n    <- as.integer(as.character(data$A1SA9X)) # diabetes
#data$A1SA10A.n   <- as.integer(as.character(data$A1SA10A)) # hypertension medication

# create variable that indicates the number of risk factors
data$A1SBMI.b <- ifelse(data$A1SBMI >=30,1,0)
rf            <- data[,c("A1SA9X.n","A1SA9S.n","A1PA43.n", "A1SBMI.b")]
data$n_rf     <- rowSums(rf)

# depressed affect 
oldvalues.depr <- c("(1) ALL THE TIME", "(2) MOST OF THE TIME", "(3) SOME OF THE TIME", "(4) A LITTLE OF THE TIME", "(5) NONE OF THE TIME")
newvalues.depr <- (c(5,4,3,2,1))  

for (i in names(data[,c(grep("A1SA13", colnames(data)), # wave 1
                        grep("B1SA24", colnames(data)), # wave 2
                        grep("C1SA20", colnames(data)) # wave 3
                        )])) {
  data[[i]] <- newvalues.depr[ match(data[[i]], oldvalues.depr) ]}


# ----------------------------------------------
# 3) Unidimensionality: depressed affect scale
# ----------------------------------------------

# 3.a.) CFA models: all items 

### wave 1
depr.1.m <- 'depr.1 =~ A1SA13A + A1SA13B + A1SA13C + A1SA13D + A1SA13E + A1SA13F'
depr.1.f <- cfa(model = depr.1.m, data = data)
summary(depr.1.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.1.f, sort = TRUE, minimum.value = 100) # modification indices

### wave 2 
depr.2.m <- 'depr.2 =~ B1SA24A + B1SA24B + B1SA24C + B1SA24D + B1SA24E + B1SA24F'
depr.2.f <- cfa(model = depr.2.m, data = data)
summary(depr.2.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.2.f, sort = TRUE, minimum.value = 100) # modification indices

### wave 3 
depr.3.m <- 'depr.3 =~ C1SA20A + C1SA20B +  C1SA20C + C1SA20D + C1SA20E + C1SA20F'
depr.3.f <- cfa(model = depr.3.m, data = data)
summary(depr.3.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.3.f, sort = TRUE, minimum.value = 100) # modification indices


# 3.b) CFA models: without item 3 
# Given the poor fit of all of the above specified CFA models and the high residual correlation
# between items '-B' ('restless') and '-C' ('nervous'), item C was excluded from the following CFA models. 

### wave 1
depr.1.m.r <- 'depr.1 =~ A1SA13A + A1SA13B + A1SA13D + A1SA13E + A1SA13F'
depr.1.r <- cfa(model = depr.1.m.r, data = data)
summary(depr.1.r, fit.measures = TRUE, standardized = TRUE)
modindices(depr.1.r, sort = TRUE, minimum.value = 100) # modification indices

### wave 2 
depr.2.m <- 'depr.2 =~ B1SA24A + B1SA24B +  B1SA24D + B1SA24E + B1SA24F'
depr.2.f <- cfa(model = depr.2.m, data = data)
summary(depr.2.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.2.f, sort = TRUE, minimum.value = 100) # modification indices

### wave 3
depr.3.m <- 'depr.3 =~ C1SA20A + C1SA20B + C1SA20D + C1SA20E + C1SA20F'
depr.3.f <- cfa(model = depr.3.m, data = data)
summary(depr.3.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.3.f, sort = TRUE, minimum.value = 100) # modification indices


# -----------------------------------------
# 4) Reliability: depressed affect scale
# -----------------------------------------

# select items depressed affect for each wave (note tis eclude item -C (nervous))
depr.items.1 <- data[,c("A1SA13A", "A1SA13B", "A1SA13D", "A1SA13E", "A1SA13F")] # wave 1
depr.items.2 <- data[,c("B1SA24A", "B1SA24B", "B1SA24D", "B1SA24E", "B1SA24F")] # wave 2
depr.items.3 <- data[,c("C1SA20A", "C1SA20B", "C1SA20D", "C1SA20E", "C1SA20F")] # wavw 3

# cronbachs alpha
psych::alpha(depr.items.1) # wave 1
psych::alpha(depr.items.2) # wave 2
psych::alpha(depr.items.3) # wave 3


# --------------------------------------
# 5) Calculate depression mean scores
# --------------------------------------

depr.items.1 <- data[,c("A1SA13A", "A1SA13B",  "A1SA13D", "A1SA13E", "A1SA13F")]
depr.items.2 <- data[,c("B1SA24A", "B1SA24B",  "B1SA24D", "B1SA24E", "B1SA24F")]
depr.items.3 <- data[,c("C1SA20A", "C1SA20B",  "C1SA20D", "C1SA20E", "C1SA20F")]

data$w1.deprmean <- rowMeans(depr.items.1) 
data$w2.deprmean <- rowMeans(depr.items.2)
data$w3.deprmean <- rowMeans(depr.items.3)


# ---------------------------
# 6) Save preprocessed data
# ---------------------------

save(data, wave1_s, wave2_s, wave3_s, file = "data/processed/midus_proc_data.RData")

