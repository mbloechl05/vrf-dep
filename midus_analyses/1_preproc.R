# ===============================================================
# MIDUS: Data preprocessing 
# (contact maria.bloechl@gmail.com in case of questions)
# ==============================================================

# clean work space
rm(list = ls()) 

# load packages
library(dplyr)
library(lavaan)
library(psych)


# ----------------------------------------------------
# 1) Read in raw data, select variables, and merge 
# ----------------------------------------------------

# load raw data all waves
load("data/midus/raw/02760-0001-Data.rda") # wave 1
load("data/midus/raw/04652-0001-Data.rda") # wave 2
load("data/midus/raw/36346-0001-Data.rda") # wave 3

# select only relevant varibales from waves (see codebook for explanations)
wave1_s <- da02760.0001[,c("M2ID", "A1PAGE_M2", "A1PRSEX", "A1SS7",  "A1PB1", "A1SA9X", "A1PA43", "A1SBMI", "A1SA9S", 
                           "A1SA13A", "A1SA13B", "A1SA13C", "A1SA13D", "A1SA13E", "A1SA13F", "A1PA33", "A1SA10A","A1SA10K")]

wave2_s <- da04652.0001[,c("M2ID", "B1PAGE_M2", "B1SA24A", "B1SA24B", "B1SA24C", "B1SA24D", "B1SA24E", "B1SA24F")]

wave3_s <- da36346.0001[,c("M2ID", "C1PRAGE", "C1SA20A", "C1SA20B", "C1SA20C", "C1SA20D", "C1SA20E", "C1SA20F")]

# Create variables indicating whether people participated in wave: YES
wave1_s$wave1 <- 1
wave2_s$wave2 <- 1
wave3_s$wave3 <- 1

# We have to code missing values from wave 2 for smoking to "NO", because question was actually inapplicable
# to some participants who responded that they never smoked (A1PA41) or never smoked regularly (A1PA40).
wave1_s$A1PA43[wave1_s$A1PA41 ==  96     ]   <- "(2) NO"
wave1_s$A1PA43[wave1_s$A1PA40 == "(2) NO"]   <- "(2) NO"

# merge data from all waves into one dataframe (wide format)
data = merge(wave1_s, wave2_s, all = T, by = "M2ID", sort = T)
data = merge(data,    wave3_s, all = T, by = "M2ID", sort = T )


# ------------------------
# 2) Recode variables
# ------------------------

# 2.1) Age
# center age variable for analyses
data$A1PAGE_M2.c <- scale(data$A1PAGE_M2, center = T, scale = F) 

# 2.1) Sex
# recode to dummy (0 = female, 1 = male)
data$A1PRSEX  <- recode_factor(data$A1PRSEX, `(1) MALE` = 1, `(2) FEMALE` = 0)

# 2.2) Ethnicity 
# recode variable from wave 1 to dummy (0 = "white", 1 = "non-white")
data$A1SS7    <- ifelse(data$A1SS7 == "(1) WHITE",  0, 1) 

# recode variable from wave 2 to dummy (0 = "white", 1 = "non-white")
data$B1PF7A <- ifelse(data$B1PF7A == "(1) WHITE", 1,
                      ifelse(data$B1PF7A == "(7) DON'T KNOW", NA, 
                             ifelse(data$B1PF7A == "(8) REFUSED", NA, 0)
                             )
                      )

# a lot of people (about 900) do not have data on this variable at wave 1, but at wave 2
# so this replaces missing values from wave 1 with data from wave 2
for(i in 1:nrow(data)){
       if (is.na(data$A1SS7[i]) == T){
         data$A1SS7[i] <- data$B1PF7A[i]}} 
  
# 2.3) Education 
# recode to dummy (0 = lower education, 1 = higher education)
data$A1PB1    <- as.integer(data$A1PB1) 
data$A1PB1    <- ifelse(data$A1PB1 <= 5, 0, 1) # now indicates higher education level


# 2.4) Dichotomous variables
# recode all dichotomous variables to dummies (0 = no, 1 = yes)
binary.vars <- data[,c("A1PA11BC", "A1SA9S", "A1SA9X", "A1PA40", "A1SA10A", "A1SA10C", "A1SA10K", 
                       "A1PA33", "A1PA36", "A1PA43", "A1SA9Y", "A1SA9Z", 
                       
                       "B1PA6A", "B1PA8", "B1PA26", "B1PA6D", "B1SA11X", "B1SA11S", "B1SA12K", 
                       "B1PA12", "B1SA12A", "B1PA39", "B1SA12C", "B1SA11Y", "B1SA11Z", "B1PA38A", 
                       "B1PA7BC",
                       
                       "C1SA11X", "C1PA38A", "C1SA11Y", "C1PA6A", "C1SA11Z", "C1PA8", "C1PA26", "C1PA7BC", 
                       "C1PA39")]

for (i in names(binary.vars)) {
  data[[i]] <- recode_factor(data[[i]],`(1) YES` = 1, `(2) NO` = 0)}

# 2.5) Recode factors
# recode factors to numeric for modelling in lavaan (doesn't like factors)
data$A1PRSEX.n   <- as.integer(as.character(data$A1PRSEX))
data$A1SS7.n     <- as.integer(as.character(data$A1SS7)) 
data$A1PB1.n     <- as.integer(as.character(data$A1PB1))
data$A1SA9S.n    <- as.integer(as.character(data$A1SA9S)) 
data$A1PA43.n    <- as.integer(as.character(data$A1PA43)) 
data$A1SA9X.n    <- as.integer(as.character(data$A1SA9X)) 

# 2.6.) Number of risk factors
# create a variable that indicates obesity (BMI >= 30)
data$A1SBMI.b <- ifelse(data$A1SBMI >=30,1,0)

# create variable that indicates the number of reported risk factors
rf            <- data[,c("A1SA9X.n","A1SA9S.n","A1PA43.n", "A1SBMI.b")]
data$n_rf     <- rowSums(rf)

# 2.7) K-6 scale
# define values of items of the scale and how they should be recoded
oldvalues.depr <- c("(1) ALL THE TIME", "(2) MOST OF THE TIME", "(3) SOME OF THE TIME", "(4) A LITTLE OF THE TIME", "(5) NONE OF THE TIME")
newvalues.depr <- (c(5,4,3,2,1))  

# for all K-6 items, recode values so higher values indicate poorer mood
for (i in names(data[,c(grep("A1SA13", colnames(data)), # wave 1
                        grep("B1SA24", colnames(data)), # wave 2
                        grep("C1SA20", colnames(data)) # wave 3
)])) {
  data[[i]] <- newvalues.depr[ match(data[[i]], oldvalues.depr) ]}


# ----------------------------------------------
# 3) K-6 scale
# ----------------------------------------------

# 3.1) Unidimensionality
# CFA full 6-item scale
depr.1.m <- 'depr.1 =~ A1SA13A + A1SA13B + A1SA13C + A1SA13D + A1SA13E + A1SA13F'
depr.1.f <- cfa(model = depr.1.m, data = data)
summary(depr.1.f, fit.measures = TRUE, standardized = TRUE)
modindices(depr.1.f, sort = TRUE, minimum.value = 100) # modification indices

# CFA reduced 5-item scale
depr.1.m.r <- 'depr.1 =~ A1SA13A + A1SA13B + A1SA13D + A1SA13E + A1SA13F'
depr.1.r <- cfa(model = depr.1.m.r, data = data)
summary(depr.1.r, fit.measures = TRUE, standardized = TRUE)
modindices(depr.1.r, sort = TRUE, minimum.value = 100) # modification indices


# 3.2) Internal consistency
# select items depressed affect for each wave (note tis eclude item -C (nervous))
depr.items.1 <- data[,c("A1SA13A", "A1SA13B", "A1SA13D", "A1SA13E", "A1SA13F")] # wave 1
depr.items.2 <- data[,c("B1SA24A", "B1SA24B", "B1SA24D", "B1SA24E", "B1SA24F")] # wave 2
depr.items.3 <- data[,c("C1SA20A", "C1SA20B", "C1SA20D", "C1SA20E", "C1SA20F")] # wavw 3

# calculate Cronbach's alpha
psych::alpha(depr.items.1) # wave 1
psych::alpha(depr.items.2) # wave 2
psych::alpha(depr.items.3) # wave 3

# 3.3) Calculate means for descriptive stats
data$w1_deprmean <- rowMeans(depr.items.1)
data$w2_deprmean <- rowMeans(depr.items.2)
data$w3_deprmean <- rowMeans(depr.items.3)


# ----------------------------
# 4) Save preprocessed data
# ----------------------------

save(data, wave1_s, wave2_s, wave3_s, file = "data/midus/processed/midus_proc_data.RData")

