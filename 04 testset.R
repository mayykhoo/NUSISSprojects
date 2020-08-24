install.packages('pacman', repos = "http://cran.us.r-project.org")
library(SASxport)
library(plyr)
library(dplyr)
library(tidyverse)
library(splitstackshape)
library(mice)
library(VIM)
library(caret)
library(mlbench)
library(kknn)
library(fastmatch)
library(matrixStats)
setwd("C:/Users/May Khoo/Desktop/NUS/Internship")

testset = readRDS("testset.RDS")  #530 variables
testset_select = readRDS("testset_select.RDS")   #129 variables

#########################################################################
#   Cleaning and Feature Engineering - ALQ120Q and ALQ120U (testset)
#########################################################################

#combining ALQ120Q and ALQ120U to get standardised units: week, month, year now exists.

ALQ120 = cbind(testset_select$SEQN, testset_select$ALQ120Q, testset_select$ALQ120U)
colnames(ALQ120) = c('SEQN', 'ALQ120Q', 'ALQ120U')
class(ALQ120)
ALQ120 = data.frame(ALQ120)
str(ALQ120)
ALQ120$ALQ120U = as.factor(ALQ120$ALQ120U)
levels(ALQ120$ALQ120U) = c("week","month","year")

#standardise week to year

week = ALQ120 %>% filter(ALQ120U == 'week')
week$ALQ120Q = week$ALQ120Q * 52

#standardise month to year

month = ALQ120 %>% filter(ALQ120U == 'month')
month$ALQ120Q = month$ALQ120Q * 12

#replace week and month back into ALQ120

year = ALQ120 %>% filter(ALQ120U == 'year')
alq120na = ALQ120 %>% filter(is.na(ALQ120U))
ALQ120QU = rbind(year, month, week, alq120na)

ALQ120QU <- arrange(ALQ120QU, SEQN)
ALQ120QU <- ALQ120QU[,1:2]
summary(ALQ120QU)
colnames(ALQ120QU) = c('SEQN', 'ALQ120QU')

#add this variable to testset_select dataframe
testset_select2 = arrange(testset_select, SEQN)
testset_select2$ALQ120QU = ALQ120QU$ALQ120QU
glimpse(testset_select2)

#remove original ALQ120Q and ALQ120U
testset_select3 = subset(testset_select2, select = c(-ALQ120Q, -ALQ120U))
summary(testset_select3)

#0-365 days per year, 999 to recode to NA
testset_select3$ALQ120QU <- replace(testset_select3$ALQ120QU, testset_select3$ALQ120QU == 999, NA)
summary(testset_select3$ALQ120QU)

saveRDS(testset_select2, file = "testset_select2.RDS") 
saveRDS(testset_select3, file = "testset_select3.RDS")


#########################################################################
#   Cleaning and Feature Engineering - ALQ141Q and ALQ141U (testset)
#########################################################################

#combining ALQ141Q and ALQ141U to get standardised units: week, month, year now exists.

ALQ141 = cbind(testset_select$SEQN, testset_select$ALQ141Q, testset_select$ALQ141U)
colnames(ALQ141) = c('SEQN', 'ALQ141Q', 'ALQ141U')
class(ALQ141)
ALQ141 = data.frame(ALQ141)
str(ALQ141)
ALQ141$ALQ141U = as.factor(ALQ141$ALQ141U)
levels(ALQ141$ALQ141U) = c("week","month","year")

#standardise week to year

week = ALQ141 %>% filter(ALQ141U == 'week')
week$ALQ141Q = week$ALQ141Q * 52

# ALQ141 %>% group_by(ALQ141U) %>%
#   filter(ALQ141U == 'week') %>% mutate(ALQ141Q * 52)

#standardise month to year

month = ALQ141 %>% filter(ALQ141U == 'month')
month$ALQ141Q = month$ALQ141Q * 12

#replace week and month back into ALQ141

year = ALQ141 %>% filter(ALQ141U == 'year')
alqna = ALQ141 %>% filter(is.na(ALQ141U))
ALQ141QU = rbind(year, month, week, alqna)

ALQ141QU <- arrange(ALQ141QU, SEQN)
ALQ141QU <- ALQ141QU[,1:2]
summary(ALQ141QU)
colnames(ALQ141QU) = c('SEQN', 'ALQ141QU')

#add this variable to testset_select dataframe
testset_select3$ALQ141QU = ALQ141QU$ALQ141QU
glimpse(testset_select3)

#remove original ALQ141Q and ALQ141U
testset_select3 = subset(testset_select3, select = c(-ALQ141Q, -ALQ141U))
summary(testset_select3)

saveRDS(testset_select3, file = "testset_select3.RDS")

#to read it

testset_select3 = readRDS("testset_select3.RDS")

################################################################
#   Cleaning and Feature Engineering - DIQ010 (testset)
################################################################

#1 for diagnosed; 
#if answer No (2), check fasting plasma glucose level more than or = 126; 
#FPG 100-125 (pre-diabetes/borderline); 
#less than or = 100 - No diabetes

diabetes = cbind(testset_select3$SEQN, testset_select3$DIQ010, testset_select3$LBXGLU)
colnames(diabetes) = c('SEQN', 'DIQ010', 'LBXGLU')
class(diabetes)
diabetes = data.frame(diabetes)

#clean missing values of LBXGLU with median
#no missing values for DIQ010
summary(diabetes)
#median for LBXGLU is 100

diabetes2 = data.frame(sapply(diabetes, function(x) ifelse(is.na(x), median(x, na.rm = TRUE),x)))
sum(is.na(diabetes2))

#duplicate DIQ010 2x
diabetes2 = diabetes2 %>% 
  mutate(DIQ010A = DIQ010) %>%
  mutate(DIQ010B = DIQ010)

#DIQ010A: 1 with diabetes (told yes or FPG >= 126), 2 without diabetes (told no or FPG< 126)
#DIQ010B: 1 (yes/1 in DIQ010 or borderline/3 with FPG >= 126 in DIQ010); 2 (no/2 in DIQ010 with FPG >= 126);
#         3 (prediabetes; FPG 100-125); 4 (no diabetes FPG <= 100)

#check DIQ010 = 1 and DIQ010A = 1 and DIQ010B = 1
with_diabetes = diabetes2 %>% filter(DIQ010 == 1) #45 observations
#LBXGLU have less than 126

#check DIQ010=3 (Borderline) and recode for DIQ010A and DIQ010B accordingly

borderline = diabetes2 %>% filter(DIQ010 == 3)  #8 observations

borderline_highFPG = borderline %>% filter(DIQ010 == 3) %>% filter(LBXGLU >= 126) #0 observations

borderline$DIQ010A[borderline$LBXGLU >= 126] <- 1
borderline$DIQ010A[borderline$LBXGLU < 126]  <- 2
summary(borderline)

borderline$DIQ010B[borderline$LBXGLU >= 126] <- 1
borderline$DIQ010B[borderline$LBXGLU > 100 & borderline$LBXGLU < 125]  <- 3
borderline$DIQ010B[borderline$LBXGLU <= 100] <- 4
summary(borderline)

#check that DIQ010=2 with FPG level more than or = 126 and recode to 1 for DIQ010A and 2 for DIQ010B

undiagnosed = diabetes2 %>% filter(DIQ010 == 2) %>% filter(LBXGLU >= 126) #7 observations
undiagnosed$DIQ010A[undiagnosed$LBXGLU >= 126] <- 1
undiagnosed$DIQ010B[undiagnosed$LBXGLU >= 126] <- 2

#check that DIQ010=2 with FPG level 100-125 and recode to 3 for DIQ010B
prediabetes = diabetes2 %>% filter(DIQ010 == 2) %>% filter(LBXGLU > 100 & LBXGLU < 125) #54 observations
prediabetes$DIQ010B[prediabetes$LBXGLU > 100 & prediabetes$LBXGLU < 125] <- 3
summary(prediabetes)

#check that DIQ010=2 with FPG level <= 100 and recode to 4 for DIQ010B
no_diabetes = diabetes2 %>% filter(DIQ010 == 2) %>% filter(LBXGLU <= 100) #288 observations
no_diabetes$DIQ010B[no_diabetes$LBXGLU <= 100] <- 4
summary(no_diabetes)

diabetes3 = rbind(with_diabetes, undiagnosed, borderline, prediabetes, no_diabetes)
summary(diabetes3)
diabetes3 <- arrange(diabetes3, SEQN)

#add the 2 new variables and replace LBXGLU to testset_select3 dataframe
testset_select3$DIQ010A = diabetes3$DIQ010A
testset_select3$DIQ010B = diabetes3$DIQ010B
testset_select3$LBXGLU = diabetes3$LBXGLU
glimpse(testset_select3)

saveRDS(testset_select3, file = "testset_select3.RDS")

#to read it

testset_select3 = readRDS("testset_select3.RDS")

#remove original DIQ010
testset_select4 = subset(testset_select3, select = c(-DIQ010))
summary(testset_select4)

saveRDS(testset_select4, file = "testset_select4.RDS")

testset_select4 = readRDS("testset_select4.RDS")

################################################################
#       Cleaning - CBC and SBP NA values (testset)
################################################################
# Binh: if remove, then future participants without results of these tests will not be able to be predicted.
# summary(testset_select4)
# 
# #check how many participants do not have BOTH complete blood count and standard biochemistry profiles
# 
# CBCSBP_na = testset_select4 %>%
#   select(SEQN, LBXWBCSI, LBXLYPCT, LBXMOPCT, LBXNEPCT, LBXEOPCT, LBXBAPCT, LBDLYMNO, LBDMONO, LBDNENO, LBDEONO, LBDBANO, LBXRBCSI,
#          LBXHGB, LBXHCT, LBXMCVSI, LBXMCHSI, LBXMC, LBXRDW, LBXPLTSI, LBXMPSI, LBXSAL, LBXSAPSI, LBXSASSI,
#          LBXSATSI, LBXSBU, LBXSC3SI, LBXSCA, LBXSCH, LBXSCK, LBXSCLSI, LBXSCR, LBXSGB, LBXSGL, LBXSGTSI, LBXSIR, LBXSKSI,
#          LBXSLDSI, LBXSNASI, LBXSOSSI, LBXSPH, LBXSTB, LBXSTP, LBXSTR, LBXSUA) %>%
#   filter_at(vars(starts_with("LB")), all_vars(is.na(.)))
# 
# #96 observations (about 10% of the dataset; will remove the participants)
# 
# testset_select5 = anti_join(testset_select4, CBCSBP_na)
# summary(testset_select5)

#########################################
#       Cleaning - WHQ150 (testset)
#########################################
summary(testset_select4)
#there are missing and outlier values of 99999 to indicate those who said don't know
#treat them with median of the variable
medianWHQ150 = median(testset_select4$WHQ150, na.rm = TRUE)

testset_select4$WHQ150 = replace_na(testset_select4$WHQ150, medianWHQ150)
sum(is.na(testset_select4$WHQ150))

summary(testset_select4$WHQ150)

testset_select4[testset_select4$WHQ150 == 99999, "WHQ150"] <- medianWHQ150

summary(testset_select4$WHQ150)

saveRDS(testset_select4, file = "testset_select4.RDS")

################################################################
#   Cleaning and Feature Engineering - SMQ040 (testset)
################################################################

#Create a new variable for this - Qn: Do you smoke now? 1- Y, 2 - N

#treat NAs with median
medianSMQ040 = median(testset_select4$SMQ040, na.rm = TRUE)
testset_select4$SMQ040 = replace_na(testset_select4$SMQ040, medianSMQ040)
sum(is.na(testset_select4$SMQ040))

saveRDS(testset_select4, file = "testset_select4.RDS")

#to read it

testset_select4 = readRDS("testset_select4.RDS")

#create a new column for this categorical variable and save into a new dataset
testset_select5 = testset_select4 %>% 
  mutate(SMQ040A = SMQ040)

#recode level 2 to level 1 (yes)
testset_select5[testset_select5$SMQ040A == 2, "SMQ040A"] <- 1
summary(testset_select5$SMQ040A)

#recode level 3 to level 2 (no)
testset_select5[testset_select5$SMQ040A == 3, "SMQ040A"] <- 2
summary(testset_select5$SMQ040A)

#remove original SMQ040
testset_select5 = subset(testset_select5, select = c(-SMQ040))
glimpse(testset_select5)

saveRDS(testset_select5, file = "testset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - SMQ915 (testset)
################################################################

#New Qn: Do you use smokeless tobacco? 1 to 30 days - Y, 0 days - N
sum(is.na(testset_select4$SMQ915))
summary(testset_select4$SMQ915)
#treat NAs with median
medianSMQ915 = median(testset_select4$SMQ915, na.rm = TRUE)

testset_select4$SMQ915 = replace_na(testset_select4$SMQ915, medianSMQ915)
sum(is.na(testset_select4$SMQ915))

#create a new column for this categorical variable and save into a new dataset
testset_select5$SMQ915A = testset_select4$SMQ915

testset_select5$SMQ915A <- ifelse(testset_select5$SMQ915A %in% 1:30, 1, 2)
summary(testset_select5$SMQ915A)

# #remove original SMQ915
# testset_select5 = subset(testset_select5, select = c(-SMQ915))
# summary(testset_select5)

saveRDS(testset_select5, file = "testset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - LBXTC (testset)
################################################################

#New Variable: Total cholesterol level at risk?  >=200 - Y (at risk), <200 - N (normal)

#treat NAs with median
medianLBXTC = median(testset_select5$LBXTC, na.rm = TRUE)

testset_select5$LBXTC = replace_na(testset_select5$LBXTC, medianLBXTC)
sum(is.na(testset_select5$LBXTC))
summary(testset_select5$LBXTC)

#create a new column for categorical variable
testset_select5 = testset_select5 %>% 
  mutate(LBXTCA = LBXTC)

testset_select5$LBXTCA <- ifelse(testset_select5$LBXTCA >=200, 1, 2)
summary(testset_select5$LBXTCA)

saveRDS(testset_select5, file = "testset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - LBXTR (testset)
################################################################

#New Variable: Triglyceride level at risk?  >=150 - Y (at risk), <150 - N (normal)

#treat NAs with median
medianLBXTR = median(testset_select5$LBXTR, na.rm = TRUE)

testset_select5$LBXTR = replace_na(testset_select5$LBXTR, medianLBXTR)
sum(is.na(testset_select5$LBXTR))
summary(testset_select5$LBXTR)

#create a new column for categorical variable
testset_select5 = testset_select5 %>% 
  mutate(LBXTRA = LBXTR)

testset_select5$LBXTRA <- ifelse(testset_select5$LBXTRA >=150, 1, 2)
summary(testset_select5$LBXTRA)
summary(testset_select5)

saveRDS(testset_select5, file = "testset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - LBDLDL (testset)
################################################################

#New Variable: LDL Cholesterol level at risk?  >=100 - Y (at risk), <100 - N (normal)

#treat NAs with median
medianLBDLDL = median(testset_select5$LBDLDL, na.rm = TRUE)

testset_select5$LBDLDL = replace_na(testset_select5$LBDLDL, medianLBDLDL)
sum(is.na(testset_select5$LBDLDL))
summary(testset_select5$LBDLDL)

#create a new column for categorical variable
testset_select5 = testset_select5 %>% 
  mutate(LBDLDLA = LBDLDL)

testset_select5$LBDLDLA <- ifelse(testset_select5$LBDLDLA >=100, 1, 2)
summary(testset_select5$LBDLDLA)
summary(testset_select5)

saveRDS(testset_select5, file = "testset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - LBDHDD (testset)
################################################################

#New Variable: HDD Cholesterol level at risk? <60 - Y (at risk); >=60 and above - N (good) 

#treat NAs with median
medianLBDHDD = median(testset_select5$LBDHDD, na.rm = TRUE)

testset_select5$LBDHDD = replace_na(testset_select5$LBDHDD, medianLBDHDD)
sum(is.na(testset_select5$LBDHDD))
summary(testset_select5$LBDHDD)

#create a new column for categorical variable
testset_select5 = testset_select5 %>% 
  mutate(LBDHDDA = LBDHDD)

testset_select5$LBDHDDA <- ifelse(testset_select5$LBDHDDA < 60, 1, 2)
summary(testset_select5$LBDHDDA)
summary(testset_select5)

saveRDS(testset_select5, file = "testset_select5.RDS")

###################################################################
#   Cleaning and Feature Engineering - BPXSY1,2,3,4 (testset)
###################################################################

#New variable: max reading out of the 4 for the participant
#create a new column for this variable BPXSYmax

library(fastmatch)
fmatch("BPXSY1",names(testset_select5))  #99
fmatch("BPXSY2",names(testset_select5))  #100
fmatch("BPXSY3",names(testset_select5))  #101
fmatch("BPXSY4",names(testset_select5))  #102

#because there are 92 rows with NA value for all 4 readings
my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)

testset_select5$BPXSYmax = apply(testset_select5[, 99:102], 1, my.max)
head(testset_select5$BPXSYmax)
summary(testset_select5$BPXSYmax)

# library(matrixStats)
# testset_select5$BPXSYmax = rowMaxs(as.matrix(testset_select5[, 78:81]), na.rm=TRUE)

#New variable: min reading out of the 4 for the participant
#create a new column for this variable BPXSYmin
my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)

testset_select5$BPXSYmin = apply(testset_select5[, 99:102], 1, my.min)
head(testset_select5$BPXSYmin)
summary(testset_select5$BPXSYmin)

#New Variable: average out of the 4 readings
#create a new column for this variable BPXSYave
my.mean <- function(x) ifelse(!all(is.na(x)), mean(x, na.rm=T), NA)

testset_select5$BPXSYave = apply(testset_select5[, 99:102], 1, my.mean)
head(testset_select5$BPXSYave)
summary(testset_select5$BPXSYave)

summary(testset_select5)
saveRDS(testset_select5, file = "testset_select5.RDS")

#only use first reading BPXSY1, second reading BPXSY2, third reading BPXSY3, min, max, average for modelling
#remove BPXSY4 from dataset because first 3 are taken for all participants

testset_select6 = subset(testset_select5, select = c(-BPXSY4))
summary(testset_select6)

#treat NA values in BPXSY1,2,3, BPXSYmin, BPXSYmax, BPXSYave with median
medianBPXSY1 = median(testset_select6$BPXSY1, na.rm = TRUE)
medianBPXSY2 = median(testset_select6$BPXSY2, na.rm = TRUE)
medianBPXSY3 = median(testset_select6$BPXSY3, na.rm = TRUE)
medianBPXSYmin = median(testset_select6$BPXSYmin, na.rm = TRUE)
medianBPXSYmax = median(testset_select6$BPXSYmax, na.rm = TRUE)
medianBPXSYave = median(testset_select6$BPXSYave, na.rm = TRUE)

testset_select6$BPXSY1 = replace_na(testset_select6$BPXSY1, medianBPXSY1)
sum(is.na(testset_select6$BPXSY1))
summary(testset_select6$BPXSY1)

testset_select6$BPXSY2 = replace_na(testset_select6$BPXSY2, medianBPXSY2)
sum(is.na(testset_select6$BPXSY2))
summary(testset_select6$BPXSY2)

testset_select6$BPXSY3 = replace_na(testset_select6$BPXSY3, medianBPXSY3)
sum(is.na(testset_select6$BPXSY3))
summary(testset_select6$BPXSY3)

testset_select6$BPXSYmin = replace_na(testset_select6$BPXSYmin, medianBPXSYmin)
sum(is.na(testset_select6$BPXSYmin))
summary(testset_select6$BPXSYmin)

testset_select6$BPXSYmax = replace_na(testset_select6$BPXSYmax, medianBPXSYmax)
sum(is.na(testset_select6$BPXSYmax))
summary(testset_select6$BPXSYmax)

testset_select6$BPXSYave = replace_na(testset_select6$BPXSYave, medianBPXSYave)
sum(is.na(testset_select6$BPXSYave))
summary(testset_select6$BPXSYave)

#create a new column for categorical variable where average Systolic BP at risk? >=120 high (1), <120 normal (2)
testset_select6 = testset_select6 %>% 
  mutate(BPXSYaveC = BPXSYave)

testset_select6$BPXSYaveC <- ifelse(testset_select6$BPXSYaveC >= 120, 1, 2)
summary(testset_select6$BPXSYaveC)
summary(testset_select6)

saveRDS(testset_select6, file = "testset_select6.RDS")

###################################################################
#   Cleaning and Feature Engineering - BPXDI1,2,3,4 (testset)
###################################################################

#New variable: max reading out of the 4 for the participant
#create a new column for this variable BPXDImax

fmatch("BPXDI1",names(testset_select5))  #103
fmatch("BPXDI2",names(testset_select5))  #104
fmatch("BPXDI3",names(testset_select5))  #105
fmatch("BPXDI4",names(testset_select5))  #106

#there are rows with 0 values in the readings - BPXDI1,2,3
#convert to NA values

testset_select5$BPXDI1 <- replace(testset_select5$BPXDI1, testset_select5$BPXDI1 == 0, NA)
testset_select5$BPXDI2 <- replace(testset_select5$BPXDI2, testset_select5$BPXDI2 == 0, NA)
testset_select5$BPXDI3 <- replace(testset_select5$BPXDI3, testset_select5$BPXDI3 == 0, NA)

summary(testset_select5$BPXDI1)
summary(testset_select5$BPXDI2)
summary(testset_select5$BPXDI3)

#because there are rows with NA value for all 4 readings
my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)

testset_select5$BPXDImax = apply(testset_select5[, 103:106], 1, my.max)
head(testset_select5$BPXDImax)
summary(testset_select5$BPXDImax)

#New variable: min reading out of the 4 for the participant
#create a new column for this variable BPXDImin
my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)

testset_select5$BPXDImin = apply(testset_select5[, 103:106], 1, my.min)
head(testset_select5$BPXDImin)
summary(testset_select5$BPXDImin)

#New Variable: average out of the 4 readings
#create a new column for this variable BPXDIave
my.mean <- function(x) ifelse(!all(is.na(x)), mean(x, na.rm=T), NA)

testset_select5$BPXDIave = apply(testset_select5[, 103:106], 1, my.mean)
head(testset_select5$BPXDIave)
summary(testset_select5$BPXDIave)

summary(testset_select5)
saveRDS(testset_select5, file = "testset_select5.RDS")

#to read it

testset_select5 = readRDS("testset_select5.RDS")

#to read it

testset_select6 = readRDS("testset_select6.RDS")

#only use first reading BPXDI1, second reading BPXDI2, third reading BPXDI3, min, max, average for modelling

testset_select6 = cbind(testset_select6, testset_select5$BPXDImax, testset_select5$BPXDImin, testset_select5$BPXDIave)
testset_select6 = subset(testset_select6, select = c(-BPXDI4))
summary(testset_select6)

testset_select6$BPXDI1 <- replace(testset_select6$BPXDI1, testset_select6$BPXDI1 == 0, NA)
testset_select6$BPXDI2 <- replace(testset_select6$BPXDI2, testset_select6$BPXDI2 == 0, NA)
testset_select6$BPXDI3 <- replace(testset_select6$BPXDI3, testset_select6$BPXDI3 == 0, NA)

summary(testset_select6$BPXDI1)
summary(testset_select6$BPXDI2)
summary(testset_select6$BPXDI3)

#rename the new binded columns
names(testset_select6)[names(testset_select6) == "testset_select5$BPXDImax"] <- "BPXDImax"
names(testset_select6)[names(testset_select6) == "testset_select5$BPXDImin"] <- "BPXDImin"
names(testset_select6)[names(testset_select6) == "testset_select5$BPXDIave"] <- "BPXDIave"

#treat NA values in BPXDI1,2,3, BPXDImin, BPXDImax, BPXDIave with median
medianBPXDI1 = median(testset_select6$BPXDI1, na.rm = TRUE)
medianBPXDI2 = median(testset_select6$BPXDI2, na.rm = TRUE)
medianBPXDI3 = median(testset_select6$BPXDI3, na.rm = TRUE)
medianBPXDImin = median(testset_select6$BPXDImin, na.rm = TRUE)
medianBPXDImax = median(testset_select6$BPXDImax, na.rm = TRUE)
medianBPXDIave = median(testset_select6$BPXDIave, na.rm = TRUE)

testset_select6$BPXDI1 = replace_na(testset_select6$BPXDI1, medianBPXDI1)
sum(is.na(testset_select6$BPXDI1))
summary(testset_select6$BPXDI1)

testset_select6$BPXDI2 = replace_na(testset_select6$BPXDI2, medianBPXDI2)
sum(is.na(testset_select6$BPXDI2))
summary(testset_select6$BPXDI2)

testset_select6$BPXDI3 = replace_na(testset_select6$BPXDI3, medianBPXDI3)
sum(is.na(testset_select6$BPXDI3))
summary(testset_select6$BPXDI3)

testset_select6$BPXDImin = replace_na(testset_select6$BPXDImin, medianBPXDImin)
sum(is.na(testset_select6$BPXDImin))
summary(testset_select6$BPXDImin)

testset_select6$BPXDImax = replace_na(testset_select6$BPXDImax, medianBPXDImax)
sum(is.na(testset_select6$BPXDImax))
summary(testset_select6$BPXDImax)

testset_select6$BPXDIave = replace_na(testset_select6$BPXDIave, medianBPXDIave)
sum(is.na(testset_select6$BPXDIave))
summary(testset_select6$BPXDIave)

#create a new column for categorical variable where average Diastolic BP at risk? >=80 high (1), <80 normal (2)
testset_select6 = testset_select6 %>%
  mutate(BPXDIaveC = BPXDIave)

testset_select6$BPXDIaveC <- ifelse(testset_select6$BPXDIaveC >= 80, 1, 2)
summary(testset_select6$BPXDIaveC)
summary(testset_select6)

saveRDS(testset_select6, file = "testset_select6.RDS")


########################################################################
#  Cleaning and Feature Engineering - BMXWT, BMXHT, BMXBMI (testset)
########################################################################

#For AA, they have different BMI cutoff levels
# >=23 overweight;18.5-22.9 normal; <18.5 underweight
#original: Underweight = <18.5; Normal weight = 18.5-24.9; Overweight = 25-29.9; Obesity = BMI of 30 or greater
# weight in kg / height in m^2

#Treat missing Weight, Height and BMI values with median

#check median of Weight BMXWT
medianBMXWT = median(testset_select6$BMXWT, na.rm = TRUE)

#check median of Height BMWHT
medianBMXHT = median(testset_select6$BMXHT, na.rm = TRUE)

64.9/(1.61)^2 #25.04

#Check median of BMI
medianBMXBMI = median(testset_select6$BMXBMI, na.rm = TRUE)  #24.4

#if input the NA values on weight and height with their missing values, the BMI will not be 24.2. 
#Thus, will clean weight and height first and then calculate individually.

testset_select6$BMXWT = replace_na(testset_select6$BMXWT, medianBMXWT)
sum(is.na(testset_select6$BMXWT))
summary(testset_select6$BMXWT)

testset_select6$BMXHT = replace_na(testset_select6$BMXHT, medianBMXHT)
sum(is.na(testset_select6$BMXHT))
summary(testset_select6$BMXHT)

#create a function for BMI calc
bmi <- testset_select6$BMXWT/((testset_select6$BMXHT/100)^2)
testset_select6$BMXBMI = bmi
testset_select6$BMXBMI = round(testset_select6$BMXBMI, 1)
head(testset_select6$BMXBMI)
sum(is.na(testset_select6$BMXBMI))
summary(testset_select6$BMXBMI)
head(testset_select6$BMXBMI)

#create a new column for categorical variable where >=23 overweight (1) ;18.5-22.9 normal (2) ; <18.5 underweight (3)
testset_select6 = testset_select6 %>%
  mutate(BMXBMIAA = BMXBMI)

testset_select6$BMXBMIAA[testset_select6$BMXBMIAA>=27] <- "Obese"
testset_select6$BMXBMIAA[testset_select6$BMXBMIAA>=23 & testset_select6$BMXBMIAA<27] <- "Overweight"
testset_select6$BMXBMIAA[testset_select6$BMXBMIAA>=18.5 & testset_select6$BMXBMIAA<23] <- "Normal"
testset_select6$BMXBMIAA[testset_select6$BMXBMIAA<18.5] <- "Underweight"

class(testset_select6$BMXBMIAA)
sum(testset_select6$BMXBMIAA == "Overweight") #390
sum(testset_select6$BMXBMIAA == "Obese")  #236

#plot a histogram of BMXBMIAA
testset_select6 %>%
  group_by(BMXBMIAA) %>%
  summarise(count_level = n()) %>%
  ggplot(aes(x = as.factor(BMXBMIAA), y = count_level, fill = BMXBMIAA)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(label = count_level, vjust = 1)) +
  labs(x = "BMI Categories", y = "Number of Participants", 
       title = "Asian Americans by New BMI Cut-offs") +
  scale_fill_manual("legend", values = c("Normal" = "light blue", "Overweight" = "pink", "Obese" = "red", "Underweight" = "light blue"))

# Convert the column to a factor
testset_select6$BMXBMIAA <- factor(testset_select6$BMXBMIAA)
head(testset_select6$BMXBMIAA)
summary(testset_select6$BMXBMIAA)

# #recode
# testset_select6$BMXBMIAA <- revalue(testset_select6$BMXBMIAA, c("overweight"="1", "normal"="2", "underweight" = "3"))
# 
# summary(testset_select6$BMXBMIAA)
# #2   1   3 
# #281 626  32


#for comparison, use the normal BMI cutoff to create another categorical variable.
#original: Underweight = <18.5; Normal weight = 18.5-24.9; Overweight = 25-29.9; Obesity = BMI of 30 or greater
#create a new column for categorical variable where >=25 overweight;18.5-24.9 normal; <18.5 underweight
testset_select6 = testset_select6 %>%
  mutate(BMXBMIO = BMXBMI)

testset_select6$BMXBMIO[testset_select6$BMXBMIO>=30] <- "Obese"
testset_select6$BMXBMIO[testset_select6$BMXBMIO>=25 & testset_select6$BMXBMIO<30] <- "Overweight"
testset_select6$BMXBMIO[testset_select6$BMXBMIO>=18.5 & testset_select6$BMXBMIO<25] <- "Normal"
testset_select6$BMXBMIO[testset_select6$BMXBMIO<18.5] <- "Underweight"

#plot a histogram of BMXBMIO
testset_select6 %>%
  group_by(BMXBMIO) %>%
  summarise(count_level = n()) %>%
  ggplot(aes(x = as.factor(BMXBMIO), y = count_level, fill = BMXBMIO)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(label = count_level, vjust = 1)) +
  labs(x = "BMI Categories", y = "Number of Participants", 
       title = "Asian Americans by Ordinary BMI Cut-offs") +
  scale_fill_manual("legend", values = c("Normal" = "light blue", "Overweight" = "pink", "Obese" = "red", "Underweight" = "light blue"))

# Convert the column to a factor
testset_select6$BMXBMIO <- factor(testset_select6$BMXBMIO)
head(testset_select6$BMXBMIO)
summary(testset_select6$BMXBMIO)

# #recode
# testset_select6$BMXBMIO <- revalue(testset_select6$BMXBMIO, c("overweight"="1", "normal"="2", "underweight" = "3"))
# summary(testset_select6$BMXBMIO)
# #  2   1   3 
# #539 368  32      #nearly half become normal instead of overweight

summary(testset_select6)
saveRDS(testset_select6, file = "testset_select6.RDS")

########################################################################
# Cleaning and Feature Engineering - BMXSAD1,2,3,4, average (testset)
########################################################################

#This variable was created by averaging up to four SAD readings. 
#The majority of survey participants have two readings (BMXSAD1, and BMXSAD2); as such, these two readings were used to obtain mean of SAD value. 
#If there were four SAD readings (BMXSAD1, BMXSAD2, BMXSAD3, and BMXSAD4) because the difference between the first and second SAD measurements 
#was greater than 0.5 cm, then three closest SAD readings were used to obtain mean of SAD value.
#In a few instances where two outlying measurements are equally distant from the means of the two closest measurements, 
#then all four readings were used to obtain mean of SAD value.

#create a variable BMXSADmin: min reading out of the 4 for the participant
#create a new column for this variable BMXSADmin

fmatch("BMXSAD1",names(testset_select5))  #115
fmatch("BMXSAD2",names(testset_select5))  #116
fmatch("BMXSAD3",names(testset_select5))  #117
fmatch("BMXSAD4",names(testset_select5))  #118

#because there are 115 rows with NA value for all 4 readings
my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)

testset_select6$BMXSADmin = apply(testset_select5[, 115:118], 1, my.min)
head(testset_select6$BMXSADmin)
summary(testset_select6$BMXSADmin)

#create a variable BMXSADmax: max reading out of the 4 for the participant
#create a new column for this variable BMXSADmax
my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)

testset_select6$BMXSADmax = apply(testset_select5[, 115:118], 1, my.max)
head(testset_select6$BMXSADmax)
summary(testset_select6$BMXSADmax)

#remove BMXSAD2,3,4 from dataset

testset_select6 = subset(testset_select6, select = c(-BMXSAD2, -BMXSAD3, -BMXSAD4))
summary(testset_select6)

#treat NA values in BMXSAD1, BMXSADmin, BMXSADmax, BMDAVSAD with median

medianBMXSAD1 = median(testset_select6$BMXSAD1, na.rm = TRUE)
medianBMXSADmin = median(testset_select6$BMXSADmin, na.rm = TRUE)
medianBMXSADmax = median(testset_select6$BMXSADmax, na.rm = TRUE)
medianBMDAVSAD = median(testset_select6$BMDAVSAD, na.rm = TRUE)

testset_select6$BMXSAD1 = replace_na(testset_select6$BMXSAD1, medianBMXSAD1)
sum(is.na(testset_select6$BMXSAD1))
summary(testset_select6$BMXSAD1)

testset_select6$BMXSADmin = replace_na(testset_select6$BMXSADmin, medianBMXSADmin)
sum(is.na(testset_select6$BMXSADmin))
summary(testset_select6$BMXSADmin)

testset_select6$BMXSADmax = replace_na(testset_select6$BMXSADmax, medianBMXSADmax)
sum(is.na(testset_select6$BMXSADmax))
summary(testset_select6$BMXSADmax)

testset_select6$BMDAVSAD = replace_na(testset_select6$BMDAVSAD, medianBMDAVSAD)
sum(is.na(testset_select6$BMDAVSAD))
summary(testset_select6$BMDAVSAD)

summary(testset_select6)

saveRDS(testset_select6, file = "testset_select6.RDS")

############################################
# Cleaning - Physical Activity (testset)
############################################

summary(testset_select6$PAQ605) #no missing, no outliers

summary(testset_select6$PAD615) #missing values, no outliers
medianPAD615 = median(testset_select6$PAD615, na.rm = TRUE)
testset_select6$PAD615 = replace_na(testset_select6$PAD615, medianPAD615)
sum(is.na(testset_select6$PAD615))
summary(testset_select6$PAD615)

summary(testset_select6$PAQ620) #no missing values, have outliers
testset_select6$PAQ620 <- replace(testset_select6$PAQ620, testset_select6$PAQ620 == 9, NA)
summary(testset_select6$PAQ620)

summary(testset_select6$PAD630) #missing values, no outliers

summary(testset_select6$PAQ635) #all good

summary(testset_select6$PAD645) #missing values, no outliers

summary(testset_select6$PAQ650) #all good

summary(testset_select6$PAD660) #missing values, no outliers

summary(testset_select6$PAQ665) #all good

summary(testset_select6$PAD675) #missing values, no outliers

summary(testset_select6$PAD680) #all good

summary(testset_select6$PAQ710) #no missing values, have outliers
testset_select6$PAQ710 <- replace(testset_select6$PAQ710, testset_select6$PAQ710 == 8, 0)
summary(testset_select6$PAQ710)

###############################################################
# Cleaning - categorical variables with 7 and 9 (testset)
###############################################################

#to get a more accurate median value, convert outliers 7 and 9 to NA values

testset_select6$ALQ101 <- replace(testset_select6$ALQ101, testset_select6$ALQ101 == 9, NA)
summary(testset_select6$ALQ101)
testset_select6$BPQ080 <- replace(testset_select6$BPQ080, testset_select6$BPQ080 == 9, NA)
summary(testset_select6$BPQ080)
testset_select6$BPQ090D <- replace(testset_select6$BPQ090D, testset_select6$BPQ090D == 9, NA)
summary(testset_select6$BPQ090D)
testset_select6$DIQ170 <- replace(testset_select6$DIQ170, testset_select6$DIQ170 == 9, NA)
summary(testset_select6$DIQ170)
testset_select6$DIQ172 <- replace(testset_select6$DIQ172, testset_select6$DIQ172 == 7, NA)
summary(testset_select6$DIQ172)
testset_select6$DIQ172 <- replace(testset_select6$DIQ172, testset_select6$DIQ172 == 9, NA)
summary(testset_select6$DIQ172)
testset_select6$DIQ180 <- replace(testset_select6$DIQ180, testset_select6$DIQ180 == 7, NA)
summary(testset_select6$DIQ180)
testset_select6$DIQ180 <- replace(testset_select6$DIQ180, testset_select6$DIQ180 == 9, NA)
summary(testset_select6$DIQ180)
testset_select6$MCQ092 <- replace(testset_select6$MCQ092, testset_select6$MCQ092 == 7, NA)
summary(testset_select6$MCQ092)
testset_select6$MCQ092 <- replace(testset_select6$MCQ092, testset_select6$MCQ092 == 9, NA)
summary(testset_select6$MCQ092)
testset_select6$MCQ160B <- replace(testset_select6$MCQ160B, testset_select6$MCQ160B == 9, NA)
summary(testset_select6$MCQ160B)
testset_select6$MCQ160F <- replace(testset_select6$MCQ160F, testset_select6$MCQ160F == 9, NA)
summary(testset_select6$MCQ160F)
testset_select6$MCQ160K <- replace(testset_select6$MCQ160K, testset_select6$MCQ160K == 9, NA)
summary(testset_select6$MCQ160K)
testset_select6$MCQ160L <- replace(testset_select6$MCQ160L, testset_select6$MCQ160L == 9, NA)
summary(testset_select6$MCQ160L)
testset_select6$MCQ160M <- replace(testset_select6$MCQ160M, testset_select6$MCQ160M == 9, NA)
summary(testset_select6$MCQ160M)
testset_select6$MCQ160N <- replace(testset_select6$MCQ160N, testset_select6$MCQ160N == 9, NA)
summary(testset_select6$MCQ160N)
testset_select6$MCQ203 <- replace(testset_select6$MCQ203, testset_select6$MCQ203 == 9, NA)
summary(testset_select6$MCQ203)
testset_select6$MCQ300C <- replace(testset_select6$MCQ300C, testset_select6$MCQ300C == 7, NA)
summary(testset_select6$MCQ300C)
testset_select6$MCQ300C <- replace(testset_select6$MCQ300C, testset_select6$MCQ300C == 9, NA)
summary(testset_select6$MCQ300C)
testset_select6$MCQ365C <- replace(testset_select6$MCQ365C, testset_select6$MCQ365C == 9, NA)
summary(testset_select6$MCQ365C)
testset_select6$MCQ370A <- replace(testset_select6$MCQ370A, testset_select6$MCQ370A == 9, NA)
summary(testset_select6$MCQ370A)
testset_select6$MCQ370C <- replace(testset_select6$MCQ370C, testset_select6$MCQ370C == 9, NA)
summary(testset_select6$MCQ370C)
testset_select6$WHQ070 <- replace(testset_select6$WHQ070, testset_select6$WHQ070 == 9, NA)
summary(testset_select6$WHQ070)
testset_select6$INDFMIN2 <- replace(testset_select6$INDFMIN2, testset_select6$INDFMIN2 == 77, NA)
summary(testset_select6$INDFMIN2)
testset_select6$INDFMIN2 <- replace(testset_select6$INDFMIN2, testset_select6$INDFMIN2 == 99, NA)
summary(testset_select6$INDFMIN2)

saveRDS(testset_select6, file = "testset_select6.RDS")

#to read it

testset_select6 = readRDS("testset_select6.RDS")
summary(testset_select6)

########################################################################
#  Cleaning and Feature Engineering - SMQ020 and SMQ925 (testset)
########################################################################

#Create a new variable for this - Qn: Ever smoked a cigarette even 1 time? 1- Y, 2 - N
#because SMQ925 is conditional on SMQ020, if answer no to SMQ020 then will be asked this question.

summary(testset_select6$SMQ020)
summary(testset_select6$SMQ925)

#create a new column for this categorical variable and save into a new dataset
testset_select6 = testset_select6 %>% 
  mutate(SMQ925A = SMQ925)

#recode SMQ020 to level 1 for SMQ925A (yes)
testset_select6[testset_select6$SMQ020 == 1, "SMQ925A"] <- 1
summary(testset_select6$SMQ925A)

#recode SMQ925 to level 1 for SMQ925A (yes)
testset_select6[testset_select6$SMQ925 == 1, "SMQ925A"] <- 1
summary(testset_select6$SMQ925A)

#recode SMQ925 to level 2 for SMQ925A (no)
testset_select6[testset_select6$SMQ925 == 2, "SMQ925A"] <- 2
summary(testset_select6$SMQ925A)

testset_select6$SMQ925A <- replace(testset_select6$SMQ925A, testset_select6$SMQ925A == 9, NA)
summary(testset_select6$SMQ925A)

#treat NAs with median
medianSMQ925A = median(testset_select6$SMQ925A, na.rm = TRUE)
testset_select6$SMQ925A = replace_na(testset_select6$SMQ925A, medianSMQ925A)
sum(is.na(testset_select6$SMQ925A))

#remove original SMQ925
testset_select6 = subset(testset_select6, select = c(-SMQ925))
glimpse(testset_select6)

saveRDS(testset_select6, file = "testset_select6.RDS")

###############################################################
# Cleaning - replace NA values with column median (testset)
###############################################################

testset_select7 = testset_select6 %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

sum(is.na(testset_select7))

summary(testset_select7)
saveRDS(testset_select7, file = "testset_select7.RDS")

#to read it

testset_select7 = readRDS("testset_select7.RDS")
glimpse(testset_select7)
# Observations: 402
# Variables: 140

##########################################
#    Categorical variables (test set)
##########################################

str(testset_select7)

cols = c("ALQ101", "BPQ020","BPQ080", "BPQ090D", "DIQ050", "DIQ160", "DIQ170", "DIQ172", "DIQ180", 
         "MCQ092", "MCQ160B", "MCQ160F", "MCQ160K", "MCQ160L", "MCQ160M", "MCQ160N", "MCQ203", "MCQ300C", "MCQ365A", "MCQ365B", "MCQ365C",
         "MCQ365D", "MCQ370A", "MCQ370B", "MCQ370C", "MCQ370D", "PAQ605", "PAQ620", "PAQ635", "PAQ650", "PAQ665", "WHQ060", "WHQ070", "SMQ910",
         "DMDEDUC2", "INDFMIN2", "DIQ010A", "DIQ010B", "SMQ020", "SMQ040A", "SMQ915A", "SMQ925A", "LBXTCA", "LBXTRA","LBDLDLA","LBDHDDA", 
         "BPXSYaveC", "BPXDIaveC", "BPXPULS", "BMXBMIAA", "BMXBMIO", "RIAGENDR")

testset_select7[cols] <- lapply(testset_select7[cols], factor)

saveRDS(testset_select7, file = "testset_select7.RDS")

#to read testset_select7

testset_select7 = readRDS("testset_select7.RDS")
glimpse(testset_select7) #140 variables
colnames(testset_select7)

############################################################
#   Feature scaling for continuous variables (test set)
############################################################

#minmax scaling/normalization: scaled into the range of [0, 1]
#use caret package preProcess()

cont = testset_select7 %>% select_if(is.numeric)
summary(cont)
cont = subset(cont, select = c(-SEQN)) #87 variables
cat = testset_select7 %>% select_if(is.factor) #52 variables

#calculate the pre-process parameters from the dataset
contNorm = preProcess(cont, method = "range")
summary(contNorm)

# summarize transform parameters
print(contNorm)

# transform the dataset using the parameters
transformed <- predict(contNorm, cont)

# summarize the transformed dataset
summary(transformed)

#create dataset with transformed variables

testset_transformed = cbind(testset_select7$SEQN, transformed, cat)
names(testset_transformed)[names(testset_transformed) == "testset_select7$SEQN"] <- "SEQN"
testset_transformed = arrange(testset_transformed, SEQN)
saveRDS(testset_transformed, file = "testset_transformed.RDS")
summary(testset_transformed)

#to read testset_transformed

testset_transformed = readRDS("testset_transformed.RDS")

##########################################################################################################
# Remove Diabetes Questionnaire, Insulin, Blood Glucose tests and CBC variables (test set)
##########################################################################################################

testset_select8 = subset(testset_transformed, select = c(-DIQ050, -DIQ160, -DIQ170, -DIQ172, -DIQ180, 
                                                         -LBXGH, -LBXGLU, -LBXIN, -LBXSGL, - LBXWBCSI, -LBXPLTSI, -LBXRBCSI,
                                                         -LBXLYPCT, -LBXMOPCT, -LBXNEPCT, -LBXEOPCT, -LBXBAPCT, 
                                                         -LBDLYMNO, -LBDMONO, -LBDNENO, -LBDEONO, -LBDBANO, -LBXHGB, 
                                                         -LBXHCT, -LBXMCVSI, -LBXMCHSI,-LBXMC, -LBXRDW, -LBXMPSI))

saveRDS(testset_select8, file = "testset_select8.RDS")
summary(testset_select8) #111 variables

#to read it

testset_select8 = readRDS("testset_select8.RDS")
colnames(testset_select8)
glimpse(testset_select8)

##########################
# Datasets for modelling 
##########################

#remove SEQN
#remove all bloodcell counts and biochemistry profile variables
#use only average calculations, remove other similar/redundant variables

test_model1 = subset(testset_select8, select = c(-SEQN, -LBXSAL, -LBXSAPSI, -LBXSASSI, -LBXSATSI, -LBXSBU,
                                                 -LBXSC3SI, -LBXSCA, -LBXSCH, -LBXSCK, -LBXSCLSI,
                                                 -LBXSCR, -LBXSGB, -LBXSGTSI, -LBXSIR, -LBXSKSI,
                                                 -LBXSLDSI, -LBXSNASI, -LBXSOSSI, -LBXSPH, -LBXSTB,
                                                 -LBXSTP, -LBXSTR, -LBXSUA,
                                                 -SMQ915A, -BPXSY1, -BPXSY2, -BPXSY3, -BPXSYmax, -BPXSYmin,
                                                 -BPXDI1, -BPXDI2, -BPXDI3, -BPXDImax, -BPXDImin, -BMXSAD1,
                                                 -BMXSADmax, -BMXSADmin, -WHQ060))

saveRDS(test_model1, file = "test_model1.RDS")
summary(test_model1)  #72 variables

#to read it

test_model1 = readRDS("test_model1.RDS")

#remove variables for sensitivity analysis

test_model2 = subset(test_model1, select = c(-PAQ605, -PAQ620, -PAQ635, -PAQ650, -PAQ665, -SMQ915, -LBXTCA,
                                             -LBDHDDA, -LBXTRA, -LBDLDLA, -BPXSYaveC, -BPXDIaveC, -BMXBMI, -BMXBMIO))

saveRDS(test_model2, file = "test_model2.RDS")
summary(test_model2)  #58 variables

#to read it

test_model2 = readRDS("test_model2.RDS")

#include some significant variables from the lab tests

test_model3 = cbind(test_model2, testset_select8$LBXSCA, testset_select8$LBXSGTSI, testset_select8$LBXSKSI, testset_select8$LBXSPH)

#rename the new binded columns
names(test_model3)[names(test_model3) == "testset_select8$LBXSCA"] <- "LBXSCA"
names(test_model3)[names(test_model3) == "testset_select8$LBXSGTSI"] <- "LBXSGTSI"
names(test_model3)[names(test_model3) == "testset_select8$LBXSKSI"] <- "LBXSKSI"
names(test_model3)[names(test_model3) == "testset_select8$LBXSPH"] <- "LBXSPH"


saveRDS(test_model3, file = "test_model3.RDS")
summary(test_model3)  #62 variables

#to read it

test_model3 = readRDS("test_model3.RDS")


######################################
#  Datasets for sensitivity analysis 
######################################
#includes both DIQ010A and DIQ010B
#Physical Activity variables
test_sa1 = subset(test_model2, select = c(-PAD615, -PAD630, -PAD645, -PAD660, -PAD675))
test_sa1 = cbind(test_sa1, testset_select8$PAQ605, testset_select8$PAQ620, testset_select8$PAQ635, 
                 testset_select8$PAQ650, testset_select8$PAQ665)

names(test_sa1)[names(test_sa1) == "testset_select8$PAQ605"] <- "PAQ605"
names(test_sa1)[names(test_sa1) == "testset_select8$PAQ620"] <- "PAQ620"
names(test_sa1)[names(test_sa1) == "testset_select8$PAQ635"] <- "PAQ635"
names(test_sa1)[names(test_sa1) == "testset_select8$PAQ650"] <- "PAQ650"
names(test_sa1)[names(test_sa1) == "testset_select8$PAQ665"] <- "PAQ665"
summary(test_sa1)

saveRDS(test_sa1, file = "test_sa1.RDS")

#to read it

test_sa1 = readRDS("test_sa1.RDS")

#Smoking variable
test_sa2 = subset(test_model2, select = c(-SMQ910))
test_sa2 = cbind(test_sa2, testset_select8$SMQ915A)

names(test_sa2)[names(test_sa2) == "testset_select8$SMQ915A"] <- "SMQ915A"
summary(test_sa2)

saveRDS(test_sa2, file = "test_sa2.RDS")

#to read it

test_sa2 = readRDS("test_sa2.RDS")

#Cholesterol variables
test_sa3 = subset(test_model2, select = c(-LBXTC, -LBDHDD, -LBXTR, -LBDLDL))
test_sa3 = cbind(test_sa3, testset_select8$LBXTCA, testset_select8$LBDHDDA, testset_select8$LBXTRA, testset_select8$LBDLDLA)

names(test_sa3)[names(test_sa3) == "testset_select8$LBXTCA"] <- "LBXTCA"
names(test_sa3)[names(test_sa3) == "testset_select8$LBDHDDA"] <- "LBDHDDA"
names(test_sa3)[names(test_sa3) == "testset_select8$LBXTRA"] <- "LBXTRA"
names(test_sa3)[names(test_sa3) == "testset_select8$LBDLDLA"] <- "LBDLDLA"
summary(test_sa3)

saveRDS(test_sa3, file = "test_sa3.RDS")

#to read it

test_sa3 = readRDS("test_sa3.RDS")

#Blood Pressure variables
test_sa4 = subset(test_model2, select = c(-BPXSYave, -BPXDIave))
test_sa4 = cbind(test_sa4, testset_select8$BPXSYaveC, testset_select8$BPXDIaveC)

names(test_sa4)[names(test_sa4) == "testset_select8$BPXSYaveC"] <- "BPXSYaveC"
names(test_sa4)[names(test_sa4) == "testset_select8$BPXDIaveC"] <- "BPXDIaveC"
summary(test_sa4)

saveRDS(test_sa4, file = "test_sa4.RDS")

#to read it

test_sa4 = readRDS("test_sa4.RDS")

#BMI-ordinary variable
test_sa5 = subset(test_model2, select = c(-BMXBMIAA))
test_sa5 = cbind(test_sa5, testset_select8$BMXBMIO)

names(test_sa5)[names(test_sa5) == "testset_select8$BMXBMIO"] <- "BMXBMIO"
summary(test_sa5)

saveRDS(test_sa5, file = "test_sa5.RDS")

#to read it

test_sa5 = readRDS("test_sa5.RDS")

#BMI continuous variable
test_sa6 = subset(test_model2, select = c(-BMXBMIAA))
test_sa6 = cbind(test_sa6, testset_select8$BMXBMI)

names(test_sa6)[names(test_sa6) == "testset_select8$BMXBMI"] <- "BMXBMI"
summary(test_sa6)

saveRDS(test_sa6, file = "test_sa6.RDS")

#to read it

test_sa6 = readRDS("test_sa6.RDS")