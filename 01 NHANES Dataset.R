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

#to read SAS files
library(SASxport)

############################################################################
#                           importing 2015-2016
############################################################################

##############
#Demographics#
##############

DEMO_I = read.xport("./2015-2016/Demographics/Demographics_I.XPT")
head(DEMO_I)

#############
#Examination#
#############

BloodPressure_I = read.xport("./2015-2016/Examination/Blood Pressure_I.XPT")
head(BloodPressure_I)
summary(BloodPressure_I)
nrow(BloodPressure_I)
ncol(BloodPressure_I)

BodyMeasures_I = read.xport("./2015-2016/Examination/Body Measures_I.XPT")
nrow(BodyMeasures_I)
ncol(BodyMeasures_I)

# Merge component files
# Full Outer Join: Keep all records, even if SEQN does not match
EXAM_I = merge(x=BloodPressure_I,y=BodyMeasures_I,by="SEQN",all=TRUE)
nrow(EXAM_I)
ncol(EXAM_I)
head(EXAM_I)

############
#Laboratory#
############

HighCholesterol_I = read.xport("./2015-2016/Lab/Cholesterol - High_I.XPT")
head(HighCholesterol_I)
summary(HighCholesterol_I)
nrow(HighCholesterol_I)
ncol(HighCholesterol_I)

LowCholesterol_I = read.xport("./2015-2016/Lab/Cholesterol - Low_I.XPT")
head(LowCholesterol_I)
nrow(LowCholesterol_I)
ncol(LowCholesterol_I)

TotalCholesterol_I = read.xport("./2015-2016/Lab/Cholesterol - Total_I.XPT")
head(TotalCholesterol_I)
nrow(TotalCholesterol_I)
ncol(TotalCholesterol_I)

CBC_I = read.xport("./2015-2016/Lab/Complete Blood Count_I.XPT")
head(CBC_I)
nrow(CBC_I)
ncol(CBC_I)

Glycohemoglobin_I = read.xport("./2015-2016/Lab/Glycohemoglobin_I.XPT")
nrow(Glycohemoglobin_I)
ncol(Glycohemoglobin_I)

HSCRP_I = read.xport("./2015-2016/Lab/High-Sensitivity C-Reactive Protein_I.XPT")
nrow(HSCRP_I)
ncol(HSCRP_I)

Insulin_I = read.xport("./2015-2016/Lab/Insulin_I.XPT")
nrow(Insulin_I)
ncol(Insulin_I)

OGTT_I = read.xport("./2015-2016/Lab/Oral Glucose Tolerance Test_I.XPT")
nrow(OGTT_I)
ncol(OGTT_I)

PFG_I = read.xport("./2015-2016/Lab/Plasma Fasting Glucose_I.XPT")
nrow(PFG_I)
ncol(PFG_I)

SBP_I = read.xport("./2015-2016/Lab/Standard Biochemistry Profile_I.XPT")
nrow(SBP_I)
ncol(SBP_I)

# Merge component files using tidyverse dplyr
# Full Outer Join: Keep all records, even if SEQN does not match
#full_dplyr  <- full_join(flights,  weather, by = mergeCols)
#full_dplyr  <- flights %>% full_join(weather,  by = mergeCols)

library(dplyr)

Cholesterol_I = HighCholesterol_I %>% full_join(LowCholesterol_I, by = "SEQN") %>%
  full_join(TotalCholesterol_I, by = "SEQN") 
head(Cholesterol_I)

LAB_I = Cholesterol_I %>% full_join(CBC_I, by = "SEQN") %>%
  full_join(Glycohemoglobin_I, by = "SEQN") %>%
  full_join(HSCRP_I, by = "SEQN") %>%
  full_join(Insulin_I, by = "SEQN") %>%
  full_join(OGTT_I, by = "SEQN") %>%
  full_join(PFG_I, by = "SEQN") %>%
  full_join(SBP_I, by = "SEQN")
              
nrow(LAB_I)
ncol(LAB_I)
head(LAB_I)

LAB_I_2 = CBC_I %>%
  full_join(SBP_I, by = "SEQN")

###############
#Questionnaire#
###############

Alcohol_I = read.xport("./2015-2016/Questionnaire/Alcohol Use_I.XPT")
head(Alcohol_I)
summary(Alcohol_I)
nrow(Alcohol_I)
ncol(Alcohol_I)

BPC_I = read.xport("./2015-2016/Questionnaire/Blood Pressure & Cholesterol_I.XPT")
head(BPC_I)
nrow(BPC_I)
ncol(BPC_I)

Diabetes_I = read.xport("./2015-2016/Questionnaire/Diabetes_I.XPT")
head(Diabetes_I)
nrow(Diabetes_I)
ncol(Diabetes_I)

MC_I = read.xport("./2015-2016/Questionnaire/Medical Conditions_I.XPT")
head(MC_I)
nrow(MC_I)
ncol(MC_I)

PA_I = read.xport("./2015-2016/Questionnaire/Physical Activity_I.XPT")
nrow(PA_I)
ncol(PA_I)

SmokeCigUse_I = read.xport("./2015-2016/Questionnaire/Smoking - Cigarette Use_I.XPT")
nrow(SmokeCigUse_I)
ncol(SmokeCigUse_I)

SmokeHH_I = read.xport("./2015-2016/Questionnaire/Smoking - Household Smokers_I.XPT")
nrow(SmokeHH_I)
ncol(SmokeHH_I)

WeightHist_I = read.xport("./2015-2016/Questionnaire/Weight History_I.XPT")
nrow(WeightHist_I)
ncol(WeightHist_I)

# Merge component files
# Full Outer Join: Keep all records, even if SEQN does not match

QUESTION_I = Alcohol_I %>% full_join(BPC_I, by = "SEQN") %>%
  full_join(Diabetes_I, by = "SEQN") %>%
  full_join(MC_I, by = "SEQN") %>%
  full_join(PA_I, by = "SEQN") %>%
  full_join(SmokeCigUse_I, by = "SEQN") %>%
  full_join(SmokeHH_I, by = "SEQN") %>%
  full_join(WeightHist_I, by = "SEQN")

nrow(QUESTION_I)
ncol(QUESTION_I)
head(QUESTION_I)

QUESTION_I_2 = Alcohol_I %>%  full_join(MC_I, by = "SEQN") %>%
  full_join(PA_I, by = "SEQN") %>%
  full_join(SmokeCigUse_I, by = "SEQN") %>%
  full_join(SmokeHH_I, by = "SEQN") %>%
  full_join(WeightHist_I, by = "SEQN")

#Merging all 4 datasets#

NHANES1516 = DEMO_I %>% full_join(EXAM_I, by = "SEQN") %>%
  full_join(LAB_I, by = "SEQN") %>%
  full_join(QUESTION_I, by = "SEQN")

nrow(NHANES1516)
ncol(NHANES1516)
head(NHANES1516)

saveRDS(NHANES1516, file = "NHANES1516.RDS") 

#to read it

NHANES1516 = readRDS("NHANES1516.RDS")

#without obvious Diabetes features
NHANES1516_2 = DEMO_I %>% full_join(BodyMeasures_I, by = "SEQN") %>%
  full_join(LAB_I_2, by = "SEQN") %>%
  full_join(QUESTION_I_2, by = "SEQN")

saveRDS(NHANES1516_2, file = "NHANES1516_2.RDS") 

#to read it

NHANES1516_2 = readRDS("NHANES1516_2.RDS")

############################################################################
#                           importing 2013-2014
############################################################################

##############
#Demographics#
##############

DEMO_H = read.xport("./2013-2014/Demographics/Demographics_H.XPT")
head(DEMO_H)

#############
#Examination#
#############

BloodPressure_H = read.xport("./2013-2014/Examination/Blood Pressure_H.XPT")
head(BloodPressure_H)
summary(BloodPressure_H)
nrow(BloodPressure_H)
ncol(BloodPressure_H)

BodyMeasures_H = read.xport("./2013-2014/Examination/Body Measures_H.XPT")
nrow(BodyMeasures_H)
ncol(BodyMeasures_I)

# Merge component files
# Full Outer Join: Keep all records, even if SEQN does not match
EXAM_H = merge(x=BloodPressure_H,y=BodyMeasures_H,by="SEQN",all=TRUE)
nrow(EXAM_H)
ncol(EXAM_H)
head(EXAM_H)

############
#Laboratory#
############

HighCholesterol_H = read.xport("./2013-2014/Lab/Cholesterol - HDL_H.XPT")
head(HighCholesterol_H)
summary(HighCholesterol_H)
nrow(HighCholesterol_H)
ncol(HighCholesterol_H)

LowCholesterol_H = read.xport("./2013-2014/Lab/Cholesterol - Low_H.XPT")
head(LowCholesterol_H)
nrow(LowCholesterol_H)
ncol(LowCholesterol_H)

TotalCholesterol_H = read.xport("./2013-2014/Lab/Cholesterol - Total_H.XPT")
head(TotalCholesterol_H)
nrow(TotalCholesterol_H)
ncol(TotalCholesterol_H)

CBC_H = read.xport("./2013-2014/Lab/Complete Blood Count_H.XPT")
head(CBC_H)
nrow(CBC_H)
ncol(CBC_H)

Glycohemoglobin_H = read.xport("./2013-2014/Lab/Glycohemoglobin_H.XPT")
nrow(Glycohemoglobin_H)
ncol(Glycohemoglobin_H)


Insulin_H = read.xport("./2013-2014/Lab/Insulin_H.XPT")
nrow(Insulin_H)
ncol(Insulin_H)

OGTT_H = read.xport("./2013-2014/Lab/Oral Glucose Tolerance Test_H.XPT")
nrow(OGTT_H)
ncol(OGTT_H)

PFG_H = read.xport("./2013-2014/Lab/Plasma Fasting Glucose_H.XPT")
nrow(PFG_H)
ncol(PFG_H)

SBP_H = read.xport("./2013-2014/Lab/Standard Biochemistry Profile_H.XPT")
nrow(SBP_H)
ncol(SBP_H)

# Merge component files using tidyverse dplyr
# Full Outer Join: Keep all records, even if SEQN does not match

Cholesterol_H = HighCholesterol_H %>% full_join(LowCholesterol_H, by = "SEQN") %>%
  full_join(TotalCholesterol_H, by = "SEQN") 
head(Cholesterol_H)

LAB_H = Cholesterol_H %>% full_join(CBC_H, by = "SEQN") %>%
  full_join(Glycohemoglobin_H, by = "SEQN") %>%
  full_join(Insulin_H, by = "SEQN") %>%
  full_join(OGTT_H, by = "SEQN") %>%
  full_join(PFG_H, by = "SEQN") %>%
  full_join(SBP_H, by = "SEQN")

nrow(LAB_H)
ncol(LAB_H)
head(LAB_H)

LAB_H_2 = CBC_H %>% full_join(SBP_H, by = "SEQN")

###############
#Questionnaire#
###############

Alcohol_H = read.xport("./2013-2014/Questionnaire/Alcohol Use_H.XPT")
head(Alcohol_H)
summary(Alcohol_H)
nrow(Alcohol_H)
ncol(Alcohol_H)

BPC_H = read.xport("./2013-2014/Questionnaire/Blood Pressure & Cholesterol_H.XPT")
head(BPC_H)
nrow(BPC_H)
ncol(BPC_H)

Diabetes_H = read.xport("./2013-2014/Questionnaire/Diabetes_H.XPT")
head(Diabetes_H)
nrow(Diabetes_H)
ncol(Diabetes_H)

MC_H = read.xport("./2013-2014/Questionnaire/Medical Conditions_H.XPT")
head(MC_H)
nrow(MC_H)
ncol(MC_H)

PA_H = read.xport("./2013-2014/Questionnaire/Physical Activity_H.XPT")
nrow(PA_H)
ncol(PA_H)

SmokeCigUse_H = read.xport("./2013-2014/Questionnaire/Smoking - Cigarette Use_H.XPT")
nrow(SmokeCigUse_H)
ncol(SmokeCigUse_H)

SmokeHH_H = read.xport("./2013-2014/Questionnaire/Smoking - Household Smokers_H.XPT")
nrow(SmokeHH_H)
ncol(SmokeHH_H)

WeightHist_H = read.xport("./2013-2014/Questionnaire/Weight History_H.XPT")
nrow(WeightHist_H)
ncol(WeightHist_H)

# Merge component files
# Full Outer Join: Keep all records, even if SEQN does not match

QUESTION_H = Alcohol_H %>% full_join(BPC_H, by = "SEQN") %>%
  full_join(Diabetes_H, by = "SEQN") %>%
  full_join(MC_H, by = "SEQN") %>%
  full_join(PA_H, by = "SEQN") %>%
  full_join(SmokeCigUse_H, by = "SEQN") %>%
  full_join(SmokeHH_H, by = "SEQN") %>%
  full_join(WeightHist_H, by = "SEQN")

nrow(QUESTION_H)
ncol(QUESTION_H)
head(QUESTION_H)

QUESTION_H_2 = Alcohol_H %>% 
  full_join(MC_H, by = "SEQN") %>%
  full_join(PA_H, by = "SEQN") %>%
  full_join(SmokeCigUse_H, by = "SEQN") %>%
  full_join(SmokeHH_H, by = "SEQN") %>%
  full_join(WeightHist_H, by = "SEQN")

#Merging all 4 datasets#

NHANES1314 = DEMO_H %>% full_join(EXAM_H, by = "SEQN") %>%
  full_join(LAB_H, by = "SEQN") %>%
  full_join(QUESTION_H, by = "SEQN")

nrow(NHANES1314)
ncol(NHANES1314)
head(NHANES1314)

saveRDS(NHANES1314, file = "NHANES1314.RDS") 

#to read it

NHANES1314 = readRDS("NHANES1314.RDS")

#without obvious Diabetes features
NHANES1314_2 = DEMO_H %>% full_join(BodyMeasures_H, by = "SEQN") %>%
  full_join(LAB_H_2, by = "SEQN") %>%
  full_join(QUESTION_H_2, by = "SEQN")

saveRDS(NHANES1314_2, file = "NHANES1314_2.RDS") 

#to read it

NHANES1314_2 = readRDS("NHANES1314_2.RDS")

##########################################
#    Merging 2013-2014 and 2015-2016
##########################################

library(plyr)
NHANES_1 = rbind.fill(NHANES1314, NHANES1516)
nrow(NHANES_1)
ncol(NHANES_1)
head(NHANES_1)
summary(NHANES_1)
#Observations: 20,146
#Variables: 530

saveRDS(NHANES_1, file = "NHANES_1.RDS") 

#to read it

NHANES_1 = readRDS("NHANES_1.RDS")

NHANES_1_without = rbind.fill(NHANES1314_2, NHANES1516_2)
saveRDS(NHANES_1_without, file = "NHANES_1_without.RDS") 

#to read it

NHANES_1_without = readRDS("NHANES_1_without.RDS")

##########################################
#       Selecting Ages >=20
##########################################

#check missing values in RIDAGEYR (should be none)

sum(is.na(NHANES_1$RIDAGEYR))

#select Adults ages >=20

NHANES_2 = NHANES_1 %>% filter(RIDAGEYR >= 20)
nrow(NHANES_2)
summary(NHANES_2$RIDAGEYR)

saveRDS(NHANES_2, file = "NHANES_2.RDS") 

#to read it

NHANES_2 = readRDS("NHANES_2.RDS")


##########################################
#     Filter out Pregnant ladies
##########################################

#check missing values in RIDEXPRG (including values = 3 and .)
#treat any missing values

summary(NHANES_2$RIDEXPRG)
class(NHANES_2$RIDEXPRG)
#NHANES_2$RIDEXPRG = as.factor(NHANES_2$RIDEXPRG)

NHANES_2$RIDEXPRG = replace_na(NHANES_2$RIDEXPRG, 2)
sum(is.na(NHANES_2$RIDEXPRG))

NHANES_2[NHANES_2$RIDEXPRG == 3, "RIDEXPRG"] <- 2

#filter out/remove any pregnant ladies where RIDEXPRG = 1
NHANES_3 = NHANES_2 %>% filter(RIDEXPRG == 2)
nrow(NHANES_3)
summary(NHANES_3$RIDEXPRG)

saveRDS(NHANES_3, file = "NHANES_3.RDS") 

#to read it

NHANES_3 = readRDS("NHANES_3.RDS")

hist(as.factor(NHANES_3$RIDRETH3))


##########################################
#     Selecting Asian Americans
##########################################

#check missing values in RIDRETH3 (should be none)

sum(is.na(NHANES_3$RIDRETH3))

class(NHANES_3$RIDRETH3)
#NHANES_3$RIDRETH3 = as.factor(NHANES_3$RIDRETH3)
summary(NHANES_3$RIDRETH3)

#select Asian Americans (non-Hispanic Asian - RIDRETH3=6)

NHANES_4 = NHANES_3 %>% filter(RIDRETH3 == 6)
nrow(NHANES_4)
summary(NHANES_4$RIDRETH3)

saveRDS(NHANES_4, file = "NHANES_4.RDS") 

#to read it

NHANES_4 = readRDS("NHANES_4.RDS")

#1341 Asian Americans who are 20yo and above and not pregnant.


##########################################
#      Exploratory Data Analysis
##########################################

glimpse(NHANES_4)
#Observations: 1,341
#Variables: 530

head(NHANES_4)
names(NHANES_4) #View column names
str(NHANES_4) #Look at structure
dim(NHANES_4)
summary(NHANES_4)

#explore distribution of Age, Weight, Height and BMI
hist(NHANES_4$RIDAGEYR)
hist(NHANES_4$BMXWT)
hist(NHANES_4$BMXHT)
hist(NHANES_4$BMXBMI)


# NHANES_4%>%
#   group_by(RIDAGEYR) %>%
#   ggplot(aes(RIDAGEYR, ..count.., fill = RIDAGEYR)) + geom_bar() +
#   labs(x = 'Age', y = "Number of Participants", title = "Age Distribution") +
#   scale_fill_hue(l=65,c=100)

#explore number of Diabetics in the dataset where DIQ010 = 1
hist(NHANES_4$DIQ010)

any(is.na(NHANES_4$DIQ010))
sum(NHANES_4$DIQ010==1) / 1341  #11.26% told they are diabetic
sum(NHANES_4$DIQ010==3) / 1341  #2.01% at risk


##########################################
#         Train/Test Split
##########################################

library(splitstackshape)
set.seed(500)  #for reproducibility
train = stratified(NHANES_4, c('DIQ010'), 0.7, bothSets = TRUE)
trainset = train$SAMP1
testset = train$SAMP2
class(trainset)
trainset = data.frame(trainset)

#Check stratification
sum(trainset$DIQ010==1) / 939  #0.112886
sum(testset$DIQ010==1) / 402   #0.1119403

saveRDS(trainset, file = "trainset.RDS") 
saveRDS(testset, file = "testset.RDS")

#to read it

trainset = readRDS("trainset.RDS")
testset = readRDS("testset.RDS")

##########################################
#      Missing Values (train set)
##########################################

library(mice)
md.pattern(trainset)

library(VIM)
aggr_plot <- aggr(trainset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(trainset), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#there are variables with 100% missing (116 variables)
#remove variables with more than 60% missing

NHANES_5 = trainset[colSums(is.na(trainset))/nrow(trainset) < 0.6]
#left with 231 variables
aggr_plot2 <- aggr(NHANES_5, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(NHANES_5), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#presence of missings in the dataset
sapply(NHANES_5, function(x) sum(is.na(x)))

#imputation using the mice package
methods(mice) #check available imputation methods

init = mice(NHANES_5, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#run the imputation
set.seed(500) #to ensure results are repeatable
imputed = mice(NHANES_5, method=meth, predictorMatrix=predM, m=5)

saveRDS(imputed, file = "imputed.RDS") 

#to read it

imputed = readRDS("imputed.RDS")

#check the imputed data - eg. for the variable BMXWAIST
imputed$imp$BMXWAIST

#check the imputation method used for each variable
imputed$meth

#Create a dataset after imputation
#The missing values have been replaced with the imputed values in the first of the five datasets. 
#If you wish to use another one, just change the second parameter in the complete() function

imputed1 <- complete(imputed, 1)
warnings()

#Check for missings in the imputed dataset.
sapply(imputed1, function(x) sum(is.na(x)))

#delete variables with still missing data
NHANES_6 = imputed1[!colSums(is.na(imputed1))>0]
#Check for missings
sapply(NHANES_6, function(x) sum(is.na(x)))

#check on the distribution pre and post imputation
hist(NHANES_5$BMXWAIST)
hist(NHANES_6$BMXWAIST)

summary(NHANES_5$BMXWAIST)
summary(NHANES_6$BMXWAIST)


##########################################
#     Feature Selection (train set)
##########################################

library(caret)
library(mlbench)


set.seed(500) #ensure the results are repeatable

###Remove redundant features that are highly correlated with each other###
# # calculate correlation matrix
# correlationMatrix <- cor(NHANES_5)
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)

###Rank Features By Importance (K-NN)###

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
?caret::train
library(kknn)
model1 <- train(DIQ010~., data=NHANES_6, method="kknn", preProcess="scale", trControl=control1)

# estimate variable importance
importance1 <- varImp(model1, scale=FALSE)

# summarize importance
print(importance1)

# loess r-squared variable importance
# 
# only 20 most important variables shown (out of 204)
# 
# Overall
# LBXGH    0.19697
# DIQ172   0.14862
# DIQ050   0.13653
# LBXGLU   0.10796
# LBXSGL   0.08263
# RIDAGEYR 0.07306
# BPQ090D  0.06554
# DIQ160   0.05676
# GTDCODE  0.05529
# LBDLDL   0.05127
# BPQ020   0.04983
# DMDHHSZE 0.04940
# MCQ365D  0.04672
# BMXWAIST 0.03388
# MCQ365C  0.03328
# LBXTC    0.03283
# LBXSCH   0.03175
# MCQ365B  0.03086
# PEASCTM1 0.02670
# BMXSAD1  0.02496

# plot importance
plot(importance1)

###Recursive Feature Elimination (explore all possible subsets - Random Forest algorithm is used on each iteration to evaluate the model)###
# define the control using a random forest selection function
control2 <- rfeControl(functions=rfFuncs, method="cv", number=10)  #cross-validation with 10 folds
# control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", repeats=10)  #repeated k-fold cross validation with 3 repeats

#dataframe of the variables excluding DIQ010
NHANES_rfe = NHANES_6[!NHANES_6$DIQ010]

outcomeName = 'DIQ010'
predictors = names(NHANES_6)[!names(NHANES_6) %in% outcomeName]

# run the RFE algorithm
results1 = rfe(NHANES_6[,predictors], NHANES_6[,outcomeName],
                         rfeControl = control2)

# summarize the results
print(results1)
#The top 5 variables (out of 8): LBXGH, DIQ172, DIQ050, LBDLDL, LBXGLU
#Variables   RMSE Rsquared    MAE  RMSESD RsquaredSD   MAESD   Selected 
# 4          0.2941   0.3040 0.1347 0.04053    0.11650 0.02202          
# 8          0.2896   0.3251 0.1327 0.03949    0.12777 0.02345      
# 16         0.2792   0.3746 0.1281 0.04014    0.14060 0.02315        *
# 206        0.2827   0.3661 0.1362 0.03349    0.09471 0.02003         

colnames(NHANES_6[4]) #RIAGENDR  #Gender
colnames(NHANES_6[8]) #RIDEXMON  #Six month time period when the examination was performed - two categories: November 1 through April 30, May 1 through October 31.
colnames(NHANES_6[16]) #SIALANG  #Language of the Sample Person Interview Instrument
colnames(NHANES_6[206]) #SMQ910  #Ever used smokeless tobacco?

# list the chosen features
predictors(results1) 

# [1] "LBXGH"    "LBXGLU"   "DIQ050"   "LBXSGL"   "RIDAGEYR" "BPQ090D"  "LBDLDL"   "GTDCODE"  "LBXIN"    "WTSOG2YR" "DMDHRAGE" "BPXSY3"   "BMXWAIST"
# [14] "LBDLYMNO" "LBXSCLSI" "LBXTC"   

# plot the results
plot(results1, type=c("g", "o"))

#try the above without the obvious features that are linked to diabetes
#see R Script 'NHANES_without'

#run ridge regression
control = trainControl(method="cv", number=4, allowParallel = TRUE)
ridge_mod = train(NHANES_6[,predictors], NHANES_6[,outcomeName], method = 'glmnet', trControl=control, preProc = c("center", "scale"),
                  tuneGrid=expand.grid(alpha=0, lambda=seq(0.01,0.1,by=0.01)))
coef(ridge_mod$finalModel,ridge_mod$finalModel$lambdaOpt)

# estimate variable importance
importanceridge <- varImp(ridge_mod, scale=FALSE)

# summarize importance
print(importanceridge)

# glmnet variable importance
# 
# only 20 most important variables shown (out of 206)
# 
# Overall
# LBXGH      0.06294
# GTDCODE    0.05881
# DIQ050     0.05719
# WTSOG2YR   0.05247
# MCQ160N    0.04297
# WTSAF2YR.x 0.03558
# MCQ160L    0.03500
# DIQ170     0.02707
# LBXGLU     0.02643
# DIQ160     0.02636
# LBXSCA     0.02375
# DIQ180     0.02143
# BPXSY3     0.01940
# LBXSKSI    0.01823
# LBDHRPLC   0.01795
# BPXSY1     0.01746
# WHD050     0.01729
# MCQ300C    0.01604
# LBXTR      0.01556
# MCQ160K    0.01530

#run lasso regression
lasso_mod = train(NHANES_6[,predictors], NHANES_6[,outcomeName], method = 'glmnet', trControl=control, preProc = c("center", "scale"),
                  tuneGrid=expand.grid(alpha=1, lambda=seq(0.01,0.1,by=0.01)))
coef(lasso_mod$finalModel,lasso_mod$finalModel$lambdaOpt)

# estimate variable importance
importancelasso <- varImp(lasso_mod, scale=FALSE)

# summarize importance
print(importancelasso)

# glmnet variable importance
# 
# only 20 most important variables shown (out of 206)
# 
# Overall
# WTSOG2YR   0.104207
# GTDCODE    0.098975
# LBXGH      0.086535
# WTSAF2YR.x 0.065799
# DIQ050     0.060941
# MCQ160N    0.047674
# MCQ160L    0.036000
# DIQ160     0.030681
# DIQ170     0.022842
# LBDLDL     0.022136
# BPXDI3     0.017086
# LBXSCA     0.016599
# DIQ180     0.013987
# MCQ160F    0.013569
# LBXSKSI    0.011981
# BMXWAIST   0.011816
# MCQ160K    0.011606
# MCQ160M    0.009559
# LBXSCH     0.009059
# ALQ130     0.008343


##########################################################
#     Put together Dataset with selected features
##########################################################

NHANES_4 = readRDS("NHANES_4.RDS")
colnames(NHANES_4[])

NHANES_select = NHANES_4 %>%
                select(SEQN, ALQ101, ALQ120Q, ALQ120U, ALQ130, ALQ141Q, ALQ141U, BPQ020, BPQ080, BPQ090D, 
                       DIQ010, DIQ050, DIQ160, DIQ170, DIQ172, DIQ180, MCQ092, MCQ160B, MCQ160F, MCQ160K,
                       MCQ160L, MCQ160M, MCQ160N, MCQ203, MCQ300C, MCQ365A, MCQ365B, MCQ365C, MCQ365D, MCQ370A, MCQ370B, MCQ370C, MCQ370D, 
                       PAQ605, PAD615, PAQ620, PAD630, PAQ635, PAD645, PAQ650, PAD660, PAQ665, PAD675, PAD680, PAQ710,
                       WHQ060, WHQ070, WHQ150, SMQ020, SMQ040, SMQ910, SMQ915, SMQ925, LBXTC, LBDHDD, LBXTR, LBDLDL, 
                       LBXWBCSI, LBXLYPCT, LBXMOPCT, LBXNEPCT, LBXEOPCT, LBXBAPCT, LBDLYMNO, LBDMONO, LBDNENO, LBDEONO, LBDBANO, LBXRBCSI,
                       LBXHGB, LBXHCT, LBXMCVSI, LBXMCHSI, LBXMC, LBXRDW, LBXPLTSI, LBXMPSI, LBXGH, LBXGLU, LBXIN, LBXSAL, LBXSAPSI, LBXSASSI,
                       LBXSATSI, LBXSBU, LBXSC3SI, LBXSCA, LBXSCH, LBXSCK, LBXSCLSI, LBXSCR, LBXSGB, LBXSGL, LBXSGTSI, LBXSIR, LBXSKSI,
                       LBXSLDSI, LBXSNASI, LBXSOSSI, LBXSPH, LBXSTB, LBXSTP, LBXSTR, LBXSUA, BPXSY1, BPXSY2, BPXSY3, BPXSY4, BPXDI1,
                       BPXDI2, BPXDI3, BPXDI4, BPXPULS, BMXWT, BMXHT, BMXBMI, BMXLEG, BMXARML, BMXARMC,  BMXWAIST, BMXSAD1, BMXSAD2, BMXSAD3,
                       BMXSAD4, BMDAVSAD, RIAGENDR, RIDAGEYR, DMDEDUC2, INDFMIN2)
                    
glimpse(NHANES_select)                       
#Observations: 1,341
#Variables: 128 (exc SEQN)
summary(NHANES_select)

saveRDS(NHANES_select, file = "NHANES_select.RDS") 

#to read it

NHANES_select = readRDS("NHANES_select.RDS")

##########################################
#         Train/Test Split
##########################################

set.seed(500)  #for reproducibility
train_select = stratified(NHANES_select, c('DIQ010'), 0.7, bothSets = TRUE)
trainset_select = train_select$SAMP1
testset_select = train_select$SAMP2
class(trainset_select)
trainset_select = data.frame(trainset_select)

#Check stratification
sum(trainset_select$DIQ010==1) / 939  #0.112886
sum(testset_select$DIQ010==1) / 402   #0.1119403

saveRDS(trainset_select, file = "trainset_select.RDS") 
saveRDS(testset_select, file = "testset_select.RDS")

#to read it

trainset_select = readRDS("trainset_select.RDS")
testset_select = readRDS("testset_select.RDS")

summary(trainset_select)

#########################################################################
#   Cleaning and Feature Engineering - ALQ120Q and ALQ120U (trainset)
#########################################################################

#combining ALQ120Q and ALQ120U to get standardised units: week, month, year now exists.

ALQ120 = cbind(trainset_select$SEQN, trainset_select$ALQ120Q, trainset_select$ALQ120U)
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

#add this variable to trainset_select dataframe
trainset_select2 = arrange(trainset_select, SEQN)
trainset_select2$ALQ120QU = ALQ120QU$ALQ120QU
glimpse(trainset_select2)

#remove original ALQ120Q and ALQ120U
trainset_select3 = subset(trainset_select2, select = c(-ALQ120Q, -ALQ120U))
summary(trainset_select3)

#0-365 days per year, 999 to recode to NA
trainset_select3$ALQ120QU <- replace(trainset_select3$ALQ120QU, trainset_select3$ALQ120QU == 999, NA)
summary(trainset_select3$ALQ120QU)

saveRDS(trainset_select2, file = "trainset_select2.RDS") 
saveRDS(trainset_select3, file = "trainset_select3.RDS")


#########################################################################
#   Cleaning and Feature Engineering - ALQ141Q and ALQ141U (trainset)
#########################################################################

#combining ALQ141Q and ALQ141U to get standardised units: week, month, year now exists.

ALQ141 = cbind(trainset_select$SEQN, trainset_select$ALQ141Q, trainset_select$ALQ141U)
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

#add this variable to trainset_select dataframe
trainset_select3$ALQ141QU = ALQ141QU$ALQ141QU
glimpse(trainset_select3)

#remove original ALQ141Q and ALQ141U
trainset_select3 = subset(trainset_select3, select = c(-ALQ141Q, -ALQ141U))
summary(trainset_select3)

saveRDS(trainset_select3, file = "trainset_select3.RDS")

#to read it

trainset_select3 = readRDS("trainset_select3.RDS")

################################################################
#   Cleaning and Feature Engineering - DIQ010 (trainset)
################################################################

#1 for diagnosed; 
#if answer No (2), check fasting plasma glucose level more than or = 126; 
#FPG 100-125 (pre-diabetes/borderline); 
#less than or = 100 - No diabetes

diabetes = cbind(trainset_select3$SEQN, trainset_select3$DIQ010, trainset_select3$LBXGLU)
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
with_diabetes = diabetes2 %>% filter(DIQ010 == 1) #106 observations
#LBXGLU have less than 126

#check DIQ010=3 (Borderline) and recode for DIQ010A and DIQ010B accordingly

borderline = diabetes2 %>% filter(DIQ010 == 3)  #19 observations

borderline_highFPG = borderline %>% filter(DIQ010 == 3) %>% filter(LBXGLU >= 126) #4 observations and check that they are recoded

borderline$DIQ010A[borderline$LBXGLU >= 126] <- 1
borderline$DIQ010A[borderline$LBXGLU < 126]  <- 2
summary(borderline)

borderline$DIQ010B[borderline$LBXGLU >= 126] <- 1
borderline$DIQ010B[borderline$LBXGLU > 100 & borderline$LBXGLU < 125]  <- 3
borderline$DIQ010B[borderline$LBXGLU <= 100] <- 4
summary(borderline)

#check that DIQ010=2 with FPG level more than or = 126 and recode to 1 for DIQ010A and 2 for DIQ010B

undiagnosed = diabetes2 %>% filter(DIQ010 == 2) %>% filter(LBXGLU >= 126) #13 observations
undiagnosed$DIQ010A[undiagnosed$LBXGLU >= 126] <- 1
undiagnosed$DIQ010B[undiagnosed$LBXGLU >= 126] <- 2

#check that DIQ010=2 with FPG level 100-125 and recode to 3 for DIQ010B
prediabetes = diabetes2 %>% filter(DIQ010 == 2) %>% filter(LBXGLU > 100 & LBXGLU < 125) #147 observations
prediabetes$DIQ010B[prediabetes$LBXGLU > 100 & prediabetes$LBXGLU < 125] <- 3
summary(prediabetes)

#check that DIQ010=2 with FPG level <= 100 and recode to 4 for DIQ010B
no_diabetes = diabetes2 %>% filter(DIQ010 == 2) %>% filter(LBXGLU <= 100) #654 observations
no_diabetes$DIQ010B[no_diabetes$LBXGLU <= 100] <- 4
summary(no_diabetes)

106+13+19+654+147 #with_diabetes + undiagnosed + borderline + prediabetes + no_diabetes
diabetes3 = rbind(with_diabetes, undiagnosed, borderline, prediabetes, no_diabetes)
summary(diabetes3)
diabetes3 <- arrange(diabetes3, SEQN)

#add the 2 new variables and replace LBXGLU to trainset_select3 dataframe
trainset_select3$DIQ010A = diabetes3$DIQ010A
trainset_select3$DIQ010B = diabetes3$DIQ010B
trainset_select3$LBXGLU = diabetes3$LBXGLU
glimpse(trainset_select3)

saveRDS(trainset_select3, file = "trainset_select3.RDS")

#to read it

trainset_select3 = readRDS("trainset_select3.RDS")

#remove original DIQ010
trainset_select4 = subset(trainset_select3, select = c(-DIQ010))
summary(trainset_select4)

saveRDS(trainset_select4, file = "trainset_select4.RDS")

trainset_select4 = readRDS("trainset_select4.RDS")

################################################################
#       Cleaning - CBC and SBP NA values (trainset)
################################################################
# Binh: if remove, then future participants without results of these tests will not be able to be predicted.
# summary(trainset_select4)
# 
# #check how many participants do not have BOTH complete blood count and standard biochemistry profiles
# 
# CBCSBP_na = trainset_select4 %>%
#   select(SEQN, LBXWBCSI, LBXLYPCT, LBXMOPCT, LBXNEPCT, LBXEOPCT, LBXBAPCT, LBDLYMNO, LBDMONO, LBDNENO, LBDEONO, LBDBANO, LBXRBCSI,
#          LBXHGB, LBXHCT, LBXMCVSI, LBXMCHSI, LBXMC, LBXRDW, LBXPLTSI, LBXMPSI, LBXSAL, LBXSAPSI, LBXSASSI,
#          LBXSATSI, LBXSBU, LBXSC3SI, LBXSCA, LBXSCH, LBXSCK, LBXSCLSI, LBXSCR, LBXSGB, LBXSGL, LBXSGTSI, LBXSIR, LBXSKSI,
#          LBXSLDSI, LBXSNASI, LBXSOSSI, LBXSPH, LBXSTB, LBXSTP, LBXSTR, LBXSUA) %>%
#   filter_at(vars(starts_with("LB")), all_vars(is.na(.)))
# 
# #96 observations (about 10% of the dataset; will remove the participants)
# 
# trainset_select5 = anti_join(trainset_select4, CBCSBP_na)
# summary(trainset_select5)

#########################################
#       Cleaning - WHQ150 (trainset)
#########################################
summary(trainset_select4)
#there are missing and outlier values of 99999 to indicate those who said don't know
#treat them with median of the variable
medianWHQ150 = median(trainset_select4$WHQ150, na.rm = TRUE)

trainset_select4$WHQ150 = replace_na(trainset_select4$WHQ150, medianWHQ150)
sum(is.na(trainset_select4$WHQ150))

summary(trainset_select4$WHQ150)

trainset_select4[trainset_select4$WHQ150 == 99999, "WHQ150"] <- medianWHQ150

summary(trainset_select4$WHQ150)

saveRDS(trainset_select4, file = "trainset_select4.RDS")

################################################################
#   Cleaning and Feature Engineering - SMQ040 (trainset)
################################################################

#Create a new variable for this - Qn: Do you smoke now? 1- Y, 2 - N

#treat NAs with median
medianSMQ040 = median(trainset_select4$SMQ040, na.rm = TRUE)
trainset_select4$SMQ040 = replace_na(trainset_select4$SMQ040, medianSMQ040)
sum(is.na(trainset_select4$SMQ040))

saveRDS(trainset_select4, file = "trainset_select4.RDS")

#to read it

trainset_select4 = readRDS("trainset_select4.RDS")

#create a new column for this categorical variable and save into a new dataset
trainset_select5 = trainset_select4 %>% 
  mutate(SMQ040A = SMQ040)

#recode level 2 to level 1 (yes)
trainset_select5[trainset_select5$SMQ040A == 2, "SMQ040A"] <- 1
summary(trainset_select5$SMQ040A)

#recode level 3 to level 2 (no)
trainset_select5[trainset_select5$SMQ040A == 3, "SMQ040A"] <- 2
summary(trainset_select5$SMQ040A)

#remove original SMQ040
trainset_select5 = subset(trainset_select5, select = c(-SMQ040))
glimpse(trainset_select5)

saveRDS(trainset_select5, file = "trainset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - SMQ915 (trainset)
################################################################

#New Qn: Do you use smokeless tobacco? 1 to 30 days - Y, 0 days - N
sum(is.na(trainset_select4$SMQ915))
summary(trainset_select4$SMQ915)
#treat NAs with median
medianSMQ915 = median(trainset_select4$SMQ915, na.rm = TRUE)

trainset_select4$SMQ915 = replace_na(trainset_select4$SMQ915, medianSMQ915)
sum(is.na(trainset_select4$SMQ915))

#create a new column for this categorical variable and save into a new dataset
trainset_select5$SMQ915A = trainset_select4$SMQ915

trainset_select5$SMQ915A <- ifelse(trainset_select5$SMQ915A %in% 1:30, 1, 2)
summary(trainset_select5$SMQ915A)

# #remove original SMQ915
# trainset_select5 = subset(trainset_select5, select = c(-SMQ915))
# summary(trainset_select5)

saveRDS(trainset_select5, file = "trainset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - LBXTC (trainset)
################################################################

#New Variable: Total cholesterol level at risk?  >=200 - Y (at risk), <200 - N (normal)

#treat NAs with median
medianLBXTC = median(trainset_select5$LBXTC, na.rm = TRUE)

trainset_select5$LBXTC = replace_na(trainset_select5$LBXTC, medianLBXTC)
sum(is.na(trainset_select5$LBXTC))
summary(trainset_select5$LBXTC)

#create a new column for categorical variable
trainset_select5 = trainset_select5 %>% 
  mutate(LBXTCA = LBXTC)

trainset_select5$LBXTCA <- ifelse(trainset_select5$LBXTCA >=200, 1, 2)
summary(trainset_select5$LBXTCA)

saveRDS(trainset_select5, file = "trainset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - LBXTR (trainset)
################################################################

#New Variable: Triglyceride level at risk?  >=150 - Y (at risk), <150 - N (normal)

#treat NAs with median
medianLBXTR = median(trainset_select5$LBXTR, na.rm = TRUE)

trainset_select5$LBXTR = replace_na(trainset_select5$LBXTR, medianLBXTR)
sum(is.na(trainset_select5$LBXTR))
summary(trainset_select5$LBXTR)

#create a new column for categorical variable
trainset_select5 = trainset_select5 %>% 
  mutate(LBXTRA = LBXTR)

trainset_select5$LBXTRA <- ifelse(trainset_select5$LBXTRA >=150, 1, 2)
summary(trainset_select5$LBXTRA)
summary(trainset_select5)

saveRDS(trainset_select5, file = "trainset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - LBDLDL (trainset)
################################################################

#New Variable: LDL Cholesterol level at risk?  >=100 - Y (at risk), <100 - N (normal)

#treat NAs with median
medianLBDLDL = median(trainset_select5$LBDLDL, na.rm = TRUE)

trainset_select5$LBDLDL = replace_na(trainset_select5$LBDLDL, medianLBDLDL)
sum(is.na(trainset_select5$LBDLDL))
summary(trainset_select5$LBDLDL)

#create a new column for categorical variable
trainset_select5 = trainset_select5 %>% 
  mutate(LBDLDLA = LBDLDL)

trainset_select5$LBDLDLA <- ifelse(trainset_select5$LBDLDLA >=100, 1, 2)
summary(trainset_select5$LBDLDLA)
summary(trainset_select5)

saveRDS(trainset_select5, file = "trainset_select5.RDS")

################################################################
#   Cleaning and Feature Engineering - LBDHDD (trainset)
################################################################

#New Variable: HDD Cholesterol level at risk? <60 - Y (at risk); >=60 and above - N (good) 

#treat NAs with median
medianLBDHDD = median(trainset_select5$LBDHDD, na.rm = TRUE)

trainset_select5$LBDHDD = replace_na(trainset_select5$LBDHDD, medianLBDHDD)
sum(is.na(trainset_select5$LBDHDD))
summary(trainset_select5$LBDHDD)

#create a new column for categorical variable
trainset_select5 = trainset_select5 %>% 
  mutate(LBDHDDA = LBDHDD)

trainset_select5$LBDHDDA <- ifelse(trainset_select5$LBDHDDA < 60, 1, 2)
summary(trainset_select5$LBDHDDA)
summary(trainset_select5)

saveRDS(trainset_select5, file = "trainset_select5.RDS")

###################################################################
#   Cleaning and Feature Engineering - BPXSY1,2,3,4 (trainset)
###################################################################

#New variable: max reading out of the 4 for the participant
#create a new column for this variable BPXSYmax

library(fastmatch)
fmatch("BPXSY1",names(trainset_select5))  #99
fmatch("BPXSY2",names(trainset_select5))  #100
fmatch("BPXSY3",names(trainset_select5))  #101
fmatch("BPXSY4",names(trainset_select5))  #102

#because there are 92 rows with NA value for all 4 readings
my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)

trainset_select5$BPXSYmax = apply(trainset_select5[, 99:102], 1, my.max)
head(trainset_select5$BPXSYmax)
summary(trainset_select5$BPXSYmax)

# library(matrixStats)
# trainset_select5$BPXSYmax = rowMaxs(as.matrix(trainset_select5[, 78:81]), na.rm=TRUE)

#New variable: min reading out of the 4 for the participant
#create a new column for this variable BPXSYmin
my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)

trainset_select5$BPXSYmin = apply(trainset_select5[, 99:102], 1, my.min)
head(trainset_select5$BPXSYmin)
summary(trainset_select5$BPXSYmin)

#New Variable: average out of the 4 readings
#create a new column for this variable BPXSYave
my.mean <- function(x) ifelse(!all(is.na(x)), mean(x, na.rm=T), NA)

trainset_select5$BPXSYave = apply(trainset_select5[, 99:102], 1, my.mean)
head(trainset_select5$BPXSYave)
summary(trainset_select5$BPXSYave)

summary(trainset_select5)
saveRDS(trainset_select5, file = "trainset_select5.RDS")

#only use first reading BPXSY1, second reading BPXSY2, third reading BPXSY3, min, max, average for modelling
#remove BPXSY4 from dataset because first 3 are taken for all participants

trainset_select6 = subset(trainset_select5, select = c(-BPXSY4))
summary(trainset_select6)

#treat NA values in BPXSY1,2,3, BPXSYmin, BPXSYmax, BPXSYave with median
medianBPXSY1 = median(trainset_select6$BPXSY1, na.rm = TRUE)
medianBPXSY2 = median(trainset_select6$BPXSY2, na.rm = TRUE)
medianBPXSY3 = median(trainset_select6$BPXSY3, na.rm = TRUE)
medianBPXSYmin = median(trainset_select6$BPXSYmin, na.rm = TRUE)
medianBPXSYmax = median(trainset_select6$BPXSYmax, na.rm = TRUE)
medianBPXSYave = median(trainset_select6$BPXSYave, na.rm = TRUE)

trainset_select6$BPXSY1 = replace_na(trainset_select6$BPXSY1, medianBPXSY1)
sum(is.na(trainset_select6$BPXSY1))
summary(trainset_select6$BPXSY1)

trainset_select6$BPXSY2 = replace_na(trainset_select6$BPXSY2, medianBPXSY2)
sum(is.na(trainset_select6$BPXSY2))
summary(trainset_select6$BPXSY2)

trainset_select6$BPXSY3 = replace_na(trainset_select6$BPXSY3, medianBPXSY3)
sum(is.na(trainset_select6$BPXSY3))
summary(trainset_select6$BPXSY3)

trainset_select6$BPXSYmin = replace_na(trainset_select6$BPXSYmin, medianBPXSYmin)
sum(is.na(trainset_select6$BPXSYmin))
summary(trainset_select6$BPXSYmin)

trainset_select6$BPXSYmax = replace_na(trainset_select6$BPXSYmax, medianBPXSYmax)
sum(is.na(trainset_select6$BPXSYmax))
summary(trainset_select6$BPXSYmax)

trainset_select6$BPXSYave = replace_na(trainset_select6$BPXSYave, medianBPXSYave)
sum(is.na(trainset_select6$BPXSYave))
summary(trainset_select6$BPXSYave)

#create a new column for categorical variable where average Systolic BP at risk? >=120 high (1), <120 normal (2)
trainset_select6 = trainset_select6 %>% 
  mutate(BPXSYaveC = BPXSYave)

trainset_select6$BPXSYaveC <- ifelse(trainset_select6$BPXSYaveC >= 120, 1, 2)
summary(trainset_select6$BPXSYaveC)
summary(trainset_select6)

saveRDS(trainset_select6, file = "trainset_select6.RDS")

###################################################################
#   Cleaning and Feature Engineering - BPXDI1,2,3,4 (trainset)
###################################################################

#New variable: max reading out of the 4 for the participant
#create a new column for this variable BPXDImax

fmatch("BPXDI1",names(trainset_select5))  #103
fmatch("BPXDI2",names(trainset_select5))  #104
fmatch("BPXDI3",names(trainset_select5))  #105
fmatch("BPXDI4",names(trainset_select5))  #106

#there are rows with 0 values in the readings - BPXDI1,2,3
#convert to NA values

trainset_select5$BPXDI1 <- replace(trainset_select5$BPXDI1, trainset_select5$BPXDI1 == 0, NA)
trainset_select5$BPXDI2 <- replace(trainset_select5$BPXDI2, trainset_select5$BPXDI2 == 0, NA)
trainset_select5$BPXDI3 <- replace(trainset_select5$BPXDI3, trainset_select5$BPXDI3 == 0, NA)

summary(trainset_select5$BPXDI1)
summary(trainset_select5$BPXDI2)
summary(trainset_select5$BPXDI3)

#because there are rows with NA value for all 4 readings
my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)

trainset_select5$BPXDImax = apply(trainset_select5[, 103:106], 1, my.max)
head(trainset_select5$BPXDImax)
summary(trainset_select5$BPXDImax)

#New variable: min reading out of the 4 for the participant
#create a new column for this variable BPXDImin
my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)

trainset_select5$BPXDImin = apply(trainset_select5[, 103:106], 1, my.min)
head(trainset_select5$BPXDImin)
summary(trainset_select5$BPXDImin)

#New Variable: average out of the 4 readings
#create a new column for this variable BPXDIave
my.mean <- function(x) ifelse(!all(is.na(x)), mean(x, na.rm=T), NA)

trainset_select5$BPXDIave = apply(trainset_select5[, 103:106], 1, my.mean)
head(trainset_select5$BPXDIave)
summary(trainset_select5$BPXDIave)

summary(trainset_select5)
saveRDS(trainset_select5, file = "trainset_select5.RDS")

#to read it

trainset_select5 = readRDS("trainset_select5.RDS")

#to read it

trainset_select6 = readRDS("trainset_select6.RDS")

#only use first reading BPXDI1, second reading BPXDI2, third reading BPXDI3, min, max, average for modelling

trainset_select6 = cbind(trainset_select6, trainset_select5$BPXDImax, trainset_select5$BPXDImin, trainset_select5$BPXDIave)
trainset_select6 = subset(trainset_select6, select = c(-BPXDI4))
summary(trainset_select6)

trainset_select6$BPXDI1 <- replace(trainset_select6$BPXDI1, trainset_select6$BPXDI1 == 0, NA)
trainset_select6$BPXDI2 <- replace(trainset_select6$BPXDI2, trainset_select6$BPXDI2 == 0, NA)
trainset_select6$BPXDI3 <- replace(trainset_select6$BPXDI3, trainset_select6$BPXDI3 == 0, NA)

summary(trainset_select6$BPXDI1)
summary(trainset_select6$BPXDI2)
summary(trainset_select6$BPXDI3)

#rename the new binded columns
names(trainset_select6)[names(trainset_select6) == "trainset_select5$BPXDImax"] <- "BPXDImax"
names(trainset_select6)[names(trainset_select6) == "trainset_select5$BPXDImin"] <- "BPXDImin"
names(trainset_select6)[names(trainset_select6) == "trainset_select5$BPXDIave"] <- "BPXDIave"

#treat NA values in BPXDI1,2,3, BPXDImin, BPXDImax, BPXDIave with median
medianBPXDI1 = median(trainset_select6$BPXDI1, na.rm = TRUE)
medianBPXDI2 = median(trainset_select6$BPXDI2, na.rm = TRUE)
medianBPXDI3 = median(trainset_select6$BPXDI3, na.rm = TRUE)
medianBPXDImin = median(trainset_select6$BPXDImin, na.rm = TRUE)
medianBPXDImax = median(trainset_select6$BPXDImax, na.rm = TRUE)
medianBPXDIave = median(trainset_select6$BPXDIave, na.rm = TRUE)

trainset_select6$BPXDI1 = replace_na(trainset_select6$BPXDI1, medianBPXDI1)
sum(is.na(trainset_select6$BPXDI1))
summary(trainset_select6$BPXDI1)

trainset_select6$BPXDI2 = replace_na(trainset_select6$BPXDI2, medianBPXDI2)
sum(is.na(trainset_select6$BPXDI2))
summary(trainset_select6$BPXDI2)

trainset_select6$BPXDI3 = replace_na(trainset_select6$BPXDI3, medianBPXDI3)
sum(is.na(trainset_select6$BPXDI3))
summary(trainset_select6$BPXDI3)

trainset_select6$BPXDImin = replace_na(trainset_select6$BPXDImin, medianBPXDImin)
sum(is.na(trainset_select6$BPXDImin))
summary(trainset_select6$BPXDImin)

trainset_select6$BPXDImax = replace_na(trainset_select6$BPXDImax, medianBPXDImax)
sum(is.na(trainset_select6$BPXDImax))
summary(trainset_select6$BPXDImax)

trainset_select6$BPXDIave = replace_na(trainset_select6$BPXDIave, medianBPXDIave)
sum(is.na(trainset_select6$BPXDIave))
summary(trainset_select6$BPXDIave)

#create a new column for categorical variable where average Diastolic BP at risk? >=80 high (1), <80 normal (2)
trainset_select6 = trainset_select6 %>%
   mutate(BPXDIaveC = BPXDIave)

trainset_select6$BPXDIaveC <- ifelse(trainset_select6$BPXDIaveC >= 80, 1, 2)
summary(trainset_select6$BPXDIaveC)
summary(trainset_select6)

saveRDS(trainset_select6, file = "trainset_select6.RDS")


########################################################################
#  Cleaning and Feature Engineering - BMXWT, BMXHT, BMXBMI (trainset)
########################################################################

#For AA, they have different BMI cutoff levels
# >=23 overweight;18.5-22.9 normal; <18.5 underweight
#original: Underweight = <18.5; Normal weight = 18.5-24.9; Overweight = 25-29.9; Obesity = BMI of 30 or greater
# weight in kg / height in m^2

#Treat missing Weight, Height and BMI values with median

#check median of Weight BMXWT
medianBMXWT = median(trainset_select6$BMXWT, na.rm = TRUE)

#check median of Height BMWHT
medianBMXHT = median(trainset_select6$BMXHT, na.rm = TRUE)

64.9/(1.625)^2 #24.6

#Check median of BMI
medianBMXBMI = median(trainset_select6$BMXBMI, na.rm = TRUE)  #24.2

#if input the NA values on weight and height with their missing values, the BMI will not be 24.2. 
#Thus, will clean weight and height first and then calculate individually.

trainset_select6$BMXWT = replace_na(trainset_select6$BMXWT, medianBMXWT)
sum(is.na(trainset_select6$BMXWT))
summary(trainset_select6$BMXWT)

trainset_select6$BMXHT = replace_na(trainset_select6$BMXHT, medianBMXHT)
sum(is.na(trainset_select6$BMXHT))
summary(trainset_select6$BMXHT)

#create a function for BMI calc
bmi <- trainset_select6$BMXWT/((trainset_select6$BMXHT/100)^2)
trainset_select6$BMXBMI = bmi
trainset_select6$BMXBMI = round(trainset_select6$BMXBMI, 1)
head(trainset_select6$BMXBMI)
sum(is.na(trainset_select6$BMXBMI))
summary(trainset_select6$BMXBMI)
head(trainset_select6$BMXBMI)

#create a new column for categorical variable where >=23 overweight (1) ;18.5-22.9 normal (2) ; <18.5 underweight (3)
trainset_select6 = trainset_select6 %>%
  mutate(BMXBMIAA = BMXBMI)

trainset_select6$BMXBMIAA[trainset_select6$BMXBMIAA>=27] <- "Obese"
trainset_select6$BMXBMIAA[trainset_select6$BMXBMIAA>=23 & trainset_select6$BMXBMIAA<27] <- "Overweight"
trainset_select6$BMXBMIAA[trainset_select6$BMXBMIAA>=18.5 & trainset_select6$BMXBMIAA<23] <- "Normal"
trainset_select6$BMXBMIAA[trainset_select6$BMXBMIAA<18.5] <- "Underweight"

class(trainset_select6$BMXBMIAA)
sum(trainset_select6$BMXBMIAA == "Overweight") #390
sum(trainset_select6$BMXBMIAA == "Obese")  #236

#plot a histogram of BMXBMIAA
trainset_select6 %>%
  group_by(BMXBMIAA) %>%
  summarise(count_level = n()) %>%
  ggplot(aes(x = as.factor(BMXBMIAA), y = count_level, fill = BMXBMIAA)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(label = count_level, vjust = 1)) +
  labs(x = "BMI Categories", y = "Number of Participants", 
       title = "Asian Americans by New BMI Cut-offs") +
  scale_fill_manual("legend", values = c("Normal" = "light blue", "Overweight" = "pink", "Obese" = "red", "Underweight" = "light blue"))

# Convert the column to a factor
trainset_select6$BMXBMIAA <- factor(trainset_select6$BMXBMIAA)
head(trainset_select6$BMXBMIAA)
summary(trainset_select6$BMXBMIAA)

# #recode
# trainset_select6$BMXBMIAA <- revalue(trainset_select6$BMXBMIAA, c("overweight"="1", "normal"="2", "underweight" = "3"))
# 
# summary(trainset_select6$BMXBMIAA)
# #2   1   3 
# #281 626  32


#for comparison, use the normal BMI cutoff to create another categorical variable.
#original: Underweight = <18.5; Normal weight = 18.5-24.9; Overweight = 25-29.9; Obesity = BMI of 30 or greater
#create a new column for categorical variable where >=25 overweight;18.5-24.9 normal; <18.5 underweight
trainset_select6 = trainset_select6 %>%
  mutate(BMXBMIO = BMXBMI)

trainset_select6$BMXBMIO[trainset_select6$BMXBMIO>=30] <- "Obese"
trainset_select6$BMXBMIO[trainset_select6$BMXBMIO>=25 & trainset_select6$BMXBMIO<30] <- "Overweight"
trainset_select6$BMXBMIO[trainset_select6$BMXBMIO>=18.5 & trainset_select6$BMXBMIO<25] <- "Normal"
trainset_select6$BMXBMIO[trainset_select6$BMXBMIO<18.5] <- "Underweight"

#plot a histogram of BMXBMIO
trainset_select6 %>%
  group_by(BMXBMIO) %>%
  summarise(count_level = n()) %>%
  ggplot(aes(x = as.factor(BMXBMIO), y = count_level, fill = BMXBMIO)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(label = count_level, vjust = 1)) +
  labs(x = "BMI Categories", y = "Number of Participants", 
       title = "Asian Americans by Ordinary BMI Cut-offs") +
  scale_fill_manual("legend", values = c("Normal" = "light blue", "Overweight" = "pink", "Obese" = "red", "Underweight" = "light blue"))

# Convert the column to a factor
trainset_select6$BMXBMIO <- factor(trainset_select6$BMXBMIO)
head(trainset_select6$BMXBMIO)
summary(trainset_select6$BMXBMIO)

# #recode
# trainset_select6$BMXBMIO <- revalue(trainset_select6$BMXBMIO, c("overweight"="1", "normal"="2", "underweight" = "3"))
# summary(trainset_select6$BMXBMIO)
# #  2   1   3 
# #539 368  32      #nearly half become normal instead of overweight

summary(trainset_select6)
saveRDS(trainset_select6, file = "trainset_select6.RDS")

########################################################################
# Cleaning and Feature Engineering - BMXSAD1,2,3,4, average (trainset)
########################################################################

#This variable was created by averaging up to four SAD readings. 
#The majority of survey participants have two readings (BMXSAD1, and BMXSAD2); as such, these two readings were used to obtain mean of SAD value. 
#If there were four SAD readings (BMXSAD1, BMXSAD2, BMXSAD3, and BMXSAD4) because the difference between the first and second SAD measurements 
#was greater than 0.5 cm, then three closest SAD readings were used to obtain mean of SAD value.
#In a few instances where two outlying measurements are equally distant from the means of the two closest measurements, 
#then all four readings were used to obtain mean of SAD value.

#create a variable BMXSADmin: min reading out of the 4 for the participant
#create a new column for this variable BMXSADmin

fmatch("BMXSAD1",names(trainset_select5))  #115
fmatch("BMXSAD2",names(trainset_select5))  #116
fmatch("BMXSAD3",names(trainset_select5))  #117
fmatch("BMXSAD4",names(trainset_select5))  #118

#because there are 115 rows with NA value for all 4 readings
my.min <- function(x) ifelse(!all(is.na(x)), min(x, na.rm=T), NA)

trainset_select6$BMXSADmin = apply(trainset_select5[, 115:118], 1, my.min)
head(trainset_select6$BMXSADmin)
summary(trainset_select6$BMXSADmin)

#create a variable BMXSADmax: max reading out of the 4 for the participant
#create a new column for this variable BMXSADmax
my.max <- function(x) ifelse(!all(is.na(x)), max(x, na.rm=T), NA)

trainset_select6$BMXSADmax = apply(trainset_select5[, 115:118], 1, my.max)
head(trainset_select6$BMXSADmax)
summary(trainset_select6$BMXSADmax)

#remove BMXSAD2,3,4 from dataset

trainset_select6 = subset(trainset_select6, select = c(-BMXSAD2, -BMXSAD3, -BMXSAD4))
summary(trainset_select6)

#treat NA values in BMXSAD1, BMXSADmin, BMXSADmax, BMDAVSAD with median

medianBMXSAD1 = median(trainset_select6$BMXSAD1, na.rm = TRUE)
medianBMXSADmin = median(trainset_select6$BMXSADmin, na.rm = TRUE)
medianBMXSADmax = median(trainset_select6$BMXSADmax, na.rm = TRUE)
medianBMDAVSAD = median(trainset_select6$BMDAVSAD, na.rm = TRUE)

trainset_select6$BMXSAD1 = replace_na(trainset_select6$BMXSAD1, medianBMXSAD1)
sum(is.na(trainset_select6$BMXSAD1))
summary(trainset_select6$BMXSAD1)

trainset_select6$BMXSADmin = replace_na(trainset_select6$BMXSADmin, medianBMXSADmin)
sum(is.na(trainset_select6$BMXSADmin))
summary(trainset_select6$BMXSADmin)

trainset_select6$BMXSADmax = replace_na(trainset_select6$BMXSADmax, medianBMXSADmax)
sum(is.na(trainset_select6$BMXSADmax))
summary(trainset_select6$BMXSADmax)

trainset_select6$BMDAVSAD = replace_na(trainset_select6$BMDAVSAD, medianBMDAVSAD)
sum(is.na(trainset_select6$BMDAVSAD))
summary(trainset_select6$BMDAVSAD)

summary(trainset_select6)

saveRDS(trainset_select6, file = "trainset_select6.RDS")

############################################
# Cleaning - Physical Activity (trainset)
############################################

summary(trainset_select6$PAQ605) #no missing, no outliers

summary(trainset_select6$PAD615) #missing values, no outliers
medianPAD615 = median(trainset_select6$PAD615, na.rm = TRUE)
trainset_select6$PAD615 = replace_na(trainset_select6$PAD615, medianPAD615)
sum(is.na(trainset_select6$PAD615))
summary(trainset_select6$PAD615)

summary(trainset_select6$PAQ620) #no missing values, have outliers
trainset_select6$PAQ620 <- replace(trainset_select6$PAQ620, trainset_select6$PAQ620 == 9, NA)
summary(trainset_select6$PAQ620)

summary(trainset_select6$PAD630) #missing values, no outliers

summary(trainset_select6$PAQ635) #all good

summary(trainset_select6$PAD645) #missing values, no outliers

summary(trainset_select6$PAQ650) #all good

summary(trainset_select6$PAD660) #missing values, no outliers

summary(trainset_select6$PAQ665) #all good

summary(trainset_select6$PAD675) #missing values, no outliers

summary(trainset_select6$PAD680) #no missing values, have outliers
trainset_select6$PAD680 <- replace(trainset_select6$PAD680, trainset_select6$PAD680 == 9999, NA)
summary(trainset_select6$PAD680)

summary(trainset_select6$PAQ710) #no missing values, have outliers
trainset_select6$PAQ710 <- replace(trainset_select6$PAQ710, trainset_select6$PAQ710 == 77, NA)
trainset_select6$PAQ710 <- replace(trainset_select6$PAQ710, trainset_select6$PAQ710 == 99, NA)
summary(trainset_select6$PAQ710)
trainset_select6$PAQ710 <- replace(trainset_select6$PAQ710, trainset_select6$PAQ710 == 8, 0)

###############################################################
# Cleaning - categorical variables with 7 and 9 (trainset)
###############################################################

#to get a more accurate median value, convert outliers 7 and 9 to NA values

trainset_select6$ALQ101 <- replace(trainset_select6$ALQ101, trainset_select6$ALQ101 == 9, NA)
summary(trainset_select6$ALQ101)
trainset_select6$BPQ080 <- replace(trainset_select6$BPQ080, trainset_select6$BPQ080 == 9, NA)
summary(trainset_select6$BPQ080)
trainset_select6$BPQ090D <- replace(trainset_select6$BPQ090D, trainset_select6$BPQ090D == 9, NA)
summary(trainset_select6$BPQ090D)
trainset_select6$DIQ170 <- replace(trainset_select6$DIQ170, trainset_select6$DIQ170 == 9, NA)
summary(trainset_select6$DIQ170)
trainset_select6$DIQ172 <- replace(trainset_select6$DIQ172, trainset_select6$DIQ172 == 7, NA)
summary(trainset_select6$DIQ172)
trainset_select6$DIQ172 <- replace(trainset_select6$DIQ172, trainset_select6$DIQ172 == 9, NA)
summary(trainset_select6$DIQ172)
trainset_select6$DIQ180 <- replace(trainset_select6$DIQ180, trainset_select6$DIQ180 == 7, NA)
summary(trainset_select6$DIQ180)
trainset_select6$DIQ180 <- replace(trainset_select6$DIQ180, trainset_select6$DIQ180 == 9, NA)
summary(trainset_select6$DIQ180)
trainset_select6$MCQ092 <- replace(trainset_select6$MCQ092, trainset_select6$MCQ092 == 7, NA)
summary(trainset_select6$MCQ092)
trainset_select6$MCQ092 <- replace(trainset_select6$MCQ092, trainset_select6$MCQ092 == 9, NA)
summary(trainset_select6$MCQ092)
trainset_select6$MCQ160B <- replace(trainset_select6$MCQ160B, trainset_select6$MCQ160B == 9, NA)
summary(trainset_select6$MCQ160B)
trainset_select6$MCQ160F <- replace(trainset_select6$MCQ160F, trainset_select6$MCQ160F == 9, NA)
summary(trainset_select6$MCQ160F)
trainset_select6$MCQ160K <- replace(trainset_select6$MCQ160K, trainset_select6$MCQ160K == 9, NA)
summary(trainset_select6$MCQ160K)
trainset_select6$MCQ160L <- replace(trainset_select6$MCQ160L, trainset_select6$MCQ160L == 9, NA)
summary(trainset_select6$MCQ160L)
trainset_select6$MCQ160M <- replace(trainset_select6$MCQ160M, trainset_select6$MCQ160M == 9, NA)
summary(trainset_select6$MCQ160M)
trainset_select6$MCQ160N <- replace(trainset_select6$MCQ160N, trainset_select6$MCQ160N == 9, NA)
summary(trainset_select6$MCQ160N)
trainset_select6$MCQ203 <- replace(trainset_select6$MCQ203, trainset_select6$MCQ203 == 9, NA)
summary(trainset_select6$MCQ203)
trainset_select6$MCQ300C <- replace(trainset_select6$MCQ300C, trainset_select6$MCQ300C == 7, NA)
summary(trainset_select6$MCQ300C)
trainset_select6$MCQ300C <- replace(trainset_select6$MCQ300C, trainset_select6$MCQ300C == 9, NA)
summary(trainset_select6$MCQ300C)
trainset_select6$MCQ365C <- replace(trainset_select6$MCQ365C, trainset_select6$MCQ365C == 9, NA)
summary(trainset_select6$MCQ365C)
trainset_select6$MCQ370A <- replace(trainset_select6$MCQ370A, trainset_select6$MCQ370A == 9, NA)
summary(trainset_select6$MCQ370A)
trainset_select6$MCQ370C <- replace(trainset_select6$MCQ370C, trainset_select6$MCQ370C == 9, NA)
summary(trainset_select6$MCQ370C)
trainset_select6$WHQ070 <- replace(trainset_select6$WHQ070, trainset_select6$WHQ070 == 9, NA)
summary(trainset_select6$WHQ070)
trainset_select6$INDFMIN2 <- replace(trainset_select6$INDFMIN2, trainset_select6$INDFMIN2 == 77, NA)
summary(trainset_select6$INDFMIN2)
trainset_select6$INDFMIN2 <- replace(trainset_select6$INDFMIN2, trainset_select6$INDFMIN2 == 99, NA)
summary(trainset_select6$INDFMIN2)

saveRDS(trainset_select6, file = "trainset_select6.RDS")

#to read it

trainset_select6 = readRDS("trainset_select6.RDS")
summary(trainset_select6)

########################################################################
#  Cleaning and Feature Engineering - SMQ020 and SMQ925 (trainset)
########################################################################

#Create a new variable for this - Qn: Ever smoked a cigarette even 1 time? 1- Y, 2 - N
#because SMQ925 is conditional on SMQ020, if answer no to SMQ020 then will be asked this question.

summary(trainset_select6$SMQ020)
summary(trainset_select6$SMQ925)

#create a new column for this categorical variable and save into a new dataset
trainset_select6 = trainset_select6 %>% 
  mutate(SMQ925A = SMQ925)

#recode SMQ020 to level 1 for SMQ925A (yes)
trainset_select6[trainset_select6$SMQ020 == 1, "SMQ925A"] <- 1
summary(trainset_select6$SMQ925A)

#recode SMQ925 to level 1 for SMQ925A (yes)
trainset_select6[trainset_select6$SMQ925 == 1, "SMQ925A"] <- 1
summary(trainset_select6$SMQ925A)

#recode SMQ925 to level 2 for SMQ925A (no)
trainset_select6[trainset_select6$SMQ925 == 2, "SMQ925A"] <- 2
summary(trainset_select6$SMQ925A)

#treat NAs with median
medianSMQ925A = median(trainset_select6$SMQ925A, na.rm = TRUE)
trainset_select6$SMQ925A = replace_na(trainset_select6$SMQ925A, medianSMQ925A)
sum(is.na(trainset_select6$SMQ925A))

#remove original SMQ925
trainset_select6 = subset(trainset_select6, select = c(-SMQ925))
glimpse(trainset_select6)

saveRDS(trainset_select6, file = "trainset_select6.RDS")

###############################################################
# Cleaning - replace NA values with column median (trainset)
###############################################################

trainset_select7 = trainset_select6 %>% 
  mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))

sum(is.na(trainset_select7))

# #add back SMQ915
# 
# trainset_select7 = cbind(trainset_select7, trainset_select4$SMQ915)
# summary(trainset_select7)
# names(trainset_select7)[names(trainset_select7) == "trainset_select4$SMQ915"] <- "SMQ915"

summary(trainset_select7)
saveRDS(trainset_select7, file = "trainset_select7.RDS")

#to read it

trainset_select7 = readRDS("trainset_select7.RDS")
glimpse(trainset_select7)
# Observations: 939
# Variables: 140
