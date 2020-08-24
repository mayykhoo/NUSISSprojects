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
library(ggplot2)
library(GGally)
library(neuralnet)
library(glmnet)
setwd("C:/Users/May Khoo/Desktop/NUS/Internship")


##########################################
#     Correlation Analysis (train set)
##########################################

##Remove redundant features that are highly correlated with each other###
# calculate correlation matrix
correlationMatrix <- cor(trainset_select7)

# summarize the correlation matrix
print(correlationMatrix)

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)

# print indexes of highly correlated attributes
print(highlyCorrelated)
# [1]  43  49  55  81  97  98  99 100 101 108 115 116 117 119 120 121 125 126  40  52  36  87  88  90  91  93  94  70  38  37

correlated = data.frame(colnames(trainset_select7))
correlated[43,] #LBXNEPCT
correlated[49,] #LBDEONO
correlated[55,] #LBXMCHSI
correlated[81,] #LBXSOSSI
correlated[97,] #BMXARML
correlated[98,] #BMXARMC
correlated[99,] #BMXWAIST
correlated[100,] #BMXSAD1
correlated[101,] #BMDAVSAD
correlated[108,] #DIQ010B
correlated[115,] #BPXSYmax
correlated[116,] #BPXSYmin
correlated[117,] #BPXSYave
correlated[119,] #BPXDImax
correlated[120,] #BPXDImin
correlated[121,] #BPXDIave
correlated[125,] #BMXSADmin
correlated[126,] #BMXSADmax
correlated[40,] #LBXWBCSI
correlated[52,] #LBXHGB
correlated[36,] #LBXTC
correlated[87,] #BPXSY1
correlated[88,] #BPXSY2
correlated[90,] #BPXDI1
correlated[91,] #BPXDI2
correlated[93,] #BMXWT
correlated[94,] #BMXHT
correlated[70,] #LBXSCH
correlated[38,] #LBXTR
correlated[37,] #LBDHDD

##########################################
#    Categorical variables (train set)
##########################################

str(trainset_select7)

cols = c("ALQ101", "BPQ020","BPQ080", "BPQ090D", "DIQ050", "DIQ160", "DIQ170", "DIQ172", "DIQ180", 
         "MCQ092", "MCQ160B", "MCQ160F", "MCQ160K", "MCQ160L", "MCQ160M", "MCQ160N", "MCQ203", "MCQ300C", "MCQ365A", "MCQ365B", "MCQ365C",
         "MCQ365D", "MCQ370A", "MCQ370B", "MCQ370C", "MCQ370D", "PAQ605", "PAQ620", "PAQ635", "PAQ650", "PAQ665", "WHQ060", "WHQ070", "SMQ910",
         "DMDEDUC2", "INDFMIN2", "DIQ010A", "DIQ010B", "SMQ020", "SMQ040A", "SMQ915A", "SMQ925A", "LBXTCA", "LBXTRA","LBDLDLA","LBDHDDA", 
         "BPXSYaveC", "BPXDIaveC", "BPXPULS", "BMXBMIAA", "BMXBMIO", "RIAGENDR")

trainset_select7[cols] <- lapply(trainset_select7[cols], factor)

saveRDS(trainset_select7, file = "trainset_select7.RDS")

#to read trainset_select7

trainset_select7 = readRDS("trainset_select7.RDS")
glimpse(trainset_select7) #140 variables
colnames(trainset_select7)

############################################################
#   Feature scaling for continuous variables (train set)
############################################################

#minmax scaling/normalization: scaled into the range of [0, 1]
#use caret package preProcess()

cont = trainset_select7 %>% select_if(is.numeric)
summary(cont)
cont = subset(cont, select = c(-SEQN)) #87 variables
cat = trainset_select7 %>% select_if(is.factor) #52 variables

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

trainset_transformed = cbind(trainset_select7$SEQN, transformed, cat)

saveRDS(trainset_transformed, file = "trainset_transformed.RDS")
summary(trainset_transformed)
names(trainset_transformed)[names(trainset_transformed) == "trainset_select7$SEQN"] <- "SEQN"

trainset_transformed = arrange(trainset_transformed, SEQN)

#to read trainset_transformed

trainset_transformed = readRDS("trainset_transformed.RDS")
summary(trainset_transformed)


#########################
#   EDA (train set)
#########################

sum(trainset_select7$DIQ010A == "1") #123
class(trainset_select7$DIQ010A)
trainset_select7$DIQ010A = factor(trainset_select7$DIQ010A)

#plot a histogram of DIQ010A
trainset_select7 %>%
  group_by(DIQ010A) %>%
  summarise(count_level = n()) %>%
  ggplot(aes(x = as.factor(DIQ010A), y = count_level, fill = DIQ010A)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(label = count_level, vjust = 1)) +
  labs(x = "DIQ010A", y = "Number of Participants", 
       title = "Has the participant ever been told that they have diabetes?") +
  scale_x_discrete(labels=c("Yes", "No")) +
  scale_fill_manual("legend", values = c("1" = "light blue", "2" = "pink"))

sum(trainset_select7$DIQ010B == "1") #110
class(trainset_select7$DIQ010B)
summary(trainset_select7$DIQ010B)
trainset_select7$DIQ010B = factor(trainset_select7$DIQ010B)

#plot a histogram of DIQ010B
#DIQ010B: 1 (yes/1 in DIQ010 or borderline/3 with FPG >= 126 in DIQ010); 2 (no/2 in DIQ010 with FPG >= 126);
#         3 (prediabetes; FPG 100-125); 4 (no diabetes FPG <= 100)
trainset_select7 %>%
  group_by(DIQ010B) %>%
  summarise(count_level = n()) %>%
  ggplot(aes(x = DIQ010B, y = count_level, fill = DIQ010B)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(label = count_level, vjust = -0.5)) +
  labs(x = "DIQ010B", y = "Number of Participants", 
       title = "Is the patient diagnosed with diabetes or pre-diabetes?") +
  scale_x_discrete(labels=c("Yes, been told", "FPG >= 126 mg/dL", "Prediabetes", "No diabetes")) +
  scale_fill_manual("legend", values = c("1" = "red", "2" = "pink", "3" = "medium blue", "4" = "light blue"))


#explore distribution of Age, Weight, Height and BMI
hist(trainset_select7$RIDAGEYR)
hist(trainset_select7$BMXWT)
hist(trainset_select7$BMXHT)
hist(trainset_select7$BMXBMI)

#relationship bw LBXGLU and age (FPG)
ggplot(trainset_select7, aes(x=RIDAGEYR, y=LBXGLU)) +
  geom_point() + geom_smooth()

#relationship bw LBXGH and age (Hb1Ac)
ggplot(trainset_select7, aes(x=RIDAGEYR, y=LBXGH)) +
  geom_point() + geom_smooth()

#relationship bw Physical Activity and age 
#minutes vigorous intensity work
ggplot(trainset_select7, aes(x=RIDAGEYR, y=PAD615)) +
  geom_point() + geom_smooth()

#minutes moderate intensity work
ggplot(trainset_select7, aes(x=RIDAGEYR, y=PAD630))+
  geom_point() + geom_smooth()

#walk/bicycle
ggplot(trainset_select7, aes(x=RIDAGEYR, y=PAD645))+
  geom_point() + geom_smooth()

# Minutes vigorous recreational activities
ggplot(trainset_select7, aes(x=RIDAGEYR, y=PAD660))+
  geom_point() + geom_smooth()

# Minutes moderate recreational activities
ggplot(trainset_select7, aes(x=RIDAGEYR, y=PAD675))+
  geom_point() + geom_smooth()

# Minutes sedentary activity
ggplot(trainset_select7, aes(x=RIDAGEYR, y=PAD680)) +
  geom_point() + geom_smooth()



##########################################################################################################
# Remove Diabetes Questionnaire, Insulin, Blood Glucose tests and CBC variables (train set)
##########################################################################################################

trainset_select8 = subset(trainset_transformed, select = c(-DIQ050, -DIQ160, -DIQ170, -DIQ172, -DIQ180, 
                                                       -LBXGH, -LBXGLU, -LBXIN, -LBXSGL, - LBXWBCSI, -LBXPLTSI, -LBXRBCSI,
                                                       -LBXLYPCT, -LBXMOPCT, -LBXNEPCT, -LBXEOPCT, -LBXBAPCT, 
                                                       -LBDLYMNO, -LBDMONO, -LBDNENO, -LBDEONO, -LBDBANO, -LBXHGB, 
                                                       -LBXHCT, -LBXMCVSI, -LBXMCHSI,-LBXMC, -LBXRDW, -LBXMPSI))
                                                       
saveRDS(trainset_select8, file = "trainset_select8.RDS")
summary(trainset_select8) #111 variables

#to read it

trainset_select8 = readRDS("trainset_select8.RDS")
colnames(trainset_select8)
glimpse(trainset_select8)

############################
#    DIQ010A (train set)
############################

trainset_DIQ010A = subset(trainset_select8, select = c(-DIQ010B))
saveRDS(trainset_DIQ010A, file = "trainset_DIQ010A.RDS")

#to read it

trainset_DIQ010A = readRDS("trainset_DIQ010A.RDS")

trainset_DIQ010A_reg = subset(trainset_select8, select = c(-DIQ010B))
saveRDS(trainset_DIQ010A, file = "trainset_DIQ010A.RDS")

############################
#    DIQ010B (train set)
############################

trainset_DIQ010B = subset(trainset_select8, select = c(-DIQ010A))
saveRDS(trainset_DIQ010B, file = "trainset_DIQ010B.RDS")

#to read it

trainset_DIQ010B = readRDS("trainset_DIQ010B.RDS")

##########################################
#     Feature Selection (train set)
##########################################

set.seed(500) #ensure the results are repeatable

###Rank Features By Importance (K-NN)###

# prepare training scheme
control_select1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
?caret::train
library(kknn)
knn_A <- train(DIQ010A~., data=trainset_DIQ010A, method="kknn", preProcess="scale", trControl=control_select1)
knn_B <- train(DIQ010B~., data=trainset_DIQ010B, method="kknn", preProcess="scale", trControl=control_select1)

# estimate variable importance
importance_knnA <- varImp(knn_A, scale=FALSE)
importance_knnB <- varImp(knn_B, scale=FALSE)

# summarize importance
print(importance_knnA)

# ROC curve variable importance
# 
# only 20 most important variables shown (out of 100)
# 
# Importance
# RIDAGEYR      0.7938
# BPQ090D       0.7474
# WHQ150        0.6874
# BPXSYmax      0.6770
# BMXWAIST      0.6749
# LBXSOSSI      0.6725
# BPXSY1        0.6715
# BPQ080        0.6707
# MCQ365B       0.6702
# BPXSYave      0.6687
# BMXSAD1       0.6682
# BMXSADmin     0.6681
# BMXSADmax     0.6668
# BMDAVSAD      0.6668
# BPQ020        0.6668
# MCQ365D       0.6644
# BPXSY2        0.6631
# MCQ300C       0.6592
# BPXSYmin      0.6541
# BPXSY3        0.6501

print(importance_knnB)

# ROC curve variable importance
# 
# variables are sorted by maximum importance across the classes
# only 20 most important variables shown (out of 100)
# 
# X1     X2     X3     X4
# RIDAGEYR  0.7545 0.8189 0.6444 0.8189
# LBXTC     0.7794 0.7794 0.7794 0.6778
# BPQ090D   0.7312 0.7686 0.6350 0.7686
# LBDLDL    0.7636 0.7636 0.7636 0.6269
# LBXSCH    0.7552 0.7552 0.7552 0.6742
# WHQ150    0.6458 0.7172 0.6458 0.7172
# BPXSY3    0.6364 0.6540 0.7066 0.6540
# BPXDI3    0.7059 0.7059 0.7059 0.6149
# BMXWAIST  0.5914 0.7049 0.5706 0.7049
# MCQ300C   0.7035 0.6611 0.6116 0.7035
# BPXSYaveC 0.6479 0.6479 0.6999 0.6384
# BMXSAD1   0.5721 0.6981 0.5357 0.6981
# BMXSADmax 0.5733 0.6962 0.5378 0.6962
# BMXSADmin 0.5726 0.6961 0.5428 0.6961
# BPXSYmin  0.6203 0.6562 0.6961 0.6562
# BMDAVSAD  0.5718 0.6959 0.5357 0.6959
# BPQ020    0.6234 0.6931 0.6234 0.6931
# BPXSYave  0.6119 0.6754 0.6885 0.6754
# LBXSOSSI  0.6506 0.6881 0.6021 0.6881
# BPXSYmax  0.5884 0.6868 0.6691 0.6868

# plot importance
plot(importance_knnA)
plot(importance_knnB)

###Recursive Feature Elimination (explore all possible subsets - Random Forest algorithm is used on each iteration to evaluate the model)###
# define the control using a random forest selection function
control_select2 <- rfeControl(functions=rfFuncs, method="cv", number=10)  #cross-validation with 10 folds

#dataframe of the variables excluding DIQ010A
rfeA = trainset_DIQ010A[!trainset_DIQ010A$DIQ010A]
rfeB = trainset_DIQ010B[!trainset_DIQ010B$DIQ010B]

outcomeName1 = 'DIQ010A'
predictors1 = names(trainset_DIQ010A)[!names(trainset_DIQ010A) %in% outcomeName1]

outcomeName2 = 'DIQ010B'
predictors2 = names(trainset_DIQ010B)[!names(trainset_DIQ010B) %in% outcomeName2]

# run the RFE algorithm
results_rfeA = rfe(trainset_DIQ010A[,predictors1], trainset_DIQ010A[,outcomeName1],
               rfeControl = control_select2)

results_rfeB = rfe(trainset_DIQ010B[,predictors2], trainset_DIQ010B[,outcomeName2],
                      rfeControl = control_select2)

# summarize the results
print(results_rfeA)
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold) 
# 
# Resampling performance over subset size:
#   
# Variables Accuracy  Kappa AccuracySD KappaSD Selected
#       4   0.8712 0.2894    0.01606 0.11539         
#       8   0.8872 0.3580    0.02096 0.11555         
#      16   0.8872 0.3632    0.01545 0.07953         
#     100   0.8893 0.2621    0.02031 0.16824        *
#   
# The top 5 variables (out of 100):
# BPQ090D, RIDAGEYR, LBXSCH, LBXTC, LBXSOSSI

colnames(trainset_DIQ010A[4]) #BPQ080 
colnames(trainset_DIQ010A[8]) #MCQ300C 
colnames(trainset_DIQ010A[16]) #MCQ370D  
colnames(trainset_DIQ010A[100]) #BMXSADmax  

print(results_rfeB)
# Recursive feature selection
# 
# Outer resampling method: Cross-Validated (10 fold) 
# 
# Resampling performance over subset size:
#   
# Variables Accuracy  Kappa AccuracySD KappaSD Selected
#         4   0.7254 0.3093    0.03335 0.08528         
#         8   0.7338 0.3143    0.01556 0.03940         
#        16   0.7454 0.3455    0.02884 0.09452        *
#       100   0.7412 0.2581    0.03578 0.12073         
# 
# The top 5 variables (out of 16):
# LBXTR, LBDLDL, BPQ090D, RIDAGEYR, LBXTRA

# list the chosen features
predictors(results_rfeA) 

# [1] "BPQ090D"   "RIDAGEYR"  "LBXSCH"    "LBXTC"     "LBXSOSSI"  "MCQ300C"   "MCQ365D"   "LBXSCLSI"  "LBDLDL"    "BPQ020"    "BPQ080"    "MCQ365B"  
# [13] "BPXSY1"    "WHQ150"    "LBXTR"     "LBXSNASI"  "LBXSTR"    "LBXSCR"    "LBXSGTSI"  "BPXSY2"    "BMXWAIST"  "BPXSYmax"  "BMXSAD1"   "LBXSPH"   
# [25] "BMXSADmax" "BPXSYave"  "BMDAVSAD"  "BPXDI3"    "LBXTCA"    "BMXSADmin" "BMXLEG"    "MCQ365A"   "LBXRBCSI"  "BPXDIave"  "BPXDImax"  "LBXSCA"   
# [37] "BPXSYmin"  "LBDLDLA"   "MCQ365C"   "BPXDImin"  "BMXWT"     "LBXSBU"    "LBXSKSI"   "LBDHDD"    "LBXSAPSI"  "BMXBMI"    "BPXSY3"    "BMXARMC"  
# [49] "LBXSUA"    "BPXDI1"    "LBXSAL"    "LBXSASSI"  "LBXTRA"    "BMXBMIO"   "LBXSATSI"  "BMXARML"   "BMXHT"     "BPXSYaveC" "MCQ370C"   "LBXSCK"   
# [61] "INDHHIN2"  "MCQ160N"   "MCQ370D"   "BPXDI2"    "LBXSTB"    "RIAGENDR"  "LBXSTP"    "BMXBMIAA"  "DMDEDUC2"  "MCQ370A"   "LBXSLDSI"  "LBDHDDA"  
# [73] "PAD675"    "PAD660"    "BPXDIaveC" "PAQ605"    "PAD680"    "PAQ650"    "LBXSIR"    "SEQN"      "WHQ070"    "LBXWBCSI"  "ALQ130"    "LBXSC3SI" 
# [85] "ALQ141QU"  "LBXSGB"    "LBXPLTSI"  "MCQ370B"   "SMQ910"    "MCQ160L"   "SMQ915"    "SMQ915A"   "PAQ665"    "PAD615"    "WHQ060"    "PAQ620"   
# [97] "SMQ040A"   "PAD645"    "PAD630"    "PAQ635" 

predictors(results_rfeB) 

# [1] "LBXTR"    "LBDLDL"   "BPQ090D"  "RIDAGEYR" "LBXTRA"   "LBXTC"    "LBXSOSSI" "LBXSCH"   "LBDLDLA"  "LBXSCR"   "MCQ300C"  "MCQ365D"  "BPQ020"  
# [14] "LBXSGTSI" "BMXSAD1"  "LBXRBCSI"

# plot the results
plot(results_rfeA, type=c("g", "o"))
plot(results_rfeB, type=c("g", "o"))

# estimate variable importance
importance_rfeA <- varImp(results_rfeA, scale=FALSE)
importance_rfeB <- varImp(results_rfeB, scale=FALSE)

# summarize importance
print(importance_rfeA)
print(importance_rfeB)

# plot importance
plot(importance_rfeA)
plot(importance_rfeB)

##########################################
# Neural Networks - DIQ010A (train set)
##########################################

library(neuralnet)

# define neural network parameter

# combine the attributes name for convenience.
names1 <- colnames(trainset_DIQ010A)
f1 <- as.formula(paste("DIQ010A~", paste(names1[!names1 %in% "DIQ010A"], collapse = " + ")))

# train model
set.seed(500)
st = Sys.time() 
nnmodel1 <- train(f1, trainset_DIQ010A, method='nnet', trace = FALSE,
                 #Grid of tuning parameters to try:
                 tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1))) 
Sys.time() - st

# show neural network result

nnmodel1[["finalModel"]] #a 119-5-1 network with 606 weights
plot(nnmodel1)

# save model in rds format (to save time rerunning model)
saveRDS(nnmodel1, file = "nnmodel1.rds")

# read model
nnmodel1 <- readRDS("nnmodel1.rds") 

# test model on training set 
nnA_datatrain <- subset(trainset_DIQ010A, select = -c(DIQ010A))  
predictNN_A <- predict(nnmodel1, nnA_datatrain, type = "raw")

confusionMatrix(data = predictNN_A, reference = trainset_DIQ010A$DIQ010A)
#Accuracy : 1
# Confusion Matrix and Statistics
# 
# Reference
# Prediction  1   2
#         1 123   0
#         2   0 816

# show relative importance
VarImp_nnA = varImp(nnmodel1)
print(VarImp_nnA)

# nnet variable importance
# 
# only 20 most important variables shown (out of 119)
# 
# Overall
# RIDAGEYR    100.00
# LBXSOSSI     97.80
# PAQ6502      82.09
# WHQ0702      81.74
# MCQ300C2     77.53
# LBDLDLA2     75.95
# BMXBMIO3     75.10
# INDHHIN23    70.77
# INDHHIN214   70.62
# MCQ370A2     69.10
# DMDEDUC22    67.15
# LBXSCLSI     67.03
# BMXBMIO2     66.34
# BPXDIaveC2   63.76
# INDHHIN212   63.18
# LBXSNASI     62.23
# LBXTCA2      61.81
# BPQ090D2     61.64
# RIAGENDR     61.21
# BPQ0802      60.47

VarImp_nnA %>% 
  ggplot(aes(x = names1, y = overall))+ geom_bar(stat ='identity') + coord_flip() + labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")

# use confusion matrix to evaluate model performance on test data.

##########################################
# Neural Networks - DIQ010B (train set)
##########################################

# define neural network parameter

# combine the attributes name for convenience.
names2 <- colnames(trainset_DIQ010B)
f2 <- as.formula(paste("DIQ010B~", paste(names2[!names2 %in% "DIQ010B"], collapse = " + ")))

# train model
set.seed(500)
st = Sys.time() 
nnmodel2 <- train(f2, trainset_DIQ010B, method='nnet', trace = FALSE,
                  #Grid of tuning parameters to try:
                  tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1))) 
Sys.time() - st

# show neural network result

nnmodel2[["finalModel"]] #a 119-1-4 network with 128 weights
plot(nnmodel2)

# save model in rds format (to save time rerunning model)
saveRDS(nnmodel2, file = "nnmodel2.rds")

# read model
nnmodel2 <- readRDS("nnmodel2.rds") 

# test model on training set 
nnB_datatrain <- subset(trainset_DIQ010B, select = -c(DIQ010B))  
predictNN_B <- predict(nnmodel2, nnB_datatrain, type = "raw")

confusionMatrix(data = predictNN_B, reference = trainset_DIQ010B$DIQ010B)
#Accuracy : 0.771 
#Confusion Matrix and Statistics
# 
# Reference
# Prediction  1   2   3   4
#         1  77   9  22  18
#         2   0   0   0   0
#         3   0   0   0   0
#         4  33   4 129 647

# Statistics by Class:
#   
#                       Class: 1 Class: 2 Class: 3 Class: 4
# Sensitivity            0.7000  0.00000   0.0000   0.9729
# Specificity            0.9409  1.00000   1.0000   0.3942
# Pos Pred Value         0.6111      NaN      NaN   0.7958
# Neg Pred Value         0.9594  0.98616   0.8392   0.8571

# show relative importance
VarImp_nnB = varImp(nnmodel2)
print(VarImp_nnB)

# nnet variable importance
# 
# variables are sorted by maximum importance across the classes
# only 20 most important variables shown (out of 119)
# 
# Overall      1      2      3      4
# LBXSOSSI  100.00 100.00 100.00 100.00 100.00
# LBXSNASI   71.58  71.58  71.58  71.58  71.58
# LBXSLDSI   31.58  31.58  31.58  31.58  31.58
# BPXSY3     30.93  30.93  30.93  30.93  30.93
# LBDLDL     30.83  30.83  30.83  30.83  30.83
# RIDAGEYR   27.20  27.20  27.20  27.20  27.20
# BPXDI2     26.04  26.04  26.04  26.04  26.04
# LBXSCLSI   25.85  25.85  25.85  25.85  25.85
# BMXWT      25.03  25.03  25.03  25.03  25.03
# PAD645     23.14  23.14  23.14  23.14  23.14
# LBXSASSI   22.85  22.85  22.85  22.85  22.85
# LBXSATSI   19.37  19.37  19.37  19.37  19.37
# BPXSYmax   18.26  18.26  18.26  18.26  18.26
# BMXARML    17.21  17.21  17.21  17.21  17.21
# LBXSCH     16.78  16.78  16.78  16.78  16.78
# LBXTR      16.77  16.77  16.77  16.77  16.77
# LBXSBU     16.73  16.73  16.73  16.73  16.73
# BPXDI3     15.90  15.90  15.90  15.90  15.90
# LBXSKSI    15.11  15.11  15.11  15.11  15.11
# BPXSYmin   15.04  15.04  15.04  15.04  15.04

# use confusion matrix to evaluate model performance on test data.

#######################################################
# Lasso and Ridge Regression - DIQ010A (train set)
#######################################################
# set.seed(500)
# 
# trainset_regA = subset(trainset_reg, select = c(-DIQ010B))
# summary(trainset_regA)
# 
# trainsetA = as.numeric(trainset_DIQ010A)
# 
# outcomeName = 'DIQ010A'
# predictors = names(trainset_regA)[!names(trainset_regA) %in% outcomeName]
# 
# levels(trainset_regA$DIQ010A) <- c("yes", "no")
# # 
# # sum(is.na(trainset_DIQ010A))
# 
# library(glmnet)
# #run ridge regression on DIQ010A
# control = trainControl(method="cv", number=4, allowParallel = TRUE, classProbs = FALSE)
# ridge_modA = train(trainset_regA[,predictors], trainset_regA[,outcomeName], method = 'glmnet', trControl=control, preProc = c("center", "scale"),
#                   tuneGrid=expand.grid(alpha=0, lambda=seq(0.01,0.1,by=0.01)))
# coef(ridge_modA$finalModel,ridge_modA$finalModel$lambdaOpt)
# 
# #run lasso regression on DIQ010A
# lasso_modA = train(trainset_regA[,predictors], trainset_regA[,outcomeName], method = 'glmnet', trControl=control, preProc = c("center", "scale"),
#                  tuneGrid=expand.grid(alpha=1, lambda=seq(0.01,0.1,by=0.01)))
# coef(lasso_modA$finalModel,lasso_modA$finalModel$lambdaOpt)

#cannot run due to zero variances in some variables. it is not fair to remove and assume they have no predictive value.


##########################
# Datasets for modelling 
##########################

#remove SEQN
#remove all bloodcell counts and biochemistry profile variables
#use only average calculations, remove other similar/redundant variables

train_model1 = subset(trainset_select8, select = c(-SEQN, -LBXSAL, -LBXSAPSI, -LBXSASSI, -LBXSATSI, -LBXSBU,
                                                   -LBXSC3SI, -LBXSCA, -LBXSCH, -LBXSCK, -LBXSCLSI,
                                                   -LBXSCR, -LBXSGB, -LBXSGTSI, -LBXSIR, -LBXSKSI,
                                                   -LBXSLDSI, -LBXSNASI, -LBXSOSSI, -LBXSPH, -LBXSTB,
                                                   -LBXSTP, -LBXSTR, -LBXSUA,
                                                   -SMQ915A, -BPXSY1, -BPXSY2, -BPXSY3, -BPXSYmax, -BPXSYmin,
                                                   -BPXDI1, -BPXDI2, -BPXDI3, -BPXDImax, -BPXDImin, -BMXSAD1,
                                                   -BMXSADmax, -BMXSADmin, -WHQ060))

saveRDS(train_model1, file = "train_model1.RDS")
summary(train_model1)  #72 variables

#to read it

train_model1 = readRDS("train_model1.RDS")

#remove variables for sensitivity analysis

train_model2 = subset(train_model1, select = c(-PAQ605, -PAQ620, -PAQ635, -PAQ650, -PAQ665, -SMQ915, -LBXTCA,
                                               -LBDHDDA, -LBXTRA, -LBDLDLA, -BPXSYaveC, -BPXDIaveC, -BMXBMI, -BMXBMIO))

saveRDS(train_model2, file = "train_model2.RDS")
summary(train_model2)  #58 variables

#to read it

train_model2 = readRDS("train_model2.RDS")

#include some significant variables from the lab tests

train_model3 = cbind(train_model2, trainset_select8$LBXSCA, trainset_select8$LBXSGTSI, trainset_select8$LBXSKSI, trainset_select8$LBXSPH)

#rename the new binded columns
names(train_model3)[names(train_model3) == "trainset_select8$LBXSCA"] <- "LBXSCA"
names(train_model3)[names(train_model3) == "trainset_select8$LBXSGTSI"] <- "LBXSGTSI"
names(train_model3)[names(train_model3) == "trainset_select8$LBXSKSI"] <- "LBXSKSI"
names(train_model3)[names(train_model3) == "trainset_select8$LBXSPH"] <- "LBXSPH"


saveRDS(train_model3, file = "train_model3.RDS")
summary(train_model3)  #62 variables

#to read it

train_model3 = readRDS("train_model3.RDS")


######################################
#  Datasets for sensitivity analysis 
######################################
#includes both DIQ010A and DIQ010B

#Physical Activity variables
train_sa1 = subset(train_model2, select = c(-PAD615, -PAD630, -PAD645, -PAD660, -PAD675))
train_sa1 = cbind(train_sa1, trainset_select8$PAQ605, trainset_select8$PAQ620, trainset_select8$PAQ635, 
                  trainset_select8$PAQ650, trainset_select8$PAQ665)

names(train_sa1)[names(train_sa1) == "trainset_select8$PAQ605"] <- "PAQ605"
names(train_sa1)[names(train_sa1) == "trainset_select8$PAQ620"] <- "PAQ620"
names(train_sa1)[names(train_sa1) == "trainset_select8$PAQ635"] <- "PAQ635"
names(train_sa1)[names(train_sa1) == "trainset_select8$PAQ650"] <- "PAQ650"
names(train_sa1)[names(train_sa1) == "trainset_select8$PAQ665"] <- "PAQ665"
summary(train_sa1)

saveRDS(train_sa1, file = "train_sa1.RDS")

#to read it

train_sa1 = readRDS("train_sa1.RDS")

#Smoking variable
train_sa2 = subset(train_model2, select = c(-SMQ910))
train_sa2 = cbind(train_sa2, trainset_select8$SMQ915A)

names(train_sa2)[names(train_sa2) == "trainset_select8$SMQ915A"] <- "SMQ915A"
summary(train_sa2)

saveRDS(train_sa2, file = "train_sa2.RDS")

#to read it

train_sa2 = readRDS("train_sa2.RDS")

#Cholesterol variables
train_sa3 = subset(train_model2, select = c(-LBXTC, -LBDHDD, -LBXTR, -LBDLDL))
train_sa3 = cbind(train_sa3, trainset_select8$LBXTCA, trainset_select8$LBDHDDA, trainset_select8$LBXTRA, trainset_select8$LBDLDLA)

names(train_sa3)[names(train_sa3) == "trainset_select8$LBXTCA"] <- "LBXTCA"
names(train_sa3)[names(train_sa3) == "trainset_select8$LBDHDDA"] <- "LBDHDDA"
names(train_sa3)[names(train_sa3) == "trainset_select8$LBXTRA"] <- "LBXTRA"
names(train_sa3)[names(train_sa3) == "trainset_select8$LBDLDLA"] <- "LBDLDLA"
summary(train_sa3)

saveRDS(train_sa3, file = "train_sa3.RDS")

#to read it

train_sa3 = readRDS("train_sa3.RDS")

#Blood Pressure variables
train_sa4 = subset(train_model2, select = c(-BPXSYave, -BPXDIave))
train_sa4 = cbind(train_sa4, trainset_select8$BPXSYaveC, trainset_select8$BPXDIaveC)

names(train_sa4)[names(train_sa4) == "trainset_select8$BPXSYaveC"] <- "BPXSYaveC"
names(train_sa4)[names(train_sa4) == "trainset_select8$BPXDIaveC"] <- "BPXDIaveC"
summary(train_sa4)

saveRDS(train_sa4, file = "train_sa4.RDS")

#to read it

train_sa4 = readRDS("train_sa4.RDS")

#BMI-ordinary variable
train_sa5 = subset(train_model2, select = c(-BMXBMIAA))
train_sa5 = cbind(train_sa5, trainset_select8$BMXBMIO)

names(train_sa5)[names(train_sa5) == "trainset_select8$BMXBMIO"] <- "BMXBMIO"
summary(train_sa5)

saveRDS(train_sa5, file = "train_sa5.RDS")

#to read it

train_sa5 = readRDS("train_sa5.RDS")

#BMI continuous variable
train_sa6 = subset(train_model2, select = c(-BMXBMIAA))
train_sa6 = cbind(train_sa6, trainset_select8$BMXBMI)

names(train_sa6)[names(train_sa6) == "trainset_select8$BMXBMI"] <- "BMXBMI"
summary(train_sa6)

saveRDS(train_sa6, file = "train_sa6.RDS")

#to read it

train_sa6 = readRDS("train_sa6.RDS")

