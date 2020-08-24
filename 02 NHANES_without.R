NHANES_1_without = readRDS("NHANES_1_without.RDS")

##########################################
#       Selecting Ages >=20
##########################################

#check missing values in RIDAGEYR (should be none)

sum(is.na(NHANES_1_without$RIDAGEYR))

#select Adults ages >=20

NHANES_2_without = NHANES_1_without %>% filter(RIDAGEYR >= 20)


##########################################
#     Filter out Pregnant ladies
##########################################

#check missing values in RIDEXPRG (including values = 3 and .)
#treat any missing values

NHANES_2_without$RIDEXPRG = replace_na(NHANES_2_without$RIDEXPRG, 2)
sum(is.na(NHANES_2_without$RIDEXPRG))

NHANES_2_without[NHANES_2_without$RIDEXPRG == 3, "RIDEXPRG"] <- 2

#filter out/remove any pregnant ladies where RIDEXPRG = 1
NHANES_3_without = NHANES_2_without %>% filter(RIDEXPRG == 2)

##########################################
#     Selecting Asian Americans
##########################################

#check missing values in RIDRETH3 (should be none)

sum(is.na(NHANES_3_without$RIDRETH3))


#select Asian Americans (non-Hispanic Asian - RIDRETH3=6)

NHANES_4_without = NHANES_3_without %>% filter(RIDRETH3 == 6)

saveRDS(NHANES_4_without, file = "NHANES_4_without.RDS") 

#to read it

NHANES_4_without = readRDS("NHANES_4_without.RDS")

#1341 Asian Americans who are 20yo and above and not pregnant.


##########################################
#      Exploratory Data Analysis
##########################################

glimpse(NHANES_4_without)
#Observations: 1,341
#Variables: 408

#bind DIQ010 to this dataset as its outcome variable
NHANES_4_without = cbind(NHANES_4_without, NHANES_4$DIQ010)

##########################################
#         Train/Test Split
##########################################

library(splitstackshape)
set.seed(500)  #for reproducibility
train2 = stratified(NHANES_4_without, c('NHANES_4$DIQ010'), 0.7, bothSets = TRUE)
trainset2 = train2$SAMP1
testset2 = train2$SAMP2
class(trainset2)
trainset2 = data.frame(trainset2)

saveRDS(trainset2, file = "trainset2.RDS") 
saveRDS(testset2, file = "testset2.RDS")

#to read it

trainset2 = readRDS("trainset2.RDS")
testset2 = readRDS("testset2.RDS")

##########################################
#      Missing Values (train set)
##########################################

#remove variables with more than 60% missing

NHANES_5_without = trainset2[colSums(is.na(trainset2))/nrow(trainset2) < 0.6]

#presence of missings in the dataset
sapply(NHANES_5_without, function(x) sum(is.na(x)))

#imputation using the mice package
methods(mice) #check available imputation methods

init2 = mice(NHANES_5_without, maxit=0) 
meth2 = init2$method
predM2 = init2$predictorMatrix

#run the imputation
set.seed(500) #to ensure results are repeatable
imputed_without = mice(NHANES_5_without, method=meth2, predictorMatrix=predM2, m=5)

saveRDS(imputed_without, file = "imputed_without.RDS") 

#to read it

imputed_without = readRDS("imputed_without.RDS")

#Create a dataset after imputation
#The missing values have been replaced with the imputed values in the first of the five datasets. 
#If you wish to use another one, just change the second parameter in the complete() function

imputed_without_1 <- complete(imputed_without, 1)
warnings()

#Check for missings in the imputed dataset.
sapply(imputed_without_1, function(x) sum(is.na(x)))

#delete variables with still missing data
NHANES_6_without = imputed_without_1[!colSums(is.na(imputed_without_1))>0]
#Check for missings
sapply(NHANES_6_without, function(x) sum(is.na(x)))

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
control1_without <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
?caret::train
library(kknn)
model1_without <- train(NHANES_4.DIQ010~., data=NHANES_6_without, method="kknn", preProcess="scale", trControl=control1_without)

# estimate variable importance
importance1_without <- varImp(model1_without, scale=FALSE)

# summarize importance
print(importance1_without)

# loess r-squared variable importance
# 
# only 20 most important variables shown (out of 163)
# 
# Overall
# LBXSGL   0.12326
# RIDAGEYR 0.07729
# DMDHHSZE 0.04137
# LBXSCH   0.04090
# BMXWAIST 0.04031
# DMDHRAGE 0.03665
# MCQ160N  0.03481
# BMXSAD2  0.03471
# MCQ365D  0.03316
# MCQ365B  0.03293
# BMDAVSAD 0.03242
# BMXSAD1  0.03211
# DMDEDUC2 0.02946
# LBXSTR   0.02705
# MCQ365C  0.02633
# LBXSOSSI 0.02384
# BMXBMI   0.02336
# MCQ160L  0.02168
# LBXSBU   0.02151
# LBXRBCSI 0.02095

# plot importance
plot(importance1_without)

###Recursive Feature Elimination (explore all possible subsets - Random Forest algorithm is used on each iteration to evaluate the model)###
# define the control using a random forest selection function
control2_without <- rfeControl(functions=rfFuncs, method="cv", number=10)  #cross-validation with 10 folds
# control2 <- rfeControl(functions=rfFuncs, method="repeatedcv", repeats=10)  #repeated k-fold cross validation with 3 repeats

#dataframe of the variables excluding DIQ010
NHANES_rfe_without = NHANES_6_without[!NHANES_6_without$NHANES_4.DIQ010]

outcomeName_without = 'NHANES_4.DIQ010'
predictors_without = names(NHANES_6_without)[!names(NHANES_6_without) %in% outcomeName_without]

# run the RFE algorithm
results1_without = rfe(NHANES_6_without[,predictors_without], NHANES_6_without[,outcomeName_without],
               rfeControl = control2_without)

# summarize the results
print(results1_without)
#The top 5 variables (out of 8):  LBXSGL, RIDAGEYR, LBXSCH, DMDHRAGE, MCQ160L
# Variables  RMSE Rsquared    MAE  RMSESD RsquaredSD   MAESD Selected
# 4         0.3176   0.1938 0.1602 0.03173    0.08289 0.02291         
# 8         0.3116   0.2255 0.1539 0.03046    0.06392 0.02133         
# 16        0.3116   0.2292 0.1558 0.03436    0.06482 0.02534        *
# 163       0.3117   0.2302 0.1631 0.03310    0.06857 0.02209         

colnames(NHANES_6_without[4]) #RIAGENDR  #Gender
colnames(NHANES_6_without[8]) #RIDEXMON  #Six month time period when the examination was performed - two categories: November 1 through April 30, May 1 through October 31.
colnames(NHANES_6_without[16]) #SIALANG  #Language of the Sample Person Interview Instrument
colnames(NHANES_6_without[163]) #WHQ225  #Times lost 10 lbs or more to lose weight

# list the chosen features
predictors(results1_without)

#[1] "LBXSGL"   "RIDAGEYR" "LBXSCH"   "DMDHRAGE" "MCQ160L"  "LBXSCLSI" "BMXWAIST" "WHD130"   "WHQ150"   "SIAPROXY" "LBXRBCSI" "MCQ160N"  "BMXSAD1" 
#[14] "LBXSTR"   "LBXSUA"   "BMDAVSAD"

# plot the results
plot(results1_without, type=c("g", "o"))
