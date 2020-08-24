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
library(DMwR)
library(rpart)
library(randomForest)
library(gbm)
library(fastAdaboost)
library(klaR)
library(caTools)
library(naivebayes)
library(UBL)
library(pROC)
library(PRROC)
setwd("C:/Users/May Khoo/Desktop/NUS/Internship")

#DIQ010B: 1 (yes/1 in DIQ010 or borderline/3 with FPG >= 126 in DIQ010); 2 (no/2 in DIQ010 with FPG >= 126);
#         3 (prediabetes; FPG 100-125); 4 (no diabetes FPG <= 100)


############################
#    DIQ010B (train set)
############################

trainmodel_B1 = subset(train_model2, select = c(-DIQ010A))
saveRDS(trainmodel_B1, file = "trainmodel_B1.RDS")

#to read it

trainmodel_B1 = readRDS("trainmodel_B1.RDS")
summary(trainmodel_B1)

trainmodel_B2 = subset(train_model3, select = c(-DIQ010A))
saveRDS(trainmodel_B2, file = "trainmodel_B2.RDS")

#to read it

trainmodel_B2 = readRDS("trainmodel_B2.RDS")
summary(trainmodel_B2)

############################
#    DIQ010B (test set)
############################

testmodel_B1 = subset(test_model2, select = c(-DIQ010A))
saveRDS(testmodel_B1, file = "testmodel_B1.RDS")

#to read it

testmodel_B1 = readRDS("testmodel_B1.RDS")
summary(testmodel_B1)

testmodel_B2 = subset(test_model3, select = c(-DIQ010A))
saveRDS(testmodel_B2, file = "testmodel_B2.RDS")

#to read it

testmodel_B2 = readRDS("testmodel_B2.RDS")
summary(testmodel_B2)


############################
#   1 vs Rest (train set)
############################

#in a multiclass dataset, can split into multiple binary classification problems

#create a new column for this categorical variable and save into a new dataset
trainmodel_B1a = trainmodel_B1 %>% 
  mutate(DIQ010Ba = DIQ010B)

#recode levels 1,2,3 to level 1 for DIQ010Ba (yes)
trainmodel_B1a[trainmodel_B1a$DIQ010Ba == 2, "DIQ010Ba"] <- 1
trainmodel_B1a[trainmodel_B1a$DIQ010Ba == 3, "DIQ010Ba"] <- 1
summary(trainmodel_B1a$DIQ010Ba)

#recode level 4 to level 2 for DIQ010Ba (no)
trainmodel_B1a[trainmodel_B1a$DIQ010Ba == 4, "DIQ010Ba"] <- 2
summary(trainmodel_B1a$DIQ010Ba)
# 1   2   3   4 
# 274 665   0   0 
#about 40:60, no need to balance
trainmodel_B1a = subset(trainmodel_B1a, select = c(-DIQ010B))
trainmodel_B1a$DIQ010Ba = factor(trainmodel_B1a$DIQ010Ba, levels = c('1', '2'))
saveRDS(trainmodel_B1a, file = "trainmodel_B1a.RDS")

#to read it

trainmodel_B1a = readRDS("trainmodel_B1a.RDS")

############################
#   1 vs Rest (test set)
############################

#in a multiclass dataset, can split into multiple binary classification problems

#create a new column for this categorical variable and save into a new dataset
testmodel_B1a = testmodel_B1 %>% 
  mutate(DIQ010Ba = DIQ010B)

#recode levels 1,2,3 to level 1 for DIQ010Ba (yes)
testmodel_B1a[testmodel_B1a$DIQ010Ba == 2, "DIQ010Ba"] <- 1
testmodel_B1a[testmodel_B1a$DIQ010Ba == 3, "DIQ010Ba"] <- 1
summary(testmodel_B1a$DIQ010Ba)

#recode level 4 to level 2 for DIQ010Ba (no)
testmodel_B1a[testmodel_B1a$DIQ010Ba == 4, "DIQ010Ba"] <- 2
summary(testmodel_B1a$DIQ010Ba)
#  1   2   3   4 
# 108 294   0   0 

testmodel_B1a = subset(testmodel_B1a, select = c(-DIQ010B))
testmodel_B1a$DIQ010Ba = factor(testmodel_B1a$DIQ010Ba, levels = c('1', '2'))
saveRDS(testmodel_B1a, file = "testmodel_B1a.RDS")

#to read it

testmodel_B1a = readRDS("testmodel_B1a.RDS")

####################################################################################
#   Exclude patients who already have been diagnosed (level 1) (train set)
####################################################################################

#create a new column for this categorical variable and save into a new dataset
trainmodel_B234 = trainmodel_B1 %>% 
  mutate(DIQ010B234 = DIQ010B)

trainmodel_B234 = subset(trainmodel_B234, select = c(-DIQ010B))
names(trainmodel_B234)[names(trainmodel_B234) == "DIQ010B234"] <- "DIQ010B"

#remove level 1
trainmodel_B234[trainmodel_B234$DIQ010B == 1, "DIQ010B"] <- NA
trainmodel_B234 = na.omit(trainmodel_B234)
summary(trainmodel_B234$DIQ010B)

#recode level 2 to level 1 for DIQ010B234
trainmodel_B234[trainmodel_B234$DIQ010B == 2, "DIQ010B"] <- 1

#recode level 3 to level 2 for DIQ010B234
trainmodel_B234[trainmodel_B234$DIQ010B == 3, "DIQ010B"] <- 2

#recode level 4 to level 3 for DIQ010B234
trainmodel_B234[trainmodel_B234$DIQ010B == 4, "DIQ010B"] <- 3
summary(trainmodel_B234$DIQ010B)

trainmodel_B234$DIQ010B = factor(trainmodel_B234$DIQ010B, levels = c('1', '2', '3'))
saveRDS(trainmodel_B234, file = "trainmodel_B234.RDS")

#to read it

trainmodel_B234 = readRDS("trainmodel_B234.RDS")

####################################################################################
#   Exclude patients who already have been diagnosed (level 1) (test set)
####################################################################################

#create a new column for this categorical variable and save into a new dataset
testmodel_B234 = testmodel_B1 %>% 
  mutate(DIQ010B234 = DIQ010B)

tesmodel_B234 = subset(testmodel_B234, select = c(-DIQ010B))
names(testmodel_B234)[names(testmodel_B234) == "DIQ010B234"] <- "DIQ010B"

#remove level 1
testmodel_B234[testmodel_B234$DIQ010B == 1, "DIQ010B"] <- NA
testmodel_B234 = na.omit(testmodel_B234)
summary(testmodel_B234$DIQ010B)

#recode level 2 to level 1 for DIQ010B234
testmodel_B234[testmodel_B234$DIQ010B == 2, "DIQ010B"] <- 1

#recode level 3 to level 2 for DIQ010B234
testmodel_B234[testmodel_B234$DIQ010B == 3, "DIQ010B"] <- 2

#recode level 4 to level 3 for DIQ010B234
testmodel_B234[testmodel_B234$DIQ010B == 4, "DIQ010B"] <- 3
summary(testmodel_B234$DIQ010B)

testmodel_B234$DIQ010B = factor(testmodel_B234$DIQ010B, levels = c('1', '2', '3'))
saveRDS(testmodel_B234, file = "testmodel_B234.RDS")

#to read it

testmodel_B234 = readRDS("testmodel_B234.RDS")


####################################
#  Class Imbalance: SMOTE (train)
####################################

library(DMwR)

print(table(trainmodel_B1$DIQ010B))
# 1   2   3   4 
# 110  13 151 665 
print(prop.table(table(trainmodel_B1$DIQ010B)))
# 1          2          3          4 
# 0.11714590 0.01384452 0.16080937 0.70820021 


# Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalance
# create more of the positive cases (upsample) 
# #trial
# j <- SMOTE(DIQ010B ~ ., trainmodel_B1, perc.over = 5000, k=5, perc.under = 115)
# print(table(j$DIQ010B))
# j1 <- SMOTE(DIQ010B ~ ., j, perc.over = 700, k = 5, perc.under=200)
# print(table(j1$DIQ010B))
# j2 <- SMOTE(DIQ010B ~ ., j1, perc.over = 600, k = 5, perc.under=180)
# print(table(j2$DIQ010B))
# print(prop.table(table(j2$DIQ010B)))

library(UBL)
balancedtrain_B1 = SmoteClassif(DIQ010B ~ ., trainmodel_B1, C.perc = "balance", k = 5, dist = "HVDM")

# Let's check the count of unique value in the target variable
print(table(balancedtrain_B1$DIQ010B))
print(prop.table(table(balancedtrain_B1$DIQ010B)))
# 1   2   3   4 
# 235 234 235 235 
# 1         2         3         4 
# 0.2502662 0.2492013 0.2502662 0.2502662 

saveRDS(balancedtrain_B1, file = "balancedtrain_B1.RDS")

#to read it

balancedtrain_B1 = readRDS("balancedtrain_B1.RDS")

###train_B2

balancedtrain_B2 = SmoteClassif(DIQ010B ~ ., trainmodel_B2, C.perc = "balance", k = 5, dist = "HVDM")
print(table(balancedtrain_B2$DIQ010B))
print(prop.table(table(balancedtrain_B2$DIQ010B)))

saveRDS(balancedtrain_B2, file = "balancedtrain_B2.RDS")

#to read it

balancedtrain_B2 = readRDS("balancedtrain_B2.RDS")

###train_B234

print(table(trainmodel_B234$DIQ010B))
# 1   2   3 
# 13 151 665
print(prop.table(table(trainmodel_B234$DIQ010B)))
# 1          2          3 
# 0.01568154 0.18214717 0.80217129 

balancedtrain_B234 = SmoteClassif(DIQ010B ~ ., trainmodel_B234, C.perc = "balance", k = 5, dist = "HVDM")
print(table(balancedtrain_B234$DIQ010B))
print(prop.table(table(balancedtrain_B234$DIQ010B)))
# 1   2   3 
# 275 276 276

saveRDS(balancedtrain_B234, file = "balancedtrain_B234.RDS")

#to read it

balancedtrain_B234 = readRDS("balancedtrain_B234.RDS")

###################################
#    Decision Tree (model B1)
###################################

library(rpart)

#try on imbalanced and compare accuracy

B1_rpart <- rpart(formula = DIQ010B~ .,
                          data=trainmodel_B1,
                          method = "class",
                          parms=list(split="information"),
                          control= rpart.control(minsplit=20,
                                                 minbucket=7,
                                                 usesurrogate=0,
                                                 maxsurrogate=0),
                          model=TRUE)

# test model on trainset and check accuracy with confusion matrix.
B1_traintree = predict(B1_rpart, trainmodel_B1, type = "class")
confusionMatrix(B1_traintree, reference = trainmodel_B1$DIQ010B, mode="prec_recall")


# Perform prediction on testset and look at confusion matrix.
B1_testtree = predict(B1_rpart, testmodel_B1, type = "class")
confusionMatrix(B1_testtree, reference = testmodel_B1$DIQ010B)
confusionMatrix(B1_testtree, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# Generate a textual view of the Decision Tree model.
rattle::fancyRpartPlot(B1_rpart)
summary(B1_rpart)

#see variable importance
B1_rpart[["variable.importance"]]

#use caret#
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
B1tree_fit <- train(DIQ010B~ ., data = trainmodel_B1, method = "rpart",
                   trControl=trctrl,
                   tuneLength = 10, 
                   parms=list(split='information'))

# test model on trainset and check accuracy with confusion matrix.
B1_traintreefit = predict(B1tree_fit, trainmodel_B1, type = "raw")
confusionMatrix(B1_traintreefit, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1test_treefit <- predict(B1tree_fit, newdata = testmodel_B1, type = "raw")
confusionMatrix(data = B1test_treefit, reference = testmodel_B1$DIQ010B, mode="prec_recall")

B1tree_fit
varImp(B1tree_fit)

############################################################################################################################################
# Perform modelling on balanced dataset

balancedB1_rpart <- rpart(formula = DIQ010B~ .,
                          data=balancedtrain_B1,
                          method = "class",
                          parms=list(split="information"),
                          control= rpart.control(minsplit=20,
                                                 minbucket=7,
                                                 usesurrogate=0,
                                                 maxsurrogate=0),
                          model=TRUE)


# Generate a textual view of the Decision Tree model.
rattle::fancyRpartPlot(balancedB1_rpart)
summary(balancedB1_rpart)

#see variable importance
balancedB1_rpart[["variable.importance"]]

# test model on trainset and check accuracy with confusion matrix.
balancedB1_traintree = predict(balancedB1_rpart, balancedtrain_B1, type = "class")
confusionMatrix(balancedB1_traintree, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balancedB1_testtree = predict(balancedB1_rpart, testmodel_B1, type = "class")
confusionMatrix(balancedB1_testtree, reference = testmodel_B1$DIQ010B, mode="prec_recall")

#use caret#

balancedB1tree_fit <- train(DIQ010B~ ., data = balancedtrain_B1, method = "rpart",
                            trControl=trctrl,
                            tuneLength = 10, 
                            parms=list(split='information'))

# test model on trainset and check accuracy with confusion matrix.
balancedB1_traintreefit = predict(balancedB1_rpart, balancedtrain_B1, type = "class")
confusionMatrix(balancedB1_traintreefit, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balancedB1test_treefit <- predict(balancedB1tree_fit, newdata = testmodel_B1, type = "raw")
confusionMatrix(data = balancedB1test_treefit, reference = testmodel_B1$DIQ010B, mode="prec_recall")

balancedB1tree_fit
varImp(balancedB1tree_fit)


# balvarimp_tree = as.data.frame(varImp(balancedB1_rpart)) %>%
#   `colnames<-`(c("importance")) %>%
#   rownames_to_column("Variable")
# balvarimp_tree %>%
#   ggplot(aes(x = reorder(Variable, importance), y = importance))+ geom_col() +
#   coord_flip() + labs(title = "Relative Importance of Variables for Decision Tree", x = 'Variable', y = 'Relative Importance')


######################################
#   Random Forest (model B1)
######################################
library(randomForest)
#####using caret#####

#mtry: Number of variables available for splitting at each tree node. In the random forests literature, this is referred to as the mtry parameter.
# ntrees: Number of trees to grow. For small datasets, 50 trees may be sufficient. For larger datasets, 500 or more may be required

#try on imbalanced and compare accuracy

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  B1_rf = train(DIQ010B~.,
                      data = trainmodel_B1,
                      method = "rf",
                      importance = TRUE,
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trainControl(method = "cv",
                                               number = 5,
                                               search = 'grid',
                                               classProbs = FALSE,
                                               savePredictions = "final"),
                      ntree = ntree)
}

print(B1_rf)

# Best tuning parameter
B1_rf$bestTune

# Test model on trainset and check accuracy with confusion matrix.
B1train_rf <- predict(B1_rf, newdata = trainmodel_B1, type = "raw")
confusionMatrix(data = B1train_rf, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1test_rf <- predict(B1_rf, newdata = testmodel_B1, type = "raw")
confusionMatrix(data = B1test_rf, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(B1_rf)

###########################################################################################################################################
#try on balanced dataset

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  balancedB1_rf = train(DIQ010B~.,
                data = balancedtrain_B1,
                method = "rf",
                importance = TRUE,
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trainControl(method = "cv",
                                         number = 5,
                                         search = 'grid',
                                         classProbs = FALSE,
                                         savePredictions = "final"),
                ntree = ntree)
}

print(balancedB1_rf)

# Best tuning parameter
balancedB1_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
balancedB1_trainrf = predict(balancedB1_rf, balancedtrain_B1, type = "raw")
confusionMatrix(balancedB1_trainrf, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balancedB1_testrf <- predict(balancedB1_rf, newdata = testmodel_B1, type = "raw")
confusionMatrix(data = balancedB1_testrf, reference = testmodel_B1$DIQ010B, mode="prec_recall")

balancedB1_rf
varImp(balancedB1_rf)

varImp(balancedB1_rf) %>%
  ggplot(aes(x = reorder(Variable, importance), y = importance))+ geom_col() +
  coord_flip() + labs(title = "Relative Importance of Variables for Random Forest", x = 'Variable', y = 'Relative Importance')


#################################################
#   Tree Extreme Gradient Boosting (model B1)
#################################################

#try on imbalanced and compare accuracy
B1_xgbtrees = train(DIQ010B~.,
                    data = trainmodel_B1,
                    method = "xgbTree",
                    trControl = trainControl(method = "cv",
                                             number = 10,
                                             classProbs = FALSE,
                                             savePredictions = "final"))

print(B1_xgbtrees)

# Best tuning parameter
B1_xgbtrees$bestTune
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 94      50         3 0.4     0              0.6                1      0.75

# test model on trainset and check accuracy with confusion matrix.
B1_trainxgbtree = predict(B1_xgbtrees, trainmodel_B1, type = "raw")
confusionMatrix(B1_trainxgbtree, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1_testxgbtree = predict(B1_xgbtrees, testmodel_B1, type = "raw")
confusionMatrix(B1_testxgbtree, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(B1_xgbtrees)

##########################################################################################################################################
# Fit the model on the balanced set
set.seed(500)
balB1_xgbtrees = train(DIQ010B~.,
                 data = balancedtrain_B1,
                 method = "xgbTree",
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          classProbs = FALSE,
                                          savePredictions = "final"))

print(balB1_xgbtrees)

# Best tuning parameter
balB1_xgbtrees$bestTune
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
#104     100         3 0.4     0              0.8                1      0.75

# test model on trainset and check accuracy with confusion matrix.
balB1_trainxgbtree = predict(balB1_xgbtrees, balancedtrain_B1, type = "raw")
confusionMatrix(balB1_trainxgbtree, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB1_testxgbtree = predict(balB1_xgbtrees, testmodel_B1, type = "raw")
confusionMatrix(balB1_testxgbtree, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(balB1_xgbtrees)

varimp_tree %>%
  ggplot(aes(x = reorder(Variable, importance), y = importance))+ geom_col() +
  coord_flip() + labs(title = "Relative Importance of Variables for Decision Tree", x = 'Variable', y = 'Relative Importance')

##################################################
#  Stochastic Gradient Boosted Trees (model B1)
##################################################
library(gbm)

#try on imbalanced and compare accuracy

trainControl <- trainControl(method="cv", number=10)

set.seed(500)
B1.gbm.caret <- train(DIQ010B ~ .
                   , data=trainmodel_B1
                   #, distribution="gaussian"
                   , method="gbm"
                   , trControl=trainControl
                   , verbose=FALSE
                   #, tuneGrid=caretGrid
                   #, metric="Accuracy"
                   # , bag.fraction=0.75
)

print(B1.gbm.caret)

# test model on trainset and check accuracy with confusion matrix.
B1_train_gbm <- predict(B1.gbm.caret, newdata=trainmodel_B1, type="raw")
confusionMatrix(B1_train_gbm, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1_test_gbm <- predict(B1.gbm.caret, newdata=testmodel_B1, type="raw")
confusionMatrix(B1_test_gbm, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(B1.gbm.caret)

##########################################################################################################################################
# Fit the model on the balanced set
set.seed(500)
balB1.gbm <- train(DIQ010B ~ .
                      , data=balancedtrain_B1
                      #, distribution="gaussian"
                      , method="gbm"
                      , trControl=trainControl
                      , verbose=FALSE
                      #, tuneGrid=caretGrid
                      #, metric="Accuracy"
                      #, bag.fraction=0.75
)

print(balB1.gbm)

# test model on trainset and check accuracy with confusion matrix.
balB1train_gbm <- predict(balB1.gbm, newdata=balancedtrain_B1, type="raw")
confusionMatrix(balB1train_gbm, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB1test_gbm <- predict(balB1.gbm, newdata=testmodel_B1, type="raw")
confusionMatrix(balB1test_gbm, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(balB1.gbm)

varImp(balB1.gbm)%>%
  ggplot(aes(x = reorder(Variable, importance), y = importance))+ geom_col() +
  coord_flip() + labs(title = "Relative Importance of Variables for Decision Tree", x = 'Variable', y = 'Relative Importance')

################################
#  AdaBoost Trees (model B1)
################################

# library(fastAdaboost)
# #try on imbalanced and compare accuracy
# 
# set.seed(500)
# trainControl <- trainControl(method="cv", number=10)
# Grid <- expand.grid(maxdepth=25,nu=2,iter=100)
# 
# B1_adaboost = train(DIQ010B ~., 
#                        data= trainmodel_B1, 
#                        method="ada", 
#                        trControl= trainControl,
#                        tuneGrid=Grid
# )
# # error
# # Something is wrong; all the Accuracy metric values are missing
# 
# print(B1_adaboost)
# 
# # test model on trainset and check accuracy with confusion matrix.
# B1_train_ada <- predict(B1_adaboost, newdata=trainmodel_B1, type="raw")
# confusionMatrix(B1_train_ada, reference = trainmodel_B1$DIQ010B, mode="prec_recall")
# 
# # Perform prediction on testset and look at confusion matrix.
# B1_test_ada <- predict(B1_adaboost, newdata=testmodel_B1, type="raw")
# confusionMatrix(B1_test_ada, reference = testmodel_B1$DIQ010B, mode="prec_recall")
# 
# varImp(B1_adaboost)
# 
# ##########################################################################################################################################
# # Fit the model on the balanced set
# set.seed(500)
# trainControl <- trainControl(method="cv", number=10)
# Grid <- expand.grid(maxdepth=25,nu=2,iter=100)
# 
# balB1_adaboost = train(DIQ010B ~., 
#                           data= balancedtrain_B1, 
#                           method="ada", 
#                           trControl= trainControl,
#                           tuneGrid=Grid
# )
# 
# 
# print(balB1_adaboost)
# 
# # test model on trainset and check accuracy with confusion matrix.
# balB1train_ada <- predict(balB1_adaboost, newdata=balancedtrain_B1, type="raw")
# confusionMatrix(balB1train_ada, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")
# # accuracy of trainset is 96.17%
# 
# # Perform prediction on testset and look at confusion matrix.
# balB1test_ada <- predict(balB1_adaboost, newdata=testmodel_B1, type="raw")
# confusionMatrix(balB1test_ada, reference = testmodel_B1$DIQ010B, mode="prec_recall")
# 
# varImp(balB1_adaboost)


###################################
#   Neural Networks (model B1)
###################################

library(neuralnet)
#try on imbalanced and compare accuracy
set.seed(500)
st = Sys.time()
B1.nnets <- train(DIQ010B~., data = trainmodel_B1, method='nnet', trace = FALSE,
               #Grid of tuning parameters to try:
               tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))
Sys.time() - st

# show neural network result
print(B1.nnets)
B1.nnets[["finalModel"]] #a 73-1-4 network with 82 weights
plot(B1.nnets)

# test model on training set
predictNN_B1 <- predict(B1.nnets, trainmodel_B1, type = "raw")
confusionMatrix(data = predictNN_B1, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# test model on test set
predictNN_B1_test <- predict(B1.nnets, testmodel_B1, type = "raw")
confusionMatrix(data = predictNN_B1_test, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# show relative importance
varImp(B1.nnets)

##########################################################################################################################################
# Fit the model on the balanced set
set.seed(500)
st = Sys.time()
balB1.nnets <- train(DIQ010B~., data = balancedtrain_B1, method='nnet', trace = FALSE,
                  #Grid of tuning parameters to try:
                  tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))
Sys.time() - st

# show neural network result
print(balB1.nnets)
balB1.nnets[["finalModel"]] #a 73-9-4 network with 706 weights
plot(balB1.nnets)

# test model on training set
predictNN_balB1 <- predict(balB1.nnets, balancedtrain_B1, type = "raw")
confusionMatrix(data = predictNN_balB1, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# test model on test set
predictNN_balB1_test <- predict(balB1.nnets, testmodel_B1, type = "raw")
confusionMatrix(data = predictNN_balB1_test, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# show relative importance
varImp(balB1.nnets)

varImp(B1.nnets) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")

#######################
#   SVM (model B1)
#######################
#for 1-1 or 1vsrest
#try on imbalanced and compare accuracy
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(500)

B1_svmLinear <- train(DIQ010B ~., data = trainmodel_B1, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

#check the result of our train() method
print(B1_svmLinear)

# test model on training set
B1_svmtrain <- predict(B1_svmLinear, trainmodel_B1, type = "raw")
confusionMatrix(data = B1_svmtrain, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# test model on test set
B1_svmtest <- predict(B1_svmLinear, testmodel_B1, type = "raw")
confusionMatrix(data = B1_svmtest, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# show relative importance
varImp(B1_svmLinear)



##########################################################################################################################################
# Fit the model on the balanced set
set.seed(500)

balB1_svmLinear <- train(DIQ010B ~., data = balancedtrain_B1, method = "svmLinear",
                       trControl=trctrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)

#check the result of our train() method
print(balB1_svmLinear)

# test model on training set
balB1_svmtrain <- predict(balB1_svmLinear, balancedtrain_B1, type = "raw")
confusionMatrix(data = balB1_svmtrain, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# test model on test set
balB1_svmtest <- predict(balB1_svmLinear, testmodel_B1, type = "raw")
confusionMatrix(data = balB1_svmtest, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# show relative importance
varImp(balB1_svmLinear)


varImp(B1_svm_Linear) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")


#####SVM grid

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(500)
B1_svmGrid <- train(DIQ010B ~., data = trainmodel_B1, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)


#check the result of our train() method
B1_svmGrid

# test model on training set
B1_svmtrainGrid <- predict(B1_svmGrid, trainmodel_B1, type = "raw")
confusionMatrix(data = B1_svmtrainGrid, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# test model on test set
B1_svmtestGrid <- predict(B1_svmGrid, testmodel_B1, type = "raw")
confusionMatrix(data = B1_svmtestGrid, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# show relative importance
varImp(B1_svmGrid)
plot(B1_svmGrid)

##########################################################################################################################################
# Fit the model on the balanced set

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(500)
balB1_svmGrid <- train(DIQ010B ~., data = balancedtrain_B1, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)


#check the result of our train() method
balB1_svmGrid

# test model on training set
balB1_trainGrid <- predict(balB1_svmGrid, balancedtrain_B1, type = "raw")
confusionMatrix(data = balB1_trainGrid, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# test model on test set
balB1_testGrid <- predict(balB1_svmGrid, testmodel_B1, type = "raw")
confusionMatrix(data = balB1_testGrid, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# show relative importance
varImp(balB1_svmGrid)
plot(balB1_svmGrid)



varImp(B1_svmLinear_Grid) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")

#####SVM Radial

svmR <- train(DIQ010B ~., data = balancedtrain_B1, method = "svmRadial",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 10
)

# Print the best tuning parameter sigma and C that
# maximizes model accuracy
svmR$bestTune

# test model on test set
balB1_svmR <- predict(svmR, testmodel_B1, type = "raw")
confusionMatrix(data = balB1_svmR, reference = testmodel_B1$DIQ010B, mode="prec_recall")

#####SVM Poly

svmP <- train(DIQ010B ~., data = balancedtrain_B1, method = "svmPoly",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 4
)

# Print the best tuning parameter sigma and C that
# maximizes model accuracy
svmP$bestTune

# test model on test set
balB1_svmP <- predict(svmP, testmodel_B1, type = "raw")
confusionMatrix(data = balB1_svmP, reference = testmodel_B1$DIQ010B, mode="prec_recall")

foreval_svm = cbind(DIQ010B = testmodel_B1$DIQ010B,
                    pred_svmr = balB1_svmR,
                    pred_svmp = balB1_svmP)

write.csv(foreval_svm, "foreval_svm.csv", row.names = F)

foreval_svm = read.csv("foreval_svm.csv", as.is = TRUE)


balROC_svmr = multiclass.roc(foreval_svm$pred_svmr, foreval_svm$DIQ010B)
auc(balROC_svmr)
balrs_svmr <- balROC_svmr[['rocs']]
plot.roc(balrs_svmr[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_svmr[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_svmr[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_svmr[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_svmr[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_svmr[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balpr_svmr <- pr.curve(scores.class0 = foreval_svm$pred_svmr, scores.class1 = foreval_svm$DIQ010B, curve=T)
print(balpr_svmr)
plot(balpr_svmr, legend = FALSE)

balROC_svmp = multiclass.roc(foreval_svm$pred_svmp, foreval_svm$DIQ010B)
auc(balROC_svmp)
balrs_svmp <- balROC_svmp[['rocs']]
plot.roc(balrs_svmp[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_svmp[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_svmp[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_svmp[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_svmp[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_svmp[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balpr_svmp <- pr.curve(scores.class0 = foreval_svm$pred_svmp, scores.class1 = foreval_svm$DIQ010B, curve=T)
print(balpr_svmp)
plot(balpr_svmp, legend = FALSE)


###################################
#   nb/Naive Bayes (model B1)
###################################

library(klaR)
#try on imbalanced and compare accuracy
# train model
set.seed(500)
# set up 10-fold cross validation procedure
trainControl <- trainControl(method="cv", number=10)

# set up tuning grid
search_grid <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))


# train model with(out) search grid
B1.nb <- train(DIQ010B ~., 
                  data = trainmodel_B1,
                  method = "nb",
                  trControl = trainControl,
                  tuneGrid = search_grid,
                  #preProc = c("BoxCox", "center", "scale", "pca")
)

print(B1.nb)

# top 5 models
B1.nb$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# test model on trainset and check accuracy with confusion matrix.
B1train_nb <- predict(B1.nb, newdata=trainmodel_B1, type="raw")
confusionMatrix(B1train_nb, reference = trainmodel_B1$DIQ010B, mode="prec_recall")


# Perform prediction on testset and look at confusion matrix.
B1test_nb <- predict(B1.nb, newdata=testmodel_B1, type="raw")
confusionMatrix(B1test_nb, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(B1.nb)

##########################################################################################################################################
# Fit the model on the balanced set

# train model
set.seed(500)
# set up 10-fold cross validation procedure
trainControl <- trainControl(method="cv", number=10)

# set up tuning grid
search_grid <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))


# train model with(out) search grid
balB1.nb <- train(DIQ010B ~., 
                     data = balancedtrain_B1,
                     method = "nb",
                     trControl = trainControl,
                     tuneGrid = search_grid,
                     #preProc = c("BoxCox", "center", "scale", "pca")
)

print(balB1.nb)

# top 5 models
balB1.nb$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# test model on trainset and check accuracy with confusion matrix.
balB1train_nb <- predict(balB1.nb, newdata=balancedtrain_B1, type="raw")
confusionMatrix(balB1train_nb, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB1test_nb <- predict(balB1.nb, newdata=testmodel_B1, type="raw")
confusionMatrix(balB1test_nb, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(balB1.nb)

###########################################
#   naive_bayes/Naive Bayes (model B1)
###########################################

library(naivebayes)
#try on imbalanced and compare accuracy
# train model
set.seed(500)
# set up 10-fold cross validation procedure
trainControl <- trainControl(method="cv", number=10)


# train model
B1.naivebayes <- train(DIQ010B ~.,
                    data = trainmodel_B1,
                    method = "naive_bayes",
                    trControl = trainControl,
                    #preProc = c("center", "scale")
)
print(B1.naivebayes)

# test model on trainset and check accuracy with confusion matrix.
B1train_naivebayes <- predict(B1.naivebayes, newdata=trainmodel_B1, type="raw")
confusionMatrix(B1train_naivebayes, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1test_naivebayes <- predict(B1.naivebayes, newdata=testmodel_B1, type="raw")
confusionMatrix(B1test_naivebayes, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(B1.naivebayes)

##########################################################################################################################################
# Fit the model on the balanced set

# train model
set.seed(500)
# set up 10-fold cross validation procedure
trainControl <- trainControl(method="cv", number=10)


# train model
balB1.naivebayes <- train(DIQ010B ~.,
                       data = balancedtrain_B1,
                       method = "naive_bayes",
                       trControl = trainControl,
                       #preProc = c("center", "scale")
)
print(balB1.naivebayes)

# test model on trainset and check accuracy with confusion matrix.
balB1train_naivebayes <- predict(balB1.naivebayes, newdata=balancedtrain_B1, type="raw")
confusionMatrix(balB1train_naivebayes, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB1test_naivebayes <- predict(balB1.naivebayes, newdata=testmodel_B1, type="raw")
confusionMatrix(balB1test_naivebayes, reference = testmodel_B1$DIQ010B, mode="prec_recall")

varImp(balB1.naivebayes)


##########################
#    k-NN (model B1)
##########################
#try on imbalanced and compare accuracy
# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_B1 <- train(DIQ010B~., data=trainmodel_B1, method="kknn", preProcess="scale", trControl=control1)

print(knn_B1)

# test model on trainset and check accuracy with confusion matrix.
B1train_knn <- predict(knn_B1, newdata=trainmodel_B1, type="raw")
confusionMatrix(B1train_knn, reference = trainmodel_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1test_knn <- predict(knn_B1, newdata=testmodel_B1, type="raw")
confusionMatrix(B1test_knn, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# estimate variable importance
importance_knn <- varImp(knn_B1, scale=FALSE)

# summarize importance
print(importance_knn)



##########################################################################################################################################
# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_balB1 <- train(DIQ010B~., data=balancedtrain_B1, method="kknn", preProcess="scale", trControl=control1)

print(knn_balB1)

# test model on trainset and check accuracy with confusion matrix.
balB1train_knn <- predict(knn_balB1, newdata=balancedtrain_B1, type="raw")
confusionMatrix(balB1train_knn, reference = balancedtrain_B1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB1test_knn <- predict(knn_balB1, newdata=testmodel_B1, type="raw")
confusionMatrix(balB1test_knn, reference = testmodel_B1$DIQ010B, mode="prec_recall")

# estimate variable importance
balimportance_knn <- varImp(knn_balB1, scale=FALSE)

# summarize importance
print(balimportance_knn)

varImp(knn_balB1) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")


#########See AUC R script for PR and ROC evaluation#########
