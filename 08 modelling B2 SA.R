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

############################
#    DIQ010B (train set)
############################

trainmodel_B2 = subset(train_model3, select = c(-DIQ010A))
saveRDS(trainmodel_B2, file = "trainmodel_B2.RDS")

#to read it

trainmodel_B2 = readRDS("trainmodel_B2.RDS")
summary(trainmodel_B2)

############################
#    DIQ010B (test set)
############################

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

################################
#   Random Forest (model B2)
################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  balancedB2_rf = train(DIQ010B~.,
                        data = balancedtrain_B2,
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

print(balancedB2_rf)

# Best tuning parameter
balancedB2_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
balancedB2_trainrf = predict(balancedB2_rf, balancedtrain_B2, type = "raw")
confusionMatrix(balancedB2_trainrf, reference = balancedtrain_B2$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balancedB2_testrf <- predict(balancedB2_rf, newdata = testmodel_B2, type = "raw")
confusionMatrix(data = balancedB2_testrf, reference = testmodel_B2$DIQ010B, mode="prec_recall")

varImp(balancedB2_rf)



##################################################
#  Stochastic Gradient Boosted Trees (model B2)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
balB2.gbm <- train(DIQ010B ~ .
                   , data=balancedtrain_B2
                   #, distribution="gaussian"
                   , method="gbm"
                   , trControl=trainControl
                   , verbose=FALSE
                   #, tuneGrid=caretGrid
                   #, metric="Accuracy"
                   #, bag.fraction=0.75
)

print(balB2.gbm)

# test model on trainset and check accuracy with confusion matrix.
balB2train_gbm <- predict(balB2.gbm, newdata=balancedtrain_B2, type="raw")
confusionMatrix(balB2train_gbm, reference = balancedtrain_B2$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB2test_gbm <- predict(balB2.gbm, newdata=testmodel_B2, type="raw")
confusionMatrix(balB2test_gbm, reference = testmodel_B2$DIQ010B, mode="prec_recall")

varImp(balB2.gbm)



##########################
#    k-NN (model B2)
##########################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_balB2 <- train(DIQ010B~., data=balancedtrain_B2, method="kknn", preProcess="scale", trControl=control1)

print(knn_balB2)

# test model on trainset and check accuracy with confusion matrix.
balB2train_knn <- predict(knn_balB2, newdata=balancedtrain_B2, type="raw")
confusionMatrix(balB2train_knn, reference = balancedtrain_B2$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB2test_knn <- predict(knn_balB2, newdata=testmodel_B2, type="raw")
confusionMatrix(balB2test_knn, reference = testmodel_B2$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_balB2, scale=FALSE)

######################################
#    PR/ROC-AUC evaluation (B2)
######################################


foreval_b2 = cbind(DIQ010B = testmodel_B2$DIQ010B,
                    pred_rf = balancedB2_testrf,
                    pred_gbm = balB2test_gbm,
                    pred_knn = balB2test_knn)

write.csv(foreval_b2, "foreval_b2.csv", row.names = F)

foreval_b2 = read.csv("foreval_b2.csv", as.is = TRUE)

b2ROC_rf = multiclass.roc(foreval_b2$pred_rf, foreval_b2$DIQ010B)
auc(b2ROC_rf)

b2ROC_knn = multiclass.roc(foreval_b2$pred_knn, foreval_b2$DIQ010B)
auc(b2ROC_knn)

b2ROC_gbm = multiclass.roc(foreval_b2$pred_gbm, foreval_b2$DIQ010B)
auc(b2ROC_gbm)


################################
#  Random Forest (model B234)
################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  balancedB234_rf = train(DIQ010B~.,
                        data = balancedtrain_B234,
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

print(balancedB234_rf)

# Best tuning parameter
balancedB234_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
balancedB234_trainrf = predict(balancedB234_rf, balancedtrain_B234, type = "raw")
confusionMatrix(balancedB234_trainrf, reference = balancedtrain_B234$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balancedB234_testrf <- predict(balancedB234_rf, newdata = testmodel_B234, type = "raw")
confusionMatrix(data = balancedB234_testrf, reference = testmodel_B234$DIQ010B, mode="prec_recall")

varImp(balancedB234_rf)



##################################################
#  Stochastic Gradient Boosted Trees (model B234)
##################################################

# Fit the model on the balanced set
set.seed(500)
balB234.gbm <- train(DIQ010B ~ .
                   , data=balancedtrain_B234
                   #, distribution="gaussian"
                   , method="gbm"
                   , trControl=trainControl
                   , verbose=FALSE
                   #, tuneGrid=caretGrid
                   #, metric="Accuracy"
                   # , bag.fraction=0.75
)

print(balB234.gbm)

# test model on trainset and check accuracy with confusion matrix.
balB234train_gbm <- predict(balB234.gbm, newdata=balancedtrain_B234, type="raw")
confusionMatrix(balB234train_gbm, reference = balancedtrain_B234$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB234test_gbm <- predict(balB234.gbm, newdata=testmodel_B234, type="raw")
confusionMatrix(balB234test_gbm, reference = testmodel_B234$DIQ010B, mode="prec_recall")

varImp(balB234.gbm)


##########################
#    k-NN (model B234)
##########################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_balB234 <- train(DIQ010B~., data=balancedtrain_B234, method="kknn", preProcess="scale", trControl=control1)

print(knn_balB234)

# test model on trainset and check accuracy with confusion matrix.
balB234train_knn <- predict(knn_balB234, newdata=balancedtrain_B234, type="raw")
confusionMatrix(balB234train_knn, reference = balancedtrain_B234$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
balB234test_knn <- predict(knn_balB234, newdata=testmodel_B234, type="raw")
confusionMatrix(balB234test_knn, reference = testmodel_B234$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_balB234, scale=FALSE)

######################################
# PR/ROC-AUC evaluation (B234)
######################################


foreval_b234 = cbind(DIQ010B = testmodel_B234$DIQ010B,
                   pred_rf = balancedB234_testrf,
                   pred_gbm = balB234test_gbm,
                   pred_knn = balB234test_knn)

write.csv(foreval_b234, "foreval_b234.csv", row.names = F)

foreval_b234 = read.csv("foreval_b234.csv", as.is = TRUE)

b234ROC_rf = multiclass.roc(foreval_b234$pred_rf, foreval_b234$DIQ010B)
auc(b234ROC_rf)

b234ROC_knn = multiclass.roc(foreval_b234$pred_knn, foreval_b234$DIQ010B)
auc(b234ROC_knn)

b234ROC_gbm = multiclass.roc(foreval_b234$pred_gbm, foreval_b234$DIQ010B)
auc(b234ROC_gbm)

#################################################################################################################################################
#testing One vs Rest classifier using the top 3 models of binary classification

##################################
#   LogitBoost (model B1a)
##################################

#higher recall, lower accuracy
cctrlR_B1a <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.caretB1a <- train(DIQ010Ba~ .,
                            data=trainmodel_B1a, 
                            method = "LogitBoost", 
                            trControl = cctrlR_B1a,
                            tuneLength = 4)

print(logitboost.caretB1a)

# test model on trainset and check accuracy with confusion matrix.
B1a_train_lb <- predict(logitboost.caretB1a, newdata = trainmodel_B1a, type = "raw")

#confusion matrix
confusionMatrix(B1a_train_lb, trainmodel_B1a$DIQ010Ba, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1a_test_lb <- predict(logitboost.caretB1a, newdata = testmodel_B1a, type = "raw")

# confusion matrix
confusionMatrix(B1a_test_lb, testmodel_B1a$DIQ010Ba, mode="prec_recall")

varImp(logitboost.caretB1a)

################################
#   Random Forest (model B1a)
################################
library(randomForest)

#####using caret#####

#mtry: Number of variables available for splitting at each tree node. In the random forests literature, this is referred to as the mtry parameter.
# ntrees: Number of trees to grow. For small datasets, 50 trees may be sufficient. For larger datasets, 500 or more may be required

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtreesB1a = train(DIQ010Ba~.,
                        data = trainmodel_B1a,
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

print(rf_maxtreesB1a)

# Best tuning parameter
rf_maxtreesB1a$bestTune

# Test model on trainset and check accuracy with confusion matrix.
B1atrain_rf <- predict(rf_maxtreesB1a, newdata = trainmodel_B1a, type = "raw")
confusionMatrix(data = B1atrain_rf, reference = trainmodel_B1a$DIQ010Ba, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1atest_rf <- predict(rf_maxtreesB1a, newdata = testmodel_B1a, type = "raw")
confusionMatrix(data = B1atest_rf, reference = testmodel_B1a$DIQ010Ba
                #, mode="prec_recall"
                )


varImp(rf_maxtreesB1a)


#################################################
# Tree Extreme Gradient Boosting (model B1a)
#################################################

# Fit the model on the training set
set.seed(500)
xgbtreesB1a = train(DIQ010Ba~.,
                   data = trainmodel_B1a,
                   method = "xgbTree",
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            classProbs = FALSE,
                                            savePredictions = "final"))

print(xgbtreesB1a)

# Best tuning parameter
xgbtreesB1a$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 28      50         2 0.3     0              0.8                1       0.5

# test model on trainset and check accuracy with confusion matrix.
B1a_trainxgbtree = predict(xgbtreesB1a, trainmodel_B1a, type = "raw")
confusionMatrix(B1a_trainxgbtree, reference = trainmodel_B1a$DIQ010Ba, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
B1a_testxgbtree = predict(xgbtreesB1a, testmodel_B1a, type = "raw")
confusionMatrix(B1a_testxgbtree, reference = testmodel_B1a$DIQ010Ba, mode="prec_recall")

varImp(xgbtreesB1a)

#######################
#   SVM (model B1a)
#######################
#for 1-1 or 1vsrest
# Fit the model on the balanced set
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(500)
B1a_svmGrid <- train(DIQ010Ba ~., data = trainmodel_B1a, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)


#check the result of our train() method
B1a_svmGrid

# test model on training set
B1a_trainGrid <- predict(B1a_svmGrid, trainmodel_B1a, type = "raw")
confusionMatrix(data = B1a_trainGrid, reference = trainmodel_B1a$DIQ010Ba, mode="prec_recall")

# test model on test set
B1a_testGrid <- predict(B1a_svmGrid, testmodel_B1a, type = "raw")
confusionMatrix(data = B1a_testGrid, reference = testmodel_B1a$DIQ010Ba, mode="prec_recall")

# show relative importance
varImp(B1a_svmGrid)
plot(B1a_svmGrid)



varImp(B1_svmLinear_Grid) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_b1a = cbind(DIQ010Ba = testmodel_B1a$DIQ010Ba,
                    pred_lb = B1a_test_lb,
                    pred_rf = B1atest_rf,
                    pred_xgb = B1a_testxgbtree,
                    pre_svm = B1a_testGrid)

write.csv(foreval_b1a, "foreval_b1a.csv", row.names = F)

foreval_b1a = read.csv("foreval_b1a.csv", as.is = TRUE)


b1aROC_lb = roc(foreval_b1a$pred_lb, foreval_b1a$DIQ010Ba)
auc(b1aROC_lb)

b1aROC_rf = roc(foreval_b1a$pred_rf, foreval_b1a$DIQ010Ba)
auc(sa6ROC_rf)

b1aROC_xgb = roc(foreval_b1a$pred_xgb, foreval_b1a$DIQ010Ba)
auc(b1aROC_xgb)

b1aROC_svm = roc(foreval_b1a$pre_svm, foreval_b1a$DIQ010Ba)
auc(b1aROC_svm)


#############################################
#  Sensitivity analysis - Physical Activity
#############################################
#includes both DIQ010A and DIQ010B
#Physical Activity variables
#to read it

train_sa1 = readRDS("train_sa1.RDS")

b1_balancedtrain_sa1 <- SmoteClassif(DIQ010B ~ ., train_sa1, C.perc = "balance", k = 5, dist = "HVDM")
print(table(b1_balancedtrain_sa1$DIQ010B))
b1_balancedtrain_sa1 = subset(b1_balancedtrain_sa1, select = c(-DIQ010A))

saveRDS(b1_balancedtrain_sa1, file = "b1_balancedtrain_sa1.RDS")

#to read it

b1_balancedtrain_sa1 = readRDS("b1_balancedtrain_sa1.RDS")

#to read it

test_sa1 = readRDS("test_sa1.RDS")
b_test_sa1 = subset(test_sa1, select = c(-DIQ010A))
saveRDS(b_test_sa1, file = "b_test_sa1.RDS")

#to read it

b_test_sa1 = readRDS("b_test_sa1.RDS")

##################################################
#   Stochastic Gradient Boosted Trees (sa1)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
sa1.gbm <- train(DIQ010B ~ .
                   , data=b1_balancedtrain_sa1
                   #, distribution="gaussian"
                   , method="gbm"
                   , trControl=trainControl
                   , verbose=FALSE
                   #, tuneGrid=caretGrid
                   #, metric="Accuracy"
                   #, bag.fraction=0.75
)

print(sa1.gbm)

# test model on trainset and check accuracy with confusion matrix.
sa1train_gbm <- predict(sa1.gbm, newdata=b1_balancedtrain_sa1, type="raw")
confusionMatrix(sa1train_gbm, reference = b1_balancedtrain_sa1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa1test_gbm <- predict(sa1.gbm, newdata=b_test_sa1, type="raw")
confusionMatrix(sa1test_gbm, reference = b_test_sa1$DIQ010B, mode="prec_recall")

varImp(sa1.gbm)

##########################
#  Random Forest (sa1)
##########################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  sa1_rf = train(DIQ010B~.,
                          data = b1_balancedtrain_sa1,
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

print(sa1_rf)

# Best tuning parameter
sa1_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
sa1_trainrf = predict(sa1_rf, b1_balancedtrain_sa1, type = "raw")
confusionMatrix(sa1_trainrf, reference = b1_balancedtrain_sa1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa1_testrf <- predict(sa1_rf, newdata = b_test_sa1, type = "raw")
confusionMatrix(data = sa1_testrf, reference = b_test_sa1$DIQ010B, mode="prec_recall")

varImp(sa1_rf)

####################
#    k-NN (sa1)
####################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_sa1 <- train(DIQ010B~., data=b1_balancedtrain_sa1, method="kknn", preProcess="scale", trControl=control1)

print(knn_sa1)

# test model on trainset and check accuracy with confusion matrix.
sa1train_knn <- predict(knn_sa1, newdata=b1_balancedtrain_sa1, type="raw")
confusionMatrix(sa1train_knn, reference = b1_balancedtrain_sa1$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa1test_knn <- predict(knn_sa1, newdata=b_test_sa1, type="raw")
confusionMatrix(sa1test_knn, reference = b_test_sa1$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_sa1, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bsa1 = cbind(DIQ010B = b_test_sa1$DIQ010B,
                    pred_gbm = sa1test_gbm,
                    pred_rf = sa1_testrf,
                    pre_knn = sa1test_knn)

write.csv(foreval_bsa1, "foreval_bsa1.csv", row.names = F)

foreval_bsa1 = read.csv("foreval_bsa1.csv", as.is = TRUE)


bsa1ROC_gbm = multiclass.roc(foreval_bsa1$pred_gbm, foreval_bsa1$DIQ010B)
auc(bsa1ROC_gbm)

bsa1ROC_rf = multiclass.roc(foreval_bsa1$pred_rf, foreval_bsa1$DIQ010B)
auc(bsa1ROC_rf)

bsa1ROC_knn = multiclass.roc(foreval_bsa1$pre_knn, foreval_bsa1$DIQ010B)
auc(bsa1ROC_knn)

######################################
#  Sensitivity analysis - Smoking
######################################

#Smoking variable - SMQ915A
#to read it

train_sa2 = readRDS("train_sa2.RDS")

b1_balancedtrain_sa2 <- SmoteClassif(DIQ010B ~ ., train_sa2, C.perc = "balance", k = 5, dist = "HVDM")
b1_balancedtrain_sa2 = subset(b1_balancedtrain_sa2, select = c(-DIQ010A))

saveRDS(b1_balancedtrain_sa2, file = "b1_balancedtrain_sa2.RDS")

#to read it

b1_balancedtrain_sa2 = readRDS("b1_balancedtrain_sa2.RDS")

#to read it

test_sa2 = readRDS("test_sa2.RDS")
b_test_sa2 = subset(test_sa2, select = c(-DIQ010A))
saveRDS(b_test_sa2, file = "b_test_sa2.RDS")

#to read it

b_test_sa2 = readRDS("b_test_sa2.RDS")

##################################################
#   Stochastic Gradient Boosted Trees (sa2)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
sa2.gbm <- train(DIQ010B ~ .
                 , data=b1_balancedtrain_sa2
                 #, distribution="gaussian"
                 , method="gbm"
                 , trControl=trainControl
                 , verbose=FALSE
                 #, tuneGrid=caretGrid
                 #, metric="Accuracy"
                 #, bag.fraction=0.75
)

print(sa2.gbm)

# test model on trainset and check accuracy with confusion matrix.
sa2train_gbm <- predict(sa2.gbm, newdata=b1_balancedtrain_sa2, type="raw")
confusionMatrix(sa2train_gbm, reference = b1_balancedtrain_sa2$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa2test_gbm <- predict(sa2.gbm, newdata=b_test_sa2, type="raw")
confusionMatrix(sa2test_gbm, reference = b_test_sa2$DIQ010B, mode="prec_recall")

varImp(sa2.gbm)

##########################
#  Random Forest (sa2)
##########################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  sa2_rf = train(DIQ010B~.,
                 data = b1_balancedtrain_sa2,
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

print(sa2_rf)

# Best tuning parameter
sa2_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
sa2_trainrf = predict(sa2_rf, b1_balancedtrain_sa2, type = "raw")
confusionMatrix(sa2_trainrf, reference = b1_balancedtrain_sa2$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa2_testrf <- predict(sa2_rf, newdata = b_test_sa2, type = "raw")
confusionMatrix(data = sa2_testrf, reference = b_test_sa2$DIQ010B, mode="prec_recall")

varImp(sa2_rf)

####################
#    k-NN (sa2)
####################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_sa2 <- train(DIQ010B~., data=b1_balancedtrain_sa2, method="kknn", preProcess="scale", trControl=control1)

print(knn_sa2)

# test model on trainset and check accuracy with confusion matrix.
sa2train_knn <- predict(knn_sa2, newdata=b1_balancedtrain_sa2, type="raw")
confusionMatrix(sa2train_knn, reference = b1_balancedtrain_sa2$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa2test_knn <- predict(knn_sa2, newdata=b_test_sa2, type="raw")
confusionMatrix(sa2test_knn, reference = b_test_sa2$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_sa2, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bsa2 = cbind(DIQ010B = b_test_sa2$DIQ010B,
                     pred_gbm = sa2test_gbm,
                     pred_rf = sa2_testrf,
                     pred_knn = sa2test_knn)

write.csv(foreval_bsa2, "foreval_bsa2.csv", row.names = F)

foreval_bsa2 = read.csv("foreval_bsa2.csv", as.is = TRUE)


bsa2ROC_gbm = multiclass.roc(foreval_bsa2$pred_gbm, foreval_bsa2$DIQ010B)
auc(bsa2ROC_gbm)

bsa2ROC_rf = multiclass.roc(foreval_bsa2$pred_rf, foreval_bsa2$DIQ010B)
auc(bsa2ROC_rf)

bsa2ROC_knn = multiclass.roc(foreval_bsa2$pred_knn, foreval_bsa2$DIQ010B)
auc(bsa2ROC_knn)

######################################
# Sensitivity analysis - Cholesterol
######################################

#Cholesterol variables - LBXTCA, LBDHDDA, LBXTRA, LBDLDLA
#to read it

train_sa3 = readRDS("train_sa3.RDS")
b1_balancedtrain_sa3 <- SmoteClassif(DIQ010B ~ ., train_sa3, C.perc = "balance", k = 5, dist = "HVDM")
b1_balancedtrain_sa3 = subset(b1_balancedtrain_sa3, select = c(-DIQ010A))

saveRDS(b1_balancedtrain_sa3, file = "b1_balancedtrain_sa3.RDS")

#to read it

b1_balancedtrain_sa3 = readRDS("b1_balancedtrain_sa3.RDS")

#to read it

test_sa3 = readRDS("test_sa3.RDS")
b_test_sa3 = subset(test_sa3, select = c(-DIQ010A))
saveRDS(b_test_sa3, file = "b_test_sa3.RDS")

#to read it

b_test_sa3 = readRDS("b_test_sa3.RDS")

#######################################################################################################################################
#remove Cholesterol variables - LBXTC, LBDHDD, LBXTR, LBDLDL
balancedtrain_B1_tc = subset(balancedtrain_B1, select = c(-LBXTC, -LBDHDD, -LBXTR, -LBDLDL))

saveRDS(balancedtrain_B1_tc, file = "balancedtrain_B1_tc.RDS")

#to read it

balancedtrain_B1_tc = readRDS("balancedtrain_B1_tc.RDS")

testmodel_B1_tc = subset(testmodel_B1, select = c(-LBXTC, -LBDHDD, -LBXTR, -LBDLDL))

saveRDS(testmodel_B1_tc, file = "testmodel_B1_tc.RDS")

#to read it

testmodel_B1_tc = readRDS("testmodel_B1_tc.RDS")

####################################################
#   Random Forest (model B1 exc Cholesterol)
####################################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rfB1_maxtrees_tc = train(DIQ010B~.,
                         data = balancedtrain_B1_tc,
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

print(rfB1_maxtrees_tc)

# Test model on trainset and check accuracy with confusion matrix.
B1_tc_train_rf <- predict(rfB1_maxtrees_tc, newdata = balancedtrain_B1_tc, type = "raw")
confusionMatrix(data = B1_tc_train_rf, reference = balancedtrain_B1_tc$DIQ010B
                , mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
B1_tc_test_rf <- predict(rfB1_maxtrees_tc, newdata = testmodel_B1_tc, type = "raw")
confusionMatrix(data = B1_tc_test_rf, reference = testmodel_B1_tc$DIQ010B
                , mode="prec_recall"
)

varImp(rfB1_maxtrees_tc)

############################################################################
#       Stochastic Gradient Boosted Trees (model B1 exc Cholesterol)
############################################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
tc.gbm <- train(DIQ010B ~ .
                 , data=balancedtrain_B1_tc
                 #, distribution="gaussian"
                 , method="gbm"
                 , trControl=trainControl
                 , verbose=FALSE
                 #, tuneGrid=caretGrid
                 #, metric="Accuracy"
                 #, bag.fraction=0.75
)

print(tc.gbm)

# test model on trainset and check accuracy with confusion matrix.
tctrain_gbm <- predict(tc.gbm, newdata=balancedtrain_B1_tc, type="raw")
confusionMatrix(tctrain_gbm, reference = balancedtrain_B1_tc$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
tctest_gbm <- predict(tc.gbm, newdata=testmodel_B1_tc, type="raw")
confusionMatrix(tctest_gbm, reference = testmodel_B1_tc$DIQ010B, mode="prec_recall")

varImp(tc.gbm)

########################################
#    k-NN (model B1 exc Cholesterol)
########################################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_tc <- train(DIQ010B~., data=balancedtrain_B1_tc, method="kknn", preProcess="scale", trControl=control1)

print(knn_tc)

# test model on trainset and check accuracy with confusion matrix.
tctrain_knn <- predict(knn_tc, newdata=balancedtrain_B1_tc, type="raw")
confusionMatrix(tctrain_knn, reference = balancedtrain_B1_tc$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
tctest_knn <- predict(knn_tc, newdata=testmodel_B1_tc, type="raw")
confusionMatrix(tctest_knn, reference =testmodel_B1_tc$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_tc)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_btc = cbind(DIQ010B = testmodel_B1_tc$DIQ010B,
                     pred_gbm = tctest_gbm,
                     pred_rf = B1_tc_test_rf,
                     pred_knn = tctest_knn)

write.csv(foreval_btc, "foreval_btc.csv", row.names = F)

foreval_btc = read.csv("foreval_btc.csv", as.is = TRUE)


btcROC_gbm = multiclass.roc(foreval_btc$pred_gbm, foreval_btc$DIQ010B)
auc(btcROC_gbm)

btcROC_rf = multiclass.roc(foreval_btc$pred_rf, foreval_btc$DIQ010B)
auc(btcROC_rf)

btcROC_knn = multiclass.roc(foreval_btc$pred_knn, foreval_btc$DIQ010B)
auc(btcROC_knn)


##################################################
#   Stochastic Gradient Boosted Trees (sa3)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
sa3.gbm <- train(DIQ010B ~ .
                 , data=b1_balancedtrain_sa3
                 #, distribution="gaussian"
                 , method="gbm"
                 , trControl=trainControl
                 , verbose=FALSE
                 #, tuneGrid=caretGrid
                 #, metric="Accuracy"
                 #, bag.fraction=0.75
)

print(sa3.gbm)

# test model on trainset and check accuracy with confusion matrix.
sa3train_gbm <- predict(sa3.gbm, newdata=b1_balancedtrain_sa3, type="raw")
confusionMatrix(sa3train_gbm, reference = b1_balancedtrain_sa3$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa3test_gbm <- predict(sa3.gbm, newdata=b_test_sa3, type="raw")
confusionMatrix(sa3test_gbm, reference = b_test_sa3$DIQ010B, mode="prec_recall")

varImp(sa3.gbm)

##########################
#  Random Forest (sa3)
##########################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  sa3_rf = train(DIQ010B~.,
                 data = b1_balancedtrain_sa3,
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

print(sa3_rf)

# Best tuning parameter
sa3_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
sa3_trainrf = predict(sa3_rf, b1_balancedtrain_sa3, type = "raw")
confusionMatrix(sa3_trainrf, reference = b1_balancedtrain_sa3$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa3_testrf <- predict(sa3_rf, newdata = b_test_sa3, type = "raw")
confusionMatrix(data = sa3_testrf, reference = b_test_sa3$DIQ010B, mode="prec_recall")

varImp(sa3_rf)

####################
#    k-NN (sa3)
####################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_sa3 <- train(DIQ010B~., data=b1_balancedtrain_sa3, method="kknn", preProcess="scale", trControl=control1)

print(knn_sa3)

# test model on trainset and check accuracy with confusion matrix.
sa3train_knn <- predict(knn_sa3, newdata=b1_balancedtrain_sa3, type="raw")
confusionMatrix(sa3train_knn, reference = b1_balancedtrain_sa3$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa3test_knn <- predict(knn_sa3, newdata=b_test_sa3, type="raw")
confusionMatrix(sa3test_knn, reference = b_test_sa3$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_sa3, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bsa3 = cbind(DIQ010B = b_test_sa3$DIQ010B,
                     pred_gbm = sa3test_gbm,
                     pred_rf = sa3_testrf,
                     pred_knn = sa3test_knn)

write.csv(foreval_bsa3, "foreval_bsa3.csv", row.names = F)

foreval_bsa3 = read.csv("foreval_bsa3.csv", as.is = TRUE)


bsa3ROC_gbm = multiclass.roc(foreval_bsa3$pred_gbm, foreval_bsa3$DIQ010B)
auc(bsa3ROC_gbm)

bsa3ROC_rf = multiclass.roc(foreval_bsa3$pred_rf, foreval_bsa3$DIQ010B)
auc(bsa3ROC_rf)

bsa3ROC_knn = multiclass.roc(foreval_bsa3$pred_knn, foreval_bsa3$DIQ010B)
auc(bsa3ROC_knn)

##########################################
# Sensitivity analysis - Blood Pressure
##########################################

#Blood Pressure variables - BPXSYaveC, BPXDIaveC
#to read it

train_sa4 = readRDS("train_sa4.RDS")
b1_balancedtrain_sa4 <- SmoteClassif(DIQ010B ~ ., train_sa4, C.perc = "balance", k = 5, dist = "HVDM")
b1_balancedtrain_sa4 = subset(b1_balancedtrain_sa4, select = c(-DIQ010A))

saveRDS(b1_balancedtrain_sa4, file = "b1_balancedtrain_sa4.RDS")

#to read it

b1_balancedtrain_sa4 = readRDS("b1_balancedtrain_sa4.RDS")

#to read it

test_sa4 = readRDS("test_sa4.RDS")
b_test_sa4 = subset(test_sa4, select = c(-DIQ010A))
saveRDS(b_test_sa4, file = "b_test_sa4.RDS")

#to read it

b_test_sa4 = readRDS("b_test_sa4.RDS")

##################################################
#   Stochastic Gradient Boosted Trees (sa4)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
sa4.gbm <- train(DIQ010B ~ .
                 , data=b1_balancedtrain_sa4
                 #, distribution="gaussian"
                 , method="gbm"
                 , trControl=trainControl
                 , verbose=FALSE
                 #, tuneGrid=caretGrid
                 #, metric="Accuracy"
                 #, bag.fraction=0.75
)

print(sa4.gbm)

# test model on trainset and check accuracy with confusion matrix.
sa4train_gbm <- predict(sa4.gbm, newdata=b1_balancedtrain_sa4, type="raw")
confusionMatrix(sa4train_gbm, reference = b1_balancedtrain_sa4$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa4test_gbm <- predict(sa4.gbm, newdata=b_test_sa4, type="raw")
confusionMatrix(sa4test_gbm, reference = b_test_sa4$DIQ010B, mode="prec_recall")

varImp(sa4.gbm)

##########################
#  Random Forest (sa4)
##########################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  sa4_rf = train(DIQ010B~.,
                 data = b1_balancedtrain_sa4,
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

print(sa4_rf)

# Best tuning parameter
sa4_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
sa4_trainrf = predict(sa4_rf, b1_balancedtrain_sa4, type = "raw")
confusionMatrix(sa4_trainrf, reference = b1_balancedtrain_sa4$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa4_testrf <- predict(sa4_rf, newdata = b_test_sa4, type = "raw")
confusionMatrix(data = sa4_testrf, reference = b_test_sa4$DIQ010B, mode="prec_recall")

varImp(sa4_rf)

####################
#    k-NN (sa4)
####################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_sa4 <- train(DIQ010B~., data=b1_balancedtrain_sa4, method="kknn", preProcess="scale", trControl=control1)

print(knn_sa4)

# test model on trainset and check accuracy with confusion matrix.
sa4train_knn <- predict(knn_sa4, newdata=b1_balancedtrain_sa4, type="raw")
confusionMatrix(sa4train_knn, reference = b1_balancedtrain_sa4$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa4test_knn <- predict(knn_sa4, newdata=b_test_sa4, type="raw")
confusionMatrix(sa4test_knn, reference = b_test_sa4$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_sa4, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bsa4 = cbind(DIQ010B = b_test_sa4$DIQ010B,
                     pred_gbm = sa4test_gbm,
                     pred_rf = sa4_testrf,
                     pred_knn = sa4test_knn)

write.csv(foreval_bsa4, "foreval_bsa4.csv", row.names = F)

foreval_bsa4 = read.csv("foreval_bsa4.csv", as.is = TRUE)


bsa4ROC_gbm = multiclass.roc(foreval_bsa4$pred_gbm, foreval_bsa4$DIQ010B)
auc(bsa4ROC_gbm)

bsa4ROC_rf = multiclass.roc(foreval_bsa4$pred_rf, foreval_bsa4$DIQ010B)
auc(bsa4ROC_rf)

bsa4ROC_knn = multiclass.roc(foreval_bsa4$pred_knn, foreval_bsa4$DIQ010B)
auc(bsa4ROC_knn)

##########################################
#     Sensitivity analysis - BMI
##########################################

#remove height and weight and run RF with only BMIAA in it
balancedtrain_b1_bmi = subset(balancedtrain_B1, select = c(-BMXHT, -BMXWT))

saveRDS(balancedtrain_b1_bmi, file = "balancedtrain_b1_bmi.RDS")

#to read it

balancedtrain_b1_bmi = readRDS("balancedtrain_b1_bmi.RDS")

testmodel_b1_bmi = subset(testmodel_B1, select = c(-BMXHT, -BMXWT))

saveRDS(testmodel_b1_bmi, file = "testmodel_b1_bmi.RDS")

#to read it

testmodel_b1_bmi = readRDS("testmodel_b1_bmi.RDS")

#####################################################
#  Stochastic Gradient Boosted Trees (w/o Ht and Wt)
#####################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
b1bmi.gbm <- train(DIQ010B ~ .
                 , data=balancedtrain_b1_bmi
                 #, distribution="gaussian"
                 , method="gbm"
                 , trControl=trainControl
                 , verbose=FALSE
                 #, tuneGrid=caretGrid
                 #, metric="Accuracy"
                 #, bag.fraction=0.75
)

print(b1bmi.gbm)

# test model on trainset and check accuracy with confusion matrix.
b1bmitrain_gbm <- predict(b1bmi.gbm, newdata=balancedtrain_b1_bmi, type="raw")
confusionMatrix(sa4train_gbm, reference = b1_balancedtrain_sa4$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
b1bmitest_gbm <- predict(b1bmi.gbm, newdata=testmodel_b1_bmi, type="raw")
confusionMatrix(sa4test_gbm, reference = testmodel_b1_bmi$DIQ010B, mode="prec_recall")

varImp(b1bmi.gbm)

#################################
#  Random Forest (w/o Ht and Wt)
#################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  b1bmi_rf = train(DIQ010B~.,
                          data = balancedtrain_b1_bmi,
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

print(b1bmi_rf)

# Best tuning parameter
b1bmi_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
b1bmi_trainrf = predict(b1bmi_rf, balancedtrain_b1_bmi, type = "raw")
confusionMatrix(b1bmi_trainrf, reference = balancedtrain_b1_bmi$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
b1bmi_testrf <- predict(b1bmi_rf, newdata = testmodel_b1_bmi, type = "raw")
confusionMatrix(data = b1bmi_testrf, reference = testmodel_b1_bmi$DIQ010B, mode="prec_recall")

varImp(b1bmi_rf)

##########################
#  k-NN (w/o Ht and Wt)
##########################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_b1bmi <- train(DIQ010B~., data=balancedtrain_b1_bmi, method="kknn", preProcess="scale", trControl=control1)

print(knn_b1bmi)

# test model on trainset and check accuracy with confusion matrix.
b1bmitrain_knn <- predict(knn_b1bmi, newdata=balancedtrain_b1_bmi, type="raw")
confusionMatrix(b1bmitrain_knn, reference = balancedtrain_b1_bmi$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
b1bmitest_knn <- predict(knn_b1bmi, newdata=testmodel_b1_bmi, type="raw")
confusionMatrix(b1bmitest_knn, reference = testmodel_b1_bmi$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_b1bmi, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bbmi = cbind(DIQ010B = testmodel_b1_bmi$DIQ010B,
                     pred_gbm = b1bmitest_gbm,
                     pred_rf = b1bmi_testrf,
                     pred_knn = b1bmitest_knn)

write.csv(foreval_bbmi, "foreval_bbmi.csv", row.names = F)

foreval_bbmi = read.csv("foreval_bbmi.csv", as.is = TRUE)


bbmiROC_gbm = multiclass.roc(foreval_bbmi$pred_gbm, foreval_bbmi$DIQ010B)
auc(bbmiROC_gbm)

bbmiROC_rf = multiclass.roc(foreval_bbmi$pred_rf, foreval_bbmi$DIQ010B)
auc(bbmiROC_rf)

bbmiROC_knn = multiclass.roc(foreval_bbmi$pred_knn, foreval_bbmi$DIQ010B)
auc(bbmiROC_knn)

#########################################################################################################################################
#BMI-ordinary variable (exc Height and weight)
#to read it

train_sa5 = readRDS("train_sa5.RDS")
b1_balancedtrain_sa5 <- SmoteClassif(DIQ010B ~ ., train_sa5, C.perc = "balance", k = 5, dist = "HVDM")
b1_balancedtrain_sa5 = subset(b1_balancedtrain_sa5, select = c(-DIQ010A))

saveRDS(b1_balancedtrain_sa5, file = "b1_balancedtrain_sa5.RDS")

#to read it

b1_balancedtrain_sa5 = readRDS("b1_balancedtrain_sa5.RDS")

#to read it

test_sa5 = readRDS("test_sa5.RDS")
b_test_sa5 = subset(test_sa5, select = c(-DIQ010A))
saveRDS(b_test_sa5, file = "b_test_sa5.RDS")

#to read it

b_test_sa5 = readRDS("b_test_sa5.RDS")

#try with and without ht and wt

b1balancedtrain_sa5_bmi = subset(b1_balancedtrain_sa5, select = c(-BMXWT, -BMXHT))

saveRDS(b1balancedtrain_sa5_bmi, file = "b1balancedtrain_sa5_bmi.RDS")

#to read it

b1balancedtrain_sa5_bmi = readRDS("b1balancedtrain_sa5_bmi.RDS")

b_test_sa5_bmi = subset(b_test_sa5, select = c(-BMXWT, -BMXHT))
saveRDS(b_test_sa5_bmi, file = "b_test_sa5_bmi.RDS")

#to read it

b_test_sa5_bmi = readRDS("b_test_sa5_bmi.RDS")

##################################################
#   Stochastic Gradient Boosted Trees (sa5)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
sa5.gbm <- train(DIQ010B ~ .
                 , data=b1_balancedtrain_sa5
                 #, distribution="gaussian"
                 , method="gbm"
                 , trControl=trainControl
                 , verbose=FALSE
                 #, tuneGrid=caretGrid
                 #, metric="Accuracy"
                 #, bag.fraction=0.75
)

print(sa5.gbm)

# test model on trainset and check accuracy with confusion matrix.
sa5train_gbm <- predict(sa5.gbm, newdata=b1_balancedtrain_sa5, type="raw")
confusionMatrix(sa5train_gbm, reference = b1_balancedtrain_sa5$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa5test_gbm <- predict(sa5.gbm, newdata=b_test_sa5, type="raw")
confusionMatrix(sa5test_gbm, reference = b_test_sa5$DIQ010B, mode="prec_recall")

varImp(sa5.gbm)

##########################
#  Random Forest (sa5)
##########################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  sa5_rf = train(DIQ010B~.,
                 data = b1_balancedtrain_sa5,
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

print(sa5_rf)

# Best tuning parameter
sa5_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
sa5_trainrf = predict(sa5_rf, b1_balancedtrain_sa5, type = "raw")
confusionMatrix(sa5_trainrf, reference = b1_balancedtrain_sa5$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa5_testrf <- predict(sa5_rf, newdata = b_test_sa5, type = "raw")
confusionMatrix(data = sa5_testrf, reference = b_test_sa5$DIQ010B, mode="prec_recall")

varImp(sa5_rf)

####################
#    k-NN (sa5)
####################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_sa5 <- train(DIQ010B~., data=b1_balancedtrain_sa5, method="kknn", preProcess="scale", trControl=control1)

print(knn_sa5)

# test model on trainset and check accuracy with confusion matrix.
sa5train_knn <- predict(knn_sa5, newdata=b1_balancedtrain_sa5, type="raw")
confusionMatrix(sa5train_knn, reference = b1_balancedtrain_sa5$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa5test_knn <- predict(knn_sa5, newdata=b_test_sa5, type="raw")
confusionMatrix(sa5test_knn, reference = b_test_sa5$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_sa5, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bsa5 = cbind(DIQ010B = b_test_sa5$DIQ010B,
                     pred_gbm = sa5test_gbm,
                     pred_rf = sa5_testrf,
                     pred_knn = sa5test_knn)

write.csv(foreval_bsa5, "foreval_bsa5.csv", row.names = F)

foreval_bsa5 = read.csv("foreval_bsa5.csv", as.is = TRUE)


bsa5ROC_gbm = multiclass.roc(foreval_bsa5$pred_gbm, foreval_bsa5$DIQ010B)
auc(bsa5ROC_gbm)

bsa5ROC_rf = multiclass.roc(foreval_bsa5$pred_rf, foreval_bsa5$DIQ010B)
auc(bsa5ROC_rf)

bsa5ROC_knn = multiclass.roc(foreval_bsa5$pred_knn, foreval_bsa5$DIQ010B)
auc(bsa5ROC_knn)

##################################################
#   Stochastic Gradient Boosted Trees (sa5 wo)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
sa5bmi.gbm <- train(DIQ010B ~ .
                 , data=b1balancedtrain_sa5_bmi
                 #, distribution="gaussian"
                 , method="gbm"
                 , trControl=trainControl
                 , verbose=FALSE
                 #, tuneGrid=caretGrid
                 #, metric="Accuracy"
                 #, bag.fraction=0.75
)

print(sa5bmi.gbm)

# test model on trainset and check accuracy with confusion matrix.
sa5bmitrain_gbm <- predict(sa5bmi.gbm, newdata=b1balancedtrain_sa5_bmi, type="raw")
confusionMatrix(sa5bmitrain_gbm, reference = b1balancedtrain_sa5_bmi$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa5bmitest_gbm <- predict(sa5bmi.gbm, newdata=b_test_sa5_bmi, type="raw")
confusionMatrix(sa5bmitest_gbm, reference = b_test_sa5_bmi$DIQ010B, mode="prec_recall")

varImp(sa5bmi.gbm)

##########################
#  Random Forest (sa5 wo)
##########################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  sa5bmi_rf = train(DIQ010B~.,
                 data = b1balancedtrain_sa5_bmi,
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

print(sa5bmi_rf)

# Best tuning parameter
sa5bmi_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
sa5bmi_trainrf = predict(sa5bmi_rf, b1balancedtrain_sa5_bmi, type = "raw")
confusionMatrix(sa5bmi_trainrf, reference = b1balancedtrain_sa5_bmi$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa5bmi_testrf <- predict(sa5bmi_rf, newdata = b_test_sa5_bmi, type = "raw")
confusionMatrix(data = sa5bmi_testrf, reference = b_test_sa5_bmi$DIQ010B, mode="prec_recall")

varImp(sa5bmi_rf)

####################
#    k-NN (sa5 wo)
####################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_sa5bmi <- train(DIQ010B~., data=b1balancedtrain_sa5_bmi, method="kknn", preProcess="scale", trControl=control1)

print(knn_sa5bmi)

# test model on trainset and check accuracy with confusion matrix.
sa5bmitrain_knn <- predict(knn_sa5bmi, newdata=b1balancedtrain_sa5_bmi, type="raw")
confusionMatrix(sa5bmitrain_knn, reference = b1balancedtrain_sa5_bmi$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa5bmitest_knn <- predict(knn_sa5bmi, newdata=b_test_sa5_bmi, type="raw")
confusionMatrix(sa5test_knn, reference = b_test_sa5_bmi$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_sa5bmi, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bsa5bmi = cbind(DIQ010B = b_test_sa5_bmi$DIQ010B,
                     pred_gbm = sa5bmitest_gbm,
                     pred_rf = sa5bmi_testrf,
                     pred_knn = sa5bmitest_knn)

write.csv(foreval_bsa5bmi, "foreval_bsa5bmi.csv", row.names = F)

foreval_bsa5bmi = read.csv("foreval_bsa5bmi.csv", as.is = TRUE)


bsa5bmiROC_gbm = multiclass.roc(foreval_bsa5bmi$pred_gbm, foreval_bsa5bmi$DIQ010B)
auc(bsa5bmiROC_gbm)

bsa5bmiROC_rf = multiclass.roc(foreval_bsa5bmi$pred_rf, foreval_bsa5bmi$DIQ010B)
auc(bsa5bmiROC_rf)

bsa5bmiROC_knn = multiclass.roc(foreval_bsa5bmi$pred_knn, foreval_bsa5bmi$DIQ010B)
auc(bsa5bmiROC_knn)

##########################################################################################################################################
#BMI continuous variable (exc Height and weight)
#to read it

train_sa6 = readRDS("train_sa6.RDS")
b1_balancedtrain_sa6 <- SmoteClassif(DIQ010B ~ ., train_sa6, C.perc = "balance", k = 5, dist = "HVDM")
b1_balancedtrain_sa6 = subset(b1_balancedtrain_sa6, select = c(-DIQ010A))

saveRDS(b1_balancedtrain_sa6, file = "b1_balancedtrain_sa6.RDS")

#to read it

b1_balancedtrain_sa6 = readRDS("b1_balancedtrain_sa6.RDS")

#to read it

test_sa6 = readRDS("test_sa6.RDS")
b_test_sa6 = subset(test_sa6, select = c(-DIQ010A))
saveRDS(b_test_sa6, file = "b_test_sa6.RDS")

#to read it

b_test_sa6 = readRDS("b_test_sa6.RDS")

#try with and without ht and wt

b1balancedtrain_sa6_bmi = subset(b1_balancedtrain_sa6, select = c(-BMXWT, -BMXHT))

saveRDS(b1balancedtrain_sa6_bmi, file = "b1balancedtrain_sa6_bmi.RDS")

#to read it

b1balancedtrain_sa6_bmi = readRDS("b1balancedtrain_sa6_bmi.RDS")

b_test_sa6_bmi = subset(b_test_sa6, select = c(-BMXWT, -BMXHT))
saveRDS(b_test_sa6_bmi, file = "b_test_sa6_bmi.RDS")

#to read it

b_test_sa6_bmi = readRDS("b_test_sa6_bmi.RDS")

##################################################
#   Stochastic Gradient Boosted Trees (sa6)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
sa6.gbm <- train(DIQ010B ~ .
                 , data=b1_balancedtrain_sa6
                 #, distribution="gaussian"
                 , method="gbm"
                 , trControl=trainControl
                 , verbose=FALSE
                 #, tuneGrid=caretGrid
                 #, metric="Accuracy"
                 #, bag.fraction=0.75
)

print(sa6.gbm)

# test model on trainset and check accuracy with confusion matrix.
sa6train_gbm <- predict(sa6.gbm, newdata=b1_balancedtrain_sa6, type="raw")
confusionMatrix(sa6train_gbm, reference = b1_balancedtrain_sa6$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa6test_gbm <- predict(sa6.gbm, newdata=b_test_sa6, type="raw")
confusionMatrix(sa6test_gbm, reference = b_test_sa6$DIQ010B, mode="prec_recall")

varImp(sa6.gbm)

##########################
#  Random Forest (sa6)
##########################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  sa6_rf = train(DIQ010B~.,
                 data = b1_balancedtrain_sa6,
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

print(sa6_rf)

# Best tuning parameter
sa6_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
sa6_trainrf = predict(sa6_rf, b1_balancedtrain_sa6, type = "raw")
confusionMatrix(sa6_trainrf, reference = b1_balancedtrain_sa6$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa6_testrf <- predict(sa6_rf, newdata = b_test_sa6, type = "raw")
confusionMatrix(data = sa6_testrf, reference = b_test_sa6$DIQ010B, mode="prec_recall")

varImp(sa6_rf)

####################
#    k-NN (sa6)
####################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_sa6 <- train(DIQ010B~., data=b1_balancedtrain_sa6, method="kknn", preProcess="scale", trControl=control1)

print(knn_sa6)

# test model on trainset and check accuracy with confusion matrix.
sa6train_knn <- predict(knn_sa6, newdata=b1_balancedtrain_sa6, type="raw")
confusionMatrix(sa6train_knn, reference = b1_balancedtrain_sa6$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa6test_knn <- predict(knn_sa6, newdata=b_test_sa6, type="raw")
confusionMatrix(sa6test_knn, reference = b_test_sa6$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_sa6, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bsa6 = cbind(DIQ010B = b_test_sa6$DIQ010B,
                     pred_gbm = sa6test_gbm,
                     pred_rf = sa6_testrf,
                     pred_knn = sa6test_knn)

write.csv(foreval_bsa6, "foreval_bsa6.csv", row.names = F)

foreval_bsa6 = read.csv("foreval_bsa6.csv", as.is = TRUE)


bsa6ROC_gbm = multiclass.roc(foreval_bsa6$pred_gbm, foreval_bsa6$DIQ010B)
auc(bsa6ROC_gbm)

bsa6ROC_rf = multiclass.roc(foreval_bsa6$pred_rf, foreval_bsa6$DIQ010B)
auc(bsa6ROC_rf)

bsa6ROC_knn = multiclass.roc(foreval_bsa6$pred_knn, foreval_bsa6$DIQ010B)
auc(bsa6ROC_knn)

##################################################
#   Stochastic Gradient Boosted Trees (sa6 wo)
##################################################

# Fit the model on the balanced set
set.seed(500)
trainControl <- trainControl(method="cv", number=10)
sa6bmi.gbm <- train(DIQ010B ~ .
                    , data=b1balancedtrain_sa6_bmi
                    #, distribution="gaussian"
                    , method="gbm"
                    , trControl=trainControl
                    , verbose=FALSE
                    #, tuneGrid=caretGrid
                    #, metric="Accuracy"
                    #, bag.fraction=0.75
)

print(sa6bmi.gbm)

# test model on trainset and check accuracy with confusion matrix.
sa6bmitrain_gbm <- predict(sa6bmi.gbm, newdata=b1balancedtrain_sa6_bmi, type="raw")
confusionMatrix(sa6bmitrain_gbm, reference = b1balancedtrain_sa6_bmi$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa6bmitest_gbm <- predict(sa6bmi.gbm, newdata=b_test_sa6_bmi, type="raw")
confusionMatrix(sa6bmitest_gbm, reference = b_test_sa6_bmi$DIQ010B, mode="prec_recall")

varImp(sa6bmi.gbm)

##########################
# Random Forest (sa6 wo)
##########################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  sa6bmi_rf = train(DIQ010B~.,
                 data = b1balancedtrain_sa6_bmi,
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

print(sa6bmi_rf)

# Best tuning parameter
sa6bmi_rf$bestTune


# test model on trainset and check accuracy with confusion matrix.
sa6bmi_trainrf = predict(sa6bmi_rf, b1balancedtrain_sa6_bmi, type = "raw")
confusionMatrix(sa6bmi_trainrf, reference = b1balancedtrain_sa6_bmi$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa6bmi_testrf <- predict(sa6bmi_rf, newdata = b_test_sa6_bmi, type = "raw")
confusionMatrix(data = sa6bmi_testrf, reference = b_test_sa6_bmi$DIQ010B, mode="prec_recall")

varImp(sa6bmi_rf)

####################
#    k-NN (sa6)
####################

# Fit the model on the balanced set

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_sa6bmi <- train(DIQ010B~., data=b1balancedtrain_sa6_bmi, method="kknn", preProcess="scale", trControl=control1)

print(knn_sa6bmi)

# test model on trainset and check accuracy with confusion matrix.
sa6bmitrain_knn <- predict(knn_sa6bmi, newdata=b1balancedtrain_sa6_bmi, type="raw")
confusionMatrix(sa6bmitrain_knn, reference = b1balancedtrain_sa6_bmi$DIQ010B, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
sa6bmitest_knn <- predict(knn_sa6bmi, newdata=b_test_sa6_bmi, type="raw")
confusionMatrix(sa6bmitest_knn, reference = b_test_sa6_bmi$DIQ010B, mode="prec_recall")

# estimate variable importance
varImp(knn_sa6bmi, scale=FALSE)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bsa6bmi = cbind(DIQ010B = b_test_sa6_bmi$DIQ010B,
                     pred_gbm = sa6bmitest_gbm,
                     pred_rf = sa6bmi_testrf,
                     pred_knn = sa6bmitest_knn)

write.csv(foreval_bsa6bmi, "foreval_bsa6bmi.csv", row.names = F)

foreval_bsa6bmi = read.csv("foreval_bsa6bmi.csv", as.is = TRUE)


bsa6bmiROC_gbm = multiclass.roc(foreval_bsa6bmi$pred_gbm, foreval_bsa6bmi$DIQ010B)
auc(bsa6bmiROC_gbm)

bsa6bmiROC_rf = multiclass.roc(foreval_bsa6bmi$pred_rf, foreval_bsa6bmi$DIQ010B)
auc(bsa6bmiROC_rf)

bsa6bmiROC_knn = multiclass.roc(foreval_bsa6bmi$pred_knn, foreval_bsa6bmi$DIQ010B)
auc(bsa6bmiROC_knn)


####################
#  SVM 1+2 vs 3+4
####################

# trainset

#create a new column for this categorical variable and save into a new dataset
trainmodel_B12v34 = trainmodel_B1 %>% 
  mutate(DIQ010B12 = DIQ010B)

#recode levels 1,2 to level 1 for DIQ010B12 (yes)
trainmodel_B12v34[trainmodel_B12v34$DIQ010B12 == 2, "DIQ010B12"] <- 1
summary(trainmodel_B12v34$DIQ010B12)

#recode levels 3,4 to level 2 for DIQ010B12 (no)
trainmodel_B12v34[trainmodel_B12v34$DIQ010B12 == 3, "DIQ010B12"] <- 2
trainmodel_B12v34[trainmodel_B12v34$DIQ010B12 == 4, "DIQ010B12"] <- 2
summary(trainmodel_B12v34$DIQ010B12)


trainmodel_B12v34 = subset(trainmodel_B12v34, select = c(-DIQ010B, - DIQ010Ba))
trainmodel_B12v34$DIQ010B12 = factor(trainmodel_B12v34$DIQ010B12, levels = c('1', '2'))
saveRDS(trainmodel_B12v34, file = "trainmodel_B12v34.RDS")

#to read it

trainmodel_B12v34 = readRDS("trainmodel_B12v34.RDS")

#balance trainset
balancedtrain_B12v34 = SmoteClassif(DIQ010B12 ~ ., trainmodel_B12v34, C.perc = "balance", k = 5, dist = "HVDM")
print(table(balancedtrain_B12v34$DIQ010B12))
print(prop.table(table(balancedtrain_B12v34$DIQ010B12)))

saveRDS(balancedtrain_B12v34, file = "balancedtrain_B12v34.RDS")

#to read it

balancedtrain_B12v34 = readRDS("balancedtrain_B12v34.RDS")

# testset
testmodel_B12v34 = testmodel_B1 %>% 
  mutate(DIQ010B12 = DIQ010B)

testmodel_B12v34[testmodel_B12v34$DIQ010B12 == 2, "DIQ010B12"] <- 1
summary(testmodel_B12v34$DIQ010B12)

#recode levels 3,4 to level 2 for DIQ010B12 (no)
testmodel_B12v34[testmodel_B12v34$DIQ010B12 == 3, "DIQ010B12"] <- 2
testmodel_B12v34[testmodel_B12v34$DIQ010B12 == 4, "DIQ010B12"] <- 2
summary(testmodel_B12v34$DIQ010B12)


testmodel_B12v34 = subset(testmodel_B12v34, select = c(-DIQ010B))
testmodel_B12v34$DIQ010B12 = factor(testmodel_B12v34$DIQ010B12, levels = c('1', '2'))
saveRDS(testmodel_B12v34, file = "testmodel_B12v34.RDS")

#to read it

testmodel_B12v34 = readRDS("testmodel_B12v34.RDS")
##############################################################################################################################
#SVM linear
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(500)
B12v34_svmGrid <- train(DIQ010B12 ~., data = balancedtrain_B12v34, method = "svmLinear",
                       trControl=trctrl,
                       preProcess = c("center", "scale"),
                       tuneGrid = grid,
                       tuneLength = 10)


# test model on test set
B12v34_testGrid <- predict(B12v34_svmGrid, testmodel_B12v34, type = "raw")
confusionMatrix(data = B12v34_testGrid, reference = testmodel_B12v34$DIQ010B12, mode="prec_recall")

#####SVM Radial

B12v34.svmR <- train(DIQ010B12 ~., data = balancedtrain_B12v34, method = "svmRadial",
              trControl = trainControl("cv", number = 10),
              preProcess = c("center","scale"),
              tuneLength = 10
)

# test model on test set
B12v34_svmR <- predict(B12v34.svmR, testmodel_B12v34, type = "raw")
confusionMatrix(data = B12v34_svmR, reference = testmodel_B12v34$DIQ010B12, mode="prec_recall")

#####SVM Poly

B12v34.svmP <- train(DIQ010B12 ~., data = balancedtrain_B12v34, method = "svmPoly",
              trControl = trainControl("cv", number = 10),
              preProcess = c("center","scale"),
              tuneLength = 4
)


# test model on test set
B12v34_svmP <- predict(B12v34.svmP, testmodel_B12v34, type = "raw")
confusionMatrix(data = B12v34_svmP, reference = testmodel_B12v34$DIQ010B12, mode="prec_recall")

foreval_svm12v34 = cbind(DIQ010B12 = testmodel_B12v34$DIQ010B12,
                    pred_svml = B12v34_testGrid,
                    pred_svmr = B12v34_svmR,
                    pred_svmp = B12v34_svmP)

write.csv(foreval_svm12v34, "foreval_svm12v34.csv", row.names = F)

foreval_svm12v34 = read.csv("foreval_svm12v34.csv", as.is = TRUE)


B12v34ROC_svml = multiclass.roc(foreval_svm12v34$pred_svml, foreval_svm12v34$DIQ010B12)
auc(B12v34ROC_svml)

B12v34ROC_svmr = multiclass.roc(foreval_svm12v34$pred_svmr, foreval_svm12v34$DIQ010B12)
auc(B12v34ROC_svmr)

B12v34ROC_svmp = multiclass.roc(foreval_svm12v34$pred_svmp, foreval_svm12v34$DIQ010B12)
auc(B12v34ROC_svmp)


####################
#  SVM  2+3 vs 4
####################

#create a new column for this categorical variable and save into a new dataset
trainmodel_B23v4 = trainmodel_B1 %>% 
  mutate(DIQ010B23 = DIQ010B)

trainmodel_B23v4 = subset(trainmodel_B23v4, select = c(-DIQ010B))
names(trainmodel_B23v4)[names(trainmodel_B23v4) == "DIQ010B23"] <- "DIQ010B"

#remove level 1
trainmodel_B23v4[trainmodel_B23v4$DIQ010B == 1, "DIQ010B"] <- NA
trainmodel_B23v4 = na.omit(trainmodel_B23v4)
summary(trainmodel_B23v4$DIQ010B)

#recode level 2 to level 1 for DIQ010B234
trainmodel_B23v4[trainmodel_B23v4$DIQ010B == 2, "DIQ010B"] <- 1

#recode level 3 to level 1 for DIQ010B234
trainmodel_B23v4[trainmodel_B23v4$DIQ010B == 3, "DIQ010B"] <- 1

#recode level 4 to level 2 for DIQ010B234
trainmodel_B23v4[trainmodel_B23v4$DIQ010B == 4, "DIQ010B"] <- 2
summary(trainmodel_B23v4$DIQ010B)

trainmodel_B23v4$DIQ010B = factor(trainmodel_B23v4$DIQ010B, levels = c('1', '2'))
saveRDS(trainmodel_B23v4, file = "trainmodel_B23v4.RDS")

#to read it

trainmodel_B23v4 = readRDS("trainmodel_B23v4.RDS")

#balance trainset
balancedtrain_B23v4 = SmoteClassif(DIQ010B ~ ., trainmodel_B23v4, C.perc = "balance", k = 5, dist = "HVDM")
print(table(balancedtrain_B23v4$DIQ010B))
print(prop.table(table(balancedtrain_B23v4$DIQ010B)))

saveRDS(balancedtrain_B23v4, file = "balancedtrain_B23v4.RDS")

#to read it

balancedtrain_B23v4 = readRDS("balancedtrain_B23v4.RDS")

#testset

testmodel_B23v4 = testmodel_B1 %>% 
  mutate(DIQ010B23 = DIQ010B)

testmodel_B23v4 = subset(testmodel_B23v4, select = c(-DIQ010B))
names(testmodel_B23v4)[names(testmodel_B23v4) == "DIQ010B23"] <- "DIQ010B"

#remove level 1
testmodel_B23v4[testmodel_B23v4$DIQ010B == 1, "DIQ010B"] <- NA
testmodel_B23v4 = na.omit(testmodel_B23v4)
summary(testmodel_B23v4$DIQ010B)

#recode level 2 to level 1 for DIQ010B234
testmodel_B23v4[testmodel_B23v4$DIQ010B == 2, "DIQ010B"] <- 1

#recode level 3 to level 1 for DIQ010B234
testmodel_B23v4[testmodel_B23v4$DIQ010B == 3, "DIQ010B"] <- 1

#recode level 4 to level 2 for DIQ010B234
testmodel_B23v4[testmodel_B23v4$DIQ010B == 4, "DIQ010B"] <- 2
summary(testmodel_B23v4$DIQ010B)

testmodel_B23v4$DIQ010B = factor(testmodel_B23v4$DIQ010B, levels = c('1', '2'))
saveRDS(testmodel_B23v4, file = "testmodel_B23v4.RDS")

#to read it

testmodel_B23v4 = readRDS("testmodel_B23v4.RDS")

##############################################################################################################################
#SVM linear
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(500)
B23v4_svmGrid <- train(DIQ010B ~., data = balancedtrain_B23v4, method = "svmLinear",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),
                        tuneGrid = grid,
                        tuneLength = 10)


# test model on test set
B23v4_testGrid <- predict(B23v4_svmGrid, testmodel_B23v4, type = "raw")
confusionMatrix(data = B23v4_testGrid, reference = testmodel_B23v4$DIQ010B, mode="prec_recall")

#####SVM Radial

B23v4.svmR <- train(DIQ010B ~., data = balancedtrain_B23v4, method = "svmRadial",
                     trControl = trainControl("cv", number = 10),
                     preProcess = c("center","scale"),
                     tuneLength = 10
)

# test model on test set
B23v4_svmR <- predict(B23v4.svmR, testmodel_B23v4, type = "raw")
confusionMatrix(data = B23v4_svmR, reference = testmodel_B23v4$DIQ010B, mode="prec_recall")

#####SVM Poly

B23v4.svmP <- train(DIQ010B ~., data = balancedtrain_B23v4, method = "svmPoly",
                     trControl = trainControl("cv", number = 10),
                     preProcess = c("center","scale"),
                     tuneLength = 4
)


# test model on test set
B23v4_svmP <- predict(B23v4.svmP, testmodel_B23v4, type = "raw")
confusionMatrix(data = B23v4_svmP, reference = testmodel_B23v4$DIQ010B, mode="prec_recall")

foreval_svm23v4 = cbind(DIQ010B = testmodel_B23v4$DIQ010B,
                         pred_svml = B23v4_testGrid,
                         pred_svmr = B23v4_svmR,
                         pred_svmp = B23v4_svmP)

write.csv(foreval_svm23v4, "foreval_svm23v4.csv", row.names = F)

foreval_svm23v4 = read.csv("foreval_svm23v4.csv", as.is = TRUE)


B23v4ROC_svml = multiclass.roc(foreval_svm23v4$pred_svml, foreval_svm23v4$DIQ010B)
auc(B23v4ROC_svml)

B23v4ROC_svmr = multiclass.roc(foreval_svm23v4$pred_svmr, foreval_svm23v4$DIQ010B)
auc(B23v4ROC_svmr)

B23v4ROC_svmp = multiclass.roc(foreval_svm23v4$pred_svmp, foreval_svm23v4$DIQ010B)
auc(B23v4ROC_svmp)
