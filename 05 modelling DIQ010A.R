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
library(MLeval)
setwd("C:/Users/May Khoo/Desktop/NUS/Internship")

# Imbalanced is precision recall
# Accuracy bw trng and test 5 to 10% difference

############################
#    DIQ010A (train set)
############################

trainmodel_A1 = subset(train_model2, select = c(-DIQ010B))
saveRDS(trainmodel_A1, file = "trainmodel_A1.RDS")

#to read it

trainmodel_A1 = readRDS("trainmodel_A1.RDS")
summary(trainmodel_A1)

#include some significant variables from the lab tests

trainmodel_A2 = subset(train_model3, select = c(-DIQ010B))
saveRDS(trainmodel_A2, file = "trainmodel_A2.RDS")

#to read it

trainmodel_A2 = readRDS("trainmodel_A2.RDS")
summary(trainmodel_A2)

############################
#    DIQ010A (test set)
############################

testmodel_A1 = subset(test_model2, select = c(-DIQ010B))
saveRDS(testmodel_A1, file = "testmodel_A1.RDS")

#to read it

testmodel_A1 = readRDS("testmodel_A1.RDS")
summary(testmodel_A1)

testmodel_A2 = subset(test_model3, select = c(-DIQ010B))
saveRDS(testmodel_A2, file = "testmodel_A2.RDS")

#to read it

testmodel_A2 = readRDS("testmodel_A2.RDS")
summary(testmodel_A2)


####################################
#  Class Imbalance: SMOTE (train)
####################################

library(DMwR)

print(table(trainmodel_A1$DIQ010A))
# 1   2 
# 123 816 
print(prop.table(table(trainmodel_A1$DIQ010A)))
# 1         2 
# 0.1309904 0.8690096 

# over = 600 (6 x 123 +123)
# under = (6 x 123) x 100/100

# Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalance In Binary Classification
# create more of the positive cases (upsample) to get 1:1
balancedtrain_A1 <- SMOTE(DIQ010A ~., trainmodel_A1, perc.over = 600, k = 5, perc.under = 116.6667)

# Let's check the count of unique value in the target variable
print(table(balancedtrain_A1$DIQ010A))
# 1   2 
# 861 861 

saveRDS(balancedtrain_A1, file = "balancedtrain_A1.RDS")

#to read it

balancedtrain_A1 = readRDS("balancedtrain_A1.RDS")

###train_A2

balancedtrain_A2 <- SMOTE(DIQ010A ~., trainmodel_A2, perc.over = 600, k = 5, perc.under = 116.6667)

# Let's check the count of unique value in the target variable
print(table(balancedtrain_A2$DIQ010A))
# 1   2 
# 861 861 

saveRDS(balancedtrain_A2, file = "balancedtrain_A2.RDS")

#to read it

balancedtrain_A2 = readRDS("balancedtrain_A2.RDS")

##########################################################
#      Logistic Regression (balanced 1-1 model A1)
##########################################################

#try without dummyVars
diabetesA1_glm <- glm(formula = DIQ010A ~ .,
                        family=binomial,  data=balancedtrain_A1)
summary(diabetesA1_glm)

diabetesA1_glm2 = step(diabetesA1_glm, direction = c("backward"))
summary(diabetesA1_glm2)

# test model on trainset and check accuracy with confusion matrix.
diabetesA1_glm_train <- predict(diabetesA1_glm, newdata = balancedtrain_A1, type = "response")

glimpse(balancedtrain_A1$DIQ010A)

#confusionmatrix
diabetesA1_glm_train = ifelse(diabetesA1_glm_train > 0.5, 1, 2)
diabetesA1_glm_train = factor(diabetesA1_glm_train)
confusionMatrix(diabetesA1_glm_train, balancedtrain_A1$DIQ010A)
confusionMatrix(diabetesA1_glm_train, balancedtrain_A1$DIQ010A, mode="prec_recall")

# Perform prediction on testset and look at confusion matrix.
diabetesA1_glm_test <- predict(diabetesA1_glm, newdata = testmodel_A1, type = "response")

# confusionmatrix syntax: (predicted result (we set the threshold previously), actual results)

diabetesA1_glm_test = ifelse(diabetesA1_glm_test > 0.5, 1, 2)
diabetesA1_glm_test = factor(diabetesA1_glm_test)
confusionMatrix(diabetesA1_glm_test, testmodel_A1$DIQ010A)
confusionMatrix(diabetesA1_glm_test, testmodel_A1$DIQ010A, mode="prec_recall")

#############################################
#      LogitBoost (balanced 1-1 model A1)
#############################################
library(caTools)

#lower recall, higher accuracy
trainControl <- trainControl(method="cv", number=10)
logitboost.caret <- train(DIQ010A~ .,
                          data=balancedtrain_A1, 
                          method = "LogitBoost", 
                          trControl = trainControl,
                          #metric = "ROC", 
                          #preProc = c("center", "scale")
)

#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.caret <- train(DIQ010A~ .,
                          data=balancedtrain_A1, 
                          method = "LogitBoost", 
                          trControl = cctrlR,
                          tuneLength = 4)

print(logitboost.caret)

# test model on trainset and check accuracy with confusion matrix.
A1b_train_lb <- predict(logitboost.caret, newdata = balancedtrain_A1, type = "raw")

#confusion matrix
confusionMatrix(A1b_train_lb, balancedtrain_A1$DIQ010A,
                #mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
A1b_test_lb <- predict(logitboost.caret, newdata = testmodel_A1, type = "raw")

# confusion matrix
confusionMatrix(A1b_test_lb, testmodel_A1$DIQ010A,
                #mode="prec_recall"
                )

varImp(logitboost.caret)

#############################################
#    Decision Tree (balanced 1-1 model A1)
#############################################

library(rpart)
# Perform modelling on balanced dataset

balancedA1_rpart <- rpart(formula = DIQ010A~ .,
                          data=balancedtrain_A1,
                       method = "class",
                       parms=list(split="information"),
                       control= rpart.control(minsplit=20,
                                              minbucket=7,
                                              usesurrogate=0,
                                              maxsurrogate=0),
                       model=TRUE)

# Generate a textual view of the Decision Tree model.
rattle::fancyRpartPlot(balancedA1_rpart)
summary(balancedA1_rpart)

#see variable importance
balancedA1_rpart[["variable.importance"]]

# test model on trainset and check accuracy with confusion matrix.
A1b_traintree = predict(balancedA1_rpart, balancedtrain_A1, type = "class")
confusionMatrix(A1b_traintree, reference = balancedtrain_A1$DIQ010A,positive="1"
                #, mode="prec_recall"
                )
# accuracy of trainset is 85.77%

# Perform prediction on testset and look at confusion matrix.
A1b_testtree = predict(balancedA1_rpart, testmodel_A1, type = "class")
confusionMatrix(A1b_testtree, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 82.34%

balvarimp_tree = as.data.frame(varImp(balancedA1_rpart)) %>%
  `colnames<-`(c("importance")) %>%
  rownames_to_column("Variable")
balvarimp_tree %>%
  ggplot(aes(x = reorder(Variable, importance), y = importance))+ geom_col() +
  coord_flip() + labs(title = "Relative Importance of Variables for Decision Tree", x = 'Variable', y = 'Relative Importance')

# get probabilities for ROC curve
# A1broc_tree = predict(balancedA1_rpart, testmodel_A1, type = "prob")
# print(A1broc_tree)


#############################################
#   Random Forest (balanced 1-1 model A1)
#############################################
library(randomForest)


####Balanced dataset
#build a forest with 1000 trees, mtry is recommended as sqrt of variables. Hence mtry=4
st = Sys.time()
set.seed(500)
balancedA1_rf <- randomForest(DIQ010A~., balancedtrain_A1,
                              ntree = 1000,
                              mtry = 4,
                              importance = TRUE,
                              cutoff=c(0.5,1-0.5),
                              na.action=na.exclude)
Sys.time()-st #19.75984 secs
plot(balancedA1_rf)
#error stabilies at ntree = 200
st=Sys.time()
tb <- tuneRF(balancedtrain_A1[,-6], balancedtrain_A1[,6],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 200,
            trace = TRUE,
            improve = 0.05)
Sys.time()-st #1.239108 mins

balancedtrain_A1[,-6]

# optimum mtry=18

st = Sys.time()
set.seed(500)
balancedA1_rf <- randomForest(DIQ010A~., balancedtrain_A1,
                              ntree = 200,
                              mtry = 18,
                              importance = TRUE,
                              cutoff=c(0.5,1-0.5),
                              na.action=na.exclude)
Sys.time()-st #31.42851 secs

# Test model on trainset and check accuracy with confusion matrix.
A1btrain_rf <- predict(balancedA1_rf, newdata = balancedtrain_A1, type = "response")
confusionMatrix(data = A1btrain_rf, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of training set is 1

# Perform prediction on testset and look at confusion matrix.
A1btest_rf <- predict(balancedA1_rf, newdata = testmodel_A1, type = "response")
confusionMatrix(data = A1btest_rf, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of test set is 84.5%

A1btest_rfprob = predict(balancedA1_rf, newdata = testmodel_A1, type = "prob")

varImp(balancedA1_rf)

varimp_brf = as.data.frame(varImp(balancedA1_rf)) %>%
  `colnames<-`(c("importance","importance2")) %>%
  select(-importance2) %>%
  rownames_to_column("Variable")
varimp_rf %>%
  ggplot(aes(x = reorder(Variable, importance), y = importance))+ geom_col() +
  coord_flip() + labs(title = "Relative Importance of Variables for Random Forest", x = 'Variable', y = 'Relative Importance')

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
  rf_maxtrees = train(DIQ010A~.,
                      data = balancedtrain_A1,
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

print(rf_maxtrees)

# Best tuning parameter
rf_maxtrees$bestTune
#mtry = 4
#ntree = 2000

# Test model on trainset and check accuracy with confusion matrix.
A1btrain_rf <- predict(rf_maxtrees, newdata = balancedtrain_A1, type = "raw")
confusionMatrix(data = A1btrain_rf, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of training set is 1

# Perform prediction on testset and look at confusion matrix.
A1btest_rf <- predict(rf_maxtrees, newdata = testmodel_A1, type = "raw")
confusionMatrix(data = A1btest_rf, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of test set is 84.3%

varImp(rf_maxtrees)

# Install
install.packages("wesanderson")
# Load
library(wesanderson)

varImp(rf_maxtrees) %>%
  ggplot(aes(x = reorder(Variable, importance), y = importance))+ geom_col() +
  coord_flip() + labs(title = "Relative Importance of Variables for Random Forest", x = 'Variable', y = 'Relative Importance')


#################################################
#   Tree Extreme Gradient Boosting (model A1)
#################################################

# Fit the model on the training set
set.seed(500)
xgbtrees = train(DIQ010A~.,
                      data = balancedtrain_A1,
                      method = "xgbTree",
                      trControl = trainControl(method = "cv",
                                               number = 10,
                                               classProbs = FALSE,
                                               savePredictions = "final"))

print(xgbtrees)


#Error: The tuning parameter grid should have columns nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample
# Best tuning parameter
xgbtrees$bestTune
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 44     100         3 0.3     0              0.6                1         1

# test model on trainset and check accuracy with confusion matrix.
A1b_trainxgbtree = predict(xgbtrees, balancedtrain_A1, type = "raw")
confusionMatrix(A1b_trainxgbtree, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 99%

# Perform prediction on testset and look at confusion matrix.
A1b_testxgbtree = predict(xgbtrees, testmodel_A1, type = "raw")
confusionMatrix(A1b_testxgbtree, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 76.37%

varImp(xgbtrees)

varImp(xgbtrees) %>%
  ggplot(aes(x = reorder(Variable, importance), y = importance))+ geom_col() +
  coord_flip() + labs(title = "Relative Importance of Variables for Decision Tree", x = 'Variable', y = 'Relative Importance')

##################################################
#  Stochastic Gradient Boosted Trees (model A1)
##################################################

library(gbm)

trainControl <- trainControl(method="cv", number=10)

set.seed(500)
gbm.caret <- train(DIQ010A ~ .
                   , data=balancedtrain_A1
                   #, distribution="gaussian"
                   , method="gbm"
                   , trControl=trainControl
                   , verbose=FALSE
                   #, tuneGrid=caretGrid
                   #, metric="Accuracy"
                   # , bag.fraction=0.75
)

print(gbm.caret)

# test model on trainset and check accuracy with confusion matrix.
A1b_train_gbm <- predict(gbm.caret, newdata=balancedtrain_A1, type="raw")
confusionMatrix(A1b_train_gbm, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 96.98%

# Perform prediction on testset and look at confusion matrix.
A1b_test_gbm <- predict(gbm.caret, newdata=testmodel_A1, type="raw")
confusionMatrix(A1b_test_gbm, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 82.59%

varImp(gbm.caret)

################################
#  AdaBoost Trees (model A1)
################################

library(fastAdaboost)

set.seed(500)
trainControl <- trainControl(method="cv", number=10)
Grid <- expand.grid(maxdepth=25,nu=2,iter=100)

adaboost_caret = train(DIQ010A ~., 
                       data= balancedtrain_A1, 
                       method="ada", 
                       trControl= trainControl,
                       #tuneGrid=Grid
                      )


print(adaboost_caret)

# test model on trainset and check accuracy with confusion matrix.
A1b_train_ada <- predict(adaboost_caret, newdata=balancedtrain_A1, type="raw")
confusionMatrix(A1b_train_ada, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 96.17%

# Perform prediction on testset and look at confusion matrix.
A1b_test_ada <- predict(adaboost_caret, newdata=testmodel_A1, type="raw")
confusionMatrix(A1b_test_ada, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 79.6%

varImp(adaboost_caret)


###################################
#   Neural Networks (model A1)
###################################

library(neuralnet)

# train model
set.seed(500)
st = Sys.time()
nnets <- train(DIQ010A~., data = balancedtrain_A1, method='nnet', trace = FALSE,
                  #Grid of tuning parameters to try:
                  tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))
Sys.time() - st
#29.07613 mins

# show neural network result
print(nnets)
nnets[["finalModel"]] #a 73-9-1 network with 676 weights
plot(nnets)

# test model on training set
# nnA1b_train <- subset(balancedtrain_A1, select = -c(DIQ010A))
predictNN_A1b <- predict(nnets, balancedtrain_A1, type = "raw")
confusionMatrix(data = predictNN_A1b, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
#Accuracy : 99.77%

# test model on test set
# nnA1b_test <- subset(testmodel_A1, select = -c(DIQ010A))
predictNN_A1b_test <- predict(nnets, testmodel_A1, type = "raw")
confusionMatrix(data = predictNN_A1b_test, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
#Accuracy : 80.6%

# show relative importance
varImp(nnets)

varImp(nnets) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")

#######################
#   SVM (model A1)
#######################

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(500)

svm_Linear <- train(DIQ010A ~., data = balancedtrain_A1, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

#check the result of our train() method
print(svm_Linear)

# test model on training set
svmA1b_train <- predict(svm_Linear, balancedtrain_A1, type = "raw")
confusionMatrix(data = svmA1b_train, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
#Accuracy : 89.66%

# test model on test set
svmA1b_test <- predict(svm_Linear, testmodel_A1, type = "raw")
confusionMatrix(data = svmA1b_test, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
#Accuracy : 85.57%

# show relative importance
varImp(svm_Linear)

varImp(svm_Linear) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")


#####SVM grid

grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(500)
svm_Linear_Grid <- train(DIQ010A ~., data = balancedtrain_A1, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)

#There were 50 or more warnings

#check the result of our train() method
svm_Linear_Grid

# test model on training set
svmA1b_trainGrid <- predict(svm_Linear_Grid, balancedtrain_A1, type = "raw")
confusionMatrix(data = svmA1b_trainGrid, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
#Accuracy : 89.9%

# test model on test set
svmA1b_testGrid <- predict(svm_Linear_Grid, testmodel_A1, type = "raw")
confusionMatrix(data = svmA1b_testGrid, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
#Accuracy : 84.08%

# show relative importance
varImp(svm_Linear_Grid)

varImp(svm_Linear_Grid) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")

plot(svm_Linear_Grid)

#####SVM Radial

svmR.a1 <- train(DIQ010A ~., data = balancedtrain_A1, method = "svmRadial",
              trControl = trainControl("cv", number = 10),
              preProcess = c("center","scale"),
              tuneLength = 10
)

# Print the best tuning parameter sigma and C that
# maximizes model accuracy
svmR.a1$bestTune

# test model on test set
balA1_svmR <- predict(svmR.a1, testmodel_A1, type = "raw")
confusionMatrix(data = balA1_svmR, reference = testmodel_A1$DIQ010A, 
                #mode="prec_recall"
                )

#####SVM Poly

svmP.a1 <- train(DIQ010A ~., data = balancedtrain_A1, method = "svmPoly",
              trControl = trainControl("cv", number = 10),
              preProcess = c("center","scale"),
              tuneLength = 4
)

# Print the best tuning parameter sigma and C that
# maximizes model accuracy
svmP.a1$bestTune

# test model on test set
balA1_svmP <- predict(svmP.a1, testmodel_A1, type = "raw")
confusionMatrix(data = balA1_svmP, reference = testmodel_A1$DIQ010A, 
                #mode="prec_recall"
                )

###################################
#   nb/Naive Bayes (model A1)
###################################

library(klaR)

# train model
set.seed(500)
# set up 10-fold cross validation procedure
trainControl <- trainControl(method="cv", number=10)

# set up tuning grid
search_grid <- expand.grid(usekernel = c(TRUE, FALSE), fL = 0:5, adjust = seq(0, 5, by = 1))


# train model with(out) search grid
nb.caret <- train(DIQ010A ~., 
                  data = balancedtrain_A1,
                  method = "nb",
                  trControl = trainControl,
                  tuneGrid = search_grid,
                  #preProc = c("BoxCox", "center", "scale", "pca")
)

print(nb.caret)

# top 5 models
nb.caret$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# test model on trainset and check accuracy with confusion matrix.
A1b_train_nb <- predict(nb.caret, newdata=balancedtrain_A1, type="raw")
confusionMatrix(A1b_train_nb, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 86.53%

# Perform prediction on testset and look at confusion matrix.
A1b_test_nb <- predict(nb.caret, newdata=testmodel_A1, type="raw")
confusionMatrix(A1b_test_nb, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of trainset is 83.83%

varImp(nb.caret)

###########################################
#   naive_bayes/Naive Bayes (model A1)
###########################################

library(naivebayes)

# train model
set.seed(500)
# set up 10-fold cross validation procedure
trainControl <- trainControl(method="cv", number=10)


# train model
naivebayes <- train(DIQ010A ~.,
                    data = balancedtrain_A1,
                    method = "naive_bayes",
                    trControl = trainControl,
                    #preProc = c("center", "scale")
)
print(naivebayes)

# test model on trainset and check accuracy with confusion matrix.
A1b_train_naivebayes <- predict(naivebayes, newdata=balancedtrain_A1, type="raw")
confusionMatrix(A1b_train_naivebayes, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
A1b_test_naivebayes <- predict(naivebayes, newdata=testmodel_A1, type="raw")
confusionMatrix(A1b_test_naivebayes, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )

varImp(naivebayes)

##########################
#    k-NN (model A1)
##########################

# prepare training scheme
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model 
library(kknn)
knn_a1 <- train(DIQ010A~., data=balancedtrain_A1, method="kknn", preProcess="scale", trControl=control1)

print(knn_a1)

# test model on trainset and check accuracy with confusion matrix.
A1b_train_knn <- predict(knn_a1, newdata=balancedtrain_A1, type="raw")
confusionMatrix(A1b_train_knn, reference = balancedtrain_A1$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
A1b_test_knn <- predict(knn_a1, newdata=testmodel_A1, type="raw")
confusionMatrix(A1b_test_knn, reference = testmodel_A1$DIQ010A)

A1b_test_knn_prob <- predict(knn_a1, newdata=testmodel_A1, type="prob")
confusionMatrix(A1b_test_knn_prob, reference = testmodel_A1$DIQ010A
                #, mode="prec_recall"
                )

# estimate variable importance
importance_knn <- varImp(knn_a1, scale=FALSE)

# summarize importance
print(importance_knn)

varImp(knn_a1)

varImp(knn_a1) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")

##################################
#     PR/ROC-AUC evaluation
##################################

#names of models (train())
# diabetesA1_glm, logitboost.caret, 
# balancedA1_rpart, rf_maxtrees, xgbtrees, gbm.caret, adaboost_caret, 
# nnets, svm_Linear, svm_Linear_Grid, nb.caret, naivebayes, knn_a1

library(MLeval)
# #for caret models only
# roc <- evalm(list(logitboost.caret),gnames=c('logboost'))
# # Error in evalm(list(logitboost.caret, rf_maxtrees, xgbtrees, gbm.caret),  : 
# # No probabilities found in Caret output

foreval = cbind(DIQ010A = testmodel_A1$DIQ010A,
                pred_glm = diabetesA1_glm_test,
                pred_logboost = A1b_test_lb,
                pred_dt = A1b_testtree, 
                pred_rf = A1btest_rf,
                pred_xgb = A1b_testxgbtree,
                pred_gbm = A1b_test_gbm,
                pred_ada = A1b_test_ada,
                pred_nnets = predictNN_A1b_test,
                pred_svm = svmA1b_test,
                pred_svmg = svmA1b_testGrid,
                pred_nb = A1b_test_nb,
                pred_naivebayes = A1b_test_naivebayes,
                pred_knn = A1b_test_knn)

write.csv(foreval, "foreval.csv", row.names = F)

foreval = read.csv("foreval.csv", as.is = TRUE)

medianLogBoost = median(foreval$pred_logboost, na.rm = TRUE)
foreval$pred_logboost = replace_na(foreval$pred_logboost, medianLogBoost)
sum(is.na(foreval$pred_logboost))

#ROC
library(pROC)

ROC_glm = roc(foreval$pred_glm, foreval$DIQ010A)
plot(ROC_glm, print.auc = TRUE, col = 'blue')
auc(ROC_glm)

ROC_logboost = roc(foreval$pred_logboost, foreval$DIQ010A)
plot(ROC_logboost, print.auc = TRUE, col = 'blue')
auc(ROC_logboost)

ROC_dt = roc(foreval$pred_dt, foreval$DIQ010A)
plot(ROC_dt, print.auc = TRUE, col = 'blue')
auc(ROC_dt)

ROC_rf = roc(foreval$pred_rf, foreval$DIQ010A)
plot(ROC_rf, print.auc = TRUE, col = 'blue')
auc(ROC_rf)

ROC_xgb = roc(foreval$pred_xgb, foreval$DIQ010A)
plot(ROC_xgb, print.auc = TRUE, col = 'blue')
auc(ROC_xgb)

ROC_gbm = roc(foreval$pred_gbm, foreval$DIQ010A)
plot(ROC_gbm, print.auc = TRUE, col = 'blue')
auc(ROC_gbm)

ROC_ada = roc(foreval$pred_ada, foreval$DIQ010A)
plot(ROC_ada, print.auc = TRUE, col = 'blue')
auc(ROC_ada)

ROC_nnets = roc(foreval$pred_nnets, foreval$DIQ010A)
plot(ROC_nnets, print.auc = TRUE, col = 'blue')
auc(ROC_nnets)

ROC_svm = roc(foreval$pred_svm, foreval$DIQ010A)
plot(ROC_svm, print.auc = TRUE, col = 'blue')
auc(ROC_svm)

ROC_svmg = roc(foreval$pred_svmg, foreval$DIQ010A)
plot(ROC_svmg, print.auc = TRUE, col = 'blue')
auc(ROC_svmg)

ROC_nb = roc(foreval$pred_nb, foreval$DIQ010A)
plot(ROC_nb, print.auc = TRUE, col = 'blue')
auc(ROC_nb)

ROC_naivebayes = roc(foreval$pred_naivebayes, foreval$DIQ010A)
plot(ROC_naivebayes, print.auc = TRUE, col = 'blue')
auc(ROC_naivebayes)

ROC_knn = roc(foreval$pred_knn, foreval$DIQ010A)
plot(ROC_knn, print.auc = TRUE, col = 'blue')
auc(ROC_knn)

#top 4 ROC-AUC
plot(ROC_naivebayes, print.auc = TRUE, print.auc.y = 0.4, col = 'blue')
plot(ROC_ada, print.auc = TRUE, print.auc.y = 0.5, col = 'green', add = TRUE)
plot(ROC_svm, print.auc = TRUE, col = "purple", print.auc.y = 0.6, add = TRUE)
plot(ROC_rf, print.auc = TRUE, col = "red", print.auc.y = 0.7, add = TRUE)

#PR

library(PRROC)
pr_glm <- pr.curve(scores.class0 = foreval$pred_glm, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_glm)
plot(pr_glm, legend = FALSE)

pr_logboost <- pr.curve(scores.class0 = foreval$pred_logboost, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_logboost)
plot(pr_logboost, legend = FALSE)

pr_dt <- pr.curve(scores.class0 = foreval$pred_dt, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_dt)
plot(pr_dt, legend = FALSE)

pr_rf <- pr.curve(scores.class0 = foreval$pred_rf, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_rf)
plot(pr_rf, legend = FALSE)

pr_xgb <- pr.curve(scores.class0 = foreval$pred_xgb, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_xgb)
plot(pr_xgb, legend = FALSE)

pr_gbm <- pr.curve(scores.class0 = foreval$pred_gbm, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_gbm)
plot(pr_gbm, legend = FALSE)

pr_ada <- pr.curve(scores.class0 = foreval$pred_ada, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_ada)
plot(pr_ada, legend = FALSE)

pr_nnets <- pr.curve(scores.class0 = foreval$pred_nnets, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_nnets)
plot(pr_nnets, legend = FALSE)

pr_svm <- pr.curve(scores.class0 = foreval$pred_svm, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_svm)
plot(pr_svm, legend = FALSE)

pr_svmg <- pr.curve(scores.class0 = foreval$pred_svmg, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_svmg)
plot(pr_svmg, legend = FALSE)

pr_nb <- pr.curve(scores.class0 = foreval$pred_nb, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_nb)
plot(pr_nb, legend = FALSE)

pr_naivebayes <- pr.curve(scores.class0 = foreval$pred_naivebayes, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_naivebayes)
plot(pr_naivebayes, legend = FALSE)

pr_knn <- pr.curve(scores.class0 = foreval$pred_knn, scores.class1 = foreval$DIQ010A, curve=T)
print(pr_knn)
plot(pr_knn, legend = FALSE)

#top 4 plots
plot(pr_dt, max.plot = TRUE, min.plot = TRUE, rand.plot = TRUE,
     fill.area = T, color=2, auc.main = FALSE, main = "Top 4 PR-AUC")

plot(pr_nnets, add = TRUE, color = 3)
plot(pr_svm, add = TRUE, color = 8)
plot(pr_naivebayes, add = TRUE, color = 5)

##################################
#     LogitBoost (model A2)
##################################

#lower recall, higher accuracy
trainControl <- trainControl(method="cv", number=10)
logitboost.caretA2 <- train(DIQ010A~ .,
                          data=balancedtrain_A2, 
                          method = "LogitBoost", 
                          trControl = trainControl,
                          #metric = "ROC", 
                          #preProc = c("center", "scale")
)

#higher recall, lower accuracy
cctrlR_A2 <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.caretA2 <- train(DIQ010A~ .,
                          data=balancedtrain_A2, 
                          method = "LogitBoost", 
                          trControl = cctrlR_A2,
                          tuneLength = 4)

print(logitboost.caretA2)

# test model on trainset and check accuracy with confusion matrix.
A2b_train_lb <- predict(logitboost.caretA2, newdata = balancedtrain_A2, type = "raw")

#confusion matrix
confusionMatrix(A2b_train_lb, balancedtrain_A2$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
A2b_test_lb <- predict(logitboost.caretA2, newdata = testmodel_A2, type = "raw")

# confusion matrix
confusionMatrix(A2b_test_lb, testmodel_A2$DIQ010A
                #, mode="prec_recall"
                )

varImp(logitboost.caretA2)

################################
#   Random Forest (model A2)
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
  rf_maxtreesA2 = train(DIQ010A~.,
                      data = balancedtrain_A2,
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

print(rf_maxtreesA2)

# Best tuning parameter
rf_maxtreesA2$bestTune
#mtry = 3

# Test model on trainset and check accuracy with confusion matrix.
A2btrain_rf <- predict(rf_maxtreesA2, newdata = balancedtrain_A2, type = "raw")
confusionMatrix(data = A2btrain_rf, reference = balancedtrain_A2$DIQ010A
                #, mode="prec_recall"
                )
# accuracy of training set is 1

# Perform prediction on testset and look at confusion matrix.
A2btest_rf <- predict(rf_maxtreesA2, newdata = testmodel_A2, type = "raw")
confusionMatrix(data = A2btest_rf, reference = testmodel_A2$DIQ010A
                #, mode="prec_recall"
                )


varImp(rf_maxtreesA2)


#################################################
#   Tree Extreme Gradient Boosting (model A2)
#################################################

# Fit the model on the training set
set.seed(500)
xgbtreesA2 = train(DIQ010A~.,
                 data = balancedtrain_A2,
                 method = "xgbTree",
                 trControl = trainControl(method = "cv",
                                          number = 10,
                                          classProbs = FALSE,
                                          savePredictions = "final"))

print(xgbtreesA2)

# Best tuning parameter
xgbtreesA2$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 50     100         3 0.3     0              0.8                1      0.75

# test model on trainset and check accuracy with confusion matrix.
A2b_trainxgbtree = predict(xgbtreesA2, balancedtrain_A2, type = "raw")
confusionMatrix(A2b_trainxgbtree, reference = balancedtrain_A2$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
A2b_testxgbtree = predict(xgbtreesA2, testmodel_A2, type = "raw")
confusionMatrix(A2b_testxgbtree, reference = testmodel_A2$DIQ010A
                #, mode="prec_recall"
                )

varImp(xgbtreesA2)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_A2 = cbind(DIQ010A = testmodel_A2$DIQ010A,
                        pred_lb = A2b_test_lb,
                        pred_rf = A2btest_rf,
                        pred_xgb = A2b_testxgbtree)

write.csv(foreval_A2, "foreval_A2.csv", row.names = F)

foreval_A2 = read.csv("foreval_A2.csv", as.is = TRUE)


A2ROC_lb = roc(foreval_A2$pred_lb, foreval_A2$DIQ010A)
auc(A2ROC_lb)

A2ROC_rf = roc(foreval_A2$pred_rf, foreval_A2$DIQ010A)
auc(A2ROC_rf)

A2ROC_xgb = roc(foreval_A2$pred_xgb, foreval_A2$DIQ010A)
auc(A2ROC_xgb)


#############################################
#  Sensitivity analysis - Physical Activity
#############################################
#includes both DIQ010A and DIQ010B
#Physical Activity variables
#to read it

train_sa1 = readRDS("train_sa1.RDS")

balancedtrain_sa1 <- SMOTE(DIQ010A ~., train_sa1, perc.over = 600, k = 5, perc.under = 116.6667)
balancedtrain_sa1 = subset(balancedtrain_sa1, select = c(-DIQ010B))

saveRDS(balancedtrain_sa1, file = "balancedtrain_sa1.RDS")

#to read it

balancedtrain_sa1 = readRDS("balancedtrain_sa1.RDS")

#to read it

test_sa1 = readRDS("test_sa1.RDS")
a_test_sa1 = subset(test_sa1, select = c(-DIQ010B))
saveRDS(a_test_sa1, file = "a_test_sa1.RDS")

#to read it

a_test_sa1 = readRDS("a_test_sa1.RDS")


#################################
#      LogitBoost (sa1)
#################################
library(caTools)


#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.sa1 <- train(DIQ010A~ .,
                          data=balancedtrain_sa1, 
                          method = "LogitBoost", 
                          trControl = cctrlR,
                          tuneLength = 4)

print(logitboost.sa1)

# test model on trainset and check accuracy with confusion matrix.
sa1_train_lb <- predict(logitboost.sa1, newdata = balancedtrain_sa1, type = "raw")

#confusion matrix
confusionMatrix(sa1_train_lb, balancedtrain_sa1$DIQ010A,
                #mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
sa1_test_lb <- predict(logitboost.sa1, newdata = a_test_sa1, type = "raw")

# confusion matrix
confusionMatrix(sa1_test_lb, a_test_sa1$DIQ010A,
                #mode="prec_recall"
)

varImp(logitboost.sa1)

################################
#   Random Forest (sa1)
################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_sa1 = train(DIQ010A~.,
                        data = balancedtrain_sa1,
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

print(rf_maxtrees_sa1)

# Test model on trainset and check accuracy with confusion matrix.
sa1_train_rf <- predict(rf_maxtrees_sa1, newdata = balancedtrain_sa1, type = "raw")
confusionMatrix(data = sa1_train_rf, reference = balancedtrain_sa1$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa1_test_rf <- predict(rf_maxtrees_sa1, newdata = a_test_sa1, type = "raw")
confusionMatrix(data = sa1_test_rf, reference = a_test_sa1$DIQ010A
                #, mode="prec_recall"
                )


varImp(rf_maxtrees_sa1)


##############################################
#   Tree Extreme Gradient Boosting (sa1)
##############################################

# Fit the model on the training set
set.seed(500)
xgbtrees_sa1 = train(DIQ010A~.,
                   data = balancedtrain_sa1,
                   method = "xgbTree",
                   trControl = trainControl(method = "cv",
                                            number = 10,
                                            classProbs = FALSE,
                                            savePredictions = "final"))

print(xgbtrees_sa1)

# Best tuning parameter
xgbtrees_sa1$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 39     150         3 0.3     0              0.6                1       0.5

# test model on trainset and check accuracy with confusion matrix.
sa1_trainxgbtree = predict(xgbtrees_sa1, balancedtrain_sa1, type = "raw")
confusionMatrix(sa1_trainxgbtree, reference = balancedtrain_sa1$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa1_testxgbtree = predict(xgbtrees_sa1, a_test_sa1, type = "raw")
confusionMatrix(sa1_testxgbtree, reference = a_test_sa1$DIQ010A
                #, mode="prec_recall"
                )

varImp(xgbtrees_sa1)

###################################
#    Neural Networks (sa1)
###################################

library(neuralnet)

# train model
set.seed(500)
nnets.sa1 <- train(DIQ010A~., data = balancedtrain_sa1, method='nnet', trace = FALSE,
               #Grid of tuning parameters to try:
               tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))

# show neural network result
print(nnets.sa1)
nnets.sa1[["finalModel"]] #a 73-9-1 network with 676 weights
plot(nnets.sa1)

# test model on training set
# nnA1b_train <- subset(balancedtrain_A1, select = -c(DIQ010A))
predictNN_sa1 <- predict(nnets.sa1, balancedtrain_sa1, type = "raw")
confusionMatrix(data = predictNN_sa1, reference = balancedtrain_sa1$DIQ010A
                #, mode="prec_recall"
)

# test model on test set
# nnA1b_test <- subset(testmodel_A1, select = -c(DIQ010A))
predictNN_sa1_test <- predict(nnets.sa1, a_test_sa1, type = "raw")
confusionMatrix(data = predictNN_sa1_test, reference = a_test_sa1$DIQ010A
                #, mode="prec_recall"
)

# show relative importance
varImp(nnets.sa1)

varImp(nnets.sa1) %>%
  ggplot(aes(x = names1, y = overall))+
  geom_bar(stat ='identity') + coord_flip() +
  labs(title = "Relative Importance of Variables", x = "Variable", y = "Relative Importance")

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_sa1 = cbind(DIQ010A = a_test_sa1$DIQ010A,
                   pred_lb = sa1_test_lb,
                   pred_rf = sa1_test_rf,
                   pred_xgb = sa1_testxgbtree,
                   pre_nn = predictNN_sa1_test)

write.csv(foreval_sa1, "foreval_sa1.csv", row.names = F)

foreval_sa1 = read.csv("foreval_sa1.csv", as.is = TRUE)


sa1ROC_lb = roc(foreval_sa1$pred_lb, foreval_sa1$DIQ010A)
auc(sa1ROC_lb)

sa1ROC_rf = roc(foreval_sa1$pred_rf, foreval_sa1$DIQ010A)
auc(sa1ROC_rf)

sa1ROC_xgb = roc(foreval_sa1$pred_xgb, foreval_sa1$DIQ010A)
auc(sa1ROC_xgb)

sa1ROC_nn = roc(foreval_sa1$pre_nn, foreval_sa1$DIQ010A)
auc(sa1ROC_nn)



######################################
#  Sensitivity analysis - Smoking
######################################

#Smoking variable - SMQ915A
#to read it

train_sa2 = readRDS("train_sa2.RDS")

balancedtrain_sa2 <- SMOTE(DIQ010A ~., train_sa2, perc.over = 600, k = 5, perc.under = 116.6667)
balancedtrain_sa2 = subset(balancedtrain_sa2, select = c(-DIQ010B))

saveRDS(balancedtrain_sa2, file = "balancedtrain_sa2.RDS")

#to read it

balancedtrain_sa2 = readRDS("balancedtrain_sa2.RDS")

#to read it

test_sa2 = readRDS("test_sa2.RDS")
a_test_sa2 = subset(test_sa2, select = c(-DIQ010B))
saveRDS(a_test_sa2, file = "a_test_sa2.RDS")

#to read it

a_test_sa2 = readRDS("a_test_sa2.RDS")

#################################
#      LogitBoost (sa2)
#################################
library(caTools)


#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.sa2 <- train(DIQ010A~ .,
                        data=balancedtrain_sa2, 
                        method = "LogitBoost", 
                        trControl = cctrlR,
                        tuneLength = 4)

print(logitboost.sa2)

# test model on trainset and check accuracy with confusion matrix.
sa2_train_lb <- predict(logitboost.sa2, newdata = balancedtrain_sa2, type = "raw")

#confusion matrix
confusionMatrix(sa2_train_lb, balancedtrain_sa2$DIQ010A,
                #mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
sa2_test_lb <- predict(logitboost.sa2, newdata = a_test_sa2, type = "raw")

# confusion matrix
confusionMatrix(sa2_test_lb, a_test_sa2$DIQ010A,
                #mode="prec_recall"
)

varImp(logitboost.sa2)

################################
#   Random Forest (sa2)
################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_sa2 = train(DIQ010A~.,
                          data = balancedtrain_sa2,
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

print(rf_maxtrees_sa2)

# Test model on trainset and check accuracy with confusion matrix.
sa2_train_rf <- predict(rf_maxtrees_sa2, newdata = balancedtrain_sa2, type = "raw")
confusionMatrix(data = sa2_train_rf, reference = balancedtrain_sa2$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa2_test_rf <- predict(rf_maxtrees_sa2, newdata = a_test_sa2, type = "raw")
confusionMatrix(data = sa2_test_rf, reference = a_test_sa2$DIQ010A
                #, mode="prec_recall"
                )


varImp(rf_maxtrees_sa2)


##############################################
#   Tree Extreme Gradient Boosting (sa2)
##############################################

# Fit the model on the training set
set.seed(500)
xgbtrees_sa2 = train(DIQ010A~.,
                     data = balancedtrain_sa2,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = FALSE,
                                              savePredictions = "final"))

print(xgbtrees_sa2)

# Best tuning parameter
xgbtrees_sa2$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 42     150         3 0.3     0              0.6                1      0.75

# test model on trainset and check accuracy with confusion matrix.
sa2_trainxgbtree = predict(xgbtrees_sa2, balancedtrain_sa2, type = "raw")
confusionMatrix(sa2_trainxgbtree, reference = balancedtrain_sa2$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa2_testxgbtree = predict(xgbtrees_sa2, a_test_sa2, type = "raw")
confusionMatrix(sa2_testxgbtree, reference = a_test_sa2$DIQ010A
                #, mode="prec_recall"
                )

varImp(xgbtrees_sa2)

###################################
#    Neural Networks (sa2)
###################################

library(neuralnet)

# train model
set.seed(500)
st = Sys.time()
nnets.sa2 <- train(DIQ010A~., data = balancedtrain_sa2, method='nnet', trace = FALSE,
                   #Grid of tuning parameters to try:
                   tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))
Sys.time() - st
#29.07613 mins

# show neural network result
print(nnets.sa2)
nnets.sa2[["finalModel"]] #a 73-9-1 network with 676 weights
plot(nnets.sa2)

# test model on training set
predictNN_sa2 <- predict(nnets.sa2, balancedtrain_sa2, type = "raw")
confusionMatrix(data = predictNN_sa2, reference = balancedtrain_sa2$DIQ010A
                #, mode="prec_recall"
)

# test model on test set
predictNN_sa2_test <- predict(nnets.sa2, a_test_sa2, type = "raw")
confusionMatrix(data = predictNN_sa2_test, reference = a_test_sa2$DIQ010A
                #, mode="prec_recall"
)

# show relative importance
varImp(nnets.sa2)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_sa2 = cbind(DIQ010A = a_test_sa2$DIQ010A,
                    pred_lb = sa2_test_lb,
                    pred_rf = sa2_test_rf,
                    pred_xgb = sa2_testxgbtree,
                    pred_nn = predictNN_sa2_test)

write.csv(foreval_sa2, "foreval_sa2.csv", row.names = F)

foreval_sa2 = read.csv("foreval_sa2.csv", as.is = TRUE)


sa2ROC_lb = roc(foreval_sa2$pred_lb, foreval_sa2$DIQ010A)
auc(sa2ROC_lb)

sa2ROC_rf = roc(foreval_sa2$pred_rf, foreval_sa2$DIQ010A)
auc(sa2ROC_rf)

sa2ROC_xgb = roc(foreval_sa2$pred_xgb, foreval_sa2$DIQ010A)
auc(sa2ROC_xgb)

sa2ROC_nn = roc(foreval_sa2$pred_nn, foreval_sa2$DIQ010A)
auc(sa2ROC_nn)

######################################
# Sensitivity analysis - Cholesterol
######################################

#Cholesterol variables - LBXTCA, LBDHDDA, LBXTRA, LBDLDLA
#to read it

train_sa3 = readRDS("train_sa3.RDS")
balancedtrain_sa3 <- SMOTE(DIQ010A ~., train_sa3, perc.over = 600, k = 5, perc.under = 116.6667)
balancedtrain_sa3 = subset(balancedtrain_sa3, select = c(-DIQ010B))

saveRDS(balancedtrain_sa3, file = "balancedtrain_sa3.RDS")

#to read it

balancedtrain_sa3 = readRDS("balancedtrain_sa3.RDS")

#to read it

test_sa3 = readRDS("test_sa3.RDS")
a_test_sa3 = subset(test_sa3, select = c(-DIQ010B))
saveRDS(a_test_sa3, file = "a_test_sa3.RDS")

#to read it

a_test_sa3 = readRDS("a_test_sa3.RDS")


#remove Cholesterol variables - LBXTC, LBDHDD, LBXTR, LBDLDL
balancedtrain_A1_tc = subset(balancedtrain_A1, select = c(-LBXTC, -LBDHDD, -LBXTR, -LBDLDL))

saveRDS(balancedtrain_A1_tc, file = "balancedtrain_A1_tc.RDS")

#to read it

balancedtrain_A1_tc = readRDS("balancedtrain_A1_tc.RDS")

testmodel_A1_tc = subset(testmodel_A1, select = c(-LBXTC, -LBDHDD, -LBXTR, -LBDLDL))

saveRDS(testmodel_A1_tc, file = "testmodel_A1_tc.RDS")

#to read it

testmodel_A1_tc = readRDS("testmodel_A1_tc.RDS")

#####################################################
#      LogitBoost (model A1 exc Cholesterol)
#####################################################
library(caTools)


#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.tc <- train(DIQ010A~ .,
                        data=balancedtrain_A1_tc, 
                        method = "LogitBoost", 
                        trControl = cctrlR,
                        tuneLength = 4)

print(logitboost.tc)

# test model on trainset and check accuracy with confusion matrix.
tc_train_lb <- predict(logitboost.tc, newdata = balancedtrain_A1_tc, type = "raw")

#confusion matrix
confusionMatrix(tc_train_lb, balancedtrain_A1_tc$DIQ010A,
                #mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
tc_test_lb <- predict(logitboost.tc, newdata = testmodel_A1_tc, type = "raw")

# confusion matrix
confusionMatrix(tc_test_lb, testmodel_A1_tc$DIQ010A,
                #mode="prec_recall"
)

varImp(logitboost.tc)


####################################################
#   Random Forest (model A1 exc Cholesterol)
####################################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_tc = train(DIQ010A~.,
                          data = balancedtrain_A1_tc,
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

print(rf_maxtrees_tc)

# Test model on trainset and check accuracy with confusion matrix.
A1_tc_train_rf <- predict(rf_maxtrees_tc, newdata = balancedtrain_A1_tc, type = "raw")
confusionMatrix(data = A1_tc_train_rf, reference = balancedtrain_A1_tc$DIQ010A
                #, mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
A1_tc_test_rf <- predict(rf_maxtrees_tc, newdata = testmodel_A1_tc, type = "raw")
confusionMatrix(data = A1_tc_test_rf, reference = testmodel_A1_tc$DIQ010A
                #, mode="prec_recall"
)

varImp(rf_maxtrees_tc)

################################################################
#   Tree Extreme Gradient Boosting (model A1 exc Cholesterol)
################################################################

# Fit the model on the training set
set.seed(500)
xgbtrees_tc = train(DIQ010A~.,
                     data = balancedtrain_A1_tc,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = FALSE,
                                              savePredictions = "final"))

print(xgbtrees_tc)

# Best tuning parameter
xgbtrees_tc$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 107     100         3 0.4     0              0.8                1         1

# test model on trainset and check accuracy with confusion matrix.
tc_trainxgbtree = predict(xgbtrees_tc, balancedtrain_A1_tc, type = "raw")
confusionMatrix(tc_trainxgbtree, reference = balancedtrain_A1_tc$DIQ010A
                #, mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
tc_testxgbtree = predict(xgbtrees_tc, testmodel_A1_tc, type = "raw")
confusionMatrix(tc_testxgbtree, reference = testmodel_A1_tc$DIQ010A
                #, mode="prec_recall"
)

varImp(xgbtrees_tc)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_tc = cbind(DIQ010A = testmodel_A1_tc$DIQ010A,
                    pred_lb = tc_test_lb,
                    pred_rf = A1_tc_test_rf,
                    pred_xgb = tc_testxgbtree)

write.csv(foreval_tc, "foreval_tc.csv", row.names = F)

foreval_tc = read.csv("foreval_tc.csv", as.is = TRUE)


tcROC_lb = roc(foreval_tc$pred_lb, foreval_tc$DIQ010A)
auc(tcROC_lb)

tcROC_rf = roc(foreval_tc$pred_rf, foreval_tc$DIQ010A)
auc(tcROC_rf)

tcROC_xgb = roc(foreval_tc$pred_xgb, foreval_tc$DIQ010A)
auc(tcROC_xgb)


#################################
#      LogitBoost (sa3)
#################################
library(caTools)


#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.sa3 <- train(DIQ010A~ .,
                        data=balancedtrain_sa3, 
                        method = "LogitBoost", 
                        trControl = cctrlR,
                        tuneLength = 4)

print(logitboost.sa3)

# test model on trainset and check accuracy with confusion matrix.
sa3_train_lb <- predict(logitboost.sa3, newdata = balancedtrain_sa3, type = "raw")

#confusion matrix
confusionMatrix(sa3_train_lb, balancedtrain_sa3$DIQ010A,
                #mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
sa3_test_lb <- predict(logitboost.sa3, newdata = a_test_sa3, type = "raw")

# confusion matrix
confusionMatrix(sa3_test_lb, a_test_sa3$DIQ010A,
                #mode="prec_recall"
)

varImp(logitboost.sa3)

################################
#   Random Forest (sa3)
################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_sa3 = train(DIQ010A~.,
                          data = balancedtrain_sa3,
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

print(rf_maxtrees_sa3)

# Test model on trainset and check accuracy with confusion matrix.
sa3_train_rf <- predict(rf_maxtrees_sa3, newdata = balancedtrain_sa3, type = "raw")
confusionMatrix(data = sa3_train_rf, reference = balancedtrain_sa3$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa3_test_rf <- predict(rf_maxtrees_sa3, newdata = a_test_sa3, type = "raw")
confusionMatrix(data = sa3_test_rf, reference = a_test_sa3$DIQ010A
                #, mode="prec_recall"
                )


varImp(rf_maxtrees_sa3)


##############################################
#   Tree Extreme Gradient Boosting (sa3)
##############################################

# Fit the model on the training set
set.seed(500)
xgbtrees_sa3 = train(DIQ010A~.,
                     data = balancedtrain_sa3,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = FALSE,
                                              savePredictions = "final"))

print(xgbtrees_sa3)

# Best tuning parameter
xgbtrees_sa3$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 107     100         3 0.4     0              0.8                1         1

# test model on trainset and check accuracy with confusion matrix.
sa3_trainxgbtree = predict(xgbtrees_sa3, balancedtrain_sa3, type = "raw")
confusionMatrix(sa3_trainxgbtree, reference = balancedtrain_sa3$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa3_testxgbtree = predict(xgbtrees_sa3, a_test_sa3, type = "raw")
confusionMatrix(sa3_testxgbtree, reference = a_test_sa3$DIQ010A
                #, mode="prec_recall"
                )

varImp(xgbtrees_sa3)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_sa3 = cbind(DIQ010A = a_test_sa3$DIQ010A,
                    pred_lb = sa3_test_lb,
                    pred_rf = sa3_test_rf,
                    pred_xgb = sa3_testxgbtree)

write.csv(foreval_sa3, "foreval_sa3.csv", row.names = F)

foreval_sa3 = read.csv("foreval_sa3.csv", as.is = TRUE)


sa3ROC_lb = roc(foreval_sa3$pred_lb, foreval_sa3$DIQ010A)
auc(sa3ROC_lb)

sa3ROC_rf = roc(foreval_sa3$pred_rf, foreval_sa3$DIQ010A)
auc(sa3ROC_rf)

sa3ROC_xgb = roc(foreval_sa3$pred_xgb, foreval_sa3$DIQ010A)
auc(sa3ROC_xgb)


##########################################
# Sensitivity analysis - Blood Pressure
##########################################

#Blood Pressure variables - BPXSYaveC, BPXDIaveC
#to read it

train_sa4 = readRDS("train_sa4.RDS")
balancedtrain_sa4 <- SMOTE(DIQ010A ~., train_sa4, perc.over = 600, k = 5, perc.under = 116.6667)
balancedtrain_sa4 = subset(balancedtrain_sa4, select = c(-DIQ010B))

saveRDS(balancedtrain_sa4, file = "balancedtrain_sa4.RDS")

#to read it

balancedtrain_sa4 = readRDS("balancedtrain_sa4.RDS")

#to read it

test_sa4 = readRDS("test_sa4.RDS")
a_test_sa4 = subset(test_sa4, select = c(-DIQ010B))
saveRDS(a_test_sa4, file = "a_test_sa4.RDS")

#to read it

a_test_sa4 = readRDS("a_test_sa4.RDS")

#################################
#      LogitBoost (sa4)
#################################
library(caTools)


#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.sa4 <- train(DIQ010A~ .,
                        data=balancedtrain_sa4, 
                        method = "LogitBoost", 
                        trControl = cctrlR,
                        tuneLength = 4)

print(logitboost.sa4)

# test model on trainset and check accuracy with confusion matrix.
sa4_train_lb <- predict(logitboost.sa4, newdata = balancedtrain_sa4, type = "raw")

#confusion matrix
confusionMatrix(sa4_train_lb, balancedtrain_sa4$DIQ010A,
                #mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
sa4_test_lb <- predict(logitboost.sa4, newdata = a_test_sa4, type = "raw")

# confusion matrix
confusionMatrix(sa4_test_lb, a_test_sa4$DIQ010A,
                #mode="prec_recall"
)

varImp(logitboost.sa4)

################################
#   Random Forest (sa4)
################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_sa4 = train(DIQ010A~.,
                          data = balancedtrain_sa4,
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

print(rf_maxtrees_sa4)

# Test model on trainset and check accuracy with confusion matrix.
sa4_train_rf <- predict(rf_maxtrees_sa4, newdata = balancedtrain_sa4, type = "raw")
confusionMatrix(data = sa4_train_rf, reference = balancedtrain_sa4$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa4_test_rf <- predict(rf_maxtrees_sa4, newdata = a_test_sa4, type = "raw")
confusionMatrix(data = sa4_test_rf, reference = a_test_sa4$DIQ010A
                #, mode="prec_recall"
                )


varImp(rf_maxtrees_sa4)


##############################################
#   Tree Extreme Gradient Boosting (sa4)
##############################################

# Fit the model on the training set
set.seed(500)
xgbtrees_sa4 = train(DIQ010A~.,
                     data = balancedtrain_sa4,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = FALSE,
                                              savePredictions = "final"))

print(xgbtrees_sa4)

# Best tuning parameter
xgbtrees_sa4$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 50     100         3 0.3     0              0.8                1      0.75

# test model on trainset and check accuracy with confusion matrix.
sa4_trainxgbtree = predict(xgbtrees_sa4, balancedtrain_sa4, type = "raw")
confusionMatrix(sa4_trainxgbtree, reference = balancedtrain_sa4$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa4_testxgbtree = predict(xgbtrees_sa4, a_test_sa4, type = "raw")
confusionMatrix(sa4_testxgbtree, reference = a_test_sa4$DIQ010A
                #, mode="prec_recall"
                )

varImp(xgbtrees_sa4)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_sa4 = cbind(DIQ010A = a_test_sa4$DIQ010A,
                    pred_lb = sa4_test_lb,
                    pred_rf = sa4_test_rf,
                    pred_xgb = sa4_testxgbtree)

write.csv(foreval_sa4, "foreval_sa4.csv", row.names = F)

foreval_sa4 = read.csv("foreval_sa4.csv", as.is = TRUE)


sa4ROC_lb = roc(foreval_sa4$pred_lb, foreval_sa4$DIQ010A)
auc(sa4ROC_lb)

sa4ROC_rf = roc(foreval_sa4$pred_rf, foreval_sa4$DIQ010A)
auc(sa4ROC_rf)

sa4ROC_xgb = roc(foreval_sa4$pred_xgb, foreval_sa4$DIQ010A)
auc(sa4ROC_xgb)


##########################################
#     Sensitivity analysis - BMI
##########################################

#remove height and weight and run RF with only BMIAA in it
balancedtrain_A1_bmi = subset(balancedtrain_A1, select = c(-BMXHT, -BMXWT))

saveRDS(balancedtrain_A1_bmi, file = "balancedtrain_A1_bmi.RDS")

#to read it

balancedtrain_A1_bmi = readRDS("balancedtrain_A1_bmi.RDS")

testmodel_A1_bmi = subset(testmodel_A1, select = c(-BMXHT, -BMXWT))

saveRDS(testmodel_A1_bmi, file = "testmodel_A1_bmi.RDS")

#to read it

testmodel_A1_bmi = readRDS("testmodel_A1_bmi.RDS")

#####################################################
#      LogitBoost (model A1 exc BMXHT and BMXWT)
#####################################################
library(caTools)


#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.bmi <- train(DIQ010A~ .,
                        data=balancedtrain_A1_bmi, 
                        method = "LogitBoost", 
                        trControl = cctrlR,
                        tuneLength = 4)

print(logitboost.bmi)

# test model on trainset and check accuracy with confusion matrix.
bmi_train_lb <- predict(logitboost.bmi, newdata = balancedtrain_A1_bmi, type = "raw")

#confusion matrix
confusionMatrix(bmi_train_lb, balancedtrain_A1_bmi$DIQ010A,
                #mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
bmi_test_lb <- predict(logitboost.bmi, newdata = testmodel_A1_bmi, type = "raw")

# confusion matrix
confusionMatrix(bmi_test_lb, testmodel_A1_bmi$DIQ010A,
                #mode="prec_recall"
)

varImp(logitboost.bmi)


####################################################
#   Random Forest (model A1 exc BMXHT and BMXWT)
####################################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_bmi = train(DIQ010A~.,
                      data = balancedtrain_A1_bmi,
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

print(rf_maxtrees_bmi)

# Best tuning parameter
rf_maxtrees_bmi$bestTune

# Test model on trainset and check accuracy with confusion matrix.
A1_bmi_train_rf <- predict(rf_maxtrees_bmi, newdata = balancedtrain_A1_bmi, type = "raw")
confusionMatrix(data = A1_bmi_train_rf, reference = balancedtrain_A1_bmi$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
A1_bmi_test_rf <- predict(rf_maxtrees_bmi, newdata = testmodel_A1_bmi, type = "raw")
confusionMatrix(data = A1_bmi_test_rf, reference = testmodel_A1_bmi$DIQ010A
                #, mode="prec_recall"
                )

varImp(rf_maxtrees_bmi)

################################################################
# Tree Extreme Gradient Boosting (model A1 exc BMXHT and BMXWT)
################################################################

# Fit the model on the training set
set.seed(500)
xgbtrees_bmi = train(DIQ010A~.,
                    data = balancedtrain_A1_bmi,
                    method = "xgbTree",
                    trControl = trainControl(method = "cv",
                                             number = 10,
                                             classProbs = FALSE,
                                             savePredictions = "final"))

print(xgbtrees_bmi)

# Best tuning parameter
xgbtrees_bmi$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 

# test model on trainset and check accuracy with confusion matrix.
bmi_trainxgbtree = predict(xgbtrees_bmi, balancedtrain_A1_bmi, type = "raw")
confusionMatrix(bmi_trainxgbtree, reference = balancedtrain_A1_bmi$DIQ010A
                #, mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
bmi_testxgbtree = predict(xgbtrees_bmi, testmodel_A1_bmi, type = "raw")
confusionMatrix(bmi_testxgbtree, reference = testmodel_A1_bmi$DIQ010A
                #, mode="prec_recall"
)

varImp(xgbtrees_bmi)

#####################################################
#  Neural Networks (model A1 exc BMXHT and BMXWT)
#####################################################

library(neuralnet)

# train model
set.seed(500)
st = Sys.time()
nnets.bmi <- train(DIQ010A~., data = balancedtrain_A1_bmi, method='nnet', trace = FALSE,
                   #Grid of tuning parameters to try:
                   tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))
Sys.time() - st
#29.07613 mins

# show neural network result
print(nnets.bmi)
nnets.bmi[["finalModel"]] #a 73-9-1 network with 676 weights
plot(nnets.bmi)

# test model on training set
predictNN_bmi <- predict(nnets.bmi, balancedtrain_A1_bmi, type = "raw")
confusionMatrix(data = predictNN_bmi, reference = balancedtrain_A1_bmi$DIQ010A
                #, mode="prec_recall"
)

# test model on test set
predictNN_bmi_test <- predict(nnets.bmi, testmodel_A1_bmi, type = "raw")
confusionMatrix(data = predictNN_bmi_test, reference = testmodel_A1_bmi$DIQ010A
                #, mode="prec_recall"
)

# show relative importance
varImp(nnets.bmi)

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_bmiA = cbind(DIQ010A = testmodel_A1_bmi$DIQ010A,
                    pred_lb = bmi_test_lb,
                    pred_rf = A1_bmi_test_rf,
                    pred_xgb = bmi_testxgbtree,
                    pred_nn = predictNN_bmi_test)

write.csv(foreval_bmiA, "foreval_bmiA.csv", row.names = F)

foreval_bmiA = read.csv("foreval_bmiA.csv", as.is = TRUE)


bmiAROC_lb = roc(foreval_bmiA$pred_lb, foreval_bmiA$DIQ010A)
auc(bmiAROC_lb)

bmiAROC_rf = roc(foreval_bmiA$pred_rf, foreval_bmiA$DIQ010A)
auc(bmiAROC_rf)

bmiAROC_xgb = roc(foreval_bmiA$pred_xgb, foreval_bmiA$DIQ010A)
auc(bmiAROC_xgb)

bmiAROC_nn = roc(foreval_bmiA$pred_nn, foreval_bmiA$DIQ010A)
auc(bmiAROC_nn)

#########################################################################################################################################
#BMI-ordinary variable (exc Height and weight)
#to read it

train_sa5 = readRDS("train_sa5.RDS")
balancedtrain_sa5 <- SMOTE(DIQ010A ~., train_sa5, perc.over = 600, k = 5, perc.under = 116.6667)
balancedtrain_sa5 = subset(balancedtrain_sa5, select = c(-DIQ010B))

saveRDS(balancedtrain_sa5, file = "balancedtrain_sa5.RDS")

#to read it

balancedtrain_sa5 = readRDS("balancedtrain_sa5.RDS")

#to read it

test_sa5 = readRDS("test_sa5.RDS")
a_test_sa5 = subset(test_sa5, select = c(-DIQ010B))
saveRDS(a_test_sa5, file = "a_test_sa5.RDS")

#to read it

a_test_sa5 = readRDS("a_test_sa5.RDS")

#try with and without ht and wt

balancedtrain_sa5_bmi = subset(balancedtrain_sa5, select = c(-BMXWT, -BMXHT))

saveRDS(balancedtrain_sa5_bmi, file = "balancedtrain_sa5_bmi.RDS")

#to read it

balancedtrain_sa5_bmi = readRDS("balancedtrain_sa5_bmi.RDS")

a_test_sa5_bmi = subset(a_test_sa5, select = c(-BMXWT, -BMXHT))
saveRDS(a_test_sa5_bmi, file = "a_test_sa5_bmi.RDS")

#to read it

a_test_sa5_bmi = readRDS("a_test_sa5_bmi.RDS")

#################################
#      LogitBoost (sa5)
#################################
library(caTools)


#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.sa5 <- train(DIQ010A~ .,
                        data=balancedtrain_sa5, 
                        method = "LogitBoost", 
                        trControl = cctrlR,
                        tuneLength = 4)

print(logitboost.sa5)

# test model on trainset and check accuracy with confusion matrix.
sa5_train_lb <- predict(logitboost.sa5, newdata = balancedtrain_sa5, type = "raw")

#confusion matrix
confusionMatrix(sa5_train_lb, balancedtrain_sa5$DIQ010A,
                #mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
sa5_test_lb <- predict(logitboost.sa5, newdata = a_test_sa5, type = "raw")

# confusion matrix
confusionMatrix(sa5_test_lb, a_test_sa5_$DIQ010A,
                #mode="prec_recall"
)

varImp(logitboost.sa5)

logitboost.sa5.bmi <- train(DIQ010A~ .,
                            data=balancedtrain_sa5_bmi, 
                            method = "LogitBoost", 
                            trControl = cctrlR,
                            tuneLength = 4)

sa5bmi_test_lb <- predict(logitboost.sa5.bmi, newdata = a_test_sa5_bmi, type = "raw")


################################
#   Random Forest (sa5)
################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_sa5 = train(DIQ010A~.,
                          data = balancedtrain_sa5,
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

print(rf_maxtrees_sa5)

# Test model on trainset and check accuracy with confusion matrix.
sa5_train_rf <- predict(rf_maxtrees_sa5, newdata = balancedtrain_sa5, type = "raw")
confusionMatrix(data = sa5_train_rf, reference = balancedtrain_sa5$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa5_test_rf <- predict(rf_maxtrees_sa5, newdata = a_test_sa5, type = "raw")
confusionMatrix(data = sa5_test_rf, reference = a_test_sa5$DIQ010A
                #, mode="prec_recall"
                )


varImp(rf_maxtrees_sa5)

for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_sa5bmi = train(DIQ010A~.,
                          data = balancedtrain_sa5_bmi,
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
sa5bmi_test_rf <- predict(rf_maxtrees_sa5bmi, newdata = a_test_sa5_bmi, type = "raw")


##############################################
#   Tree Extreme Gradient Boosting (sa5)
##############################################

# Fit the model on the training set
set.seed(500)
xgbtrees_sa5 = train(DIQ010A~.,
                     data = balancedtrain_sa5,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = FALSE,
                                              savePredictions = "final"))

print(xgbtrees_sa5)

# Best tuning parameter
xgbtrees_sa5$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 93     150         3 0.4     0              0.6                1       0.5

# test model on trainset and check accuracy with confusion matrix.
sa5_trainxgbtree = predict(xgbtrees_sa5, balancedtrain_sa5, type = "raw")
confusionMatrix(sa5_trainxgbtree, reference = balancedtrain_sa5$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa5_testxgbtree = predict(xgbtrees_sa5, a_test_sa5, type = "raw")
confusionMatrix(sa5_testxgbtree, reference = a_test_sa5$DIQ010A
                #, mode="prec_recall"
                )

varImp(xgbtrees_sa5)

xgbtrees_sa5bmi = train(DIQ010A~.,
                     data = balancedtrain_sa5_bmi,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = FALSE,
                                              savePredictions = "final"))
sa5bmi_testxgbtree = predict(xgbtrees_sa5bmi, a_test_sa5_bmi, type = "raw")

###################################
#    Neural Networks (sa5)
###################################

library(neuralnet)

# train model
set.seed(500)
nnets.sa5 <- train(DIQ010A~., data = balancedtrain_sa5, method='nnet', trace = FALSE,
                   #Grid of tuning parameters to try:
                   tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))

# show neural network result
print(nnets.sa5)
nnets.sa5[["finalModel"]] 
plot(nnets.sa5)

# test model on training set
predictNN_sa5<- predict(nnets.sa5, balancedtrain_sa5, type = "raw")
confusionMatrix(data = predictNN_sa5, reference = balancedtrain_sa5$DIQ010A
                #, mode="prec_recall"
)

# test model on test set
predictNN_sa5_test <- predict(nnets.sa5, a_test_sa5, type = "raw")
confusionMatrix(data = predictNN_sa5bmi_test, reference = a_test_sa5_bmi$DIQ010A
                #, mode="prec_recall"
)

# show relative importance
varImp(nnets.sa5)

nnets.sa5bmi <- train(DIQ010A~., data = balancedtrain_sa5_bmi, method='nnet', trace = FALSE,
                   #Grid of tuning parameters to try:
                   tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))
predictNN_sa5bmi_test <- predict(nnets.sa5bmi, a_test_sa5_bmi, type = "raw")

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_sa5bmi = cbind(DIQ010A = a_test_sa5_bmi$DIQ010A,
                    pred_lb = sa5bmi_test_lb,
                    pred_rf = sa5bmi_test_rf,
                    pred_xgb = sa5bmi_testxgbtree,
                    pred_nn = predictNN_sa5bmi_test)

write.csv(foreval_sa5bmi, "foreval_sa5bmi.csv", row.names = F)

foreval_sa5bmi = read.csv("foreval_sa5bmi.csv", as.is = TRUE)


sa5bmiROC_lb = roc(foreval_sa5bmi$pred_lb, foreval_sa5bmi$DIQ010A)
auc(sa5bmiROC_lb)

sa5bmiROC_rf = roc(foreval_sa5bmi$pred_rf, foreval_sa5bmi$DIQ010A)
auc(sa5bmiROC_rf)

sa5bmiROC_xgb = roc(foreval_sa5bmi$pred_xgb, foreval_sa5bmi$DIQ010A)
auc(sa5bmiROC_xgb)

sa5bmiROC_nn = roc(foreval_sa5bmi$pred_nn, foreval_sa5bmi$DIQ010A)
auc(sa5bmiROC_nn)

foreval_sa5 = cbind(DIQ010A = a_test_sa5$DIQ010A,
                    pred_lb = sa5_test_lb,
                    pred_rf = sa5_test_rf,
                    pred_xgb = sa5_testxgbtree,
                    pred_nn = predictNN_sa5_test)

write.csv(foreval_sa5, "foreval_sa5.csv", row.names = F)

foreval_sa5 = read.csv("foreval_sa5.csv", as.is = TRUE)


sa5ROC_lb = roc(foreval_sa5$pred_lb, foreval_sa5$DIQ010A)
auc(sa5ROC_lb)

sa5ROC_rf = roc(foreval_sa5$pred_rf, foreval_sa5$DIQ010A)
auc(sa5ROC_rf)

sa5ROC_xgb = roc(foreval_sa5$pred_xgb, foreval_sa5$DIQ010A)
auc(sa5ROC_xgb)

sa5ROC_nn = roc(foreval_sa5$pred_nn, foreval_sa5$DIQ010A)
auc(sa5ROC_nn)

##########################################################################################################################################
#BMI continuous variable (exc Height and weight)
#to read it

train_sa6 = readRDS("train_sa6.RDS")
balancedtrain_sa6 <- SMOTE(DIQ010A ~., train_sa6, perc.over = 600, k = 5, perc.under = 116.6667)
balancedtrain_sa6 = subset(balancedtrain_sa6, select = c(-DIQ010B))

saveRDS(balancedtrain_sa6, file = "balancedtrain_sa6.RDS")

#to read it

balancedtrain_sa6 = readRDS("balancedtrain_sa6.RDS")

#to read it

test_sa6 = readRDS("test_sa6.RDS")
a_test_sa6 = subset(test_sa6, select = c(-DIQ010B))
saveRDS(a_test_sa6, file = "a_test_sa6.RDS")

#to read it

a_test_sa6 = readRDS("a_test_sa6.RDS")

#try with and without ht and wt

balancedtrain_sa6_bmi = subset(balancedtrain_sa6, select = c(-BMXWT, -BMXHT))

saveRDS(balancedtrain_sa6_bmi, file = "balancedtrain_sa6_bmi.RDS")

#to read it

balancedtrain_sa6_bmi = readRDS("balancedtrain_sa6_bmi.RDS")

a_test_sa6_bmi = subset(a_test_sa6, select = c(-BMXWT, -BMXHT))
saveRDS(a_test_sa6_bmi, file = "a_test_sa6_bmi.RDS")

#to read it

a_test_sa6_bmi = readRDS("a_test_sa6_bmi.RDS")

#################################
#      LogitBoost (sa6)
#################################
library(caTools)


#higher recall, lower accuracy
cctrlR <- trainControl(method = "cv", number = 3, returnResamp = "all", search = "random")
logitboost.sa6 <- train(DIQ010A~ .,
                        data=balancedtrain_sa6, 
                        method = "LogitBoost", 
                        trControl = cctrlR,
                        tuneLength = 4)

print(logitboost.sa6)

# test model on trainset and check accuracy with confusion matrix.
sa6_train_lb <- predict(logitboost.sa6, newdata = balancedtrain_sa6, type = "raw")

#confusion matrix
confusionMatrix(sa6_train_lb, balancedtrain_sa6$DIQ010A,
                #mode="prec_recall"
)

# Perform prediction on testset and look at confusion matrix.
sa6_test_lb <- predict(logitboost.sa6, newdata = a_test_sa6, type = "raw")

# confusion matrix
confusionMatrix(sa6_test_lb, a_test_sa6$DIQ010A,
                #mode="prec_recall"
)

varImp(logitboost.sa6)

logitboost.sa6bmi <- train(DIQ010A~ .,
                        data=balancedtrain_sa6_bmi, 
                        method = "LogitBoost", 
                        trControl = cctrlR,
                        tuneLength = 4)
sa6bmi_test_lb <- predict(logitboost.sa6bmi, newdata = a_test_sa6_bmi, type = "raw")

################################
#   Random Forest (sa6)
################################

mtrys = seq(1,4,1)
ntrees = c(250,300,350,400,450,500,550,600,800,1000,2000)
combo_mtrTrees = data.frame(expand.grid(mtrys, ntrees))
colnames(combo_mtrTrees) = c('mtrys', 'ntrees')

tuneGrid = expand.grid(.mtry = c(1:4))
for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_sa6 = train(DIQ010A~.,
                          data = balancedtrain_sa6,
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

print(rf_maxtrees_sa6)

# Test model on trainset and check accuracy with confusion matrix.
sa6_train_rf <- predict(rf_maxtrees_sa6, newdata = balancedtrain_sa6, type = "raw")
confusionMatrix(data = sa6_train_rf, reference = balancedtrain_sa6$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa6_test_rf <- predict(rf_maxtrees_sa6, newdata = a_test_sa6, type = "raw")
confusionMatrix(data = sa6_test_rf, reference = a_test_sa6$DIQ010A
                #, mode="prec_recall"
                )


varImp(rf_maxtrees_sa6)

for (i in 1:length(ntrees)){
  ntree = ntrees[i]
  set.seed(500)
  rf_maxtrees_sa6bmi = train(DIQ010A~.,
                          data = balancedtrain_sa6_bmi,
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
sa6_test_rf_bmi <- predict(rf_maxtrees_sa6bmi, newdata = a_test_sa6_bmi, type = "raw")

##############################################
#   Tree Extreme Gradient Boosting (sa6)
##############################################

# Fit the model on the training set
set.seed(500)
xgbtrees_sa6 = train(DIQ010A~.,
                     data = balancedtrain_sa6,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = FALSE,
                                              savePredictions = "final"))

print(xgbtrees_sa6)

# Best tuning parameter
xgbtrees_sa6$bestTune

# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 96     150         3 0.4     0              0.6                1      0.75

# test model on trainset and check accuracy with confusion matrix.
sa6_trainxgbtree = predict(xgbtrees_sa6, balancedtrain_sa6, type = "raw")
confusionMatrix(sa6_trainxgbtree, reference = balancedtrain_sa6_bmi$DIQ010A
                #, mode="prec_recall"
                )

# Perform prediction on testset and look at confusion matrix.
sa6_testxgbtree = predict(xgbtrees_sa6, a_test_sa6, type = "raw")
confusionMatrix(sa6_testxgbtree, reference = a_test_sa6$DIQ010A
                #, mode="prec_recall"
                )

varImp(xgbtrees_sa6)

xgbtrees_sa6bmi = train(DIQ010A~.,
                     data = balancedtrain_sa6_bmi,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv",
                                              number = 10,
                                              classProbs = FALSE,
                                              savePredictions = "final"))
sa6bmi_testxgbtree = predict(xgbtrees_sa6bmi, a_test_sa6_bmi, type = "raw")

###################################
#    Neural Networks (sa6)
###################################

library(neuralnet)

# train model
set.seed(500)
nnets.sa6 <- train(DIQ010A~., data = balancedtrain_sa6, method='nnet', trace = FALSE,
                   #Grid of tuning parameters to try:
                   tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))

# show neural network result
print(nnets.sa6)
nnets.sa6[["finalModel"]] 
plot(nnets.sa6)

# test model on training set
# nnA1b_train <- subset(balancedtrain_A1, select = -c(DIQ010A))
predictNN_sa6 <- predict(nnets.sa6, balancedtrain_sa6, type = "raw")
confusionMatrix(data = predictNN_sa6, reference = balancedtrain_sa6$DIQ010A
                #, mode="prec_recall"
)

# test model on test set
# nnA1b_test <- subset(testmodel_A1, select = -c(DIQ010A))
predictNN_sa6_test <- predict(nnets.sa6, a_test_sa6, type = "raw")
confusionMatrix(data = predictNN_sa6_test, reference = a_test_sa6$DIQ010A
                #, mode="prec_recall"
)

# show relative importance
varImp(nnets.sa6)

nnets.sa6bmi <- train(DIQ010A~., data = balancedtrain_sa6_bmi, method='nnet', trace = FALSE,
                   #Grid of tuning parameters to try:
                   tuneGrid=expand.grid(.size=seq(1, 10, by = 2),.decay=c(0,0.001,0.1)))
predictNN_sa6bmi_test <- predict(nnets.sa6bmi, a_test_sa6_bmi, type = "raw")

#################################################################################################################################
#     PR/ROC-AUC evaluation

foreval_sa6 = cbind(DIQ010A = a_test_sa6$DIQ010A,
                    pred_lb = sa6_test_lb,
                    pred_rf = sa6_test_rf,
                    pred_xgb = sa6_testxgbtree,
                    pred_nn = predictNN_sa6_test)

write.csv(foreval_sa6, "foreval_sa6.csv", row.names = F)

foreval_sa6 = read.csv("foreval_sa6.csv", as.is = TRUE)


sa6ROC_lb = roc(foreval_sa6$pred_lb, foreval_sa6$DIQ010A)
auc(sa6ROC_lb)

sa6ROC_rf = roc(foreval_sa6$pred_rf, foreval_sa6$DIQ010A)
auc(sa6ROC_rf)

sa6ROC_xgb = roc(foreval_sa6$pred_xgb, foreval_sa6$DIQ010A)
auc(sa6ROC_xgb)

sa6ROC_nn = roc(foreval_sa6$pred_nn, foreval_sa6$DIQ010A)
auc(sa6ROC_nn)

foreval_sa6bmi = cbind(DIQ010A = a_test_sa6_bmi$DIQ010A,
                    pred_lb = sa6bmi_test_lb,
                    pred_rf = sa6_test_rf_bmi,
                    pred_xgb = sa6bmi_testxgbtree,
                    pred_nn = predictNN_sa6bmi_test)

write.csv(foreval_sa6bmi, "foreval_sa6bmi.csv", row.names = F)

foreval_sa6bmi = read.csv("foreval_sa6bmi.csv", as.is = TRUE)


sa6bmiROC_lb = roc(foreval_sa6bmi$pred_lb, foreval_sa6bmi$DIQ010A)
auc(sa6bmiROC_lb)

sa6bmiROC_rf = roc(foreval_sa6bmi$pred_rf, foreval_sa6bmi$DIQ010A)
auc(sa6bmiROC_rf)

sa6bmiROC_xgb = roc(foreval_sa6bmi$pred_xgb, foreval_sa6bmi$DIQ010A)
auc(sa6bmiROC_xgb)

sa6bmiROC_nn = roc(foreval_sa6bmi$pred_nn, foreval_sa6bmi$DIQ010A)
auc(sa6bmiROC_nn)
