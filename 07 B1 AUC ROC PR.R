##################################
# PR/ROC-AUC evaluation (imbal)
##################################


foreval_imbal = cbind(DIQ010B = testmodel_B1$DIQ010B,
                      pred_dtrpart = B1_testtree, 
                      pred_dtcaret = B1test_treefit, 
                      pred_rf = B1test_rf,
                      pred_xgb = B1_testxgbtree,
                      pred_gbm = B1_test_gbm,
                      pred_nnets = predictNN_B1_test,
                      pred_svm = B1_svmtest,
                      pred_svmg = B1_svmtestGrid,
                      pred_nb = B1test_nb,
                      pred_naivebayes = B1test_naivebayes,
                      pred_knn = B1test_knn)

write.csv(foreval_imbal, "foreval_imbal.csv", row.names = F)

foreval_imbal = read.csv("foreval_imbal.csv", as.is = TRUE)

#ROC
library(pROC)

ROC_dtrpart = multiclass.roc(foreval_imbal$pred_dtrpart, foreval_imbal$DIQ010B)
auc(ROC_dtrpart)
rs_dtrpart <- ROC_dtrpart[['rocs']]
plot.roc(rs_dtrpart[[1]], print.auc = TRUE, col = 'blue')
plot.roc(rs_dtrpart[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
plot.roc(rs_dtrpart[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

ROC_dtcaret = multiclass.roc(foreval_imbal$pred_dtcaret, foreval_imbal$DIQ010B)
auc(ROC_dtcaret)
rs_dtcaret <- ROC_dtcaret[['rocs']]
plot.roc(rs_dtcaret[[1]], print.auc = TRUE, col = 'blue')
plot.roc(rs_dtcaret[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
plot.roc(rs_dtcaret[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

ROC_rf = multiclass.roc(foreval_imbal$pred_rf, foreval_imbal$DIQ010B)
auc(ROC_rf)
rs_rf <- ROC_rf[['rocs']]
plot.roc(rs_rf[[1]], print.auc = TRUE, col = 'blue')
# plot.roc(rs_rf[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
# plot.roc(rs_rf[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

ROC_xgb = multiclass.roc(foreval_imbal$pred_xgb, foreval_imbal$DIQ010B)
auc(ROC_xgb)
rs_xgb <- ROC_xgb[['rocs']]
plot.roc(rs_xgb[[1]], print.auc = TRUE, col = 'blue')
plot.roc(rs_xgb[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
plot.roc(rs_xgb[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

ROC_gbm = multiclass.roc(foreval_imbal$pred_gbm, foreval_imbal$DIQ010B)
auc(ROC_gbm)
rs_gbm <- ROC_gbm[['rocs']]
plot.roc(rs_gbm[[1]], print.auc = TRUE, col = 'blue')
plot.roc(rs_gbm[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
plot.roc(rs_gbm[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

ROC_nnets = multiclass.roc(foreval_imbal$pred_nnets, foreval_imbal$DIQ010B)
auc(ROC_nnets)
rs_nnets <- ROC_nnets[['rocs']]
plot.roc(rs_nnets[[1]], print.auc = TRUE, col = 'blue')
# plot.roc(rs_nnets[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
# plot.roc(rs_nnets[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

ROC_svm = multiclass.roc(foreval_imbal$pred_svm, foreval_imbal$DIQ010B)
auc(ROC_svm)
rs_svm <- ROC_svm[['rocs']]
plot.roc(rs_svm[[1]], print.auc = TRUE, col = 'blue')
plot.roc(rs_svm[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
plot.roc(rs_svm[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

ROC_svmg = multiclass.roc(foreval_imbal$pred_svmg, foreval_imbal$DIQ010B)
auc(ROC_svmg)
rs_svmg <- ROC_svmg[['rocs']]
plot.roc(rs_svmg[[1]], print.auc = TRUE, col = 'blue')
plot.roc(rs_svmg[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
plot.roc(rs_svmg[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

# ROC_nb = multiclass.roc(foreval_imbal$pred_nb, foreval_imbal$DIQ010B)
# auc(ROC_nb)
# rs_nb <- ROC_nb[['rocs']]
# plot.roc(rs_nb[[1]], print.auc = TRUE, col = 'blue')
# plot.roc(rs_nb[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
# plot.roc(rs_nb[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.7, add = TRUE)

ROC_naivebayes = multiclass.roc(foreval_imbal$pred_naivebayes, foreval_imbal$DIQ010B)
auc(ROC_naivebayes)
rs_naivebayes <- ROC_naivebayes[['rocs']]
plot.roc(rs_naivebayes[[1]], print.auc = TRUE, col = 'blue')


ROC_knn = multiclass.roc(foreval_imbal$pred_knn, foreval_imbal$DIQ010B)
auc(ROC_knn)
rs_knn <- ROC_knn[['rocs']]
plot.roc(rs_knn[[1]], print.auc = TRUE, col = 'blue')
plot.roc(rs_knn[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.6, add = TRUE)
plot.roc(rs_knn[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)

#top 4 ROC-AUC
plot.roc(rs_nnets[[1]], print.auc = TRUE, col = 'blue')
plot.roc(rs_svmg[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.4, add = TRUE)
plot.roc(rs_dtcaret[[2]], print.auc = TRUE, col = 'green', print.auc.y = 0.6, add = TRUE)
plot.roc(rs_rf[[1]], print.auc = TRUE, col = 'purple', print.auc.y = 0.7, add = TRUE)

#PR

library(PRROC)

pr_dtrpart <- pr.curve(scores.class0 = foreval_imbal$pred_dtrpart, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_dtrpart)
plot(pr_dtrpart, legend = FALSE)

pr_dtcaret <- pr.curve(scores.class0 = foreval_imbal$pred_dtcaret, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_dtcaret)
plot(pr_dtcaret, legend = FALSE)

pr_rf <- pr.curve(scores.class0 = foreval_imbal$pred_rf, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_rf)
plot(pr_rf, legend = FALSE)

pr_xgb <- pr.curve(scores.class0 = foreval_imbal$pred_xgb, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_xgb)
plot(pr_xgb, legend = FALSE)

pr_gbm <- pr.curve(scores.class0 = foreval_imbal$pred_gbm, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_gbm)
plot(pr_gbm, legend = FALSE)

pr_nnets <- pr.curve(scores.class0 = foreval_imbal$pred_nnets, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_nnets)
plot(pr_nnets, legend = FALSE)

pr_svm <- pr.curve(scores.class0 = foreval_imbal$pred_svm, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_svm)
plot(pr_svm, legend = FALSE)

pr_svmg <- pr.curve(scores.class0 = foreval_imbal$pred_svmg, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_svmg)
plot(pr_svmg, legend = FALSE)

pr_nb <- pr.curve(scores.class0 = foreval_imbal$pred_nb, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_nb)
plot(pr_nb, legend = FALSE)

pr_naivebayes <- pr.curve(scores.class0 = foreval_imbal$pred_naivebayes, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_naivebayes)
plot(pr_naivebayes, legend = FALSE)

pr_knn <- pr.curve(scores.class0 = foreval_imbal$pred_knn, scores.class1 = foreval_imbal$DIQ010B, curve=T)
print(pr_knn)
plot(pr_knn, legend = FALSE)

#top 4 plots
plot(pr_dtcaret, max.plot = TRUE, min.plot = TRUE, rand.plot = TRUE,
     fill.area = T, color=2, auc.main = FALSE, main = "Top 4 PR-AUC")

plot(pr_nnets, add = TRUE, color = 3)
plot(pr_nb, add = TRUE, color = 5)
plot(pr_rf, add = TRUE, color = 8)

######################################
# PR/ROC-AUC evaluation (balanced)
######################################


foreval_bal = cbind(DIQ010B = testmodel_B1$DIQ010B,
                      pred_dtrpart = balancedB1_testtree, 
                      pred_dtcaret = balancedB1test_treefit, 
                      pred_rf = balancedB1_testrf,
                      pred_xgb = balB1_testxgbtree,
                      pred_gbm = balB1test_gbm,
                      pred_nnets = predictNN_balB1_test,
                      pred_svm = balB1_svmtest,
                      pred_svmg = balB1_testGrid,
                      pred_svmr = balB1_svmR,
                      pred_svmp = balB1_svmP,
                      pred_nb = balB1test_nb,
                      pred_naivebayes = balB1test_naivebayes,
                      pred_knn = balB1test_knn)

write.csv(foreval_bal, "foreval_bal.csv", row.names = F)

foreval_bal = read.csv("foreval_bal.csv", as.is = TRUE)

#ROC
library(pROC)

balROC_dtrpart = multiclass.roc(foreval_bal$pred_dtrpart, foreval_bal$DIQ010B)
auc(balROC_dtrpart)
balrs_dtrpart <- balROC_dtrpart[['rocs']]
plot.roc(balrs_dtrpart[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_dtrpart[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_dtrpart[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_dtrpart[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_dtrpart[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_dtrpart[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balROC_dtcaret = multiclass.roc(foreval_bal$pred_dtcaret, foreval_bal$DIQ010B)
auc(balROC_dtcaret)
balrs_dtcaret <- balROC_dtcaret[['rocs']]
plot.roc(balrs_dtcaret[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_dtcaret[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_dtcaret[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_dtcaret[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_dtcaret[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_dtcaret[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balROC_rf = multiclass.roc(foreval_bal$pred_rf, foreval_bal$DIQ010B)
auc(balROC_rf)
balrs_rf <- balROC_rf[['rocs']]
plot.roc(balrs_rf[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_rf[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_rf[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_rf[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_rf[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_rf[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balROC_xgb = multiclass.roc(foreval_bal$pred_xgb, foreval_bal$DIQ010B)
auc(balROC_xgb)
balrs_xgb <- balROC_xgb[['rocs']]
plot.roc(balrs_xgb[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_xgb[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_xgb[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_xgb[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_xgb[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_xgb[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balROC_gbm = multiclass.roc(foreval_bal$pred_gbm, foreval_bal$DIQ010B)
auc(balROC_gbm)
balrs_gbm <- balROC_gbm[['rocs']]
plot.roc(balrs_gbm[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_gbm[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_gbm[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_gbm[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_gbm[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_gbm[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balROC_nnets = multiclass.roc(foreval_bal$pred_nnets, foreval_bal$DIQ010B)
auc(balROC_nnets)
balrs_nnets <- balROC_nnets[['rocs']]
plot.roc(balrs_nnets[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_nnets[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_nnets[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_nnets[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_nnets[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_nnets[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balROC_svm = multiclass.roc(foreval_bal$pred_svm, foreval_bal$DIQ010B)
auc(balROC_svm)
balrs_svm <- balROC_svm[['rocs']]
plot.roc(balrs_svm[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_svm[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_svm[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_svm[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_svm[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_svm[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balROC_svmg = multiclass.roc(foreval_bal$pred_svmg, foreval_bal$DIQ010B)
auc(balROC_svmg)
balrs_svmg <- balROC_svmg[['rocs']]
plot.roc(balrs_svmg[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_svmg[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_svmg[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_svmg[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_svmg[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_svmg[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

balROC_nb = multiclass.roc(foreval_bal$pred_nb, foreval_bal$DIQ010B)
auc(balROC_nb)
balrs_nb <- balROC_nb[['rocs']]
plot.roc(balrs_nb[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_nb[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_nb[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)

balROC_naivebayes = multiclass.roc(foreval_bal$pred_naivebayes, foreval_bal$DIQ010B)
auc(balROC_naivebayes)
balrs_naivebayes <- balROC_naivebayes[['rocs']]
plot.roc(balrs_naivebayes[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_naivebayes[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_naivebayes[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)


balROC_knn = multiclass.roc(foreval_bal$pred_knn, foreval_bal$DIQ010B)
auc(balROC_knn)
balrs_knn <- balROC_knn[['rocs']]
plot.roc(balrs_knn[[1]], print.auc = TRUE, print.auc.y = 0.2, col = 'blue')
plot.roc(balrs_knn[[2]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_knn[[3]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_knn[[4]], print.auc = TRUE, col = 'orange', print.auc.y = 0.5, add = TRUE)
plot.roc(balrs_knn[[5]], print.auc = TRUE, col = 'purple', print.auc.y = 0.6, add = TRUE)
plot.roc(balrs_knn[[6]], print.auc = TRUE, col = 'gray', print.auc.y = 0.7, add = TRUE)

#top 4 ROC-AUC
plot.roc(balrs_svmg[[3]], print.auc = TRUE, col = 'blue', print.auc.y = 0.2)
plot.roc(balrs_rf[[3]], print.auc = TRUE, col = 'red', print.auc.y = 0.3, add = TRUE)
plot.roc(balrs_gbm[[5]], print.auc = TRUE, col = 'green', print.auc.y = 0.4, add = TRUE)
plot.roc(balrs_naivebayes[[1]], print.auc = TRUE, print.auc.y = 0.5, col = 'purple', add = TRUE)

#PR

library(PRROC)

balpr_dtrpart <- pr.curve(scores.class0 = foreval_bal$pred_dtrpart, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_dtrpart)
plot(balpr_dtrpart, legend = FALSE)

balpr_dtcaret <- pr.curve(scores.class0 = foreval_bal$pred_dtcaret, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_dtcaret)
plot(balpr_dtcaret, legend = FALSE)

balpr_rf <- pr.curve(scores.class0 = foreval_bal$pred_rf, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_rf)
plot(balpr_rf, legend = FALSE)

balpr_xgb <- pr.curve(scores.class0 = foreval_bal$pred_xgb, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_xgb)
plot(balpr_xgb, legend = FALSE)

balpr_gbm <- pr.curve(scores.class0 = foreval_bal$pred_gbm, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_gbm)
plot(balpr_gbm, legend = FALSE)

balpr_nnets <- pr.curve(scores.class0 = foreval_bal$pred_nnets, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_nnets)
plot(balpr_nnets, legend = FALSE)

balpr_svm <- pr.curve(scores.class0 = foreval_bal$pred_svm, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_svm)
plot(balpr_svm, legend = FALSE)

balpr_svmg <- pr.curve(scores.class0 = foreval_bal$pred_svmg, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_svmg)
plot(balpr_svmg, legend = FALSE)

balpr_nb <- pr.curve(scores.class0 = foreval_bal$pred_nb, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_nb)
plot(balpr_nb, legend = FALSE)

balpr_naivebayes <- pr.curve(scores.class0 = foreval_bal$pred_naivebayes, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_naivebayes)
plot(balpr_naivebayes, legend = FALSE)

balpr_knn <- pr.curve(scores.class0 = foreval_bal$pred_knn, scores.class1 = foreval_bal$DIQ010B, curve=T)
print(balpr_knn)
plot(balpr_knn, legend = FALSE)

#top 4 plots
plot(balpr_knn, max.plot = TRUE, min.plot = TRUE, rand.plot = TRUE,
     fill.area = T, color=2, auc.main = FALSE, main = "Top 4 PR-AUC")

plot(balpr_svmg, add = TRUE, color = 3)
plot(balpr_rf, add = TRUE, color = 8)
plot(balpr_naivebayes, add = TRUE, color = 5)
