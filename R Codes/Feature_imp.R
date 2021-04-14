#setwd('D:\\Yaxin\\HKBU BM\\Courses\\Sem 2\\ECON7860 Big Data Analytics for Business (S11)\\Group Project\\HR Analytics\\working\\Results\\Workspace')


# The results object files are saved from the R workspace after compiling 
# the reports, and hence not included in R codes or previous results.
load('results_org')
load('results_ind_0')
load('results_ind_1')



library(caret)
library(randomForest)
library(cowplot)

results <- c('results_org', 'results_ind_0', 'results_ind_1')
par(mfrow=c(2,2))

varImpPlot(results_org$rf_best$model$finalModel, main = 'Feature Importance: RF_org')
varImpPlot(results_ind_0$rf_best$model$finalModel, main = 'Feature Importance: RF_ind_0')
varImpPlot(results_ind_1$rf_best$model$finalModel, main = 'Feature Importance: RF_ind_1')




l1 <- plot(varImp(results_org$glmnet_best$model), main = 'Feature Importance: Logit_org')
l2 <- plot(varImp(results_ind_0$glmnet_best$model), main = 'Feature Importance: Logit_ind_0')
l3 <- plot(varImp(results_ind_1$glmnet_best$model), main = 'Feature Importance: Logit_ind_1')
plot_grid(l1, l2, l3)




s1 <- plot(varImp(results_org$svmLinear_best$model), main = 'Feature Importance: SVM_org')
s2 <- plot(varImp(results_ind_0$svmLinear_best$model), main = 'Feature Importance: SVM_ind_0')
s3 <- plot(varImp(results_ind_1$svmLinear_best$model), main = 'Feature Importance: SVM_ind_1')
plot_grid(s1, s2, s3)




k1 <- plot(varImp(results_org$knn_best$model), main = 'Feature Importance: k-NN_org')
k2 <- plot(varImp(results_ind_0$knn_best$model), main = 'Feature Importance: k-NN_ind_0')
k3 <- plot(varImp(results_ind_1$knn_best$model), main = 'Feature Importance: k-NN_ind_1')
plot_grid(k1, k2, k3)
