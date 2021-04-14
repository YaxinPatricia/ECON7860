# Uncomment if packages not installed
## install.packages("psych")
## install.packages("caret")
## install.packages("randomForest")
## install.packages("MLmetrics")
## install.packages("doParallel")
## install.packages("kernlab")
## install.packages("glmnet")



# Load data
#setwd('D:\\Yaxin\\HKBU BM\\Courses\\Sem 2\\ECON7860 Big Data Analytics for Business (S11)\\Group Project\\HR Analytics\\working')
rawData <- read.csv2("HR_comma_sep.csv", sep = ',')
colnames(rawData)



# Move the target variable "left" after "time_spend_company"
library(dplyr)
rawData <- rawData %>% relocate(left, .after = time_spend_company)



# Transform feature types
transform_feature <- function(X) {
  X$satisfaction_level <- as.numeric(X$satisfaction_level)
  X$last_evaluation <- as.numeric(X$last_evaluation)
  X$Work_accident <- as.factor(X$Work_accident)
  X$promotion_last_5years <- as.factor(X$promotion_last_5years)
  X$sales <- as.factor(X$sales)
  X$salary <- as.factor(X$salary)
  X$left <- factor(ifelse(X$left == 0, 'no', 'yes'), levels = c('yes', 'no'))
  return(X)
}

rawData <- transform_feature(rawData)
summary(rawData)



# Separate features and target variable
X <- rawData
y <- X$left
tag <- colnames(X)
tag



# Feature engineering
## Define the function for factor extraction 
require(psych)

fe <- function(M, n) {
  # The numeric feature matrix M needs to be normalized beforehand
  fa1 <- fa(M, n)
  fa.diagram(fa1)
  return(list('scores' = fa1$scores, 'weights' = fa1$weights))
}



## Create dummy variables for "sales" and "salary"
dummySales <- dummy.code(X$sales)
dummySalary <- dummy.code(X$salary)
colnames(dummySales)
colnames(dummySalary)

### Set "sales" and "low" as the default values respectively
dummySales <- dummySales[ , -c(1)]
dummySalary <- dummySalary[ , -c(1)]

X_dummy <- cbind(X[ , -c(9, 10)], dummySales, dummySalary)
tag_dummy <- colnames(X_dummy)
tag_dummy



# Train(80%)-test(20%)-split (stratified as "left" is unbalanced)
library(caret)

## Set seed for replication purpose
set.seed(7860)
index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[index, ]
X_test <- cbind(X[-index, 1 : 5], X[-index, 7 : length(X)])
X_dummy_train <- X_dummy[index, ]
X_dummy_test <- cbind(X_dummy[-index, 1 : 5], X_dummy[-index, 7 : length(X_dummy)])
y_test <- y[-index]




# Modeling with extracted factors, 5-fold nested CV with random search
n_fold <- 5
cv <- createFolds(X_train$left, n_fold)
#models <- c('glmnet', 'svmLinear', 'rf', 'knn')
models <- c('glmnet', 'rf', 'knn')
tune <- 5
n_cluster <- 5 ## Please set the number of multiprocessing slaves accordingly


for (m in models) {
  assign(paste0(m, '_cv'), 
         list('1' = list('model' = c(), 'f1_val' = c(), 'confm' = c()), 
              '2' = list('model' = c(), 'f1_val' = c(), 'confm' = c()), 
              '3' = list('model' = c(), 'f1_val' = c(), 'confm' = c()), 
              '4' = list('model' = c(), 'f1_val' = c(), 'confm' = c()), 
              '5' = list('model' = c(), 'f1_val' = c(), 'confm' = c())))
  
  for (i in 1 : n_fold) {
    ## Extract factors and weights from EFA
    fa_result <- fe(scale(X_train[-cv[[i]], 1 : 5]), 2)
    performance <- fa_result$scores[ , 1]
    satisfaction <- fa_result$scores[ , 2]
    w <- fa_result$weights
    colnames(w) <- c('performance', 'satisfaction')
    y_val <- X_train[cv[[i]], 'left']
    
    control <- trainControl(method = 'repeatedcv', number = 5, repeats = 2,
                            summaryFunction = prSummary, classProbs = TRUE, 
                            search="random", verboseIter = TRUE)
    
    set.seed(7860)
    
    require(doParallel)
    cl <- makePSOCKcluster(n_cluster, outfile = '')
    registerDoParallel(cl)
    
    if (m == 'rf') {
      X_dev <- cbind(performance, satisfaction, 
                     X_train[-cv[[i]], 6 : length(X_train)])
      X_val <- cbind(scale(as.matrix(X_train[cv[[i]], 1 : 5]) %*% w), 
                     X_train[cv[[i]], 7 : length(X_train)])
      
      m1 <- train(left ~ ., data = X_dev, method = m, metric = 'F', 
                  tuneLength = tune, trControl = control)
      
      rf_cv[[i]][['model']] <- m1
      rf_cv[[i]][['f1_val']] <- F_meas(predict(m1, X_val), y_val)
      rf_cv[[i]][['confm']] <- confusionMatrix(predict(m1, X_val), y_val)
      
    } else {
      X_dev <- cbind(performance, satisfaction, 
                     X_dummy_train[-cv[[i]], 6 : length(X_dummy_train)])
      X_val <- cbind(scale(as.matrix(X_dummy_train[cv[[i]], 1 : 5]) %*% w),
                     X_dummy_train[cv[[i]], 7 : length(X_dummy_train)])
      
      if (m == 'glmnet') {
        m1 <- train(left ~ ., data = X_dev, method = m, family = 'binomial',
                    metric = 'F', tuneLength = tune, trControl = control)
        
        glmnet_cv[[i]][['model']] <- m1
        glmnet_cv[[i]][['f1_val']] <- F_meas(predict(m1, X_val), y_val)
        glmnet_cv[[i]][['confm']] <- confusionMatrix(predict(m1, X_val), y_val)
        
      } else if (m == 'knn') {
        m1 <- train(left ~ ., data =  X_dev, method = m, metric = 'F', 
                    tuneLength = tune, trControl = control, 
                    tuneGrid = expand.grid(k = c(2, 3, 4, 5, 10)))
        
        knn_cv[[i]][['model']] <- m1
        knn_cv[[i]][['f1_val']] <- F_meas(predict(m1, X_val), y_val)
        knn_cv[[i]][['confm']] <- confusionMatrix(predict(m1, X_val), y_val)
        
      } else if (m == 'svmLinear') {
        m1 <- train(left ~ ., data =  X_dev, method = m, metric = 'F', 
                    tuneLength = tune, trControl = control)
        
        svmLinear_cv[[i]][['model']] <- m1
        svmLinear_cv[[i]][['f1_val']] <- F_meas(predict(m1, X_val), y_val)
        svmLinear_cv[[i]][['confm']] <- confusionMatrix(predict(m1, X_val), y_val)
      }
    }
    
    stopImplicitCluster()
    stopCluster(cl)
  }
}



for (m in models) {
  f <- get(paste0(m, '_cv'))[[1]]$f1_val
  n <- 1
  
  for (i in 2 : n_fold) {
    if (get(paste0(m, '_cv'))[[i]]$f1_val > f) {
      f <- get(paste0(m, '_cv'))[[i]]$f1_val
      n <- i
    }
  }
  
  assign(paste0(m, '_best'), get(paste0(m, '_cv'))[[i]])
}


# Record best model for each method
results <- as.data.frame(cbind(glmnet_best, svmLinear_best, knn_best, rf_best))

plot(results$glmnet_best$model)
plot(results$svmLinear_best$model)
plot(results$knn_best$model)
plot(results$rf_best$model)


for (i in 1 : 4) {
  cat(rep('\n', 3))
  print(results[[i]])
  cat(rep('\n', 3))
}


#save.image("D:/Yaxin/HKBU BM/Courses/Sem 2/ECON7860 Big Data Analytics for Business (S11)/Group Project/HR Analytics/working/final_ind_1.RData")

