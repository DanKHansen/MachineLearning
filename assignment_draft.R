#Supervised Learning, Classification problem.

#Loading libraries
library(caret)
library(randomForest)
library(xgboost)
library(ggplot2)
library(lattice)


# for parallel processing
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores()-1)
registerDoParallel(cluster)

#Original datasets:

# url.train = http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# url.test = http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

#Loading data
testing <- read.csv('pml-testing.csv',na.strings = c('','NA','#DIV/0!'))
training <- read.csv('pml-training.csv',na.strings = c('','NA','#DIV/0!'))

#Cleaning data
#removing variables with close to zero variance (using caret package)
#these variables will have close to no influence on the prediction anyway
nzv_train <- nearZeroVar(training,saveMetrics = T)
nzv_test <- nearZeroVar(testing,saveMetrics = T)

myTraining <- training[,nzv_train$nzv == F]
myTesting <- testing[,nzv_test$nzv == F]

#only keep columns which do not only holds NA's
#as they will have no influence on the prediction anyway
tmp_train_names <- colnames(myTraining)[colSums(is.na(myTraining))==0]
tmp_test_names <- colnames(myTesting)[colSums(is.na(myTesting))==0]

myTraining <- myTraining[,tmp_train_names]
myTesting <- myTesting[,tmp_test_names]

#removing timestamps, X and usernames - not relevant for this excerzise
myTraining <- myTraining[,-c(1:7)]
myTesting <- myTesting[,-c(1:7)]

# using the model-syntax has bad performance. hence using x/y syntax
x <- myTraining[,-"classe"]
y <- myTraining[,"classe"]

set.seed(223344)

# config trainControl object
tc <- trainControl(method = 'cv', number = 10, allowParallel = T)

# Extended Gradient Boost - linear - out-of-the-box
mod_xgb <- train(x,y,data=myTraining, method='xgbLinear', trControl= tc)
accu_xgb <- round(max(mod_xgb$results$Accuracy)*100,2)
cfm_xgb <- confusionMatrix(mod_xgb)

# Random Forest - out-of-the-box
mod_rf <- train(x,y,data=myTraining,method = 'rf')
accu_rf <- round(max(mod$results$Accuracy)*100,2)
cfm_rf <- confusionMatrix(mod_rf)

# Random Forest with both preprocessing and cross-validation  
mod_rf_all <- train(x,y,data=myTraining,method = 'rf',
                   trControl = tc,
                   preProcess=c('center','scale'))
accu_rf_all <- round(max(mod_CV_PP$results$Accuracy)*100,2)
cfm_rf_all <- confusionMatrix(mod_rf_all)

# Return to single processing
stopCluster(cluster)
registerDoSEQ()

#Prediction (for use in Quiz)
pred_xbg <- predict(mod_xgb,myTesting)
pred_rf <- predict(mod_rf,myTesting)
pred_rf_all <- predict(mod_rf_all,myTesting)
