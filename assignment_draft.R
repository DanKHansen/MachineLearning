#Supervised Learning, Classification problem.
#Typical models could be: 
# Decision Trees
# Ensembles 
##  Bagging (usually applied to DT), 
##  Boosting (combine and weight variables), 
##  Random Forest (beware of overfitting)
# KNN
# Linear Regression
# Naive Bayes
# NN
# Logistic Regression
# Perceptron
# Relevance Vector Machine (RVM)
# Support Vector Mashine (SVM)

#Original datasets:

# http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

#Loading data
testing <- read.csv('pml-testing.csv',na.strings = c('','NA','#DIV/0!'))
training <- read.csv('pml-training.csv',na.strings = c('','NA','#DIV/0!'))

#Loading library
library(caret)

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

#XGBoost
mod_xgb <- train(classe ~.,data=myTraining, method='xgbLinear')
accu_xgb <- round(max(mod_xgb$results$Accuracy)*100,2)

#XGBTree
mod_xgbt <- train(classe ~.,data=myTraining, method='xgbTree')
accu_xgbt <- round(max(mod_xgbt$results$Accuracy)*100,2)

#bglm
mod_bglm <- train(classe ~.,data=myTraining, method='bayesglm')
accu_bglm <- round(max(mod_bglm$results$Accuracy)*100,2)


#elm
mod_elm <- train(classe ~.,data=myTraining, method='elm')
accu_elm <- round(max(mod_elm$results$Accuracy)*100,2)

#Now let's make some models :-)
#I'll take the easy way, and let caret do the cross-validation and preprocessing
#Starting of with a 'clean' Random Forest (takes forever to run)
set.seed(223344)
library(doMC)
registerDoMC(cores = 5)
mod <- train(classe ~.,data=myTraining,method = 'rf')
accu_rf <- round(max(mod$results$Accuracy)*100,2)

#With crossvalidation
mod_CV <- train(classe ~.,data=myTraining,method = 'rf', 
                 trControl = trainControl(method = 'cv', number = 8))
accu_cv <- round(max(mod_CV$results$Accuracy)*100,2)

#With preprocessing  
mod_PP <- train(classe ~.,data=myTraining,method = 'rf', 
                 preProcess=c('center','scale'))
accu_pp <- round(max(mod_PP$results$Accuracy)*100,2)

#with both  
mod_CV_PP <- train(classe ~.,data=myTraining,method = 'rf',
                   trControl = trainControl(method = 'cv', number = 8),
                   preProcess=c('center','scale'))
accu_cv_pp <- round(max(mod_CV_PP$results$Accuracy)*100,2)

#Prediction (for use in Quiz)
pred_rf <- predict(mod,myTesting)
pred_cv <- predict(mod_CV,myTesting)
pred_pp <- predict(mod_PP,myTesting)
pred_rf_cv_pp <- predict(mod_CV_PP,myTesting)





#with Logistic / multinomial Regression no preproc.  
mod_MN <- train(classe ~.,data=myTraining,method = 'multinom',
                   trControl = trainControl(method = 'cv', number = 8), verbose = F)
accu_MN_cv <- round(max(mod_CV_PP$results$Accuracy)*100,2)
pred_MN <- predict(mod_MN,myTesting)
#with Bagged CART
mod_BC <- train(classe ~.,data=myTraining,method = 'treebag',
                trControl = trainControl(method = 'cv', number = 8))
accu_BC <- round(max(mod_BC$results$Accuracy)*100,2)
pred_BC <- predict(mod_BC,myTesting)
