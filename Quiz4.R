library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)
library(gbm)
library(lubridate)
library(forecast)
library(e1071)

#Question 1
data("vowel.train");data("vowel.test")
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

fitRF <- train(y ~ .,method='rf',data=vowel.train)
fitGBM <- train(y ~ .,method='gbm',data=vowel.train,verbose=F)

predRF <- predict(fitRF,vowel.test)
predGBM <- predict(fitGBM,vowel.test)

RF_accuracy <- confusionMatrix(predRF,vowel.test$y)$overall[1]
GBM_accuracy <- confusionMatrix(predGBM,vowel.test$y)$overall[1]
Agree_accuracy <- confusionMatrix(predRF,predGBM)$overall[1]
