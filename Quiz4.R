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

#Question 2
set.seed(3433)
data(AlzheimerDisease)
adData <- data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis,p=3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
fitRF <- train(diagnosis ~.,method='rf',data=training)
fitGBM <- train(diagnosis ~.,method='gbm', data=training, verbose=F)
fitLDA <- train(diagnosis ~.,method='lda',data=training)

predRF <- predict(fitRF,testing)
predGBM <- predict(fitGBM,testing)
predLDA <- predict(fitLDA,testing)

RF_accuracy <- confusionMatrix(predRF,testing$diagnosis)$overall[1]
GBM_accuracy <- confusionMatrix(predGBM,testing$diagnosis)$overall[1]
LDA_accuracy <- confusionMatrix(predLDA,testing$diagnosis)$overall[1]

combDF <- data.frame(predRF,predGBM,predLDA, diagnosis=testing$diagnosis)
fitCOMB <- train(diagnosis~.,method='rf',data=combDF)
predCOMB <- predict(fitCOMB,combDF)
COMB_accuracy <- confusionMatrix(predCOMB,testing$diagnosis)$overall[1]

#Question 3

