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
set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]
set.seed(233)

fit <- train(CompressiveStrength ~.,method='lasso', data = training)
plot.enet(fit$finalModel,xvar = 'penalty', use.color = T)

#Question 4
library(lubridate) #for year() function below
library(forecast)
dat = read.csv("gaData.csv")
training = dat[year(dat$date)<2012,]
testing = dat[year(dat$date)>2011,]
tstrain = ts(training$visitsTumblr)
bat1 <- bats(tstrain)
fcast1 <- forecast(bat1,level = 95,h=nrow(testing))
tab1 <- table((testing$visitsTumblr>fcast1$lower) & (testing$visitsTumblr<fcast1$upper))
round(tab1[[2]]/nrow(testing) * 100,0)

#Question 5
set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data("concrete")
inTrain = createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
library(e1071)
svm1  <- svm(CompressiveStrength ~., data = training)
pred1 <- predict(svm1,testing)
round(RMSE(pred1,testing$CompressiveStrength),2)
