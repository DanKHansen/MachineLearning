#Question 1
library(AppliedPredictiveModeling)
data("segmentationOriginal")
library(caret)
library(rattle)

training <- subset(segmentationOriginal,Case=='Train')
testing <- subset(segmentationOriginal,Case=='Test')

set.seed(125)

fit <- train(Class ~.,data = training,method='rpart')

fancyRpartPlot(fit$finalModel)

#A: PS, WS, PS, utp

#Question 2
#in a K-fold cross validation:
#if K is small, the estimate bias is larger in the test set accuracy. 
#if K is small, the estimate variance is smaller in the test set accuracy.
#In a leave-1-out cross validation
#K is equal to the sample size.

#Question 3
library(pgmm)
data("olive")
olive = olive[,-1]

fit<-train(Area ~ ., data=olive, method="rpart")
fancyRpartPlot(fit$finalModel)

predict(fit,newdata = as.data.frame(t(colMeans(olive))))
#It is strange, because Area should be a qualitative variable, but tree is reporting....

#Question 4
library(ElemStatLearn)
data("SAheart")
set.seed(8484)
train = sample(1:dim(SAheart)[1],size = dim(SAheart)[1]/2,replace = FALSE)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
fit <- train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")

missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

missClass(trainSA$chd, predict(fit, trainSA))
missClass(testSA$chd, predict(fit, testSA))

#Question 5
library(ElemStatLearn)
data("vowel.train")
data("vowel.test")

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

fit <- train(y ~ .,method='rf',data=vowel.train)
varImp(fit)

library(randomForest)
fit2 <- randomForest(y ~.,data=vowel.train)
varImp(fit2)
order(varImp(fit2)[,1],decreasing = TRUE)