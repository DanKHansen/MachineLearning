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
