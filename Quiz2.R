#QUESTION1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
#option1- correct
adData = data.frame(predictors)
trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
#option2
adData = data.frame(diagnosis,predictors)
train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
test = createDataPartition(diagnosis, p = 0.50,list=FALSE)
#option3
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
#option4
adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#QUESTION2
library(AppliedPredictiveModeling)
data(concrete)
library(caret);library(Hmisc)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
names(mixtures)
plot(mixtures$CompressiveStrength)

par(mfrow=c(2,4))
cutCement <- cut2(mixtures$Cement,g=4)
plot(mixtures$CompressiveStrength,col=cutCement,main="Cement")
cutAsh <- cut2(mixtures$FlyAsh,g=4)
plot(mixtures$CompressiveStrength,col=cutAsh,main="Ash")
cutAge <- cut2(mixtures$Age,g=4)
plot(mixtures$CompressiveStrength,col=cutAge,main="Age")
cutBFG <- cut2(mixtures$BlastFurnaceSlag,g=4)
plot(mixtures$CompressiveStrength,col=cutBFG,main="BlastFurnaceSlag")
cutWater <- cut2(mixtures$Water,g=4)
plot(mixtures$CompressiveStrength,col=cutWater,main="Water")
cutSP <- cut2(mixtures$Superplasticizer,g=4)
plot(mixtures$CompressiveStrength,col=cutSP,main="Superplasticizer")
cutCA <- cut2(mixtures$CoarseAggregate,g=4)
plot(mixtures$CompressiveStrength,col=cutCA,main="CoarseAggregate")
cutFA <- cut2(mixtures$FineAggregate,g=4)
plot(mixtures$CompressiveStrength,col=cutFA,main="FineAggregate")
par(mfrow(c(1,1)))
#Answer: here is a non-random pattern in the plot of the outcome
#versus index that does not appear to be perfectly explained by 
#any predictor suggesting a variable may be missing.

#QUESTION3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(mixtures$Superplasticizer)
#The histogram confirm a skewness
#Taking the log of the variable to reduce skewness
hist(log(mixtures$Superplasticizer+1))
#Answer: There are a large number of values that are the same and 
#even if you took the log(SuperPlasticizer + 1) they would still 
#all be identical so the distribution would not be symmetric.

#QUESTION4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

preProc <- preProcess(training[,startsWith(names(training),'IL')],method="pca",thres=.9)
#answer
preProc #(9)

#QUESTION5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# trainSubSet <- data.frame(training[,startsWith(names(training),'IL')],training$diagnosis)
# preProc <- preProcess(trainSubSet[-13],method="pca",thres=.8)
# trainPC <- predict(preProc,trainSubSet[-13])
# 
# PCFit <- train(trainSubSet$training.diagnosis~.,data=trainPC,method="glm")
# NotPCFit <- train(trainSmall$training.diagnosis~.,data=trainSmall,method="glm")


trainSmall <- data.frame(training[,grep('^IL',names(training))],training$diagnosis)
testSmall <- data.frame(training[,grep('^IL',names(training))],training$diagnosis)

preProc <- preProcess(trainSmall[-13],method="pca",thres=.8)

trainPC <- predict(preProc,trainSmall[-13])
testPC <-predict(preProc,testSmall[-13])

PCFit <- train(x=trainPC,y=trainSmall$training.diagnosis, method = "glm")
NotPCFit <- train(x=trainSmall,y=trainSmall$training.diagnosis, method="glm")

PCTestPredict <- predict(PCFit,newdata=testPC)
confusionMatrix(PCTestPredict,testSmall$testing.diagnosis)
NotPCTestPredict <- predict(NotPCFit,newdata=testSmall)
confusionMatrix(NotPCTestPredict,testSmall$testing.diagnosis)


