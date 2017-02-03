#caret-package *********************************
library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)

set.seed(12345)
modelFit <- train(type ~.,data = training, method='glm')
modelFit
modelFit$finalModel

predictions <- predict(modelFit, newdata=testing)
predictions

confusionMatrix(predictions,testing$type)

#Data Slicing **********************************
#K-fold - Return train
set.seed(12346)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = TRUE)
sapply(folds,length)

folds[[1]][1:10]

#Return test
set.seed(12347)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = FALSE)
sapply(folds,length)

folds[[1]][1:10]

#Resampling - bootstrapping
set.seed(12348)
folds <- createResample(y=spam$type, times = 10, list = TRUE)
sapply(folds,length)

folds[[1]][1:10]

#Time slices
set.seed(12349)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]

#Training options **********************************
args(trainControl)
set.seed(12350)
modelFit2 <- train(type~.,data = training, method = 'glm')
modelFit2
set.seed(12350) #same seed for reproducing results
modelFit3 <- train(type~.,data = training, method = 'glm')
modelFit3

#Plotting predictors *******************************
library(ISLR); library(ggplot2); data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

#Plotting
featurePlot(x=training[,c('age','education','jobclass')],y=training$wage, plot = 'pairs')

qplot(age, wage, data = training)
qplot(age, wage, color=jobclass, data = training)

qq <- qplot(age,wage,color=education,data=training)
qq + geom_smooth(method='lm',formula=y~x)

#making factors
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

#boxplots
p1 <- qplot(cutWage,age,data=training,fill=cutWage,geom=c('boxplot'))
p1

library(gridExtra)
p2 <- qplot(cutWage,age,data=training,fill=cutWage,geom=c('boxplot','jitter'))
grid.arrange(p1,p2,ncol=2)

t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)

#Density plots
qplot(wage,color=education,data=training,geom='density')
qplot()

#Pre-processing   *******************************
inTrain <- createDataPartition(y=spam$type, p=0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,main='',xlab="ave. capital run length")
#skewed result
mean(training$capitalAve)
sd(training$capitalAve)

#Standardizing
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)
#Standardizing - test set
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

#Standardizing preprocess function
preObj <- preProcess(training[,-58], method = c('center','scale'))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

#Standardizing preprocess argument
set.seed(12351)
modelFit <- train(type ~.,data=training, preProcess=c('center','scale'),method='glm')
modelFit

preObj <- preProcess(training[,-58],method=c('BoxCox'))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainCapAveS);qqnorm(trainCapAveS)
par(1)
#Standardizing Imputing data
set.seed(12352)
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size = 1,prob = 0.05)==1
training$capAve[selectNA] <- NA

preObj <- preProcess(training[,-58],method='knnImpute')
capAve <- predict(preObj,training[,-58])$capAve

capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)

quantile(capAve-capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])

#Covariate creation
spam$capitalAveSq <- spam$capitalAve^2

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies,newdata=training))
#Near Zero Values
nsv <- nearZeroVar(training, saveMetrics = TRUE)
nsv
#Spline basis
library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis
#Fitting curves with splines
lm1 <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1,newdata=training), col='red', pch=19, cex=0.5)
#Splines on the test set
predict(bsBasis, age=testing$age)

#Preprocessing with Principal Components Analysis (PCA)
