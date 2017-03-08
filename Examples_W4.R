# http://cbcb.umd.edu/~hcorrada/PracticalML/
# http://www.biostat.jhsph.edu/~ririzarr/Teaching/649/

#Regularized regression
library(ElemStatLearn)
data("prostate")
str(prostate)

#Example from page 5
covnames <- names(prostate[-(9:10)])
y <- prostate$lpsa
x <- prostate[,covnames]
form <- as.formula(paste("lpsa~", paste(covnames, collapse="+"), sep=""))
summary(lm(form, data=prostate[prostate$train,]))
set.seed(1)
train.ind <- sample(nrow(prostate), ceiling(nrow(prostate))/2)
y.test <- prostate$lpsa[-train.ind]
x.test <- x[-train.ind,]
y <- prostate$lpsa[train.ind]
x <- x[train.ind,]
p <- length(covnames)
rss <- list()
for (i in 1:p) {
  cat(i)
  Index <- combn(p,i)
  rss[[i]] <- apply(Index, 2, function(is) {
    form <- as.formula(paste("y~", paste(covnames[is], collapse="+"), sep=""))
    isfit <- lm(form, data=x)
    yhat <- predict(isfit)
    train.rss <- sum((y - yhat)^2)
    
    yhat <- predict(isfit, newdata=x.test)
    test.rss <- sum((y.test - yhat)^2)
    c(train.rss, test.rss)
  })
}
png("selection-plots-01.png", height=432, width=432, pointsize=12)
plot(1:p, 1:p, type="n", ylim=range(unlist(rss)), xlim=c(0,p), xlab="number of predictors", ylab="residual sum of squares", main="Prostate cancer data")
for (i in 1:p) {
  points(rep(i-0.15, ncol(rss[[i]])), rss[[i]][1, ], col="blue")
  points(rep(i+0.15, ncol(rss[[i]])), rss[[i]][2, ], col="red")
}
minrss <- sapply(rss, function(x) min(x[1,]))
lines((1:p)-0.15, minrss, col="blue", lwd=1.7)
minrss <- sapply(rss, function(x) min(x[2,]))
lines((1:p)+0.15, minrss, col="red", lwd=1.7)
legend(x='topright',legend=c("Train", "Test"), col=c("blue", "red"), pch=1)
dev.off()

#*************************************************************************
library(caret)
small = prostate[1:5,]
lm(lpsa ~ .,data = small)

#Combining predictors (Ensembling Methods)

library(ISLR);library(ggplot2);library(caret)
data(Wage)
Wage <- subset(Wage,select=-c(logwage))
#creating building data and validation dataset
inBuild <- createDataPartition(y=Wage$wage,p=0.7,list=F)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage,p=0.7,list=F)
training <- buildData[inTrain,];testing <- buildData[-inTrain,]
dim(training)
dim(testing)
dim(validation)
mod1 <- train(wage~.,method='glm', data=training)
mod2 <- train(wage~.,method='rf', data=training, trControl = trainControl(method='cv'),number=3)
pred1 <- predict(mod1,testing);pred2 <- predict(mod2,testing)
qplot(pred1,pred2,color=wage,data=testing)
#fitting a model that combines predictors
predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage~.,method='gam',data=predDF)
combPred <- predict(combModFit,predDF)
#check errors on testing set
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))
#predict on validation set
pred1V <- predict(mod1,validation);pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)
#check errors on validation set
sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))

#Forecasting
library(quantmod)
from.dat <- as.Date('01/01/08', format='%m/%d/%y')
to.dat <- as.Date('12/31/13', format='%m/%d/%y')
getSymbols('GOOG', src='yahoo', from=from.dat, to=to.dat)
round(head(GOOG),2)
mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency = 12)
plot(ts1,xlab="Years+1",ylab='GOOG')
#decomposing
plot(decompose(ts1),xlab='Years+1')
#Training and test sets
ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5)
ts1Train
plot(ts1Train)
library(forecast) #adding moving average
lines(ma(ts1Train,order=3),col='red')
#Exponential smoothing
ets1 <- ets(ts1Train,model='MMM')
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col='red')
#Accuracy
round(accuracy(fcast,ts1Test),2)

#Unsupervised Prediction
data("iris");library(ggplot2)
inTrain <- createDataPartition(y=iris$Species,p=0.7,list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training)
dim(testing)

kMeans1 <- kmeans(subset(training,select=-c(Species)),centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,color=clusters,data=training)
#Compare to real
table(kMeans1$cluster,training$Species)
#Build predictor
modFit <- train(clusters~.,data=subset(training,select=-c(Species)),method='rpart')
table(predict(modFit,training),training$Species)
#apply on test
testClusterPred <- predict(modFit,testing)
table(testClusterPred,testing$Species)
