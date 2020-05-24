df<-read.csv("C:/Users/lenovo/Documents/DATA/Movie_classification.csv",header = TRUE)
summary(df)
df$Time_taken[is.na(df$Time_taken)]<-mean(df$Time_taken,na.rm=TRUE)
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
trainC=subset(df,split==TRUE)
testC=subset(df,split==FALSE)
trainC$Start_Tech_Oscar<-as.factor(trainC$Start_Tech_Oscar)
testC$Start_Tech_Oscar<-as.factor(testC$Start_Tech_Oscar)
install.packages("e1071")
svmfit=svm(Start_Tech_Oscar~.,data=trainC,kernel="linear",cost=1,scale=TRUE)
summary(svmfit)

#PREDICTION ON TEST SET
ypred=predict(svmfit,testC)
table(predict=ypred,actual=testC$Start_Tech_Oscar)

#CHECK SUPPORT VECTORS
svmfit$index
#plot(svmfit,trainC,Collection~Budget)

#tuning
tune.out=tune(svm,Start_Tech_Oscar~.,data=trainC,ranges=list(cost=c(0.001,0.01,0.1,1,10)))
bestmod=tune.out$best.model
summary(bestmod)
ypred2=predict(bestmod,testC)

#Polynomial Kernel
svmfitP=svm(Start_Tech_Oscar~.,data=trainC,kernel="polynomial",cost=1,degree=2)
#Hyperparameter tuning
tune.outP=tune(svm,Start_Tech_Oscar~.,data=trainC,kernel="polynomial",cross=4,ranges=list(cost=c(0.01,0.1,1,10),degree=c(0.5,1,2,3,5)))
bestmodP=tune.outP$best.model
summary(bestmodP)
ypredP=predict(svmfitP,testC)
table(predict=ypredP,actual=testC$Start_Tech_Oscar)

#Radial Kernel
svmfitR=svm(Start_Tech_Oscar~.,data=trainC,kernel="radial",cost=1,gamma=2)
#Radial Tuning
tune.outR=tune.outP=tune(svm,Start_Tech_Oscar~.,data=trainC,kernel="radial",cross=4,ranges=list(cost=c(0.01,0.1,1,10),gamma=c(0.5,1,2,4,10)))
bestmodR=tune.outR$best.model
summary(bestmodR)
ypredR=predict(svmfitR,testC)
table(predict=ypredR,actual=testC$Start_Tech_Oscar)

#SVM REGRESSION 
movie<-read.csv("C:/Users/lenovo/Documents/DATA/Movie_regression.csv",header=TRUE)
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken,na.rm=TRUE)
set.seed(0)
split=sample.split(movie,SplitRatio=0.8)
train=subset(movie,split==TRUE)
test=subset(movie,split==FALSE)
svmreg<-svm(Collection~.,data=train,kernel="linear",cost=1,scale=TRUE)
summary(svmreg)
collectionpred<-predict(svmreg,test)
MSE<-mean((collectionpred-test$Collection)^2)
