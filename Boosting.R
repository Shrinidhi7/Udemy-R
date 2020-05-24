install.packages("gbm")

movie<-read.csv("C:/Users/lenovo/Documents/DATA/Movie_regression.csv",header=TRUE)
movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken,na.rm=TRUE

#Test-Train Split 
set.seed(0)
split=sample.split(movie,SplitRatio=0.8)
train=subset(movie,split==TRUE)
test=subset(movie,split==FALSE)
#GBM BOOST
set.seed(0)
boosting=gbm(Collection~.,data=train,distribution="gaussian",n.trees=500,interaction.depth=4,shrinkage=0.2,verbose=F)
test$resultgbm<-predict(boosting,test,n.trees=500)
MSE2boost=mean((test$Collection-test$resultgbm)^2)
#adaboost
install.packages("adabag")
df<-read.csv("C:/Users/lenovo/Documents/DATA/Movie_classification.csv",header = TRUE)
df$Time_taken[is.na(df$Time_taken)]<-mean(df$Time_taken,na.rm=TRUE)
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
trainC=subset(df,split==TRUE)
testC=subset(df,split==FALSE)
trainC$Start_Tech_Oscar<-as.factor(trainC$Start_Tech_Oscar)
adaboost<-boosting(Start_Tech_Oscar~.,data=trainC,boos=TRUE)
predicvar<-predict(adaboost,testC)
table(predicvar$class,testC$Start_Tech_Oscar)
t1<-adaboost$trees[[1]]
plot(t1)
text(t1)
#XG Boost
install.packages("xgboost")
trainY=trainC$Start_Tech_Oscar=="1"
trainX<-model.matrix(Start_Tech_Oscar~.-1,data=trainC)
trainX<-trainX[,-12]
testY=testC$Start_Tech_Oscar=="1"
testX<-model.matrix(Start_Tech_Oscar~.-1,data=testC)
testX<-testX[,-12]
Xmatrix<-xgb.DMatrix(data=trainX,label=trainY)
Xmatrix_t<-xgb.DMatrix(data=testX,label=testY)
Xgboosting=xgboost(data=Xmatrix,nround=50,objective="multi:softmax",eta=0.3,num_class=2,max_depth=100)
xgpred<-predict(Xgboosting,Xmatrix_t)
table(testY,xgpred)
