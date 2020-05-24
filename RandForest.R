#BAGGING
install.packages("randomForest")
movie<-read.csv("C:/Users/lenovo/Documents/DATA/Movie_regression.csv",header=TRUE)
df<-read.csv("C:/Users/lenovo/Documents/DATA/Movie_classification.csv",header = TRUE)
df$Time_taken[is.na(df$Time_taken)]<-mean(df$Time_taken,na.rm=TRUE)
#SPLIT 
set.seed(0)
split2=sample.split(df,SplitRatio = 0.8)
trainC=subset(df,split2==TRUE)
testC=subset(df,split2==FALSE)
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken,na.rm=TRUE)
#Test-Train Split
set.seed(0)
split=sample.split(movie,SplitRatio=0.8)
train=subset(movie,split==TRUE)
test=subset(movie,split==FALSE)
#BAGGING
bagginga=randomForest(formula=Collection~.,data=train,mtry=17)
test$bagging<-predict(bagginga,test)
MSEbagging<-mean((test$Collection-test$bagging)^2)
plot(bagginga)
#RANDOMFOREST
randomfor<-randomForest(formula=Collection~.,data=train,ntree=500)
test$randomforest<-predict(randomfor,test)
MSErandfor<-mean((test$Collection-test$randomforest)^2)
#
classify<-randomForest(formula=Start_Tech_Oscar~.,data=trainC,ntree=500,method="class")
testC$oscar<-predict(classify,testC,type="class")
testC$oscar[testC$oscar>0.5]<-1
testC$oscar[testC$oscar<0.5]<-0
table(testC$Start_Tech_Oscar,testC$oscar)