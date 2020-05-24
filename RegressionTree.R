movie<-read.csv("C:/Users/lenovo/Documents/DATA/Movie_regression.csv",header=TRUE)
summary(movie)
movie$Time_taken[is.na(movie$Time_taken)]<-mean(movie$Time_taken,na.rm=TRUE)
pairs(~Marketing.expense+Collection,data=movie)
boxplot(movie$Marketing.expense)
#Test-Train Split
set.seed(0)
split=sample.split(movie,splitRatio=0.8)
train=subset(movie,split=TRUE)
test=subset(movie,split=FALSE)
install.packages("rpart")
install.packages("rpart.plot")
#Decision Tree
regtree<-rpart(formula=Collection~.,data=train,control=rpart.control(maxdepth = 3))
#Plot Decision Tree
rpart.plot(regtree,box.palette="green",digits=-3)
#PREDICTION
test$pred<-predict(regtree,test,type="vector")
MSE<-mean((test$pred-test$Collection)^2)
#PRUNING
fulltree<-rpart(formula=Collection~.,data=train,control=rpart.control(cp=0))
printcp(fulltree)
plotcp(fulltree)
mincp<-fulltree$cptable[which.min(fulltree$cptable[,"xerror"]),"CP"]
prunedtree=prune(fulltree,cp=mincp)
rpart.plot(prunedtree,digits=-3)
test$fulltree<-predict(fulltree,test,type="vector")
test$prunedtree<-predict(prunedtree,test,type="vector")
MSEfull=mean((test$Collection-test$fulltree)^2)
MSEprune=mean((test$Collection-test$prunedtree)^2)
