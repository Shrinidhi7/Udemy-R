#KNN
library(class, lib.loc = "C:/Program Files/R/R-3.6.3/library")
trainX=training_set[,-16]
testX=test_set[,-16]
trainY=training_set$Sold
testY=test_set$Sold
k=3
trainX_std=scale(trainX)
testX_std=scale(testX)
set.seed(0)
knn.pred=knn(trainX_std,testX_std,trainY,k=k)
table(knn.pred,testY)