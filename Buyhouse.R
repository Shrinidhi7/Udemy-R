df<-read.csv("C:/Users/lenovo/Documents/DATA/House-Price.csv",header = TRUE)
summary(df)
boxplot(df$n_hot_rooms)
pairs(~df$Sold+df$rainfall)
uv<-3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv]<-uv
summary(df$n_hot_rooms)
lv<-0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<lv]<-lv
df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds,na.rm=TRUE)
summary(df$n_hos_beds)
df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4
df2<-df[,-6:-9]
df<-df2
rm(df2)
df<-df[,-13]
df<-dummy.data.frame(df)
df<-df[,-8]
df<-df[,-13]
View(df)
#LOGISTIC REGRESSION
glm.fit=glm(Sold~.,data=df,family=binomial)
summary(glm.fit)
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
View(glm.probs)
glm.pred=rep("NO",506)
glm.pred[glm.probs>0.5]="YES"
table(glm.pred,df$Sold)
#LINEAR DISCRIMINANT ANALYSIS
lda.fit=lda(Sold~.,data=df)
lda.fit
lda.pred=predict(lda.fit,df)
lda.pred$posterior
lda.class=lda.pred$class
View(lda.pred$class)
table(lda.class,df$Sold)
k=sum(lda.pred$posterior[,1]>0.8)
#Logistic Regression SPLIT DAtA
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
training_set=subset(df,split==TRUE)
test_set=subset(df,split==FALSE)
train.fit=glm(Sold~.,data=training_set,family=binomial)
test.probs=predict(train.fit,test_set,type="response")
test.pred=rep("NO",120)
test.pred[test.probs>0.5]="YES"
table(test.pred,test_set$Sold)