car<-read.csv("C:/Users/lenovo/Documents/DATA/car.csv",header=TRUE)
summary(car)
View(car)
uv=quantile(car$disp,0.99)
car$disp[car$disp>uv]<-uv
summary(car$disp)
cor(car)
car<-car[,-2]#remove disp
set.seed(0)
split=sample.split(car,SplitRatio = 0.8)
settrain=subset(car,split==TRUE)
settest=subset(car,split==FALSE)
lm_car=lm(mpg~.,data=settrain)
summary(lm_car)
train_car=predict(lm_car,settrain)
test_car=predict(lm_car,settest)
