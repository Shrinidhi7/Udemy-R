? iris
str(iris)
str(iris3)
iris
iris3
data("iris")
X=scan()
J<-read.table("C:/Users/lenovo/Documents/DATA/data1.txt",header=TRUE,sep="\t")

str(J)

P<-read.table("C:/Users/lenovo/Documents/DATA/original.txt",header=TRUE,sep="\t")
q<-read.csv("C:/Users/lenovo/Documents/DATA/original.xls",header=TRUE)
View(q)
y<-table(q$Region)
View(y)
barplot(y)
barplot(y[order(y)])
barplot(y[order(-y)],horiz=TRUE,col =c("red","green","blue","yellow"),main="Frequencies of Regions")
hist(q$Age)
hist(q$Age,breaks=c(0,40,60,100),freq = TRUE,col="blue",main="Histogram of Age")
