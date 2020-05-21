df<-read.csv("C:/Users/lenovo/Documents/DATA/House_Price.csv",header=TRUE)
str(df)
summary(df)
hist(df$crime_rate)
pairs(~price+crime_rate+n_hot_rooms+rainfall,data=df)
barplot(table(df$airport))
barplot(table(df$waterbody))
barplot(table(df$bus_ter))
#Observations
#n_hot_rooms and rainfall has outliers
#n_hos_beds has missing values
#bus_ter is useless variable
#Crime_rate has some other functional relationship with price
quantile(df$n_hot_rooms,0.99)
uv=3*quantile(df$n_hot_rooms,0.99)
df$n_hot_rooms[df$n_hot_rooms>uv]<-uv
summary(df$n_hot_rooms)
lv=0.3*quantile(df$rainfall,0.01)
df$rainfall[df$rainfall<lv]<-lv
summary(df$rainfall)
j=mean(df$n_hos_beds,na.rm = TRUE)
which(is.na(df$n_hos_beds))
df$n_hos_beds[is.na(df$n_hos_beds)]<-mean(df$n_hos_beds,na.rm=TRUE)
summary(df$n_hos_beds)
which(is.na(df$n_hos_beds))
pairs(~price+crime_rate,data=df)
plot(df$price,df$crime_rate)
df$crime_rate=log(1+df$crime_rate)
df$avg_dist=(df$dist1+df$dist2+df$dist3+df$dist4)/4
df2<-df[,-7:-10,]
df<-df2
rm(df2)
df<-df[,-14]
install.packages("dummies")
df<-dummy.data.frame(df)
df<-df[,-9]
df<-df[,-14]
cor(df)
round(cor(df),2)
#PARKS AND AIR QUALITY ARE HIGHLY CO_RELATED and henceone of them is removed to avoid multi-colinearity
df<-df[,-16]