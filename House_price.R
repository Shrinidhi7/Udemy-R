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
