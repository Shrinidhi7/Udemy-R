install.packages("caTools")
set.seed(0)
split=sample.split(df,SplitRatio = 0.8)
training_set=subset(df,split==TRUE)