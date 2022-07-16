# step1:Insert Data
rm(list=ls())
setwd("F:/DT")
df=read.csv("2021_080910_time.csv")
head(df)
table(df$shutter)
shuffle.index=sample(1:nrow(df))
class(df)

#step2:Charactertistics
varlist=c("In_Building","Light_I","Desk_I","Desk_T","Desk_H","wind_speed","Light_on_off",
          "wind_direction","temperature","humidity","radiation","PM10","PM25","shutter")
df=df[c(varlist)]
df=na.omit(df)
df=as.data.fram(lapply(df,as.numeric))
df$shutter=cut(df$shutter, breaks = c(0,0.4,0.6,1.1),labels = c('<0.5','=0.5','>0.5'),
               include.lowest = T,ordered_result = T)

#step3:Create train set and test set
library(caret)
train.index = createDataPartition(df$shutter, p = 0.9, list= FALSE)
train.data = df[train.index, ]
test.data = df[-train.index, ]
prop.table(table(train.data$shutter)) 

#step4:Construct Decision Tree Model
#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
tree.fit = rpart(shutter~.,data=train.data,method='class')
par(mfrow=c(1,1))
rpart.plot(tree.fit, extra= 100)

#step5:Prediction
predict.unseen = predict(tree.fit, test.data, type= 'class')

#step6:Assessment
table.mat <- table(test.data$shutter, predict.unseen)
table.mat
accuracy.test <- sum(diag( table.mat)) / sum( table.mat)
print(paste( 'Accuracy for test', accuracy.test))

#step7:Improve the model
AccuracyTune <- function(fit){
  predict.unseen <- predict(fit, test.data, type= 'class')
  table.mat <- table(test.data$shutter, predict.unseen)
  accuracy.test <- sum(diag( table.mat)) / sum( table.mat)
  accuracy.test
}
control = rpart.control(minsplit = 100, minbucket = 10,maxdepth =5,cp=0.005)
tune.fit <- rpart(shutter ~ ., data = train.data, method = 'class', control = control)
print(paste('Accuracy for test', AccuracyTune(tune.fit)))
par(mfrow=c(1,1))
rpart.plot(tune.fit, extra= 100)
