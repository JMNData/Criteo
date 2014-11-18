#INITIALIZE LIBRARIES
library(RODBC)
library("e1071")
library("Metrics")
library("rpart")

#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\Criteo")

#GET DATA
myconn = odbcConnect("Citeo")
train = sqlQuery(myconn, "select top 1000000 * from train")
close(myconn) 
train.input = train[2:16]

#Train Data
model.rpart = rpart(Label~., data=train.input)
# print(model.rpart)
# summary(model.rpart)
# printcp(model.rpart)
# plot(model.rpart)
# text(model.rpart)
# rsq.rpart(model.rpart)

myconn = odbcConnect("Citeo")
test = sqlQuery(myconn, "select top 1000 * from test")
close(myconn)
test.input = test[2:15]
test.input = scale(test.input)

#Predict using tree model, Wirte back to Train to see results
Predicted.Train.tree = cbind(train, predict = predict(model.rpart, train.input, interval="predict"))
Predicted.Train.tree = cbind(test, predict = predict(model.rpart, test.input, interval="predict"))

out = c("datetime", "predict")
write.csv(Predicted.Test.tree[out], "data\\resultsTree.csv", row.names = FALSE)












