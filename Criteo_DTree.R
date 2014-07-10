#INITIALIZE LIBRARIES
library(RODBC)
library("e1071")
library("Metrics")
library("rpart")

#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\Criteo")

#GET DATA
myconn = odbcConnect("Citeo")
train = sqlQuery(myconn, "select top 10000 * from train")
close(myconn) 
train.input = train[2:15]

#Train Data
model.rpart = rpart(label~., data=Train1)
print(model.rpart)
summary(model.rpart)
printcp(model.rpart)
plot(model.rpart)
text(model.rpart)
rsq.rpart(model.rpart)

myconn = odbcConnect("Citeo")
test = sqlQuery(myconn, "select top 1000 id, I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13 from test")
close(myconn)
test.input = test[2:14]


#Predict using tree model, Wirte back to Train to see results
Predicted.Train.tree = cbind(Train, predict = as.integer(predict(model.rpart, Train1[,-1], interval="predict")))
Predicted.Test.tree = cbind(Test, predict = as.integer(predict(model.rpart, Test[,-1], interval="predict")))
Predicted.Test.tree$predict = ifelse(Predicted.Test.tree$predict < 0, 0, Predicted.Test.tree$predict)
out = c("datetime", "predict")
write.csv(Predicted.Test.tree[out], "data\\resultsTree.csv", row.names = FALSE)












