#install.packages("RODBC")

#INITIALIZE LIBRARIES
library(RODBC)
library("parallel")
library("randomForest")
library("rpart")

#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\Criteo")

#GET DATA
myconn = odbcConnect("Citeo")
train = sqlQuery(myconn, "select top 10000 id, I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13, label from train")
close(myconn) 
train.input = train[2:15]

model = glm(label~I1+I2+I3+I4+I5+I6+I7+I8+I9+I10+I11+I12+I13, data=train.input, family=binomial())

myconn = odbcConnect("Citeo")
test = sqlQuery(myconn, "select top 1000 id, I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13 from test")
close(myconn)
test.input = test[2:14]


Predicted.Test = cbind(test, predicted = round(predict(model, test.input, interval="predict", type="response"), digits=10))
out = c("id", "predicted")
write.csv(Predicted.Test[out], "data\\results.csv", row.names = FALSE)

