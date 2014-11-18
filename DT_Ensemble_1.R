#INITIALIZE LIBRARIES
library(RODBC)
library("e1071")
library("Metrics")
library("rpart")

#Set Globals
setwd("C:\\Users\\Administrator\\Documents\\GitHub\\Criteo")

#GET DATA
myconn = odbcConnect("Citeo")
train = sqlQuery(myconn, "select top 4000000 id, Label, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13,convert(bigint,convert(varbinary, c1)) as c1,convert(bigint,convert(varbinary, c2)) as c2, convert(bigint,convert(varbinary, c5)) as c5, convert(bigint,convert(varbinary, c6)) as c6, convert(bigint,convert(varbinary, c8)) as c8, convert(bigint,convert(varbinary, c9)) as c9, convert(bigint,convert(varbinary, c14)) as c14, convert(bigint,convert(varbinary, c17)) as c17, convert(bigint,convert(varbinary, c20)) as c20, convert(bigint,convert(varbinary, c22)) as c22, convert(bigint,convert(varbinary, c23)) as c23, convert(bigint,convert(varbinary, c25)) as c25 from train")
close(myconn) 
train.input = sapply(train[16:27], as.numeric)

# K-Means Cluster Analysis
fit = kmeans(train.input, 10) # 5 cluster solution
aggregate(train.input,by=list(fit$cluster),FUN=mean)
train <- data.frame(train, fit$cluster) 

#GLM
glminc = c("Label","I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12", "I13", "fit.cluster")
train.input = train[glminc]
remove(train)

model = glm(Label~., data=train.input, family=binomial())
remove(train.input)
remove(fit)
remove(glminc)
remove(myconn)
gc()
save(model, file="model_4mmrecordstraining.rda")

###RUN THIS SECTION ONLY IF MODEL SAVED OFF AND BUILT 
load(file="model_4mmrecordstraining.rda")

#Test Data
myconn = odbcConnect("Citeo")
test = sqlQuery(myconn, "select id, I1, I2, I3, I4, I5, I6, I7, I8, I9, I10, I11, I12, I13,convert(bigint,convert(varbinary, c1)) as c1,convert(bigint,convert(varbinary, c2)) as c2, convert(bigint,convert(varbinary, c5)) as c5, convert(bigint,convert(varbinary, c6)) as c6, convert(bigint,convert(varbinary, c8)) as c8, convert(bigint,convert(varbinary, c9)) as c9, convert(bigint,convert(varbinary, c14)) as c14, convert(bigint,convert(varbinary, c17)) as c17, convert(bigint,convert(varbinary, c20)) as c20, convert(bigint,convert(varbinary, c22)) as c22, convert(bigint,convert(varbinary, c23)) as c23, convert(bigint,convert(varbinary, c25)) as c25 from test")
close(myconn)
test.input = sapply(test[15:26], as.numeric)
fit = kmeans(test.input, 10) # 5 cluster solution
#aggregate(test.input,by=list(fit$cluster),FUN=mean)
test <- cbind(test, fit$cluster) 

test = test[0:27]
remove(test.input)
remove(fit)
remove(myconn)
gc()

glminc2 = c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12", "I13", "fit$cluster")
test.input = test[glminc2]
names(test.input)[14] = "fit.cluster"

Predicted.Test = cbind(test, predicted = round(predict(model, test.input, interval="predict", type="response"), digits=10))
out = c("id", "predicted")
write.csv(Predicted.Test[out], "data\\resultsES.csv", row.names = FALSE)


