####Import library
library(MASS)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ggplot2)
library(lubridate)
library(ROCR)
########

#### Import dataset
Bank_data <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
#View(Bank_data)
summary(Bank_data)
sapply(Bank_data,function(x){sum(is.na(x))})
str(Bank_data)
### Divinding data to ›train and test
Bank_data=Bank_data[c(-1,-40)]
Bank_data$ACC_OP_DATE = parse_date_time(x = Bank_data$ACC_OP_DATE,orders = c("d m y", "d B Y", "m/d/y"))
set.seed(60)
n=nrow(Bank_data)
trainIndex<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata<-Bank_data[trainIndex,]
testdata<-Bank_data[-trainIndex,]
#View(traindata)
#View(testdata)
prop.table(table(traindata$TARGET))
prop.table(table(testdata$TARGET))
#Bank_tree=rpart(TARGET~.,data=traindata)
#res<-predict(Bank_tree,newdata = testdata)
#prp(Bank_tree)
#fancyRpartPlot(Bank_tree)
class(traindata$TARGET)
traindata$TARGET=as.factor(traindata$TARGET)
testdata$TARGET=as.factor(testdata$TARGET)
library(caret)
numfold = trainControl(method = "cv", number = 10)
cpgrid = expand.grid(.cp=seq(0.001,0.005,0.0001))
cpVal = train(TARGET ~ ., data = traindata, method='rpart', trControl=numfold, tuneGrid=cpgrid)
cpVal



accu=integer();
accut=integer();
minbu=integer();
tempsen=integer();
tempsens=integer()
for (i in seq(1,n,10)){
  Bank_tree=rpart(TARGET ~ ., data = traindata,control=rpart.control(minbucket =i,minsplit = 10,xval = 10, cp=0.0022),method = "class")
  res<-predict(Bank_tree,newdata = traindata,type="class")
  result<-predict(Bank_tree,newdata = testdata,type="class")
  xy=table(traindata$TARGET,res)
  tempo=((xy[[1]]+xy[[4]])/nrow(traindata))
  tempsen=c(tempsen,xy[[2]]+xy[[4]])
  accut=c(accut,tempo) ##Ac
  xx=table(testdata$TARGET,result)
  temp=((xx[[1]]+xx[[4]])/nrow(testdata))
  tempsens=c(tempsens,xy[[2]]+xy[[4]])
  accu=c(accu,temp) ##Accuracy
  minbu=c(minbu,i)
}
accut

summary(accu)
summary(accut)
plot(accut~minbu,type="l")
plot(accu~minbu,type="l")
t=cbind(minbu,accu,accut)

View(t)
Bank_tree_final=rpart(TARGET ~ ., data = traindata,control=rpart.control(minbucket =20,minsplit = 10,xval = 10, cp=0.002),method = "class")
res<-predict(Bank_tree_final,newdata = traindata,type="class")
result<-predict(Bank_tree_final,newdata = testdata,type="class")
xy=table(traindata$TARGET,res)
Accuracytrain=((xy[[1]]+xy[[4]])/nrow(traindata))
xx=table(testdata$TARGET,result)
Accuracytest=((xx[[1]]+xx[[4]])/nrow(testdata))
prp(Bank_tree_final)
fancyRpartPlot(Bank_tree_final)




library("ROCR")
pred.data <- prediction(predict(Bank_tree_final, newdata = testdata,type = "prob")[, 2], testdata$TARGET)
plot(performance(pred.data, "tpr", "fpr"))
abline(0, 1, lty = 2)

#Accuracy plot
plot(performance(pred.data, "acc"))

###### Scaling data

Bank_data_scale <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
View(Bank_data_scale)
summary(Bank_data_scale)
sapply(Bank_data_scale,function(x){sum(is.na(x))})
str(Bank_data_scale)
### Divinding data to train and test
corp=round(cor(Bank_data_scale[c(2,3,5,8,9,12,13,19,20,21,22,26,27,31,32,33,34,35,36)]),2)
library('corrplot') #package corrplot

corrplot(corp,method = "color", addCoef.col="grey",tl.cex = 0.5,number.cex  = 0.5)
Bank_data_scale=Bank_data_scale[c(-1,-40)]
Bank_data_scale$ACC_OP_DATE = parse_date_time(x = Bank_data_scale$ACC_OP_DATE,orders = c("d m y", "d B Y", "m/d/y"))
Bank_data_scale$TARGET=as.factor(Bank_data_scale$TARGET)
#Bank_data_scale=scale(Bank_data_scale)

View(Bank_data_scale)

Bank_data_scale=Bank_data_scale[c(1,2,3,4,5,6,7,8,9,11,12,13,19,20,21,22,26,27,31,32,33,34,35,36)]
summary(Bank_data_scale)
#scaled=scale(Bank_data_scale[c(-1,-3,-5,-6,-9,-10)])
#View(scaled)
#str(scaled)
#scaled=scaled[-25]
#scaled=cbind(scaled,Bank_data_scale[c(1,3,5,6,9,10)])
#scaled$ACC_OP_DATE = parse_date_time(x = scaled$ACC_OP_DATE,orders = c("d m y", "d B Y", "m/d/y"))
#scaled$ACC_OP_DATE = scale(scaled$ACC_OP_DATE )
#scaled=scaled[-25]
set.seed(60)
n=nrow(Bank_data_scale)
trainIndex_scale<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata_scale<-Bank_data_scale[trainIndex_scale,]
testdata_scale<-Bank_data_scale[-trainIndex_scale,]

summary(traindata_scale)
summary(testdata_scale)

for (column in names(traindata_scale)){
  
  if (class(traindata_scale[, column]) != "factor"){
    traindata_scale[, column] = scale(traindata_scale[, column])
    traindata_scale[, column] = scale(traindata_scale[, column])
  }
  
  if (class(testdata_scale[, column]) != "factor"){
    testdata_scale[, column] = scale(testdata_scale[, column])
    testdata_scale[, column] = scale(testdata_scale[, column])
  }
  
}

prop.table(table(traindata_scale$TARGET))
prop.table(table(testdata_scale$TARGET))
accu_d=integer();
accut_d=integer();
minbu_d=integer();

for (i in seq(1,n,10)){
  scaled=rpart(TARGET ~ ., data = testdata_scale,control=rpart.control(minbucket =i,minsplit = 10,xval = 10, cp=0.002),method = "class")
  res_d<-predict(scaled,newdata = traindata_scale,type="class")
  result_d<-predict(scaled,newdata = testdata_scale,type="class")
  xy_d=table(traindata_scale$TARGET,res_d)
  tempo_d=((xy_d[[1]]+xy_d[[4]])/nrow(traindata_scale))
  accut_d=c(accut_d,tempo_d) ##Ac
  xx_d=table(testdata_scale$TARGET,result_d)
  temp_d=((xx_d[[1]]+xx_d[[4]])/nrow(testdata_scale))
  accu_d=c(accu_d,temp_d) ##Accuracy
  minbu_d=c(minbu_d,i)
}
summary(accut_d)
summary(accu_d)
plot(accut_d~minbu_d,type="l")
plot(accu_d~minbu_d,type="l")
accuray=cbind(minbu_d,accu_d,accut_d)
View(accuray)
Bank_tree_final_scale=rpart(TARGET ~ ., data = testdata_scale,control=rpart.control(minbucket =20,minsplit = 10,xval = 10, cp=0.002),method = "class")
res_d<-predict(Bank_tree_final_scale,newdata = traindata_scale,type="class")
result_d<-predict(Bank_tree_final_scale,newdata = testdata_scale,type="class")
xy_d=table(traindata_scale$TARGET,res_d)
xx_d=table(testdata_scale$TARGET,result_d)
tempo_d=((xy_d[[1]]+xy_d[[4]])/nrow(traindata_scale))
temp_d=((xx_d[[1]]+xx_d[[4]])/nrow(testdata_scale))

prp(Bank_tree_final_scale)
fancyRpartPlot(Bank_tree_final_scale)
##


###


#### Import dataset
Bank_data <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
#View(Bank_data)
summary(Bank_data)
sapply(Bank_data,function(x){sum(is.na(x))})
str(Bank_data)
### Divinding data to ›train and test
Bank_data=Bank_data[c(-1,-40)]
set.seed(60)
n=nrow(Bank_data)
trainIndex<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata<-Bank_data[trainIndex,]
testdata<-Bank_data[-trainIndex,]
#View(traindata)
#View(testdata)
prop.table(table(traindata$TARGET))
prop.table(table(testdata$TARGET))
#Bank_tree=rpart(TARGET~.,data=traindata)
#res<-predict(Bank_tree,newdata = testdata)
#prp(Bank_tree)
#fancyRpartPlot(Bank_tree)


accu=integer();
accut=integer();
minbu=integer();

for (i in seq(1,n,50)){
  Bank_tree=rpart(TARGET ~ ., data = testdata,control=rpart.control(minbucket =i,minsplit = 10,xval = 10, cp=0.0028),method = "class")
  res<-predict(Bank_tree,newdata = traindata,type="class")
  result<-predict(Bank_tree,newdata = testdata,type="class")
  xy=table(traindata$TARGET,res)
  tempo=((xy[[1]]+xy[[4]])/nrow(traindata))
  tempsen=((xy[[2]]+xy[[4]])/nrow(traindata))
  accut=c(accut,tempo) ##Ac
  xx=table(testdata$TARGET,result)
  temp=((xx[[1]]+xx[[4]])/nrow(testdata))
  tempsens=((xx[[2]]+xx[[4]])/nrow(testdata))
  accu=c(accu,temp) ##Accuracy
  minbu=c(minbu,i)
}
accut

summary(accu)
summary(accut)
plot(accut~minbu,type="l")
plot(accu~minbu,type="l")
accucy=cbind(accu,accut,minbu)
View(accucy)
plotdata<-cbind(accu,minbu)
fortify(data.frame(plotdata))
ggplot(as.data.frame(plotdata),aes(x=minbu,y=accu,fill=factor(accu)))+geom_bar(stat="identity",position="dodge")

Bank_tree_final=rpart(TARGET ~ ., data = testdata_scale,control=rpart.control(minbucket =1000,minsplit = 10,xval = 10, cp=0.0028),method = "class")
res<-predict(Bank_tree,newdata = traindata,type="class")
result<-predict(Bank_tree,newdata = testdata,type="class")
xy=table(traindata$TARGET,res)
xx=table(testdata$TARGET,result)
prp(Bank_tree_final)

plot(Bank_tree_final,uniform = "true",margin = 0.2)
text(Bank_tree_final,use.n = TRUE, all=TRUE, cex=.5)
fancyRpartPlot(Bank_tree_final)


######

###Random forest

library(randomForest)

Bank_data_forest <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
Bank_data_forest=Bank_data_forest[c(-1,-40)]
Bank_data_forest=Bank_data_forest[c(1,2,3,4,5,6,7,8,9,10,11,12,13,19,20,21,22,26,27,31,32,33,34,35,36)]
class(scaled_forest$ACC_OP_DATE)
scaled_forest=scale(Bank_data_forest[c(-1,-3,-5,-6,-9,-10)])
scaled_forest=cbind(scaled_forest,Bank_data_forest[c(1,3,5,6,10)])
View(scaled_forest)
summary(scaled_forest)
library(lubridate)
scaled_forest$ACC_OP_DATE = parse_date_time(x = scaled_forest$ACC_OP_DATE,orders = c("d m y", "d B Y", "m/d/y"))
rf=randomForest(TARGET~.,ntree=1000,mtry=5,data = scaled_forest)
pred=predict(rf)
pred[pred>=0.5]=1
pred[pred<0.5]=0
t=table(scaled_forest$TARGET,pred)
ac=(t[[1]]+t[[4]])/nrow(scaled_forest)
varImp(rf)
varImpPlot(rf)


library(pROC)
res = predict(rf, newdata = testBank, type = 'prob')

# Draw ROC curve.
result.roc <- roc(testBank$TARGET, res[,1])
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

pred.data <- prediction(predict(rf.fit, newdata = testBank,type = "prob")[, 2], testBank$TARGET)
plot(performance(pred.data, "tpr", "fpr"))
abline(0, 1, lty = 2)
#### random foresttt


library(randomForest)
library(caret)
numfold = trainControl(method = "cv", number = 10)
cpgrid = expand.grid(mtry=seq(3,10,3))
mtry_var = train(TARGET ~ ., data = trainBank, method='rf', trControl=numfold, tuneGrid=cpgrid)
mtry_var
Bank_data <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
Bank_data=Bank_data[c(-1,-40)]
Bank_data=Bank_data[c(1,2,3,4,5,6,7,8,9,10,11,12,13,19,20,21,22,26,27,31,32,33,34,35,36)]
Bank_data$ACC_OP_DATE = parse_date_time(x = Bank_data$ACC_OP_DATE,orders = c("d m y", "d B Y", "m/d/y"))
set.seed(60)
n=nrow(Bank_data)
trainIndex<-sample(1:n, size = round(0.70*n), replace=FALSE)
trainBank<-Bank_data[trainIndex,]
testBank<-Bank_data[-trainIndex,]
trainBank$TARGET=as.factor(trainBank$TARGET)
testBank$TARGET=as.factor(testBank$TARGET)
rf.fit = randomForest(TARGET ~ ., data = trainBank, ntree=500, mtry=10)
pred.test.rf = predict(rf.fit, newdata = testBank)
actual = testBank$TARGET
table(pred.test.rf, actual)
confusionMatrix(pred.test.rf, actual)
precision(pred.test.rf, actual)
varImp(rf.fit)
varImpPlot(rf.fit)


library(pROC)
res = predict(rf.fit, newdata = testBank, type = 'prob')

# Draw ROC curve.
result.roc <- roc(testBank$TARGET, res[,1])
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

pred.data <- prediction(predict(rf.fit, newdata = testBank,type = "prob")[, 2], testBank$TARGET)
plot(performance(pred.data, "tpr", "fpr"))
abline(0, 1, lty = 2)

#To get threshold and accuracy
result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)






####### Only import factor decison tree
Bank_data_last <- read.csv(file="/Users/rsklanu/Cart_assignmet/PL_XSELL.csv", header=TRUE)
summary(Bank_data_last)
sapply(Bank_data_last,function(x){sum(is.na(x))})
str(Bank_data_last)
### Divinding data to ›train and test
Bank_data_last=Bank_data_last[c(-1,-40)]
Bank_data_last$ACC_OP_DATE = parse_date_time(x = Bank_data_last$ACC_OP_DATE,orders = c("d m y", "d B Y", "m/d/y"))
Bank_data_last_G=Bank_data_last[c(1,2,3,4,5,7,8,12,13,19,20,22,26,32,33,34,35,36,37)]
Bank_data_last=Bank_data_last[c(1,2,3,4,5,7,8,12,13,19,20,22,26,32,33,34,35,36,37)]
Bank_data_last=Bank_data_last[-3]
summary(Bank_data_last)
str(Bank_data_last)
Bank_data_last_G_t=scale(Bank_data_last_G[c(-1,-3,-5)])
Bank_data_last_G_t=cbind(Bank_data_last_G_t,Bank_data_last_G[c(1,3,5)])
Bank_data_last_G=Bank_data_last_G_t
Bank_data_last_G$TARGET=as.factor(Bank_data_last_G$TARGET)
Bank_data_last_G_t=scale(Bank_data_last[c(-1,-4)])
Bank_data_last_G_t=cbind(Bank_data_last_G_t,Bank_data_last[c(1,4)])
Bank_data_last=Bank_data_last_G_t
Bank_data_last$TARGET=as.factor(Bank_data_last$TARGET)
set.seed(60)
n=nrow(Bank_data_last)
trainIndex<-sample(1:n, size = round(0.70*n), replace=FALSE)
traindata<-Bank_data_last[trainIndex,]
testdata<-Bank_data_last[-trainIndex,]
prop.table(table(traindata$TARGET))
prop.table(table(testdata$TARGET))





accu=integer();
accut=integer();
minbu=integer();

for (i in seq(1,n,50)){
  Bank_tree=rpart(TARGET ~ ., data = testdata,control=rpart.control(minbucket =i,minsplit = 10,xval = 10, cp=0.0028),method = "class")
  res<-predict(Bank_tree,newdata = traindata,type="class")
  result<-predict(Bank_tree,newdata = testdata,type="class")
  xy=table(traindata$TARGET,res)
  tempo=((xy[[1]]+xy[[4]])/nrow(traindata))
  tempsen=((xy[[2]]+xy[[4]])/nrow(traindata))
  accut=c(accut,tempo) ##Ac
  xx=table(testdata$TARGET,result)
  temp=((xx[[1]]+xx[[4]])/nrow(testdata))
  tempsens=((xx[[2]]+xx[[4]])/nrow(testdata))
  accu=c(accu,temp) ##Accuracy
  minbu=c(minbu,i)
}
accut

summary(accu)
summary(accut)
plot(accut~minbu,type="l")
plot(accu~minbu,type="l")
accucy=cbind(accu,accut,minbu)
summary(accucy)
