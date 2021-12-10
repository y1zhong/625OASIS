load("./data/img_list.rds")

library(ggplot2)
library(randomForest)
library(MASS)
library(e1071)
library(caret)
library(dplyr)

S.new <- array (NA, dim = c(176,208,length(img_list)))

for (i in 1:length(img_list)){
  a = img_list[[i]]
  test = a[,,88]
  S.new[,,i] = test
}

data = read.csv("./data/oasis_cross-sectional_filter.csv")
labels = data$CDR

n = nrow(S.new)
num_lst = 1:n
set.seed(123)
random_sample <- createDataPartition(num_lst, p = 0.8, list = FALSE)

train.X = S.new[,,random_sample]
test.X = S.new[,,-random_sample]

train.demo = data[random_sample,]
test.demo = data[-random_sample,]

train.Y = train.demo$CDR
test.Y = test.demo$CDR

prep_binary_classification = function(images, labels, posLabel = 1, negLabel = 0) {
  dims = dim(images)
  X.pos = t(matrix(images[,,labels %in% posLabel],nrow=dims[1]*dims[2])) ## n1 * 36608
  X.neg = t(matrix(images[,,labels %in% negLabel],nrow=dims[1]*dims[2])) ## n1 * 36608
  return(list(X=rbind(X.pos, X.neg),y = c(rep(1,nrow(X.pos)),rep(0,nrow(X.neg)))))
}

Training_data = prep_binary_classification(train.X, train.Y, 1, 0)
Testing_data = prep_binary_classification(test.X, test.Y, 1, 0)


ctrl <- trainControl(method = "cv", number=10, savePredictions=TRUE, classProbs=TRUE, allowParallel = T,verboseIter = T) 


library(e1071)
## SVM with Linear Kernel
set.seed(123)
svm.lin.fit <- train(y~., data = Training_data, method = "svmLinear2", trControl = ctrl, tuneLength = 10)

#svm.lin.Yhat = svm.lin.fit$pred[svm.lin.fit$results$cost == 0.5,]$pred

svm.lin.TrainAcc = max(svm.lin.fit$results["Accuracy"])
svm.lin.pred=predict(svm.lin.fit,Testing_data)
svm.lin.TestAcc = calc_acc(predicted = svm.lin.pred, actual = Testing_data$y)
svm.lin.res = list(svm.lin.fit=svm.lin.fit,svm.lin.TrainAcc=svm.lin.TrainAcc,svm.lin.TestAcc=svm.lin.TestAcc)

## SVM with Radial Kernel
set.seed(123)
svm.rad.fit <-train(y~., data = Training_data, method="svmRadial",trControl=ctrl,tuneLength=10)
#svm.rad.Yhat = svm.rad.fit$pred[svm.rad.fit$results$C == 0.5,]$pred
svm.rad.TrainAcc = max(svm.rad.fit$results["Accuracy"])
svm.rad.pred=predict(svm.rad.fit,Testing_data)
svm.rad.TestAcc = calc_acc(predicted = svm.rad.pred, actual = Testing_data$y)

svm.rad.res = list(svm.rad.fit=svm.rad.fit,svm.rad.TrainAcc=svm.rad.TrainAcc,svm.rad.TestAcc=svm.rad.TestAcc)

## Ramdom Forest
set.seed(123)
rf.fit <-train(y~., data = Training_data, method="rf",trControl=ctrl,tuneLength=10)

rf.TrainAcc = max(rf.lin.fit$results["Accuracy"])
rf.pred=predict(rf.fit,Testing_data)
rf.TestAcc = calc_acc(predicted = rf.pred, actual = Testing_data$y)

rf.res = list(rf.fit=rf.fit,rf.TrainAcc=rf.TrainAcc,rf.TestAcc=rf.TestAcc)

result = list(svm.lin.res = svm.lin.res,svm.rad.res = svm.rad.res,rf.res=rf.res)

save(result, file ="SVM_RF_result.rds")