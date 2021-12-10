library(caret)
library(ggplot2)
library(gtsummary)
library(mice)
library(e1071)
library(MASS)

oasis_f = read.csv("C:/Users/xtk96/Desktop/625_final/oasis_cross-sectional_filter.csv")

oasis_f2 = oasis_f[,c(1,2,4:10)]
oasis_f2$CDR = as.factor(oasis_f2$CDR)
oasis_f2$M.F = as.factor(oasis_f2$M.F)
levels(oasis_f2$CDR) <- c("EqaulTo0", "LargeThan0")
#---------------------------------------------
# summary table
#---------------------------------------------
tbl_summary(oasis_f2[,c(2:5,7)], by = 'CDR')
set.seed(123)
pos = sample(1:5,1)
mi.oasis_f2 = mice(oasis_f2, m=5, printFlag =FALSE)
mi.temp.oasis_f2 = complete(mi.oasis_f2,"all")
oasis_f3 = mi.temp.oasis_f2[[pos]]

attach(oasis_f3)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

## read in data
# take 80% to be training and 20% to be testing
n = nrow(oasis_f3)
num_lst = 1:n
set.seed(123)
random_sample <- createDataPartition(num_lst, p = 0.8, list = FALSE)

Training_data = oasis_f3[random_sample,]
Testing_data = oasis_f3[-random_sample,]

ctrl <- trainControl(method = "cv", number=10, savePredictions=TRUE, classProbs=TRUE)

#Training logistic regression
set.seed(123)
glm.fit <-train(CDR~M.F+Age+Educ+SES, data=Training_data, method="glm", trControl=ctrl, tuneLength=10)
glm.TrainAcc = glm.fit$results$Accuracy
glm.TrainAcc

glm.pred=predict(glm.fit, Testing_data)

glm.TestAcc = calc_acc(predicted = glm.pred, actual = Testing_data$CDR)
glm.TestAcc


## LDA classification
set.seed(123)
lda.fit <-train(CDR~M.F+Age+Educ+SES, data=Training_data, method="lda",trControl=ctrl,tuneLength=10)

#lda.Yhat = lda.fit$pred$pred
lda.pred = predict(lda.fit, Testing_data)
lda.TrainAcc = lda.fit$results$Accuracy
lda.TrainAcc 
lda.TestAcc = calc_acc(predicted = lda.pred, actual = Testing_data$CDR)
lda.TestAcc 

## SVM with Linear Kernel
set.seed(123)
svm.lin.fit <- train(CDR~M.F+Age+Educ+SES, data = Training_data, method = "svmLinear2", trControl = ctrl, tuneLength = 10)

#svm.lin.Yhat = svm.lin.fit$pred[svm.lin.fit$results$cost == 0.5,]$pred

svm.lin.TrainAcc = max(svm.lin.fit$results$Accuracy)
svm.lin.TrainAcc 

svm.lin.pred=predict(svm.lin.fit,Testing_data)
svm.lin.TestAcc = calc_acc(predicted = svm.lin.pred, actual = Testing_data$CDR)
svm.lin.TestAcc

## SVM with Radial Kernel
set.seed(123)
svm.rad.fit <-train(CDR~M.F+Age+Educ+SES, data=Training_data, method="svmRadial",trControl=ctrl,tuneLength=10)

#svm.rad.Yhat = svm.rad.fit$pred[svm.rad.fit$results$C == 0.5,]$pred

svm.rad.TrainAcc = max(svm.rad.fit$results$Accuracy)
svm.rad.TrainAcc

svm.rad.pred=predict(svm.rad.fit,Testing_data)

svm.rad.TestAcc = calc_acc(predicted = svm.rad.pred, actual = Testing_data$CDR)
svm.rad.TestAcc

## Ramdom Forest
set.seed(123)
rf.fit <-train(CDR~M.F+Age+Educ+SES, data=Training_data, method="rf",trControl=ctrl,tuneLength=10)

rf.TrainAcc = max(rf.fit$results$Accuracy)
rf.TrainAcc 

rf.pred=predict(rf.fit,Testing_data)

rf.TestAcc = calc_acc(predicted = rf.pred, actual = Testing_data$CDR)
rf.TestAcc

## GLMNET
set.seed(123)
glmnet.fit <-train(CDR~M.F+Age+Educ+SES, data=Training_data, method="glmnet",trControl=ctrl,tuneLength=10)
# alpha = 1 and lambda = 0.04241753
glmnet.TrainAcc = max(glmnet.fit$results$Accuracy)
glmnet.TrainAcc 

glmnet.pred=predict(glmnet.fit,Testing_data)

glmnet.TestAcc = calc_acc(predicted = glmnet.pred, actual = Testing_data$CDR)
glmnet.TestAcc

CDR_acc = data.frame(
  Model = c("GLM","LDA", "SVM linear",  "SVM radial","Random Forest", "glmnet"),
  TrainAccuracy = c(glm.TrainAcc,lda.TrainAcc, svm.lin.TrainAcc, svm.rad.TrainAcc, rf.TrainAcc,glmnet.TrainAcc ),
  TestAccuracy = c(glm.TestAcc,lda.TestAcc, svm.lin.TestAcc, svm.rad.TestAcc, rf.TestAcc,glmnet.TestAcc)
)
knitr::kable(CDR_acc)
