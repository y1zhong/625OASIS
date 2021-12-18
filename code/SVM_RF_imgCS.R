load("./data/img_list.rds")

library(ggplot2)
library(randomForest)
library(klaR)
library(MASS)
library(e1071)
library(caret)
library(dplyr)
library(mice)

S.new <- array (NA, dim = c(176,208,length(img_list)))

for (i in 1:length(img_list)){
  a = img_list[[i]]
  test = a[,,88]
  S.new[,,i] = test
}
#--------------------------------------------------------------------
#   Remove rows and columns with all 0
#--------------------------------------------------------------------
S.new =  S.new[apply(S.new != 0, 1, any), apply(S.new != 0, 2, any) ,]

data = read.csv("./data/oasis_cross-sectional_filter.csv")
oasis_f = data[,c(1,2,4:10)]
set.seed(123)
pos = sample(1:5,1)
mi.oasis_f = mice(oasis_f, m=5, printFlag =FALSE)
mi.temp.oasis_f = complete(mi.oasis_f,"all")
oasis_csdt = mi.temp.oasis_f[[pos]][,c(2:5,7)]
labels = oasis_csdt$CDR

n = nrow(S.new)
num_lst = 1:n
set.seed(123)
random_sample <- createDataPartition(num_lst, p = 0.8, list = FALSE)

train.X = S.new[,,random_sample]
test.X = S.new[,,-random_sample]

oasis_csdt$CDR = as.factor(oasis_csdt$CDR)
levels(oasis_csdt$CDR) <- c("EqaulTo0", "LargeThan0")
oasis_csdt$M.F = ifelse(oasis_csdt$M.F == "F", 1, 0)

train.demo = oasis_csdt[random_sample,]
test.demo = oasis_csdt[-random_sample,]
train.Y = as.numeric(train.demo$CDR)-1
test.Y = as.numeric(test.demo$CDR)-1

prep_binary_classification = function(images, labels, posLabel = 1, negLabel = 0) {
  dims = dim(images)
  X.pos = t(matrix(images[,,labels %in% posLabel],nrow=dims[1]*dims[2])) ## n1 * 36608
  X.neg = t(matrix(images[,,labels %in% negLabel],nrow=dims[1]*dims[2])) ## n1 * 36608
  return(list(X=rbind(X.pos, X.neg),y = c(rep(1,nrow(X.pos)),rep(0,nrow(X.neg)))))
}

trn = prep_binary_classification(train.X, train.Y, 1, 0)
tst = prep_binary_classification(test.X, test.Y, 1, 0)

# new_trnX = cbind(train.demo[,c(1:4)], trn$X)
# new_tstX = cbind(test.demo[,c(1:4)], tst$X)
# pca.X =prcomp(new_trnX)
# trn_X <- predict(pca.X, newdata = new_trnX)
# tst_X <- predict(pca.X, newdata =new_tstX)
# 
# Training_data <- cbind.data.frame(y=train.demo$CDR,trn_X)
# Testing_data <- cbind.data.frame(y=test.demo$CDR,tst_X)


pca.X =prcomp(trn$X)
trn_X <- predict(pca.X, newdata = trn$X)
tst_X <- predict(pca.X, newdata =tst$X)

Training_data <- cbind.data.frame(y=train.demo$CDR,train.demo[,c(1:4)],trn_X)
Testing_data <- cbind.data.frame(y=test.demo$CDR,test.demo[,c(1:4)],tst_X)
True_cdr = as.numeric(Testing_data$y)-1

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

# Sensitivity Analysis
FPFNSeSp <- function(TrueBeta=TrueBeta, Beta=Beta){
  FPR = length(which(TrueBeta==0 & Beta!=0))/length(TrueBeta)
  FNR= length(which(TrueBeta!=0 & Beta==0))/length(TrueBeta)
  Se = length(which(TrueBeta!=0 & Beta!=0))/length(which(TrueBeta!=0))
  Sp = length(which(TrueBeta==0 & Beta==0))/length(which(TrueBeta==0))
  return(c(FPR,FNR,Se,Sp))
}

ctrl <- trainControl(method = "cv", number=10, savePredictions=TRUE, classProbs=TRUE, allowParallel = T,verboseIter = T) 

## LDA classification
library(MASS)
set.seed(123)
lda.fit <-train(x = Training_data[,-1], y = Training_data$y, data = Training_data, method="sparseLDA",trControl=ctrl,tuneLength=10)
#lda.Yhat = lda.fit$pred$pred
lda.pred = predict(lda.fit, Testing_data)
lda.predprob=predict(lda.fit,Testing_data, type="prob")
lda.TrainAcc = max(na.omit(lda.fit$results$Accuracy))
lda.TestAcc = calc_acc(predicted = lda.pred, actual = Testing_data$y)
lda.cfmat = table(Prediction = lda.pred, Reference = True_cdr)
lda.FPFNSeSp = FPFNSeSp(TrueBeta = True_cdr,Beta = lda.pred)
lda.res = list(lda.fit=lda.fit,lda.TrainAcc=lda.TrainAcc,lda.TestAcc=lda.TestAcc,lda.cfmat=lda.cfmat,lda.FPFNSeSp=lda.FPFNSeSp,lda.predprob=lda.predprob)
lda.res

## SVM with Linear Kernel
set.seed(123)
svm.lin.fit <- train(y~., data = Training_data, method = "svmLinear2", trControl = ctrl, tuneLength = 10)
#svm.lin.Yhat = svm.lin.fit$pred[svm.lin.fit$results$cost == 0.5,]$pred
svm.lin.TrainAcc = max(svm.lin.fit$results["Accuracy"])
svm.lin.pred=predict(svm.lin.fit,Testing_data)
svm.lin.predprob=predict(svm.lin.fit,Testing_data, type="prob")
svm.lin.TestAcc = calc_acc(predicted = svm.lin.pred, actual = Testing_data$y)
svm.lin.cfmat = table(Prediction = svm.lin.pred, Reference = True_cdr)
svm.lin.FPFNSeSp = FPFNSeSp(TrueBeta = True_cdr,Beta = svm.lin.pred)
svm.lin.res = list(svm.lin.fit=svm.lin.fit,svm.lin.TrainAcc=svm.lin.TrainAcc,svm.lin.TestAcc=svm.lin.TestAcc,svm.lin.cfmat=svm.lin.cfmat,svm.lin.FPFNSeSp=svm.lin.FPFNSeSp,svm.lin.predprob=svm.lin.predprob)
svm.lin.res 

## SVM with Radial Kernel
set.seed(123)
svm.rad.fit <-train(y~., data = Training_data, method="svmRadial",trControl=ctrl,tuneLength=10)
#svm.rad.Yhat = svm.rad.fit$pred[svm.rad.fit$results$C == 0.5,]$pred
svm.rad.TrainAcc = max(svm.rad.fit$results["Accuracy"])
svm.rad.pred=predict(svm.rad.fit,Testing_data)
svm.rad.TestAcc = calc_acc(predicted = svm.rad.pred, actual = Testing_data$y)
svm.rad.predprob=predict(svm.rad.fit,Testing_data, type="prob")
svm.rad.cfmat = table(Prediction = svm.rad.pred, Reference = True_cdr)
svm.rad.FPFNSeSp = FPFNSeSp(TrueBeta = True_cdr,Beta = svm.rad.pred)
svm.rad.res = list(svm.rad.fit=svm.rad.fit,svm.rad.TrainAcc=svm.rad.TrainAcc,svm.rad.TestAcc=svm.rad.TestAcc,svm.rad.cfmat=svm.rad.cfmat,svm.rad.FPFNSeSp=svm.rad.FPFNSeSp,svm.rad.predprob=svm.rad.predprob)
svm.rad.res

## Ramdom Forest
set.seed(123)
rf.fit <-train(y~., data = Training_data, method="rf",trControl=ctrl,tuneLength=10)

rf.TrainAcc = max(rf.fit$results["Accuracy"])
rf.pred=predict(rf.fit,Testing_data)
rf.predprob=predict(rf.fit,Testing_data, type="prob")
rf.TestAcc = calc_acc(predicted = rf.pred, actual = Testing_data$y)
rf.cfmat = table(Prediction = rf.pred, Reference = True_cdr)
rf.FPFNSeSp = FPFNSeSp(TrueBeta = True_cdr,Beta = rf.pred)
rf.res = list(rf.fit=rf.fit,rf.TrainAcc=rf.TrainAcc,rf.TestAcc=rf.TestAcc,rf.cfmat=rf.cfmat,rf.FPFNSeSp=rf.FPFNSeSp,rf.predprob=rf.predprob)
rf.res

## Naive Bayes
set.seed(123)
nb.fit <-train(y~., data = Training_data, method="nb",trControl=ctrl,tuneLength=10)

nb.TrainAcc = max(nb.fit$results["Accuracy"])
nb.pred=predict(nb.fit,Testing_data)
nb.predprob=predict(nb.fit,Testing_data, type="prob")
nb.TestAcc = calc_acc(predicted = nb.pred, actual = Testing_data$y)
nb.cfmat = table(Prediction = nb.pred, Reference = True_cdr)
nb.FPFNSeSp = FPFNSeSp(TrueBeta = True_cdr,Beta = nb.pred)
nb.res = list(nb.fit=nb.fit,nb.TrainAcc=nb.TrainAcc,nb.TestAcc=nb.TestAcc,nb.cfmat=nb.cfmat,nb.FPFNSeSp=nb.FPFNSeSp,nb.predprob=nb.predprob)
nb.res

CDR_acc = data.frame(
  Model = c("LDA", "SVM linear",  "SVM radial","Random Forest","Naive Bayes"),
  TrainAccuracy = c(lda.TrainAcc, svm.lin.TrainAcc, svm.rad.TrainAcc, rf.TrainAcc,nb.TrainAcc),
  TestAccuracy = c(lda.TestAcc, svm.lin.TestAcc, svm.rad.TestAcc, rf.TestAcc, nb.TestAcc)
)
knitr::kable(CDR_acc)

pred_and_true = list(True_cdr=True_cdr,lda.pred = as.numeric(lda.pred)-1, svm.lin.pred = as.numeric(svm.lin.pred)-1,svm.rad.pred = as.numeric(svm.rad.pred)-1,rf.pred=as.numeric(rf.pred)-1,nb.pred=as.numeric(nb.pred)-1)

SVM_RF_ImgCS_result = list(lda.res = lda.res, svm.lin.res = svm.lin.res,svm.rad.res = svm.rad.res,rf.res=rf.res,nb.res = nb.res,CDR_acc=CDR_acc,pred_and_true= pred_and_true)

save(SVM_RF_ImgCS_result, file ="./result/SVM_RF_ImgCS_result.Rdata")
