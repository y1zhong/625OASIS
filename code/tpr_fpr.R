cutoff = seq(1,0, by=-0.01)
#calculate true/false positive rate
rate = function(prob){
  rate_df = t(sapply(1:length(cutoff), function(i){
    pred <- ifelse(prob[,1] > cutoff[i], 1, 0)
    tp <- sum(pred==truth & pred == 1)
    fp <- sum(pred!=truth & pred == 1)
    tn <- sum(pred==truth & pred == 0)
    fn <- sum(pred!=truth & pred == 0)
    c(fp/(fp+tn), tp/(tp+fn))}))
  colnames(rate_df) = c('FPR','TPR')
  return(rate_df)
}

#with image only results
load("result/SVM_RF_ImgCS_result.Rdata")
truth = SVM_RF_ImgCS_result[["pred_and_true"]][["True_cdr"]]
lda_ImgCS_rate = rate(SVM_RF_ImgCS_result[["lda.res"]][["lda.predprob"]])
svm_ImgCS_lin_rate = rate(SVM_RF_ImgCS_result[["svm.lin.res"]][["svm.lin.predprob"]])
svm_ImgCS_rad_rate = rate(SVM_RF_ImgCS_result[["svm.rad.res"]][["svm.rad.predprob"]])
rf_ImgCS_rate =rate(SVM_RF_ImgCS_result[["rf.res"]][["rf.predprob"]])
nb_ImgCS_rate = rate(SVM_RF_ImgCS_result[["nb.res"]][["nb.predprob"]])

save(lda_ImgCS_rate, svm_ImgCS_lin_rate, svm_ImgCS_rad_rate,rf_ImgCS_rate, nb_ImgCS_rate,
     file='result/SVM_RF_ImgCS_result_rate.RData')

#with image and demographic
load("result/SVM_RF_ImgOnly_result.Rdata")
truth = SVM_RF_ImgOnly_result[["pred_and_true"]][["True_cdr"]]
lda_ImgOnly_rate = rate(SVM_RF_ImgOnly_result[["lda.res"]][["lda.predprob"]])
svm_lin_ImgOnly_rate = rate(SVM_RF_ImgOnly_result[["svm.lin.res"]][["svm.lin.predprob"]])
svm_rad_ImgOnly_rate = rate(SVM_RF_ImgOnly_result[["svm.rad.res"]][["svm.rad.predprob"]])
rf_ImgOnly_rate =rate(SVM_RF_ImgCS_result[["rf.res"]][["rf.predprob"]])
nb_ImgOnly_rate = rate(SVM_RF_ImgOnly_result[["nb.res"]][["nb.predprob"]])

save(lda_ImgOnly_rate, svm_lin_ImgOnly_rate, svm_rad_ImgOnly_rate,rf_ImgOnly_rate,nb_ImgOnly_rate,
     file='result/SVM_RF_ImgOnly_result_rate.RData')

