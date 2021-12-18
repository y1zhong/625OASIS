cutoff = seq(1,0, by=-0.01)

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

load("result/SVM_RF_ImgCS_result.Rdata")
truth = SVM_RF_ImgCS_result[["pred_and_true"]][["True_cdr"]]
lda_ImgCS_rate = rate(SVM_RF_ImgCS_result[["lda.res"]][["lda.predprob"]])
svm_ImgCS_lin_rate = rate(SVM_RF_ImgCS_result[["svm.lin.res"]][["svm.lin.predprob"]])
svm_ImgCS_rad_rate = rate(SVM_RF_ImgCS_result[["svm.rad.res"]][["svm.rad.predprob"]])
nb_ImgCS_rate = rate(SVM_RF_ImgCS_result[["nb.res"]][["nb.predprob"]])

save(lda_ImgCS_rate, svm_ImgCS_lin_rate, svm_ImgCS_rad_rate,nb_ImgCS_rate,
     file='result/SVM_RF_ImgCS_rate.RData')

load("result/SVM_RF_ImgOnly_result.Rdata")
truth = SVM_RF_ImgOnly_result[["pred_and_true"]][["True_cdr"]]
lda_ImgOnly_rate = rate(SVM_RF_ImgOnly_result[["lda.res"]][["lda.predprob"]])
svm_ImgOnly_lin_rate = rate(SVM_RF_ImgOnly_result[["svm.lin.res"]][["svm.lin.predprob"]])
svm_ImgOnly_rad_rate = rate(SVM_RF_ImgOnly_result[["svm.rad.res"]][["svm.rad.predprob"]])
nb_ImgOnly_rate = rate(SVM_RF_ImgOnly_result[["nb.res"]][["nb.predprob"]])

save(lda_ImgOnly_rate, svm_lin_ImgOnly_rate, svm_rad_ImgOnly_rate,nb_ImgOnly_rate,
     file='result/SVM_RF_ImgOnly_result_rate.RData')

