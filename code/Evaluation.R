## Purpose: Evaluation
## Author: CC
## last update: Dec 17, 2021

## Packages --------------------------
library(dplyr)
library(caret)
library(tictoc)
library(ggplot2)
library(glmnet)
library(data.table)
library(ggpubr)
library(knitr)


## Load training and test case from teammates code------------------------------

load("./data/img_list.rds")
a = img_list[[1]]                           #read in 3D array 176x208x176
S.new <- array (NA, dim = c(176, 208, length(img_list)))

for (i in 1 : length(img_list)){            #select slice 88 and create array 176x208x198
  a = img_list[[i]]
  test = a[, , 88]
  S.new[, , i] = test
}

#   Remove rows and columns with all 0
S.new =  S.new[apply(S.new != 0, 1, any),   #137x169x198
               apply(S.new != 0, 2, any), ]
load("./data/oasis_csdt.Rdata")
labels = oasis_csdt$CDR

#   Partition Train and Test Data 8:2
n = dim(S.new)[3]   #from n = 198
num_lst = 1:n
set.seed(123)
random_sample <- createDataPartition(num_lst, p = 0.8, list = FALSE)

train.X = S.new[, , random_sample]
test.X  = S.new[, , - random_sample]

train.demo = oasis_csdt[random_sample, ]
test.demo  = oasis_csdt[ - random_sample, ]

train.Y = train.demo$CDR
test.Y  = test.demo$CDR

prep_binary_classification = function(images, labels, posLabel = 1, negLabel = 0) {
  dims = dim(images)
  X.pos = t( matrix(images[, , labels %in% posLabel], nrow=dims[1] * dims[2]) ) ## n1 x (137x169)
  X.neg = t( matrix(images[, , labels %in% negLabel], nrow=dims[1] * dims[2]) ) ## n1 x (137x169)
  return( list(X=rbind(X.pos, X.neg),
               y = c(rep(1,nrow(X.pos)), rep(0,nrow(X.neg)))))
}

trn = prep_binary_classification(train.X, train.Y, 1, 0)
tst = prep_binary_classification(test.X, test.Y, 1, 0)

#prepare whole data for cross-validation
combined = prep_binary_classification (S.new, oasis_csdt$CDR, 1, 0)

train.demo2 = subset(train.demo, select = -c(CDR))
test.demo2 = subset(test.demo, select = -c(CDR))
data2 = subset(oasis_csdt, select = -c(CDR))

train.demo2$M.F = ifelse(train.demo2$M.F == "F", 1, 0)
test.demo2$M.F  = ifelse(test.demo2$M.F == "F", 1, 0)
data2$M.F       = ifelse(data2$M.F == "F", 1, 0)

demo.trn.X = data.matrix( cbind(train.demo2, trn$X) )
demo.tst.X = data.matrix( cbind(test.demo2, tst$X) )
demo.combined.X = data.matrix( cbind(data2, combined$X) )

## synthesis results-----------------

# load model results
results<-Sys.glob(file.path("./result","*ata"))
for(i in 1:length(results)) load(results[[i]]) 

lda<-data.table(SVM_RF_ImgOnly_result$lda.res$lda.cfmat)
svm.lin<-data.table(SVM_RF_ImgOnly_result$svm.lin.res$svm.lin.cfmat)
rf<-data.table(SVM_RF_ImgOnly_result$rf.res$rf.cfmat)
nb<-data.table(SVM_RF_ImgOnly_result$nb.res$nb.cfmat)
lda.cs<-data.table(SVM_RF_ImgCS_result$lda.res$lda.cfmat)
svm.lin.cs<-data.table(SVM_RF_ImgCS_result$svm.lin.res$svm.lin.cfmat)
rf.cs<-data.table(SVM_RF_ImgCS_result$rf.res$rf.cfmat)
nb.cs<-data.table(SVM_RF_ImgCS_result$nb.res$nb.cfmat )

# put models in a list
models<-list(lasso.fit, ridge.fit)
ml.list<-list(lda,svm.lin,rf,nb, lda.cs, svm.lin.cs,rf.cs,nb)

# combined all resutls as the same structure
for (i in 1:length(ml.list)) {
  temp_df<-setDT(ml.list[[i]])
  temp_df[,Predicted:=c(0,1,0,1)][,True:=Reference]
  models[[2+i]]<-temp_df
}

models<-append(models, list(demo.fit), 6)

# name list
models.name<-c("Lasso: Image only","Ridge: Image only",
               "LDA: Image only", "SVM-L: Image only","RF : Image only","NB : Image only",
               "Ridge: Image+CS", "LDA: Image+CS", "SVM-L: Image+CS", "RF : Image+CS","NB : Image CS")

length(models)==length(models.name)

## Synthesis all results into one table list and confusion plot for all models------------
table_list<-list()
plot_list<-list()

for (i in 1:length(models)) {
  
  # create confusion matrix 
  if (i<=2){ # ridge and lasso with image only
    confusion<- data.table(confusion.glmnet(models[[i]], newx = tst$X, newy = tst$y))
  }
  if (i==7){ # ridge with image + CS
    confusion<- data.table(confusion.glmnet(models[[i]], newx = demo.tst.X, newy = tst$y))
  }
  if (i>2 & i!=7){ # other models do not need to construct fonfusion table
    confusion<-models[[i]]
  }
  
  # calculate sensitivity, specificity, and accuracy from all confusion matrix
  Accuracy<-round(confusion[Predicted==True][, sum(N)]/(confusion[,sum(N)]), digits = 2)
  plotTable<-confusion[,prop:=round(N/sum(N), digits = 2), by=True][,goodbad:=ifelse(Predicted==True,"Good", "Bad" )]
  Sensitivity<-plotTable[Predicted==1 & True==1][,prop]
  Specificity<-plotTable[Predicted==0 & True==0][,prop]
  
  # create confusion plot
  plot<-ggplot(data = plotTable, mapping = aes(x = True, y = Predicted, fill = goodbad, alpha = prop)) +
    geom_tile() +
    geom_text(aes(label = prop), vjust = .5, fontface  = "bold", alpha = 1) +
    scale_fill_manual(values = c(Good = "steelblue", Bad = "red")) +
    labs(title = paste0(models.name[i],": Accuracy ",accuracy), x = "True dementia", y="Predictive dementia")+
    theme_bw()+
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  ggexport(plot, filename = paste0("./tables and figures/", models.name[[i]]," confusion matrix.png"), res = 600, width = 6000, height = 6000)
  
  # save plot and tables
  plot_list[[i]]<-plot
  table_list[[i]]<-rbind(models.name[[i]], Sensitivity,Specificity,Accuracy)
}


# customized a data table for report table 2
image_cs<-c("","","","Image Only","","","","","","Image+CS","","")
models.name2<-c("","Lasso","Ridge",
                "LDA", "SVM-L","RF", "NB",
                "Ridge", "LDA", "SVM-L", "RF", "NB")
stat<-c("","Sensitivity","Specificity","Accuracy")
result_table<-as.data.frame(rbind(image_cs,models.name2,cbind(stat,do.call(cbind,table_list))))
result_table<-result_table[-c(1,3),]
# result_table<-result_table[-1,]
colnames(result_table) <- c("Evaluation", models.name)
rownames(result_table)<-NULL
save(result_table, file = "./tables and figures/result_table.RData")

### Accuracy plot: repeated information as table, no need to include in report
accuracy_table<-as.data.frame(t(do.call(cbind,table_list)))
accuracy_table$Accuracy<-as.numeric(accuracy_table$Accuracy)
colnames(accuracy_table)[1]<-"Model_detail"
accuracy_table$Sensitivity<-NULL
accuracy_table$Specificity<-NULL
accuracy_table$Type<-c(rep("Image only", 5), rep("Image+CS",4))
accuracy_table$Model<-c("Lasso","Ridge","LDA", "SVM-L","RF","Ridge", "LDA", "SVM-L", "RF")

# accuracy plot
library(lemon)
library(wesanderson)
ggplot(data = accuracy_table, 
       mapping = aes(x=ifelse(Type=="Image only", yes=-Accuracy, no = Accuracy),
                     y=Model, fill=Type))+
  geom_col(width=0.5)+
  scale_x_symmetric(labels=abs)+
  scale_fill_manual(values=wes_palette(n=2, name = "GrandBudapest2"))+
  labs(x="Accuracy")+
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


## ROC plot----------------
# simple version
roc<-as.data.frame(t(cbind(do.call(cbind,table_list))))
colnames(roc)[1]<-"Model"
roc$Accuracy<-NULL

roc<-rbind(roc,cbind(Model=models.name,Sensitivity=rep(0,11), Specificity=rep(1,11)))
roc<-rbind(roc,cbind(Model=models.name,Sensitivity=rep(1,11), Specificity=rep(0,11)))
roc$Sensitivity<-as.numeric(roc$Sensitivity)
roc$Specificity<-as.numeric(roc$Specificity)

ggplot(data=roc, aes(x=(1-Specificity),y=Sensitivity), group = Model, color=Model)+
  geom_line(aes(group=Model, color=Model))+
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#### final version
roc.results<-list(roc.lasso, roc.ridge, roc.demo, lda_ImgOnly_rate,lda_ImgCS_rate,rf_ImgOnly_rate, rf_ImgCS_rate)
roc.names<-c("Lasso: Image only","Ridge: Image only","Ridge: Image+CS", "LDA: Image only", "LDA: Image+CS","RF: Image only", "RF: Image+CS")
ROC<-data.frame()
for (i in 1:length(roc.results)){
  temp<-cbind(Model=rep(roc.names[i],nrow(roc.results[[i]])), roc.results[[i]])
  ROC<-rbind(ROC,temp)
}

ROC$FPR<-as.numeric(ROC$FPR)
ROC$TPR<-as.numeric(ROC$TPR)

roc_plot<-ggplot(data=ROC, aes(x=FPR,y=TPR), group = Model, color=Model)+
  geom_step(aes(group=Model, color=Model))+
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  labs(title = "Figure 2. ROC Curve for selected models")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggexport(roc_plot, filename ="./tables and figures/ROC.png", res = 600, width = 6000, height = 6000)



