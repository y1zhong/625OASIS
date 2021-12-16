library(caret)
library(glmnet)
load("C:/Users/xtk96/Desktop/625OASIS/data/img_list.rds")

S.new <- array (NA, dim = c(176,208,length(img_list)))

for (i in 1:length(img_list)){
  a = img_list[[i]]
  test = a[,,88]
  S.new[,,i] = test
}

S.new =  S.new[apply(S.new != 0, 1, any), apply(S.new != 0, 2, any) ,]

data = read.csv("C:/Users/xtk96/Desktop/625OASIS/data/oasis_cross-sectional_filter.csv")
labels = data$CDR

#--------------------------------------------------------------------
#   Separate Train and Test Data 8:2
#--------------------------------------------------------------------
n = dim(S.new)[3]   #from n = 198
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
  X.pos = t(matrix(images[,,labels %in% posLabel],nrow=dims[1]*dims[2])) ## n1 * (137*169)
  X.neg = t(matrix(images[,,labels %in% negLabel],nrow=dims[1]*dims[2])) ## n1 * (137*169)
  return(list(X=rbind(X.pos, X.neg),y = c(rep(1,nrow(X.pos)),rep(0,nrow(X.neg)))))
}

trn = prep_binary_classification(train.X, train.Y, 1, 0)
tst = prep_binary_classification(test.X, test.Y, 1, 0)


combined = prep_binary_classification (S.new, data$CDR, 1, 0)

cv.fit = cv.glmnet(combined$X, combined$y, family = "binomial", 
                   alpha = 0, 
                   standardize = FALSE, keep = TRUE)

best_param = cv.fit$lambda.min   #best parameter
best_coef = coef(cv.fit, s = "lambda.min")

#--------------------------------------------------------------------
#   Ridge Regression
#--------------------------------------------------------------------
ridge.fit <- glmnet(trn$X, trn$y, family="binomial", alpha = 0, lambda = best_param)
save(ridge.fit, file = 'ridge_fit.Rdata')
# # ridge.pred.trn <- predict(ridge.fit, trn$X, type="response")
# ridge.pred.tst <- predict(ridge.fit, tst$X, type="response")