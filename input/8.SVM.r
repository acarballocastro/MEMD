install.packages("e1071")
library(e1071)
library(caret)



dd_svm <- read.csv("../data/hotel_bookings_proc.csv")
dd_svm$is_canceled <- as.factor(dd_svm$is_canceled)



set.seed(2021)
test <- sample(1:nrow(dd_svm),size = nrow(dd_svm)/3)
dataTrain <- dd_svm[-test,]
dataTest <- dd_svm[test,]

# Setup for cross validation
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,         # do 5 repetitions of cv
                     summaryFunction=twoClassSummary,   # Use AUC to pick the best model
                     classProbs=TRUE)

str(dd_svm)
attach(dd_svm)

levels(dd_svm$hotel) <- c("City_Hotel", "Resort_Hotel")
levels(dd_svm$is_canceled) <- c("not_canceled", "canceled")
levels(dd_svm$deposit_type) <- c("No_deposit", "Non_refund", "Refundable")

str(dd_svm)

#Train and Tune the SVM
svm.tune <- train(is_canceled ~ .,
                  data = dd_svm, 
                  method = "svmLinear",
                  trControl=ctrl,
                  verbose = FALSE,
                  tuneLength = 1)
svm.tune


modelsvm <- svm(is_canceled ~ ., data=dataTrain, kernel = "linear", C = 1)
svmpred <- predict(modelsvm,dataTest[,-2])
tsvm <- table(svmpred, dataTest[,2])

accuracy <- (sum(diag(tsvm))/nrow(dataTest))
accuracy
