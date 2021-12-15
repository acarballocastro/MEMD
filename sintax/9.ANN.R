###############################################
# Título: 9. ANN
# Autor: Alba
# Fecha: 14/12/21

# Descripción: En este script empleamos redes 
# neuronales (ANN) para entrenar un 
# modelo que nos permita predecir cancelaciones
###############################################

library(MASS)
library(nnet)
library(caret)

set.seed(2021)

dd_ann <- read.csv("../data/data_preproc.csv")
str(dd_ann)

head(dd_ann)

# Dividimos en train/test
test <- sample(1:nrow(dd_ann),size = nrow(dd_ann)/3)
dataTrain <- dd_ann[-test,]
dataTest <- dd_ann[test,]

mynet <- nnet(data=dataTrain,class.ind(is_canceled) ~ ., entropy=T,size=11,decay=0,maxit=20000,trace=T)
mynet.resubst <- predict (mynet, dataTrain, type='class')

tab <- table(dataTrain$is_canceled, mynet.resubst)
mynet.resubst.error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
1-mynet.resubst.error

mynet.resubst <- predict (mynet, dataTest, type='class')

tab <- table(dataTest$is_canceled, mynet.resubst)
mynet.resubst.error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
1-mynet.resubst.error

# Hay mucho ruido, por eso tenemos un accuracy tan bajo
# Vamos a intentar aumentar el accuracy intentando predecir sólo con las variables numéricas

# Escalamos
dd_ann[c("lead_time", "stays_in_weekend_nights", "stays_in_week_nights", "adults",
         "children", "babies","booking_changes", "adr", "required_car_parking_spaces",
         "total_of_special_requests")] = scale(dd_ann[c("lead_time", "stays_in_weekend_nights", "stays_in_week_nights", "adults",
                                                        "children", "babies","booking_changes", "adr", "required_car_parking_spaces",
                                                        "total_of_special_requests")])

dd_anncat = dd_ann[c("is_canceled","lead_time", "stays_in_weekend_nights", "stays_in_week_nights", "adults",
         "children", "babies","booking_changes", "adr", "required_car_parking_spaces",
         "total_of_special_requests")]

dataTraincat <- dd_anncat[-test,]
dataTestcat <- dd_anncat[test,]

# Hyperparameter tuning
sizes = 1:25
acc = list()

for (s in sizes) {
  
  folds <- createFolds(dataTraincat$is_canceled, k = 10)
  
  cvNN <- lapply(folds, function(x){
    training_fold <- dataTraincat[-x, ]
    test_fold <- dataTraincat[x, ]
    clasificador <- nnet(data=training_fold, class.ind(is_canceled) ~ ., entropy=T,
                         size=s,decay=0,maxit=2000,trace=T)
    y_pred <- predict(mynet, test_fold, type='class')
    cm <- table(test_fold$is_canceled, y_pred)
    precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
    return(precision)
  })
  accuracy <- mean(unlist(cvNN))
  acc[[s]] <- accuracy
}

acc

mynet <- nnet(data=dataTraincat, class.ind(is_canceled) ~ ., entropy=T,size=19,decay=0,maxit=2000,trace=T)

mynet.resubst <- predict(mynet, dataTraincat, type='class')
tab <- table(dataTraincat$is_canceled, mynet.resubst)
mynet.resubst.error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
1-mynet.resubst.error

rownames(tab) = c("0","1")
confusionMatrix(tab)

mynet.resubst <- predict (mynet, dataTestcat, type='class')
tab <- table(dataTestcat$is_canceled, mynet.resubst)
mynet.resubst.error <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
1-mynet.resubst.error

colnames(tab) = c("canceled","not_canceled")
confusionMatrix(tab)

print(roc.plot(dataTest$is_canceled == 1, mynet.resubst)$roc.vol)
