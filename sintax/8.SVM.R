###############################################
# Título: 3.Preprocessing
# Autor: Alba, Pol
# Fecha: 06/12/21

# Descripción: En este script empleamos el método
# Support Vector Machine (SVM) para entrenar un 
# modelo que nos permita predecir cancelaciones
###############################################

# Instalamos las librerías necesarias
pkg <- c("e1071","caret", "rpart")
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]

if (length(new.pkg)){ 
  install.packages(new.pkg, dependencies = TRUE) ; rm(c('pkg','new.pkg'))
  }

# Cargamos las librerías
library(e1071)
library(caret)
library(rpart)

# Lectura de la base de datos
dd_svm <- read.csv("../data/data_preproc.csv")
str(dd_svm)

set.seed(2021)

# Dividimos en train/test
test <- sample(1:nrow(dd_svm),size = nrow(dd_svm)/3)
dataTrain <- dd_svm[-test,]
dataTest <- dd_svm[test,]

# Setup para cross validation
ctrl <- trainControl(method="repeatedcv",   # 10-fold cross validation
                     repeats=5,         # hace 5 repeticiones of cv
                     summaryFunction=twoClassSummary, 
                     classProbs=TRUE)

#Train and Tune el SVM
svm.tune <- train(is_canceled ~ .,
                  data = dd_svm, 
                  method = "svmLinear",
                  trControl=ctrl,
                  verbose = FALSE,
                  tuneLength = 1)

# Vemos cuales son los hiperparámetros escogidos
svm.tune

# Creamos el modelo con estos parámetros usando un kernel linear
modelsvm <- svm(is_canceled ~ ., data=dataTrain, kernel = "linear", cost = 1, scale = FALSE)
print(modelsvm)

# Predecimos para los datos test
svmpred <- predict(modelsvm,dataTest[,-2])

# Para ver como ha separado
tsvm <- table(svmpred, dataTest$is_canceled)
n <- nrow(dataTest)
tsvm

# Calculamos el error rate y la accuracy
accuracy <- (sum(diag(tsvm))/n)
accuracy
errorRate <- 1-accuracy
errorRate

# De forma automática
confusionMatrix(tsvm)

# Representamos escalando las variables y dibujando en nuevas coordenadas
plot(cmdscale(daisy(dataTest[,-2])),
     col = c("darkblue","red"),
     pch = c("o","+")[1:nrow(dataTest) %in% svm.model$index + 1],
     main="Support Vector Machine",
     xlab="Dim 1", ylab="Dim 2")

# Vemos para los datos en train
svmpredtrain <- predict(modelsvm,dataTrain[,-2])
tsvmtrain <- table(svmpredtrain, dataTrain$is_canceled)
tsvmtrain
confusionMatrix(tsvmtrain)
