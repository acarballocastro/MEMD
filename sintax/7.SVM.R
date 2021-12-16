###############################################
# T?tulo: 3.Preprocessing
# Autor: Alba, Pol
# Fecha: 06/12/21

# Descripci?n: En este script empleamos el m?todo
# Support Vector Machine (SVM) para entrenar un 
# modelo que nos permita predecir cancelaciones
###############################################

# Cargamos las librer?as

packages <- c("e1071", "caret","rpart","kernlab")
sapply(packages, require, character.only = TRUE)

# Lectura de la base de datos
if(!exists('d.e')){
  path <-'../data'
  d.e <- read.csv2(paste0(path,'/',"hotel_bookings_proc.csv"), sep=",")
  d.e$adr<-as.numeric(d.e$adr)
  v<-list(
    categoric=c('hotel','is_canceled', 'arrival_date_month','arrival_date_year', 'meal','market_segment','distribution_channel',
                'is_repeated_guest','reserved_room_type','assigned_room_type', 'room_coherence', 
                'is_company', 'is_agent', 'customer_type','deposit_type', 'if_prev_cancel','if_wait'),
    integer=c('lead_time', 'stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
              'booking_changes','required_car_parking_spaces','total_of_special_requests'),
    continua='adr')
  v$numeric <- c(v$integer,v$continua)
  library(ggplot2)
  for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
  for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
}
dd_svm <-d.e

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

# Vemos cuales son los hiperpar?metros escogidos
svm.tune

# Creamos el modelo con estos par?metros usando un kernel linear
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

# De forma autom?tica
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
