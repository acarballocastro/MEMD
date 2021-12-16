# Este es el script donde utilizamos el m?todo de Decision Tree, Random Forest y Xgboost
#para intentar predecir que reservas son canceladas.

#Primero de todo, cargamos las librer?as que necesitamos para nuestro c?digo:


packages <- c("randomForest", "rpart","rpart.plot","xgboost","tidyverse","caret", "caTools","verification")
sapply(packages, require, character.only = TRUE)

# Importamos la base de datos procesada y para poder hacer una buena validaci?n
# cogemos un tercio de nuestros datos para que sean nuestra base test.

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
dd<-d.e

test<- sample(1:nrow(dd),size = nrow(dd)/3)


dataTrain<-dd[-test,]
dataTest<-dd[test,]


# Una vez tenemos nuestra base de datos lista, utilizamos la funci?n rpart para crear
# un decision tree. Nuestra variable regresora es is_canceled y cogemos la base de datos
# que hemos asignado a Train (dataTrain). Usamos el m?todo "class" ya que lo que estamos 
# buscando es clasificar.

d_tree <- rpart(is_canceled ~ ., data = dataTrain, method = "class", parms = list(split = "information"))
d_tree

# Aqu? podemos ver gr?ficamente el ?rbol de decisi?n creado anteriormente:

rpart.plot(d_tree)

# Ahora usaremos la funci?n predict con el ?rbol creado para predecir los datos para
# los datos para la validaci?n (dataTest). 

pred_dt <- predict(d_tree, newdata = dataTest)

# Despu?s cogemos los valores que han salido en la predicci?n y los transformamos en 0 o 1,
# dependiendo de si tienen un valor m?s peque?o de 0.5 o m?s grande, resprectivamente.

pred_dtp <- pred_dt[, 2]
pred_dtp[pred_dt[, 2] < 0.5] <- "Cancelled"
pred_dtp[pred_dt[, 2] >= 0.5] <- "Not_cancelled"
 
# Aqu? vemos con una tabla cuantos valores hemos acertado y cuantos fallado.
(t_dt <- table(dataTest$is_canceled, pred_dtp))

# Ejecutamos la funci?n acu creada anteriormente:

confusionMatrix(t_dt)


# Creamos una curva de ROC para comprobar que capacidad de predicci?n tiene nuestro modelo creado:

print(roc.plot(dataTest$is_canceled == "Cancelled", pred_dt[,1])$roc.vol)


### RANDOM FOREST
# Ahora utilizaremos el paquete RandomForest para hacer el modelo. Decidimos crear 100 ?rboles para este modelo (ntree = 100)

model_rf <- randomForest(dataTrain[-2], y = dataTrain$is_canceled,importance = T, ntree=100, proximity = T, xtest = dataTest[-2], ytest=dataTest$is_canceled)

# Cogemos la tabla de confusi?n que nos muestra la predicci?n junto con los datos reales:

p_test <- model_rf$test$confusion[1:2,1:2]

confusionMatrix(p_test)

# Volvemos a crear la curva de ROC para comprobar de nuevo la capacidad de predicci?n:

print(roc.plot(dataTest$is_canceled == "Not_cancelled", model_rf$test$votes[,2])$roc.vol)


### XGBOOST
# Ahora usaremos el m?todo de Xgboost para la predicci?n de la variable is_canceled:

# Primero guardaremos los valores de la variable is_canceled de los datos de validaci?n (dataTest)

validacion_test <- dataTest$is_canceled

# Una vez hecho esto, pasamos a NAs a toda la columna de la variable regresora:

# dataTest$is_canceled <- NA

# Ahora pasamos todas las variables a num?ricas, tanto la base de datos train como 
# la base de datos test.

dataTrain <- map_df(dataTrain, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

dataTest <- map_df(dataTest, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

# Creamos un list, donde guardamos dataTrain y dataTest.

hotel <- list()

hotel$train <- dataTrain

hotel$test <- dataTest

# Transformamos los datos en una DMatrix para que los datos sean compatibles con la funci?n Xgboost:

apply(hotel$test,2,is.factor)

hotel$train_mat <- 
  hotel$train  %>%
  as.matrix() %>%
  xgb.DMatrix(data = ., label = hotel$train$is_canceled)


hotel$test_mat <- 
  hotel$test  %>%
  as.matrix() %>%
  xgb.DMatrix(data = ., label = hotel$test$is_canceled)

# Usamos la funci?n xgboost con la matriz creada con los datos de train. Usamos 
# como objective "binary:logistic", ya que queremos clasificar una variable binaria.

hotel$modelo1 <- xgboost(data = hotel$train_mat, 
                         objective = "binary:logistic",
                         colsample_bytree = 0.8, subsample = 1,
                         nrounds = 1500, max.depth = 15, eta = 0.001, nthread = 2)

hotel$modelo1

# Hacemos una predicci?n con los datos test a partir del modelo de xgboost:

hotel$predict_validation <- predict(hotel$modelo1, hotel$test_mat)

prediccion <- hotel$predict_validation

head(prediccion)

# Transformamos el vector de predicci?n para que sea de 0 y 1.

prediccion[prediccion >= 0.5] <- 1

prediccion[prediccion < 0.5] <- 0

# Hacemos la tabla de confusi?n y vemos que tenemos una predicci?n de 0.873.

t_validation <- table(prediccion, validacion_test)

confusionMatrix(p_test)

# La curva ROC sale perfecta, rollo con ?rea 1 as? que probablemente este mal

#print(roc.plot(dataTest$is_canceled == 1, hotel$predict_validation)$roc.vol)


