# Este es el script donde utilizamos el método de Decision Tree, Random Forest y Xgboost
#para intentar predecir que reservas son canceladas.

#Primero de todo, cargamos las librerías que necesitamos para nuestro código:

library(randomForest)
library(rpart)
library(rpart.plot)
library(xgboost)
library(tidyverse)
library(caret)
library(caTools)

# Importamos la base de datos procesada y para poder hacer una buena validación
# cogemos un tercio de nuestros datos para que sean nuestra base test.

dd <- read.csv2("hotel_bookings_proc.csv", header = T, sep =',')

test<- sample(1:nrow(dd),size = nrow(dd)/3)

dataTrain<-dd[-test,]
dataTest<-dd[test,]

# Una vez tenemos nuestra base de datos lista, utilizamos la función rpart para crear
# un decision tree. Nuestra variable regresora es is_canceled y cogemos la base de datos
# que hemos asignado a Train (dataTrain). Usamos el método "class" ya que lo que estamos 
# buscando es clasificar.

d_tree <- rpart(is_canceled ~ ., data = dataTrain, method = "class", parms = list(split = "information"))
d_tree

# Aquí podemos ver gráficamente el árbol de decisión creado anteriormente:

rpart.plot(d_tree)

# Ahora usaremos la función predict con el árbol creado para predecir los datos para
# los datos para la validación (dataTest). 

pred_dt <- predict(d_tree, newdata = dataTest)

# Después cogemos los valores que han salido en la predicción y los transformamos en 0 o 1,
# dependiendo de si tienen un valor más pequeño de 0.5 o más grande, resprectivamente.

pred_dtp <- pred_dt[, 2]
pred_dtp[pred_dt[, 2] < 0.5] <- 0
pred_dtp[pred_dt[, 2] >= 0.5] <- 1
 
# Aquí vemos con una tabla cuantos valores hemos acertado y cuantos fallado.
(t_dt <- table(dataTest$is_canceled, pred_dtp))

# Calculamos la precisión del modelo creado (0.848)

(accuracy <- sum(diag(t_dt))/sum(t_dt))


# Ahora utilizaremos el paquete RandomForest para hacer el modelo. Decidimos crear 100 árboles para este modelo (ntree = 100)

model_rf <- randomForest(dataTrain[-2], y = dataTrain$is_canceled,importance = T, ntree=100, proximity = T, xtest = dataTest[-2], ytest=dataTest$is_canceled)

# Cogemos la tabla que nos muestra la predicción junto con los datos reales:

p_test <- model_rf$test$confusion

# Calculamos la precisión (0.863):

(accuracy <- sum(diag(p_test))/sum(p_test[,1:2]))

  
# Ahora usaremos el método de Xgboost para la predicción de la variable is_canceled:

# Primero guardaremos los valores de la variable is_canceled de los datos de validación (dataTest)

validacion_test <- dataTest$is_canceled

# Una vez hecho esto, pasamos a NAs a toda la columna de la variable regresora:

dataTest$is_canceled <- NA

# Ahora pasamos todas las variables a numèricas, tanto la base de datos train como 
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

# Transformamos los datos en una DMatrix para que los datos sean compatibles con la función Xgboost:

hotel$train_mat <- 
  hotel$train %>%
  select(-is_canceled) %>%
  as.matrix() %>%
  xgb.DMatrix(data = ., label = hotel$train$is_canceled)


hotel$test_mat <- 
  hotel$test %>%
  select(-is_canceled) %>%
  as.matrix() %>%
  xgb.DMatrix(data = ., label = hotel$test$is_canceled)

# Usamos la función xgboost con la matriz creada con los datos de train. Usamos 
# como objective "binary:logistic", ya que queremos clasificar una variable binaria.

hotel$modelo1 <- xgboost(data = hotel$train_mat, 
                         objective = "binary:logistic",
                         colsample_bytree = 0.8, subsample = 1,
                         nrounds = 1500, max.depth = 15, eta = 0.001, nthread = 2)

hotel$modelo1

# Hacemos una predicción con los datos test a partir del modelo de xgboost:

hotel$predict_validation <- predict(hotel$modelo1, hotel$test_mat)

prediccion <- hotel$predict_validation

head(prediccion)

# Transformamos el vector de predicción para que sea de 0 y 1.

prediccion[prediccion >= 0.5] <- 1

prediccion[prediccion < 0.5] <- 0

# Hacemos la tabla de confusión y vemos que tenemos una predicción de 0.873.

t_validation <- table(prediccion, validacion_test)

(accuracy <- sum(diag(t_validation))/sum(t_validation))
