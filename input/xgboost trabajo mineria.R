library(xgboost)
library(tidyverse)
library(caret)
library(caTools)

set.seed(123456)

train <- read.csv2("hotel_bookings_proc.csv", header = T, sep =',')

test <- sample(1:nrow(train), size = nrow(train)/3)

validacion_test <- train[test,]$is_canceled

train[test,]$is_canceled <- NA


train <- map_df(train, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

hotel <- list()

hotel$train <- train

hotel$train_df <- train[-test,]

hotel$test_df <- train[test,]

hotel$train_mat <- 
  hotel$train_df %>%
  select(-is_canceled) %>%
  as.matrix() %>%
  xgb.DMatrix(data = ., label = hotel$train_df$is_canceled)


hotel$test_mat <- 
  hotel$test_df %>%
  select(-is_canceled) %>%
  as.matrix() %>%
  xgb.DMatrix(data = ., label = hotel$test_df$is_canceled)

hotel$train_mat


hotel$modelo1 <- xgboost(data = hotel$train_mat, 
                           objective = "binary:logistic",
                           colsample_bytree = 0.8, subsample = 0.9,
                           nrounds = 1500, max.depth = 15, eta = 0.001, nthread = 2)

hotel$modelo1

hotel$predict_validation <- predict(hotel$modelo1, hotel$test_mat)

prediccion <- hotel$predict_validation

head(prediccion)

prediccion[prediccion >= 0.5] <- 1

prediccion[prediccion < 0.5] <- 0

t_validation <- table(prediccion, validacion_test)

(accuracy <- sum(diag(t_validation))/sum(t_validation))


