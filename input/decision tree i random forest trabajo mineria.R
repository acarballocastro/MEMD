library(randomForest)
library(rpart)
library(rpart.plot)


d_tree <- rpart(is_canceled ~ ., data = dataTrain, method = "class", parms = list(split = "information"))
d_tree

rpart.plot(d_tree)

p1l <- predict(d_tree, newdata = dataTest)
p1lp <- p1l[, 2]
p1lp[p1l[, 2] < 0.5] <- 0
p1lp[p1l[, 2] >= 0.5] <- 1

indicadores <- as.numeric(names(p1lp))
t_dt <- table(dataTest$is_canceled, p1lp)
t_dt

(accuracy <- sum(diag(t_dt))/sum(t_dt))


set.seed(081221)

dd <- read.csv2("hotel_bookings_proc.csv", header = T, sep =',')

dd$is_canceled <- as.factor(dd$is_canceled)

test<- sample(1:nrow(dd),size = nrow(dd)/3)

dataTrain<-dd[-test,]
dataTest<-dd[test,]

model_rf <- randomForest(dataTrain[-2], y = dataTrain$is_canceled,importance = T, ntree=100, proximity = T, xtest = dataTest[-2], ytest=dataTest$is_canceled)

p_test<-model_rf$test$confusion

(accuracy <- sum(diag(p_test))/sum(p_test[,1:2]))

  