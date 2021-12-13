
#install.packages("naivebayes")
#install.packages("e1071")
#install.packages("caret")
#install.packages("c50")
#install.packages("naivebayes")

library(e1071)
library(naivebayes) 
library(caret)  
library(C50) 

dd<- read.csv2("datos_preprocesados.csv") 
set.seed(2018)
test<-sample(1:nrow(dd),size = nrow(dd)/3)
dataTrain<-dd[-test,]
dataTest<-dd[test,]
dd$is_canceled <- as.factor(dd$is_canceled)

mod <- naiveBayes(is_canceled ~ ., data = dataTrain)
mod

pred <- predict(mod, dataTest)
tab <- table(dataTest$is_canceled, pred)
confusionMatrix(tab)

