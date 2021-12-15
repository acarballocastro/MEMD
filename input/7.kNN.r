###############################################
# Título: 7.1 KNN
# Autor: Joan, Pol
# Fecha: 06/12/21

# DescripciÃ³n: uso del KNN para la predicciÃ³n.
###############################################

install.packages("VIM")
library(VIM)

dd_knn<- read.csv("hotel_bookings_proc.csv")
dd_knn$is_canceled <- as.factor(dd_knn$is_canceled)

test <- sample(1:nrow(dd_knn), size = nrow(dd_knn)/3)
dataTrain <- dd_knn[-test,]
dataTest <- dd_knn[test,]
aux <- dd_knn
aux[test, 2] <- NA


result <- kNN(aux, metric = "gower", variable = "is_canceled")
aa<-table(result$is_canceled[test], dd_knn$is_canceled[test])
((aa[1, 1] +aa[2, 2])/sum(aa))



accuracys<-c()
ks<-1:8
#Tarda en ejecutar, cuiadado!
for(i in ks){
  
  result<-kNN(aux, metric="gower",k=i, variable = "is_canceled", weightDist = T)
  tab<-table(result[test,2], dd_knn[test,2])
  acc<-(tab[1, 1] +tab[2, 2])/sum(tab)
  accuracys<-c(accuracys, acc)
}

plot(accuracys)
which.max(accuracys)
accuracys[2]
#K=2 maximiza los resultados
