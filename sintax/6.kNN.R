###############################################
# Título: 7.1 KNN
# Autor: Joan, Pol
# Fecha: 06/12/21

# DescripciÃ³n: uso del KNN para la predicciÃ³n.
###############################################


library(VIM)
library(Rcpp)
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
dd_knn<-d.e

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
result<-kNN(aux, metric="gower",k=2, variable = "is_canceled", weightDist = T)
tab<-table(result[test,2], dd_knn[test,2])

library(caret)  

confusionMatrix(tab)


