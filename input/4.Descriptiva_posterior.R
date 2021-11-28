###############################################
# Título: 4.Descriptiva posterior
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 24/11/21

# Descripción: En este script hacemos 
# la descriptiva univariante y bivariante
# de las variables tras el preprocessing
###############################################

# LIBRERÍAS
library("ggplot2")

###############################################

# Leer base de datos
path <-'../data'
data.origin <- 'hotel_bookings_proc.csv'
d.e <- read.csv(paste0(path,'/',data.origin))


# 1. Descriptiva de la base de datos

# 1.1 Análisis descriptivo de variables numéricas

for(i in v$numeric){
  p <- ggplot(d.e, aes(x=d.e[,i]))+ theme_minimal()+
    geom_histogram(color="darkblue", fill="lightblue", bins=30)
  
  print(p + labs(title= i,
                 x = names(d.e)[which(names(d.e)==i)], y = "Count")) 
}

print(ggplot(d.e, aes(x=lead_time)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=arrival_date_week_number)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=arrival_date_day_of_month)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=stays_in_weekend_nights)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=stays_in_week_nights)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=adults)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=children)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=babies)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=previous_cancellations)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=previous_bookings_not_canceled)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=booking_changes)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=required_car_parking_spaces)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=days_in_waiting_list)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=total_of_special_requests)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))

# for(i in v$continua){
#   hist(d.e[[i]],main = i,xlab = i,breaks = 100)
#   print(summary(d.e[[i]]))
# }

#print(ggplot(d.e, aes(x=adr)) + geom_histogram(bins = 100, fill = "steelblue")+
#        theme_minimal() +
#        labs(title = "Histogram"))
print(summary(d.e[["adr"]]))

for(i in v$times) hist(d.e[[i]],main = i,xlab = i,breaks = 100)

print(ggplot(d.e, aes(x=reservation_status_date)) + geom_histogram(bins = 100, fill = "steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(summary(d.e[["reservation_status_date"]]))



# 2.2 Análisis descriptivo de variables categóricas

# for(i in v$categoric){
#   plot(d.e[[i]],main=i)
#   print(table(d.e[[i]]))
# }

require("RColorBrewer")
for(i in v$categoric){
  pie(table(d.e[,which(names(d.e)==i)]), radius = 1, col=brewer.pal(length(names(table(d.e[,which(names(d.e)==i)]))),'Spectral'))
  legend(x = "topleft", legend = names(table(d.e[,which(names(d.e)==i)])),
         fill=brewer.pal(length(names(table(d.e[,which(names(d.e)==i)]))),'Spectral'))
}

print(ggplot(d.e, aes(x=is_canceled)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=hotel)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=arrival_date_year)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=arrival_date_month)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=meal)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=market_segment)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=distribution_channel)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=is_repeated_guest)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=reserved_room_type)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=assigned_room_type)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=deposit_type)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=agent)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=company)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=customer_type)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(ggplot(d.e, aes(x=reservation_status)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
