###############################################
# Título: Descriptiva
# Autor: Alba, Aleix, Pol, Yuhang, 
# Fecha: 16/11/21
# Descripción: En este script hacemos la descriptiva univariante de las variables
###############################################

# Leer base de datos
path <-'C:/Users/garys/Desktop/MINERIA DE DADES'
data.origin <- 'hotel_bookings.csv'
data<-read.csv(paste0(path,'/',data.origin))
d.e<-data[data$country=='ESP',names(data)!='country']

# Decralación de variables

# Definimos el tipo de variables
v<-list(
  categoric=c('is_canceled','hotel','arrival_date_year','arrival_date_month','meal',
              'market_segment','distribution_channel','is_repeated_guest','reserved_room_type',
              'assigned_room_type','deposit_type','agent','company','customer_type',
              'reservation_status'),
  integer=c('lead_time','arrival_date_week_number','arrival_date_day_of_month',
            'stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
            'previous_cancellations','previous_bookings_not_canceled','booking_changes',
            'days_in_waiting_list','required_car_parking_spaces','total_of_special_requests'),
  continua='adr',
  times='reservation_status_date'
)
v$numeric<-c(v$integer,v$continua,v$times)
v$withmissing<-v.mis

# Declaración de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])
levels(d.e$arrival_date_month)<-c("January", "February" ,"March", "April","May",
                                  "June" , "July", "August"  , "September" , 
                                  "November" , "October", "December")

# Descripción general
summary(d.e[,v$categoric])
summary(d.e[,v$numeric])


# Descriptiva de la base de datos

# Análisi descriptiva de variables numericas

# for(i in v$integer){
#   hist(d.e[[i]],main = i,xlab = i,breaks = 100)
#   print(summary(d.e[[i]]))
#  }

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

print(ggplot(d.e, aes(x=adr)) + geom_histogram(bins = 100, fill = "steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(summary(d.e[["adr"]]))

for(i in v$times) hist(d.e[[i]],main = i,xlab = i,breaks = 100)

print(ggplot(d.e, aes(x=reservation_status_date)) + geom_histogram(bins = 100, fill = "steelblue")+
        theme_minimal() +
        labs(title = "Histogram"))
print(summary(d.e[["reservation_status_date"]]))


## Análisi descriptiva de variables categóricas


# for(i in v$categoric){
#   plot(d.e[[i]],main=i)
#   print(table(d.e[[i]]))
# }

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