###############################################
# Título: 2.Descriptiva
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script hacemos 
# la descriptiva univariante de las variables
###############################################

# LIBRERÍAS
library("ggplot2")

###############################################

# Leer base de datos
path <-'C:/Users/Irene/Documents/RStudio/TREBALL'
data.origin <- 'hotel_bookings.csv'
data <- read.csv(paste0(path,'/',data.origin))
d.e <- data[data$country=='ESP',names(data)!='country']

# Missings
d.e[d.e=='NULL']<-NA
mis<-sapply(d.e,function(x) sum(is.na(x)))
v.mis<-which(mis>0)
mis<-mis[mis>0]
n<-list(total=prod(dim(d.e)))
n$missing<-sum(mis)
mis<-list(count=list(number=n$missing,forvar=mis),
          relative=list(forall=n$missing/n$total,formissing=mis/n$missing,
                        forvar=mis/n$observation))


# DESCRIPTIVA

# 1. Declaración de variables

# Definimos el tipo de variables
v <- list(
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

########## OTRA VERSIÓN ###########
# Cambio de variable adr (posición 27) a numérica
#d.e[,27] <- scan(text=d.e[,27], dec=",", sep=".")

# Definimos el tipo de variables
#v <- list(
#  categoric=c('is_canceled','hotel','arrival_date_year','arrival_date_month','meal',
#              'market_segment','distribution_channel','is_repeated_guest','reserved_room_type',
#              'assigned_room_type','deposit_type','agent','company','customer_type',
#              'reservation_status'),
#  integer=c('lead_time','arrival_date_week_number','arrival_date_day_of_month',
#            'stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
#            'previous_cancellations','previous_bookings_not_canceled','booking_changes',
#            'days_in_waiting_list','adr','required_car_parking_spaces','total_of_special_requests'),
#  times='reservation_status_date'
#)
#v$numeric<-c(v$integer,v$times)

# Declaración de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])
levels(d.e$arrival_date_month)<-c("January", "February" ,"March", "April","May",
                                  "June" , "July", "August"  , "September" , 
                                  "November" , "October", "December")
levels(d.e$reserved_room_type)<-levels(d.e$assigned_room_type)

# Descripción general
summary(d.e[,v$categoric])
summary(d.e[,v$numeric])


# 2. Descriptiva de la base de datos

# Este código de Yuhang mejor para hacer todos los histogramas juntos

for(j in 1:4){
  # j=1 categorica, 2 integer , 3 continua , 4 temporal
  cat('\n\nVariable',names(v)[j],'\n')
  color<-c('orange','steelblue','red','limegreen')[j]
  for(i in v[[j]]){
    cat('\n\n')
    print(ggplot(d.e, aes(x=d.e[[i]])) + geom_bar(stat = "count", fill=color,na.rm = T)+
            theme_minimal() +
            labs(title = "Histogram")+xlab(i))
    print(summary(d.e[[i]]))
  }
}

# Sino, aquí están uno por uno

# 2.1 Análisis descriptivo de variables numéricas

# for(i in v$integer){
#   hist(d.e[[i]],main = i,xlab = i,breaks = 100)
#   print(summary(d.e[[i]]))
#  }

print(ggplot(d.e, aes(x=lead_time)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram lead_time"))
print(ggplot(d.e, aes(x=arrival_date_week_number)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram arrival_date_week_number"))
print(ggplot(d.e, aes(x=arrival_date_day_of_month)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram arrival_date_day_of_month"))
print(ggplot(d.e, aes(x=stays_in_weekend_nights)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram stays_in_weekend_nights"))
print(ggplot(d.e, aes(x=stays_in_week_nights)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram stays_in_week_nights"))
print(ggplot(d.e, aes(x=adults)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram adults"))
print(ggplot(d.e, aes(x=children)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram children"))
print(ggplot(d.e, aes(x=babies)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram babies"))
print(ggplot(d.e, aes(x=previous_cancellations)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram previous_cancellations"))
print(ggplot(d.e, aes(x=previous_bookings_not_canceled)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram previous_bookings_not_canceled"))
print(ggplot(d.e, aes(x=booking_changes)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram booking_changes"))
print(ggplot(d.e, aes(x=required_car_parking_spaces)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram required_car_parking_spaces"))
print(ggplot(d.e, aes(x=days_in_waiting_list)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram days_in_waiting_list"))
print(ggplot(d.e, aes(x=total_of_special_requests)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram total_of_special_requests"))
# adr
#print(ggplot(d.e, aes(x=adr)) + geom_histogram(bins = 100, fill = "steelblue")+
#        theme_minimal() +
#        labs(title = "Histogram"))
print(summary(d.e[["adr"]]))

# reservation_status_date
print(ggplot(d.e, aes(x=reservation_status_date)) + geom_histogram(bins = 100, fill = "steelblue")+
        theme_minimal() +
        labs(title = "Histogram reservation_status_date"))
print(summary(d.e[["reservation_status_date"]]))



# Otro tipo de histogramas
for(i in v$numeric){
  p <- ggplot(d.e, aes(x=d.e[,i]))+ theme_minimal()+
    geom_histogram(color="darkblue", fill="lightblue", bins=30)
  
  print(p + labs(title= i,
                 x = names(d.e)[which(names(d.e)==i)], y = "Count")) 
}# No funciona bé per a contínua i times





# 2.2 Análisis descriptivo de variables categóricas

# for(i in v$categoric){
#   plot(d.e[[i]],main=i)
#   print(table(d.e[[i]]))
# }

print(ggplot(d.e, aes(x=is_canceled)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram is_canceled"))
print(ggplot(d.e, aes(x=hotel)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram hotel"))
print(ggplot(d.e, aes(x=arrival_date_year)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram arrival_date_year"))
print(ggplot(d.e, aes(x=arrival_date_month)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram arrival_date_month"))
print(ggplot(d.e, aes(x=meal)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram meal"))
print(ggplot(d.e, aes(x=market_segment)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram market_segment"))
print(ggplot(d.e, aes(x=distribution_channel)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram distribution_channel"))
print(ggplot(d.e, aes(x=is_repeated_guest)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram is_repeated_guest"))
print(ggplot(d.e, aes(x=reserved_room_type)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram reserved_room_type"))
print(ggplot(d.e, aes(x=assigned_room_type)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram assigned_room_type"))
print(ggplot(d.e, aes(x=deposit_type)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram deposit_type"))
print(ggplot(d.e, aes(x=agent)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram agent"))
print(ggplot(d.e, aes(x=company)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram company"))
print(ggplot(d.e, aes(x=customer_type)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram customer_type"))
print(ggplot(d.e, aes(x=reservation_status)) + geom_bar(stat = "count", fill="steelblue")+
        theme_minimal() +
        labs(title = "Histogram reservation_status"))

# Otro tipo de diagramas
require("RColorBrewer")
for(i in v$categoric){
  pie(table(d.e[,which(names(d.e)==i)]), radius = 1, col=brewer.pal(length(names(table(d.e[,which(names(d.e)==i)]))),'Spectral'))
  legend(x = "topleft", legend = names(table(d.e[,which(names(d.e)==i)])),
         fill=brewer.pal(length(names(table(d.e[,which(names(d.e)==i)]))),'Spectral'))
}