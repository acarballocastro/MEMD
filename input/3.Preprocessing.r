###############################################
# Título: Preprocessing
# Autor: Alba, Aleix, Pol, Yuhang, 
# Fecha: 16/11/21
# Descripción: En este script hacemos el preprocessing de la base de datos
# seleccionando las variables de interés y creando nuevas variables
###############################################

# Leer base de datos
path <-'C:/Users/garys/Desktop/MINERIA DE DADES'
data.origin <- 'hotel_bookings.csv'
data<-read.csv(paste0(path,'/',data.origin))
d.e<-data[data$country=='ESP',names(data)!='country']
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

# Preprocessing

## Multicolinealidad
#Miramos la correlación entre variables numéricas con un valor mayor a 0.2
v$times<-NULL
v$withmissing<-NULL
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))

corr<-cor(d.e[,v$numeric])
corr[ col(corr)<=row(corr) ]<-0
corr<-corr[!apply(corr, 1, function(x) all(abs(x)<0.2)),!apply(corr, 2, function(x) all(abs(x)<0.2))]
corrplot::corrplot(corr)

## Eliminación y creación de nuevas variables

# Visto la alta correlacion entre previous_cancellations y previous_bookings_not_canceled, decidimos que eliminamos uno
d.e<-d.e[,names(d.e)!='previous_bookings_not_canceled']
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))
plot(d.e$is_canceled~d.e$arrival_date_year+d.e$arrival_date_month+d.e$arrival_date_day_of_month)

print(ggplot(d.e, aes(x = arrival_date_year, fill = is_canceled, y = is_canceled)) + geom_col(position = "fill"))

print(ggplot(d.e, aes(x = arrival_date_month, fill = is_canceled, y = is_canceled)) + geom_col(position = "fill"))

print(ggplot(d.e, aes(x = arrival_date_day_of_month, fill = is_canceled, y = is_canceled)) + geom_col(position = "fill"))

# Nos fijamos que nuestra variable respuesta está igualmente distribuida en la variable
# arrival_date_day_of_month por lo que no aporta información nueva
# Sin embargo, la variable que hacer referencia al año si que es relevante, no obstante no la tendremos 
# en cuenta ya que valoramos que no es interesante para los modelos que estudiaremos
 
for(i in levels(d.e$arrival_date_year))
  plot(is_canceled~arrival_date_month,data = subset(d.e,d.e$arrival_date_year==i))

# Como que no tiene mucha diferencia entre número de semanas y meses dentro un año, eliminamos también el número de semanas

summary(d.e[,c('is_canceled','reservation_status')])

# La variable reservation_status tiene la misma información que nuestra variable respuesta así que la eliminamos
# Finalmente de todas estas variables nos quedamos con la variable del mes que creemos que es la más relevante
# També eliminem adr que no aporta absolutament res
v.elimina<-which(names(d.e) %in% c('adr', 'reservation_status_date','arrival_date_year', 'arrival_date_week_number',
                                   'arrival_date_day_of_month','reservation_status'))
d.e<-d.e[,-v.elimina]

d.e


# Los missing que tengamos están en las variables conpany y agent, la cuales no son falta
# de respuestas sino que indica si un cliente hizo la resrva a través de una agencia o no en el caso 
# de agent, análogamente con la variabel company
# Asi que eliminamos las variables company y agent pero añadimos 2 variables más que sean 
# is_company u is_agent que significan si han hecho la reserva a través de agencia o compañía o no
# respectivamente


d.e$is_company<-factor(ifelse(is.na(d.e$company),0,1))
d.e$is_agent<-factor(ifelse(is.na(d.e$agent),0,1))
v.elimina<-which(names(d.e) %in% c('company','agent'))
d.e<-d.e[,-v.elimina]

#Creamos una nueva variable, 1 si la habitación reservada se corresponde 
#con la habitación asignada, 0 contrariamente. Creemos qu esto puede tener más 
#relevancia que el tipo de habitaciones que no conocemos debido a que es anónimo
room_coherence <- c()

for(i in 1:length(d.e[,21])){
  if(d.e$reserved_room_type[i] == d.e$assigned_room_type[i]){
    room_coherence[i] <- 1
  }else{
    room_coherence[i] <- 0
  }
}
d.e$room_coherence <- room_coherence

#Suprimimos las variables
v.elimina<-which(names(d.e) %in% c('assigned_room_type','reserved_room_type'))
d.e<-d.e[,-v.elimina]

#QUÉ FALTA?????
# 1. Crear bién las nuevas variables is_agent y is_company
# 2. Discretizar la variable lead_time
# 3. Suprimir els outliers, ja que no aporten informació nova i podrien afectar els models posteriors
# 4. Transformar la variable previous_cancellations en si ha cancel·lat o no anteriorment 
# 5. Transformar la variable days_in_waiting_list en si han estat algun dia a la llista d'espera o no




