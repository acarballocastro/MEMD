###############################################
# T�tulo: 3.Preprocessing
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 24/11/21

# Descripci�n: En este script hacemos el preprocessing
# de la base de datos seleccionando las variables de inter�s 
# y creando nuevas variables
###############################################

# LIBRER�AS
library("ggplot2")

###############################################

# Leer base de datos
path <-'C:/Users/Irene/Downloads'
data.origin <- 'hotel_bookings.csv'
data <- read.csv(paste0(path,'/',data.origin))
d.e <- data[data$country=='ESP',names(data)!='country']

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
########## OTRA VERSI�N ###########
# Cambio de variable adr (posici�n 27) a num�rica
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

# Declaraci�n de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])
levels(d.e$arrival_date_month)<-c("January", "February" ,"March", "April","May",
                                  "June" , "July", "August"  , "September" , 
   
                                                                                                "November" , "October", "December")
# PREPROCESSING

# 1. Multicolinealidad

# Miramos la correlaci�n entre variables num�ricas con un valor mayor a 0.2
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

# 1.2 Eliminaci�n y creaci�n de nuevas variables

# Visto la alta correlacion entre 'previous_cancellations' y 'previous_bookings_not_canceled', 
# decidimos que eliminamos una de estas variables.
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

# Notamos que nuestra variable respuesta est� igual distribuida que la variable 'arrival_date_day_of_month'
# por lo que esta �ltima no aporta informaci�n nueva.
# Por otro lado, la variable que hace referencia al a�o s� que es relevante,  
# pero no la tendremos en cuenta ya que no es interesante para los modelos a estudiar.
 
for(i in levels(d.e$arrival_date_year)){
  plot(is_canceled~arrival_date_month,data = subset(d.e,d.e$arrival_date_year==i))
}

# Como que no hay mucha diferencia entre n�mero de semanas y meses en un a�o,
# eliminamos tambi�n el n�mero de semanas.

summary(d.e[,c('is_canceled','reservation_status')])

# La variable 'reservation_status' tiene la misma informaci�n que nuestra variable respuesta
# as� que la eliminamos.
# Finalmente, de todas estas variables temporales nos quedamos con la 'varrival_date_month',
# pues creemos que es la m�s relevante.
# Tambi�n eliminamos 'adr' que no aporta absolutamente nada.
v.elimina<-which(names(d.e) %in% c('adr', 'reservation_status_date','arrival_date_year', 'arrival_date_week_number',
                                   'arrival_date_day_of_month','reservation_status'))
d.e <- d.e[,-v.elimina]

# Los missings est�n concentrados en las variables 'company' y 'agent',
# pero la ausencia de respuestas no es ausencia de informaci�n, pues cada NULL
# nos indica que esos clientes NO hicieron la reserva a trav�s de una compa��a o agente.
# Son missings no aleatorios, que s� aportan informaci�n.
# As� que eliminamos las variables 'company' y 'agent' y a�adimos 2 variables m�s: 
# 'is_company' y 'is_agent', que indican si la reserva se ha hecho a trav�s de 
# una agencia o compa��a, respectivamente.
summary(d.e$company)
summary(d.e$agent)

restype <- c()
restype[d.e$agent=="NULL"] <- "company"
restype[d.e$agent!="NULL"] <- "agent"

d.e <- cbind(d.e, restype)

# Creamos una nueva variable, 1 si la habitaci�n reservada se corresponde 
# con la habitaci�n asignada, 0 en caso contrario. 
# Creemos qu esto puede tener m�s relevancia que el tipo de habitaci�n en s�, 
# pues no sabemos qu� significa cada categor�a porque los datos se han publicado as�
# para preservar el anonim�to de los clientes.
room_coherence <- c()

for(i in 1:length(d.e[,21])){
  if(d.e$reserved_room_type[i] == d.e$assigned_room_type[i]){
    room_coherence[i] <- 1
  }else{
    room_coherence[i] <- 0
  }
}
d.e$room_coherence <- room_coherence

# Transformamos la variable 'previous_cancellations' en otra que nos indique 
# si se ha cancelado o no anteriormente 

prev_cancellations <- c()
prev_cancellations[d.e$previous_cancellations>0] <- 1
prev_cancellations[d.e$previous_cancellations==0] <- 0
d.e<- cbind(d.e, prev_cancellations)

# Transformamos la variable 'days_in_waiting_list' en otra que nos muestre
# si han estado algun d�a en la lista de espera o no

waiting_list <- c()
waiting_list[d.e$days_in_waiting_list>0] <- 1
waiting_list[d.e$days_in_waiting_list==0] <- 0
d.e<- cbind(d.e, waiting_list)


# Suprimimos las variables que hemos sustituido por otras
v.elimina<-which(names(d.e) %in% c('assigned_room_type','reserved_room_type','agent','company',"previous_cancellations", "days_in_waiting_list"))
d.e <- d.e[,-v.elimina]



# QU� FALTA?????
# 1. HECHO. Crear bi�n las nuevas variables is_agent y is_company.
# 2. HECHO. Discretizar la variable lead_time (YA LO ES).
# 3. Suprimir outliers, ya que no aportan informaci�n nueva y podr�an afectar a los modelos posteriores.
# 4. HECHO (el  nombre cambia a 'prev_cancellations'). Transformar la variable 'previous_cancellations' en si se ha cancelado o no anteriormente.
# 5. HECHO (el  nombre cambia a 'waiting_list'). Transformar la variable 'days_in_waiting_list' en si han estado algun d�a en la lista de espera o no.