###############################################
# T�tulo: 3.Preprocessing
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripci�n: En este script hacemos el preprocessing
# de la base de datos seleccionando las variables de inter�s 
# y creando nuevas variables
###############################################

# LIBRER�AS
library("ggplot2")

###############################################

# Leer base de datos
path <-'../data'
data.origin <- 'hotel_bookings_proc.csv'
d.e <- read.csv(paste0(path,'/',data.origin))
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


# Declaraci�n de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])
levels(d.e$arrival_date_month)<-c("January", "February" ,"March", "April","May",
                                  "June" , "July", "August"  , "September" , 
                                  "November" , "October", "December")
levels(d.e$reserved_room_type)<-levels(d.e$assigned_room_type)


# PREPROCESSING

# 1. Missing

# Los missings est�n concentrados en las variables 'company' y 'agent',
# pero la ausencia de respuestas no es ausencia de informaci�n, pues cada NULL
# nos indica que esos clientes NO hicieron la reserva a trav�s de una compa��a o agente.
# Son missings no aleatorios, que s� aportan informaci�n.
# As� que eliminamos las variables 'company' y 'agent' y a�adimos 2 variables m�s: 
# 'is_company' y 'is_agent', que indican si la reserva se ha hecho a trav�s de 
# una agencia o compa��a, respectivamente.
d.e$is_company<-factor(ifelse(is.na(d.e$company),0,1))
d.e$is_agent<-factor(ifelse(is.na(d.e$agent),0,1))
v.elimina<-which(names(d.e) %in% c('company','agent'))
d.e<-d.e[,-v.elimina]


# 2. Creaci�n de nuevas variables

# Creemos que es mejor saber si la habitaci�n reservada se corresponde
# con la asignada que considerar estas 2 variables por separado
# ya que no sabemos qu� significa cada categor�a porque los datos
# se han publicado as� para preservar el anonim�to de los clientes.
# Por eso, creamos una nueva variable, 1 si la habitaci�n reservada se corresponde 
# con la habitaci�n asignada, 0 en caso contrario. 
d.e$room_coherence<-factor(d.e$reserved_room_type == d.e$assigned_room_type)

# Como con las habitaciones, consideramos que es mejor saber si ha habido
# d�as en la cola de espera que saber cu�ntos son.
d.e$if_wait<-factor(d.e$days_in_waiting_list>0)

# Igualmente para 'previous_cancellations' i 'previous_bookings_not_canceled'
d.e$if_prev_cancel<-factor(d.e$previous_cancellations>0)
d.e$if_prev_asign<-factor(d.e$previous_bookings_not_canceled>0)

# Eliminaremos las variables "duplicadas" al final



# 3. Multicolinealidad

# Empezamos estudiando las variables relacionadas con fechas
# A�o 
ggplot(d.e, aes(x = arrival_date_year, fill = is_canceled, y = is_canceled))+ geom_col(position = 'fill' ) +ggtitle("Correlaci�n entre A�o y cancelaci�n")+ labs(y="Cancelaciones", x = "A�o")
# Mes
ggplot(d.e, aes(x = arrival_date_month, fill = is_canceled, y = is_canceled)) + geom_col(position = "fill")+ggtitle("Correlaci�n entre Mes y cancelaci�n")+ labs(y="Cancelaciones", x = "Mes")
# D�a
ggplot(d.e, aes(x = arrival_date_day_of_month, fill = is_canceled, y = is_canceled)) + geom_col(position = "fill")+ggtitle("Correlaci�n entre D�a y cancelaci�n")+ labs(y="Cancelaciones", x = "D�a")

# Notamos que nuestra variable respuesta est� igual distribuida que la variable 'arrival_date_day_of_month'
# por lo que esta �ltima no aporta informaci�n nueva, as� que la eliminaremos.
# En cambio, la variable que hace referencia al a�o s� que es relevante,  
# pero no la tendremos en cuenta ya que no es interesante para los modelos a estudiar.
# Por otro lado, como no hay mucha diferencia entre n�mero de semanas y meses en un a�o,
# eliminaremos tambi�n el n�mero de semanas.

summary(d.e[,c('is_canceled','reservation_status')])

# Aqu� observamos que la variable 'reservation_status' tiene la misma informaci�n que
# nuestra variable respuesta as� que la eliminaremos, al igual que 'reservation_status_date'.
# Finalmente, tambi�n eliminamos 'adr' que no aporta absolutamente nada.
v.elimina<-which(names(d.e) %in% c('arrival_date_day_of_month','arrival_date_week_number',
                                   'reservation_status','reservation_status_date','adr'))
d.e <- d.e[,-v.elimina]

# Renovamos los �ndices
v$withmissing<-NULL
v$times<-NULL
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))




# Ahora, miraremos la correlaci�n entre variables num�ricas con un valor mayor a 0.2
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


# Visto la alta correlaci�n entre 'previous_cancellations' y 'previous_bookings_not_canceled', 
# decidimos que eliminamos una de estas variables.
# Y como hab�amos creado unas variables equivalentes (apartado 2.)
# eliminaremos las dos 'previous_cancellations' y 'previous_bookings_not_canceled'
# y la equivalente a una de ellas, 'if_prev_asign'.

# Y ahora que ya hemos acabado con las correlaciones, eliminaremos las variables
# que hemos sustituido por otras en el apartado 2.
v.elimina<-which(names(d.e) %in% c('reserved_room_type','assigned_room_type','days_in_waiting_list',
                                   'previous_cancellations', 'previous_bookings_not_canceled','if_prev_asign'))
d.e <- d.e[,-v.elimina]

# Renovamos los �ndices
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))

# Cambiamos las etiquetas de los factores para que no contengan espacios

levels(d.e$hotel) <- c("City_Hotel", "Resort_Hotel")
levels(d.e$is_canceled) <- c("not_canceled", "canceled")
levels(d.e$deposit_type) <- c("No_deposit", "Non_refund", "Refundable")

# Guardamos la nueva base de datos para poder usarla en scripts posteriores

write.csv(d.e, paste0(path,'/', 'hotel_bookings_proc.csv'), row.names = FALSE)

