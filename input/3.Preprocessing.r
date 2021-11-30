###############################################
# Título: 3.Preprocessing
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script hacemos el preprocessing
# de la base de datos seleccionando las variables de interés 
# y creando nuevas variables
###############################################

# LIBRERÍAS
library("ggplot2")

###############################################

# Leer base de datos
path <-'C:/Users/garys/Desktop/MINERIA DE DADES'
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


# Declaración de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])
levels(d.e$arrival_date_month)<-c("January", "February" ,"March", "April","May",
                                  "June" , "July", "August"  , "September" , 
                                  "November" , "October", "December")
levels(d.e$reserved_room_type)<-levels(d.e$assigned_room_type)


# PREPROCESSING

# 1. Missing

# Los missings están concentrados en las variables 'company' y 'agent',
# pero la ausencia de respuestas no es ausencia de información, pues cada NULL
# nos indica que esos clientes NO hicieron la reserva a través de una compañía o agente.
# Son missings no aleatorios, que sí aportan información.
# Así que eliminamos las variables 'company' y 'agent' y añadimos 2 variables más: 
# 'is_company' y 'is_agent', que indican si la reserva se ha hecho a través de 
# una agencia o compañía, respectivamente.
d.e$is_company<-factor(ifelse(is.na(d.e$company),0,1))
d.e$is_agent<-factor(ifelse(is.na(d.e$agent),0,1))
v.elimina<-which(names(d.e) %in% c('company','agent'))
d.e<-d.e[,-v.elimina]




# 2. Creación de nuevas variables

# Creemos que es mejor saber si la habitación reservada se corresponde
# con la asignada que considerar estas 2 variables por separado
# ya que no sabemos qué significa cada categoría porque los datos
# se han publicado así para preservar el anonimáto de los clientes.
# Por eso, creamos una nueva variable, 1 si la habitación reservada se corresponde 
# con la habitación asignada, 0 en caso contrario. 
d.e$room_coherence<-factor(d.e$reserved_room_type == d.e$assigned_room_type)

# Como con las habitaciones, consideramos que es mejor saber si ha habido
# días en la cola de espera que saber cuántos son.
d.e$if_wait<-factor(d.e$days_in_waiting_list>0)

# Igualmente para 'previous_cancellations' i 'previous_bookings_not_canceled'
d.e$if_prev_cancel<-factor(d.e$previous_cancellations>0)
d.e$if_prev_asign<-factor(d.e$previous_bookings_not_canceled>0)

# Eliminaremos las variables "duplicadas" al final



# 3. Multicolinealidad

# Empezamos estudiando las variables relacionadas con fechas
# Año 
ggplot(d.e, aes(x = arrival_date_year, fill = is_canceled, y = is_canceled))+ geom_col(position = 'fill' ) +ggtitle("Correlación entre Año y cancelación")+ labs(y="Cancelaciones", x = "Año")
# Mes
ggplot(d.e, aes(x = arrival_date_month, fill = is_canceled, y = is_canceled)) + geom_col(position = "fill")+ggtitle("Correlación entre Mes y cancelación")+ labs(y="Cancelaciones", x = "Mes")
# Día
ggplot(d.e, aes(x = arrival_date_day_of_month, fill = is_canceled, y = is_canceled)) + geom_col(position = "fill")+ggtitle("Correlación entre Día y cancelación")+ labs(y="Cancelaciones", x = "Día")

# Notamos que nuestra variable respuesta está igual distribuida que la variable 'arrival_date_day_of_month'
# por lo que esta última no aporta información nueva, así que la eliminaremos.
# En cambio, la variable que hace referencia al año sí que es relevante,  
# pero no la tendremos en cuenta ya que no es interesante para los modelos a estudiar.
# Por otro lado, como no hay mucha diferencia entre número de semanas y meses en un año,
# eliminaremos también el número de semanas.

summary(d.e[,c('is_canceled','reservation_status')])

# Aquí observamos que la variable 'reservation_status' tiene la misma información que
# nuestra variable respuesta así que la eliminaremos, al igual que 'reservation_status_date'.
# Finalmente, también eliminamos 'adr' que no aporta absolutamente nada.
v.elimina<-which(names(d.e) %in% c('arrival_date_day_of_month','arrival_date_week_number',
                                   'reservation_status','reservation_status_date','adr'))
d.e <- d.e[,-v.elimina]

# Renovamos los índices
v$withmissing<-NULL
v$times<-NULL
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))




# Ahora, miraremos la correlación entre variables numéricas con un valor mayor a 0.2
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


# Visto la alta correlación entre 'previous_cancellations' y 'previous_bookings_not_canceled', 
# decidimos que eliminamos una de estas variables.
# Y como habíamos creado unas variables equivalentes (apartado 2.)
# eliminaremos las dos 'previous_cancellations' y 'previous_bookings_not_canceled'
# y la equivalente a una de ellas, 'if_prev_asign'.

# Y ahora que ya hemos acabado con las correlaciones, eliminaremos las variables
# que hemos sustituido por otras en el apartado 2.
v.elimina<-which(names(d.e) %in% c('reserved_room_type','assigned_room_type','days_in_waiting_list',
                                   'previous_cancellations', 'previous_bookings_not_canceled','if_prev_asign'))
d.e <- d.e[,-v.elimina]

# Renovamos los índices
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))


#saving the dataframe in an external file
write.table(d.e, file = "datapreprocessed.csv", sep = ",", na = "NA", row.names=FALSE, col.names = TRUE)
