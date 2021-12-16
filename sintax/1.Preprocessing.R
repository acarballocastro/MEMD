###############################################
# Título: 1. Preprocessing
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 14/12/21

# Descripción: En este script hacemos el preprocessing
# de la base de datos seleccionando las variables de interés 
# y creando nuevas variables. También hay gráficos para
# ayudar a la descripción univariante y bivariante,
# tanto antes del preprocessing como después
###############################################


# Leemos la base de datos de GitHub
path <-'../data'
data <- read.csv2(paste0(path,'/',"../data/hotel_bookings.csv"), sep=",")
# Seleccionamos solo clientes de España
d.e <- data[data$country=='ESP',names(data)!='country']

name <- names(d.e)
n <- list(total=prod(dim(d.e)),observation=nrow(d.e),variable=ncol(d.e))

# Cargamos los paquetes necesarios y funciones propias
pkg <- c('ggplot2',"corrplot","PerformanceAnalytics", "FactoMineR", "factoextra",'Matrix','NbClust',
         'MASS','verification','VIM','Hmisc','caret','rpart','naivebayes','tidyverse','tidyr','cluster',
         'lattice','rpart.plot','xgboost','tidyverse','caTools','e1071','nnet','arules','arulesViz')
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)){ install.packages(new.pkg, dependencies = TRUE) ; rm(c('pkg','new.pkg'))}



###############################################
# 1. Descripción de los datos
###############################################

########### Descripción de datos faltantes

# Según la metadata son missings del tipo no aplicable
# (está comentado más adelante, en el preprocessing)
d.e[d.e=='NULL'] <- NA
mis <- sapply(d.e,function(x) sum(is.na(x)))
v.mis <- which(mis>0)
mis <- mis[mis>0]
n$missing <- sum(mis)
mis <- list(count=list(number=n$missing,forvar=mis),
            relative=list(forall=n$missing/n$total,formissing=mis/n$missing,
                          forvar=mis/n$observation))
mis$relative$formissing <- data.frame(prop=mis$relative$formissing, 
                                      variable=names(mis$relative$formissing))

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
  times='reservation_status_date',
  withmissing=v.mis
)
v$numeric <- c(v$integer,v$continua,v$times)

# Declaración de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])
levels(d.e$arrival_date_month)<-c("January", "February" ,"March", "April","May",
                                  "June" , "July", "August"  , "September" , 
                                  "November" , "October", "December")
levels(d.e$reserved_room_type)<-levels(d.e$assigned_room_type)


########### Análisis descriptivo UNIVARIANTE inicial

# Se realiza un bucle para la creación de gráficos automáticamente.
# Con la función 'capitalize' ponemos en mayúsculas la primera letra del nombre
# y con la función 'gsub' substituímos los símbolos "_" por espacios.
# Para ello necesitamos la librería 'Hmisc'.

# Con la función 'png' guardamos los gráficos en el ordenador en vez de imprimirlos
# en pantalla para así ponerlos y comentarlos en el documento word. 
# Debemos tener en cuenta que las imágenes se guardarán en el directorio de trabajo
# en el que nos encontremos.

for(j in 1:4){
  # 1 categoric, 2 integer, 3 continua, 4 temporal
  cat('\n\nVariable',names(v)[j],'\n')
  color <- "lightskyblue1" # "darkolivegreen2"
  for(i in v[[j]]){
    a <- capitalize(gsub("_", " ", i))
    cat('\n\n')
    #png(paste0(a,".","png"))
    print(ggplot(d.e, aes(x=d.e[[i]])) + geom_bar(stat = "count", fill=color,na.rm = T)+
            theme_minimal() +
            labs(title = "Histograma")+xlab(a))
    #dev.off()
    print(summary(d.e[[i]]))
  }
}

########### Análisis descriptivo BIVARIANTE inicial

# Aparte de lo mencionado anteriormente, en el caso de la bivariante se tiene 
# en cuenta qué tipo de variable tenemos a la hora de hacer el gráfico.
# Las variables numéricas que no hemos podido representar con un 'boxplot'
# se han representado con un 'barplot'.

for(j in 1:3){
  color <- c("lightskyblue1","lightskyblue") #"Indianred2","darkolivegreen2"
  for(i in v[[j]]){
    b <- capitalize(gsub("_", " ", i))
    #png(paste0(b,".","png"))
    if (j == 1){
      if(i == 'is_canceled'){ next }
      barplot(prop.table(table(d.e$is_canceled, d.e[[i]]),2),main=b,col=color)
    }else if(j == 2){
      if(median(d.e[[i]]) >= 1 && median(d.e[[i]]) != quantile(d.e[[i]],0.75)){
        boxplot(as.formula(paste0(i,"~is_canceled")),d.e,main=b,col=color,horizontal=T)}
      else barplot(prop.table(table(d.e$is_canceled, d.e[[i]]),2),main=b,col=color)
    }else {boxplot(as.formula(paste0(i,"~is_canceled")),d.e,main=b,col=color,horizontal=T)}
    #dev.off()
  }
}


###############################################
# 2. Preprocessing
###############################################

# Comenzamos por las variables que contienen missings
cat.idx <- function(data,name=NULL,mis=NULL,time=NULL){
  if(missing(name)) name <- 1:length(data)
  cla <- sapply(data, class)
  v <- list(categoric=name[which(cla == 'factor')],
          integer=name[which(cla == 'integer')],
          continua=name[which(cla == 'numeric')])
  v$numeric <- c(unlist(v$continua),unlist(v$integer))
  if(!missing(mis)) v$withmissing <- mis
  if(!missing(time)) v$times <- time
  return(v)
}


# Los missings están concentrados en las variables "company" y "agent".
# Son missings no aplicables, estructurales, ya que si un cliente no hizo la reserva
# a partir de una agencia/compañía de viajes, no tiene sentido preguntarle
# preguntarle por esta.
# Así que eliminamos las variables "company" y "agent" y añadimos 2 variables más: 
# "is_company" y "is_agent", que indican si la reserva se ha hecho a través de 
# una agencia o compañía (1) o no (0), respectivamente.
d.e$is_company <- factor(ifelse(is.na(d.e$company),0,1))
d.e$is_agent <- factor(ifelse(is.na(d.e$agent),0,1))
v.elimina <- which(names(d.e) %in% c('company','agent'))
d.e <- d.e[,-v.elimina]

# Renovamos los índices
v <- cat.idx(d.e,names(d.e),time=v$times)

# Seguimos con otras variables

# Creemos que es mejor saber si la habitación reservada se corresponde
# con la asignada que considerar estas 2 variables por separado
# ya que no sabemos qué significa cada categoría.
# Por eso, creamos la variable "room_coherence", que será 1 si
# la habitación reservada se corresponde con la asignada
# y 0 en caso contrario. 
d.e$room_coherence <- factor(d.e$reserved_room_type == d.e$assigned_room_type)

# Como con las habitaciones, consideramos que es mejor saber si ha habido
# días en la cola de espera que saber cuántos son
d.e$if_wait <- factor(d.e$days_in_waiting_list>0)

# Igualmente para "previous_cancellations" i "previous_bookings_not_canceled"
d.e$if_prev_cancel <- factor(d.e$previous_cancellations>0)
d.e$if_prev_asign <- factor(d.e$previous_bookings_not_canceled>0)

d.new_crea <- d.e[,c('is_canceled','room_coherence','if_prev_cancel','if_prev_asign','if_wait')]

# Renovamos los índices
v <- cat.idx(d.e,names(d.e),time=v$times)


# Observamos que la variable "reservation_status" tiene la misma información que
# nuestra variable respuesta así que la eliminaremos, al igual que "reservation_status_date",
# pues esta variable no nos aporta nada, y menos si hemos dicho de quitar "reservation_status".

# Notamos que nuestra variable respuesta está igual distribuida que la variable "arrival_date_day_of_month"
# por lo que esta última no aporta información nueva, así que la eliminaremos.
# Lo mismo haremos con "arrival_date_week_number", pues indica lo mismo que "arrival_date_day_of_month".
v.elimina <- which(names(d.e) %in% c('reservation_status_date','reservation_status',
                                     'arrival_date_day_of_month','arrival_date_week_number'))
d.e <- d.e[,-v.elimina]

# Renovamos los índices
v <- cat.idx(d.e,names(d.e))


# Ahora, miraremos la correlación entre variables numéricas con un valor mayor a 0.2.
# Si hay correlación, eliminaremos una de las 2 variables implicadas.
corr <- cor(d.e[,v$numeric])
corr[ col(corr)<=row(corr) ] <- 0
corr <- corr[!apply(corr, 1, function(x) all(abs(x) < 0.2)),
           !apply(corr, 2, function(x) all(abs(x) < 0.2))]

# Visto la alta correlación entre "previous_cancellations" y "previous_bookings_not_canceled", 
# decidimos que eliminamos una de estas variables.
# Y como habíamos creado unas variables equivalentes anteriormente,
# eliminaremos las dos: "previous_cancellations" y "previous_bookings_not_canceled"
# y también la equivalente a una de ellas, "if_prev_asign".
# Y ya que estamos eliminando variables duplicadas, eliminaremos también
# "days_in_waiting_list", pues antes hemos creado "if_wait".
d.e <- d.e[,!names(d.e)%in%c('previous_bookings_not_canceled',
                             'previous_cancellations','if_prev_asign',
                             'days_in_waiting_list')]
# Renovamos los índices
v <- cat.idx(d.e,names(d.e))

# Arreglamos los labels
levels(d.e$hotel) <- c("City_Hotel", "Resort_Hotel")
levels(d.e$is_canceled) <- c("Not_cancelled", "Cancelled")
levels(d.e$deposit_type) <- c("No_deposit", "Non_refund", "Refundable")
levels(d.e$market_segment) <- c("Aviation","Complementary","Corporate","Direct","Groups" , "Offline_TA/TO", 
                                "Online_TA", "Undefined")


# Guardamos la nueva base de datos para poder usarla en scripts posteriores
write.csv(d.e, paste0(path,'/', 'data_preproc.csv'), row.names = FALSE)


#########################################################
# 3. Descripción de los datos después del preprocessing
#########################################################

########### Análisis descriptivo UNIVARIANTE posterior

for(j in 1:3){
  cat('Variable',names(v)[j],'\n')
  color <- lightskyblue1 # "darkolivegreen2"
  for(i in v[[j]]){
    a2 <- capitalize(gsub("_", " ", names(d.e)[i]))
    cat('\n\n')
    #png(paste0(a2,".","png"))
    print(ggplot(d.e, aes(x=d.e[[i]])) + geom_bar(stat = "count", fill=color)+
            theme_minimal() +
            labs(title = "Histograma")+xlab(a2))
    #dev.off()
    print(summary(d.e[[i]]))
  }
}


########### Análisis descriptivo BIVARIANTE posterior

for(j in 1:3){
  cat('Variable',names(v)[j],'\n')
  color <- c("lightskyblue1","lightskyblue") # "Indianred2","darkolivegreen2"
  
  for(i in names(d.e)[v[[j]]]){
    b2 <- capitalize(gsub("_", " ", i))
    cat('\n\n')
    #png(paste0(b2,".","png"))
    if (j == 1){
      if(i == 'is_canceled') next
      barplot(prop.table(table(d.e$is_canceled, d.e[[i]]),2),main=b2,col=color)
    }else if(j == 2){
      if(median(d.e[[i]]) >= 1 && median(d.e[[i]]) != quantile(d.e[[i]],0.75)) boxplot(as.formula(paste0(i,"~is_canceled")),d.e,main=b2,col=color,horizontal=T)
      else barplot(prop.table(table(d.e$is_canceled, d.e[[i]]),2),main=b2,col=color)
    }else boxplot(as.formula(paste0(i,"~is_canceled")),d.e,main=b2,col=color,horizontal=T)
    #dev.off()
  }
}
