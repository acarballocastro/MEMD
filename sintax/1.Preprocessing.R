###############################################
# Título: 1. Preprocessing
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 24/11/21
# Descripción: En este script hacemos el preprocessing
# de la base de datos seleccionando las variables de interés 
# y creando nuevas variables
###############################################

path <-'../data'
data <- read.csv2(paste0(path,'/',"../data/hotel_bookings.csv"), sep=",")

# Seleccionamos solo clientes de España
d.e <- data[data$country=='ESP',names(data)!='country']
name <- names(d.e)

n <- list(total=prod(dim(d.e)),observation=nrow(d.e),variable=ncol(d.e))

# Cargar paquetes necesarios y funciones propias
pkg <- c('ggplot2',"corrplot","PerformanceAnalytics", "FactoMineR", "factoextra",'Matrix')
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)){ install.packages(new.pkg, dependencies = TRUE) ; rm(c('pkg','new.pkg'))}

# 2. Descripción de datos faltantes

# Missings 
# Comprobamos en la metadata que eran missings del tipo no aplicables.

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

cat.idx<-function(data,name=NULL,mis=NULL,time=NULL){
  if(missing(name)) name<-1:length(data)
  cla<-sapply(data, class)
  v<-list(categoric=name[which(cla=='factor')],
          integer=name[which(cla=='integer')],
          continua=name[which(cla=='numeric')])
  v$numeric<-c(unlist(v$continua),unlist(v$integer))
  if(!missing(mis)) v$withmissing<-mis
  if(!missing(time)) v$times<-time
  return(v)
}

d.e$is_company<-factor(ifelse(is.na(d.e$company),0,1))
d.e$is_agent<-factor(ifelse(is.na(d.e$agent),0,1))
v.elimina<-which(names(d.e) %in% c('company','agent'))
d.e<-d.e[,-v.elimina]

# renovamos los indices
v<-cat.idx(d.e,names(d.e),time=v$times)

d.e$room_coherence<-factor(d.e$reserved_room_type == d.e$assigned_room_type)
d.e$if_prev_cancel<-factor(d.e$previous_cancellations>0)
d.e$if_prev_asign<-factor(d.e$previous_bookings_not_canceled>0)
d.e$if_wait<-factor(d.e$days_in_waiting_list>0)
d.new_crea<-d.e[,c('is_canceled','room_coherence','if_prev_cancel','if_prev_asign','if_wait')]
# renovamos los indices
v<-cat.idx(d.e,names(d.e),time=v$times)

v.elimina<-which(names(d.e) %in% c('reservation_status_date','arrival_date_week_number',
                                   'arrival_date_day_of_month','reservation_status'))
d.e<-d.e[,-v.elimina]

# renovamos los indices
v<-cat.idx(d.e,names(d.e))
corr<-cor(d.e[,v$numeric])
corr[ col(corr)<=row(corr) ]<-0
corr<-corr[!apply(corr, 1, function(x) all(abs(x)<0.2)),
           !apply(corr, 2, function(x) all(abs(x)<0.2))]
d.e<-d.e[,!names(d.e)%in%c('previous_bookings_not_canceled',
                           'if_prev_asign','previous_cancellations',
                           'days_in_waiting_list')]
# renovamos los indices
v<-cat.idx(d.e,names(d.e))

# Arreglar los labels
levels(d.e$hotel) <- c("City_Hotel", "Resort_Hotel")
levels(d.e$is_canceled) <- c("not_canceled", "canceled")
levels(d.e$deposit_type) <- c("No_deposit", "Non_refund", "Refundable")
levels(d.e$market_segment) <- c("Aviation","Complementary","Corporate","Direct","Groups" , "Offline_TA/TO", 
                                "Online_TA", "undefined")

# Guardamos la nueva base de datos para poder usarla en scripts posteriores

write.csv(d.e, paste0(path,'/', 'data_preproc.csv'), row.names = FALSE)
