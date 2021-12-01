###############################################
# Título: 3.Preprocessing: multicolinealidad
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script hacemos el preprocessing
# de correcciones de multicolinealidad
###############################################

if(!exists('e3.var')) source('input/3.2.New_variable.r')
d.e<-e3.var$d
v<-e3.var$v

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

e3.cor<-new.env()
e3.cor$d<-d.e
e3.cor$v<-v

# Guardamos la nueva base de datos para poder usarla en scripts posteriores

write.csv(d.e, 'data/hotel_bookings_proc.csv', row.names = FALSE)

