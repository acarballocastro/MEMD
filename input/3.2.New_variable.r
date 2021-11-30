###############################################
# Título: 3.Preprocessing: New Variable
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script hacemos el preprocessing
# de creando nuevas variables
###############################################

if(!exists('e3.mis')) source('input/3.1.Missing_tract')
d.e<-e3.mis$d
v<-e3.mis$v

# nuevas variables
d.e$room_coherence<-factor(d.e$reserved_room_type == d.e$assigned_room_type)
d.e$if_prev_cancel<-factor(d.e$previous_cancellations>0)
d.e$if_prev_asign<-factor(d.e$previous_bookings_not_canceled>0)
d.e$if_wait<-factor(d.e$days_in_waiting_list>0)
d.new_crea<-d.e[,c('is_canceled','room_coherence','if_prev_cancel','if_prev_asign','if_wait')]
# renovamos los indices
v<-cat.idx(d.e,names(d.e),time=v$times)

e3.var<-new.env()
e3.var$d<-d.e
e3.var$v<-v
