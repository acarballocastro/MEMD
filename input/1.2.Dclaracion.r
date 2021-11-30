###############################################
# Título: 2.Declaracion de variables
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script hacemos 
# la declaracion de las variables
###############################################

if(!exists('e1.mis')) source('Input/1.1.Missings.r')
d.e<-e1.mis$d

# definir tipus de variables
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
  times='reservation_status_date',
  withmissing=v.mis
)
v$numeric<-c(v$integer,v$continua,v$times)

# decraracion de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])
levels(d.e$arrival_date_month)<-c("January", "February" ,"March", "April","May",
                                  "June" , "July", "August"  , "September" , 
                                  "November" , "October", "December")
levels(d.e$reserved_room_type)<-levels(d.e$assigned_room_type)

e1.dec<-new.env()
e1.dec$d<-d.e
e1.dec$v<-v

