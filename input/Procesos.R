
# Leer base de dada
data<-read.csv(paste0(params$path,'/',params$data.origin))

# solo seleccionando clientes de España
d.e<-data[data$country=='ESP',names(data)!='country']

# missings
d.e[d.e=='NULL']<-NA
mis<-sapply(d.e,function(x) sum(is.na(x)))
v.mis<-which(mis>0)
mis<-mis[mis>0]
n$missing<-sum(mis)
mis<-list(count=list(number=n$missing,forvar=mis),
          relative=list(forall=n$missing/n$total,formissing=mis/n$missing,
                        forvar=mis/n$observation))

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
  times='reservation_status_date'
)
v$numeric<-c(v$integer,v$continua,v$times)
v$withmissing<-v.mis

# decraracion de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])
levels(d.e$arrival_date_month)<-c("January", "February" ,"March", "April","May",
                                  "June" , "July", "August"  , "September" , 
                                  "November" , "October", "December")
levels(d.e$reserved_room_type)<-levels(d.e$assigned_room_type)



## preprossesing
d.e$is_company<-factor(ifelse(is.na(d.e$company),0,1))
d.e$is_agent<-factor(ifelse(is.na(d.e$agent),0,1))
v.elimina<-which(names(d.e) %in% c('company','agent'))
d.e<-d.e[,-v.elimina]
# renovamos los indices
v$withmissing<-NULL
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))

d.e$room_coherence<-factor(d.e$reserved_room_type == d.e$assigned_room_type)
d.e$if_prev_cancel<-factor(d.e$previous_cancellations>0)
d.e$if_prev_asign<-factor(d.e$previous_bookings_not_canceled>0)
d.e$if_wait<-factor(d.e$days_in_waiting_list>0)
# renovamos los indices
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))

v.elimina<-which(names(d.e) %in% c('reservation_status_date','arrival_date_week_number',
                                   'arrival_date_day_of_month','reservation_status'))
d.e<-d.e[,-v.elimina]
# renovamos los indices
v$times<-NULL
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))

corr<-cor(d.e[,v$numeric])
corr[ col(corr)<=row(corr) ]<-0
corr<-corr[!apply(corr, 1, function(x) all(abs(x)<0.2)),!apply(corr, 2, function(x) all(abs(x)<0.2))]
d.e<-d.e[,!names(d.e)%in%c('previous_bookings_not_canceled','if_prev_asign','previous_cancellations','days_in_waiting_list')]
# renovamos los indices
cla<-sapply(d.e, class)
v$categoric<-which(cla=='factor')
v$integer<-which(cla=='integer')
v$continua<-which(cla=='numeric')
v$numeric<-c(unlist(v$continua),unlist(v$integer))
















