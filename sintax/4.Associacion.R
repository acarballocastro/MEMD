
library(arulesViz)
library(arules)

if(!exists('d.e')){
  path <-'../data'
  d.e <- read.csv2(paste0(path,'/',"hotel_bookings_proc.csv"), sep=",")
  d.e$adr<-as.numeric(d.e$adr)
  v<-list(
    categoric=c('hotel','is_canceled', 'arrival_date_month','arrival_date_year', 'meal','market_segment','distribution_channel',
                'is_repeated_guest','reserved_room_type','assigned_room_type', 'room_coherence', 
                'is_company', 'is_agent', 'customer_type','deposit_type', 'if_prev_cancel','if_wait'),
    integer=c('lead_time', 'stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
              'booking_changes','required_car_parking_spaces','total_of_special_requests'),
    continua='adr')
  v$numeric <- c(v$integer,v$continua)
  library(ggplot2)
  for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
  for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
}
# se hacen las transformaciones necesarias para poder utilizar la función del paquete arules
dd <- d.e
dcat <- dd[, as.vector(sapply(dd, is.factor))]
trans <- as(dcat, "transactions")

# Se buscan las normas que tengan como resultado una cancelación de la reserva.
# Se filtan las que tengan longitud minima de 10, con un soporte mínimo de 0.2 y una confidence 
# mínima de 0.4
# al usar restricciones mas laxas, aparecen miles de reglas, y si se usan restricciones más 
# fuertes, no aparece ninguna regla.

rules <- apriori(dcat, parameter = list(support = 0.2, confidence = 0.4,  minlen = 10, target= "rules"),
                 appearance = list(default="lhs", rhs="is_canceled=Cancelled"))

# La función da como resultado 8 reglas
inspect(rules)

# Esta gráfica muestra la norma con mayor confidence.
subrules <- head(rules, n = 1, by = "confidence")
plot(subrules, method = "graph",  engine = "htmlwidget")

# Esta gráfica muestra las 8 reglas obtenidas
plot(rules, method = "graph",  engine = "htmlwidget")
