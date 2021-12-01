###############################################
# Título: 3.Deteccion de multicolinealidad
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script describimos los
# possibles multicolinealidad
###############################################

if(!exists('e3.var')) source('input/3.2.New_variable.r')
d.e<-e3.var$d
v<-e3.var$v

print(ggplot(d.e, aes(x = arrival_date_year, fill = is_canceled, y = is_canceled)) + 
  geom_col(position = 'fill' ) +ggtitle("Correlacion entre Fecha y cancelacion")+ 
  labs(y="Cancelaciones", x = "Fecha"))

print(ggplot(d.e, aes(x = arrival_date_month, fill = is_canceled, y = is_canceled)) + 
  geom_col(position = "fill")+ggtitle("Correlacion entre mes del año y cancelacion")+ 
  labs(y="Cancelaciones", x = "Mes del año"))

print(ggplot(d.e, aes(x = arrival_date_day_of_month, fill = is_canceled, y = is_canceled)) + 
  geom_col(position = "fill")+ggtitle("Correlacion entre dia del mes y cancelacion")+ 
  labs(y="Cancelaciones", x = "Dia del mes"))

print(summary(d.e[,c('is_canceled','reservation_status')]))

if(!exists('corr')){
  corr<-cor(d.e[,v$numeric])
  corr[ col(corr)<=row(corr) ]<-0
  corr<-corr[!apply(corr, 1, function(x) all(abs(x)<0.2)),
             !apply(corr, 2, function(x) all(abs(x)<0.2))]
}

corrplot::corrplot(corr)
