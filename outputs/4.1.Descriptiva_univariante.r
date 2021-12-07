###############################################
# Título: 4.Descriptiva Univariante
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script describimos la
# Base de datos processadas
###############################################

if(!exists('e3.cor')) source('input/3.3.Correlacion_tract.r')
d.e<-e3.cor$d
v<-e3.cor$v

for(i in v$categoric){
  print(ggplot(d.e, aes(x=d.e[[i]])) + geom_bar(stat = "count", fill='orange',na.rm = T)+
          theme_minimal() +
          labs(title = "Histogram",x=i,main=i))
  print(summary(d.e[[i]]))
}

for(i in v$numeric){
  print(ggplot(d.e, aes(x=d.e[,i]))+ theme_minimal()+
          geom_histogram(color="darkblue", fill="lightblue", bins=30)+
          labs(title= i,x = i))
  print(summary(d.e[[i]]))
}
