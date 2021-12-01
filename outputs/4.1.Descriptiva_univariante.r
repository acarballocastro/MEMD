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

for(j in 1:3){
  cat('Variable',names(v)[j],'\n')
  color<-c('orange','steelblue','red')[j]
  for(i in v[[j]]){
    cat('\n\n')
    print(ggplot(d.e, aes(x=d.e[[i]])) + geom_bar(stat = "count", fill=color)+
            theme_minimal() +
            labs(title = "Histogram")+xlab(i))
    print(summary(d.e[[i]]))
  }
}
