###############################################
# Título: 2.Descriptiva
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script hacemos 
# la descriptiva univariante de las variables
###############################################

if(!exists('e1.dec')) source('Input/1.2.Declaracion.r')
d.e<-e1.dec$d
v<-e1.dec$v

for(j in 1:4){
  cat('Variable',names(v)[j],'\n')
  color<-c('orange','steelblue','red','limegreen')[j]
  for(i in v[[j]]){
    cat('\n\n')
    print(ggplot(d.e, aes(x=d.e[[i]])) + geom_bar(stat = "count", fill=color,na.rm = T)+
            theme_minimal() +
            labs(title = "Histogram")+xlab(i))
    print(summary(d.e[[i]]))
  }
}

# adr
#print(ggplot(d.e, aes(x=adr)) + geom_histogram(bins = 100, fill = "steelblue")+
#        theme_minimal() +
#        labs(title = "Histogram"))
print(summary(d.e[["adr"]]))

# reservation_status_date
print(ggplot(d.e, aes(x=reservation_status_date)) + geom_histogram(bins = 100, fill = "steelblue")+
        theme_minimal() +
        labs(title = "Histogram reservation_status_date"))
print(summary(d.e[["reservation_status_date"]]))

# Otro tipo de histogramas
for(i in v$numeric){
  p <- ggplot(d.e, aes(x=d.e[,i]))+ theme_minimal()+
    geom_histogram(color="darkblue", fill="lightblue", bins=30)
  
  print(p + labs(title= i,
                 x = names(d.e)[which(names(d.e)==i)], y = "Count")) 
}# No funciona bé per a contínua i times

# Otro tipo de diagramas
require("RColorBrewer")
for(i in v$categoric){
  pie(table(d.e[,which(names(d.e)==i)]), radius = 1, col=brewer.pal(length(names(table(d.e[,which(names(d.e)==i)]))),'Spectral'))
  legend(x = "topleft", legend = names(table(d.e[,which(names(d.e)==i)])),
         fill=brewer.pal(length(names(table(d.e[,which(names(d.e)==i)]))),'Spectral'))
}
