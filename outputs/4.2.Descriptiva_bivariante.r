###############################################
# Título: 3. Descriptiva Bivariante
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script descibimos las
# variables en funcion de la variable de respesta
###############################################

if(!exists('e3.cor')) source('input/3.3.Correlacion.tract.r')
d.e<-e3.cor$d
v<-e3.cor$v

for(j in 1:3){
  cat('Variable',names(v)[j],'\n')
  color<-c('orange','steelblue','red')[j]
  par(mfrow=rep(c(4,3,1)[j],2),mar=c(3,3,3,1))
  for(i in v[[j]]){
    cat('\n\n')
    if (j==1){
      if(i=='is_canceled') next
      barplot(prop.table(table(d.e$is_canceled, d.e[[i]]),2),main=i,col=c(2,3))
    }else if(j==2){
      if(max(d.e[[i]])>10) boxplot(as.formula(paste0(i,"~is_canceled")),d.e,main=i,col=c(2,3),horizontal=T)
      else barplot(prop.table(table(d.e$is_canceled, d.e[[i]]),2),main=i,col=c(2,3))
    }else boxplot(as.formula(paste0(i,"~is_canceled")),d.e,main=i,col=c(2,3),horizontal=T)
  }
}
