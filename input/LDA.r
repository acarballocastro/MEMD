
if(!exists('e3.cor')) source('input/3.3.Correlacion_tract.r')
d.e<-e3.cor$d
v<-e3.cor$v

# Se carga paquetes necesarias
library(MASS)
library(verification)

# Se define funcions para calcular acuracy
acu<-function(tab,value=T){
  acu<-sum(diag(tab))/sum(tab)
  if(!value) return(c(accuracy=acu))
  pre<-mean(diag(tab)/rowSums(tab))
  rec<-mean(diag(tab)/colSums(tab))
  f<-2*pre*rec/(rec+pre)
  return(c(accuracy=acu,recall=rec,precision=pre,f.score=f))
}
accu<-function(data,mod,y,pred.select,table=F,...){
  pred<-predict(mod,data,...)
  if(!missing(pred.select)) pred<-pred[[pred.select]]
  tab<-xtabs(~pred+data[[y]])
  if(table) print(tab)
  r<-acu(tab,...)
  return(r)
}

# lda falla cuando variable tenga factor sin observacion: reserved_room_type
d.lda<-d.e
d.lda$reserved_room_type<-factor(as.character(d.e$reserved_room_type))
# Construir el model con data conjunta
mod.lda<-lda(data=d.lda,is_canceled~.)

# Construir el model con test y train
set.seed(20245481)
test<-sample(nrow(d.e),nrow(d.e)*0.2)
d.lda.train<-d.lda[-test,]
d.lda.test<-d.lda[test,]
mod.lda2<-lda(data=d.lda.train,is_canceled~.)

# Comparar los acculacy de los modelos
acuracy<-rbind(accu(d.e,mod.lda,'is_canceled',1),accu(d.e,mod.lda2,'is_canceled',1),
      accu(d.lda.train,mod.lda2,'is_canceled',1),accu(d.lda.test,mod.lda2,'is_canceled',1))
rownames(acuracy)<-c('Mod 1 test all','Mod 2 test all','Mod 2 test train','Mod 2 test test')
print(acuracy)
# no tenga mucha diferencia entre acuracy del test y train con el mod2, por lo tanto no hay overfiting

# Grafica de roc
p.train <- predict(mod.lda2)
p.test<-predict(mod.lda2,d.lda.test)
print(roc.plot(d.lda.train$is_canceled == "1", p.train$posterior[, 2])$roc.vol )
print(roc.plot(d.lda.test$is_canceled == "1", p.test$posterior[, 2])$roc.vol )

# Se busca el corte de probabilidad para tener mejor acuracy
pes<-0.5
f<-function(pes){
  cla<-sapply(p.test$posterior[,1], function(x) ifelse(x>pes,0,1))
  tab<-xtabs(~cla+d.lda.test$is_canceled)
  acu<-sum(diag(tab))/sum(tab)
  return(acu)
}
a<-numeric(99)
for(i in 1:99) a[i]<-f(i/100)
(pes<-which.max(a)/100)
print(f(pes))
vmedi<-which.min(abs(p.test$posterior[,1]-pes))
# resulta que apartir de 0.53 se obtiene mejor acuracy

# Representa los resultados en los LDA principals:
corte<-p.test$x[vmedi]
col<-apply(d.lda.test, 1, function(x) ifelse(x[2]==0,'red','blue'))
plot(p.test$x,col=col,main='Graphic of first plan factorial',ylab='LDA 1',xlab='Index')
abline(h=corte,lwd=3)


p.test$posterior[204,]
# Como que solo hay una unica LDA, resulta mejor un histograma:
plot(density(p.test$x[col=='red']),col='red',xlim = c(-4,5),ylim = c(0,0.7),lty=2,lwd=4,
     main='Density of first axe factorial',xlab='LDA 1')
lines(density(p.test$x[col=='blue']),col='blue',lty=2,lwd=4)
hist(p.test$x[col=='red'],col='red',density = T,freq=F,add=T,breaks = 50)
hist(p.test$x[col=='blue'],col='blue',add=T,density = T,freq=F,breaks = 50)
abline(v=corte,lwd=3)
legend('topleft',legend = c('is_canceled=0','is_canceled=1','puto de referencia'),
       col=c('red','blue',1),lty=c(2,2,1))










