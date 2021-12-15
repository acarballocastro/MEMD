if(!exists('filename')) filename<- 'hotel_bookings.csv'
if(!exists('path')) path<-strsplit(shell('dir ..\\TMD.Rmd /s',intern = T)[4],' ')[[1]][4]
setwd(path)
if(!exists('e3.cor')) source('input/1_Preprocessing.r')
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
d.lda<-d.e[c(v$numeric,v$categoric)]
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
rownames(acuracy)<-c('Mod 1 pred all','Mod 2 pred all','Mod 2 pred train','Mod 2 pred test')
print(acuracy)
# no tenga mucha diferencia entre acuracy del test y train con el mod2, por lo tanto no hay overfiting

# Grafica de roc
p.train<-predict(mod.lda2,d.lda.train)
p.test<-predict(mod.lda2,d.lda.test)
print(roc.plot(d.lda.test$is_canceled == "1", p.test$posterior[, 2])$roc.vol )

# Se busca el corte de probabilidad para tener mejor acuracy
pes<-0.5

f<-function(pes,tabla=F,prob,class=levels(d.e$is_canceled) , data=d.lda.test$is_canceled){
  cla<-sapply(prob, function(x) factor(ifelse(x>pes,class[1],class[2])))
  tab<-xtabs(~cla+data)
  acu<-sum(diag(tab))/sum(tab)
  if(tabla) return(list(matrix=tab,accuracy=acu))
  return(acu)
}

a<-numeric(99)
for(i in 1:99) a[i]<-f(i/100,prob = p.test$posterior[,1])
(pes<-which.max(a)/100)
print(f(pes,table = T,prob = p.test$posterior[,1]))
vmedi<-which.min(abs(p.test$posterior[,1]-pes))
# resulta que apartir de 0.53 se obtiene mejor acuracy

# Representa los resultados en los LDA principals:
corte<-p.test$x[vmedi]
col0<-rgb(1,102/255,128/255)
col1<-rgb(63/255,211/255,247/255)
col<-apply(d.lda.test, 1, function(x) ifelse(x[2]==0,col0,col1))
plot(p.test$x,col=col,pch=16,
     main='Graphic of first plan factorial',ylab='LDA 1',xlab='Index')
abline(h=corte,lwd=3)
polygon(x=c(-500,2000,2000,-500),y=c(5,5,corte,corte),
        col=col1,density = 20)
polygon(x=c(-500,2000,2000,-500),y=c(-5,-5,corte,corte),
        col=col0,density = 20)

p.test$posterior[204,]

# Como que solo hay una unica LDA, resulta mejor un histograma:
plot(density(p.test$x[col==col0]),col=c0,xlim = c(-4,5),ylim = c(0,0.7),lty=2,lwd=4,
     main='Density of first axe factorial',xlab='LDA 1')
lines(density(p.test$x[col==col1]),col=c1,lty=2,lwd=4)
hist(p.test$x[col==col0],col=col0,density = 1,freq=F,add=T,breaks = 50)
hist(p.test$x[col==col1],col=col1,add=T,density = 1,freq=F,breaks = 50)
abline(v=corte,lwd=3)
legend('topleft',legend = c('is_canceled=0','is_canceled=1','puto de referencia'),
       col=c(col0,col1,1),lty=c(2,2,1))

# matriz confusion
t.test<-f(pes,tabla = T,prob = p.test$posterior[,1])$matrix
t.train<-f(pes,tabla = T,prob = p.train$posterior[,1],data = d.lda.train$is_canceled)$matrix

library(caret)
confusionMatrix(t.test)
confusionMatrix(t.train)
