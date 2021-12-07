###############################################
# Título: 5.1. PCA
# Autor: Aleix
# Fecha: 30/11/21

# Descripci?n: PRINCIPAL COMPONENT ANALYSIS (PCA)
###############################################

if(!exists('e3.cor')) source('input/3.2.Correlacion.tract.r')
d.e<-e3.cor$d
v<-e3.cor$v

# cargamos los paquetes que usaremos
for(i in c("corrplot","PerformanceAnalytics", "FactoMineR", "factoextra")) require(i,character.only = T)

# PCA
# Si queremos suprimir el outlier d.e[v$integer][-173,]
nl<-length(v$numeric)
res.pca<-PCA(d.e[v$numeric],graph = F,ncp=nl)

ni<-which(res.pca$eig[,3]>80)[1]
res.pca<-PCA(d.e,graph = F,ncp=ni,quali.sup =which(names(d.e)%in%v$categoric))

e5.pca<-new.env()
e5.pca$d<-d.e
e5.pca$v<-v
e5.pca$pca<-res.pca


