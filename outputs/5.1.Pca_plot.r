###############################################
# Título: 5. Descriptiva PCA
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script representamos el
# analisi del componente principal
###############################################

if(!exists('e5.pca')) source('input/5.1.Pca.r')
d.e<-e5.pca$d
v<-e5.pca$v
res.pca<-e5.pca$pca

barplot(res.pca$eig[,2],col="steelblue")
lines(x = seq(0.7,length.out = nl,by=1.2), res.pca$eig[,2],type="b", pch=19, col = "red")
barplot(res.pca$eig[,3],col="steelblue")
abline(h=c(66,80),col='red',lty=3)

for(i in 1:ni) print(fviz_contrib(res.pca, choice = "var", axes = i, top = 10))

for(i in seq(1,ni-1,by=2)){
  j<-i+1
  cs<-sum(sqrt(res.pca$var$contrib[,i]^2+res.pca$var$contrib[,j]^2)>10)
  print(fviz_pca_var(res.pca, axes=c(i,j),col.var="contrib",
                     select.var = list(contrib=cs)))
}

# Control automatically the color of individuals using the cos2 values (the quality of the individuals on the factor map)
print(fviz_pca_ind(res.pca,  col.ind="cos2",geom = 'point') +
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=0.50)+
  theme_minimal())
# Si nos fijamos en este gr?fico vemos que el individuo 173 es claramente un outlier

# Make a biplot of individuals and variables :
print(fviz_pca_biplot(res.pca,  geom = "text"))

