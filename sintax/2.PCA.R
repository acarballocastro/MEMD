###############################################
# Título: 2. PCA
# Autor: Aleix
# Fecha: 30/11/21
# Descripción: PRINCIPAL COMPONENT ANALYSIS (PCA). El objetivo de PCA es la reducción de dimensiones, que se puede utilizar para:
# 1 - visualización de datos multivariados mediante diagramas de dispersión
# 2 - transformación de variables x altamente correlacionadas en un conjunto más pequeño de variables latentes no correlacionadas que pueden ser utilizadas por otros métodos
# 3 - separación de la información relevante (por algunas variables latentes) del ruido
###############################################

# cargamos los paquetes que usaremos

packages <- c("corrplot","PerformanceAnalytics", "FactoMineR", "factoextra")
sapply(packages, require, character.only = TRUE)

if(!exists('d.e')){
  path <-'../data'
  d.e <- read.csv2(paste0(path,'/',"data_preproc.csv"), sep=",")
  d.e$adr<-as.numeric(d.e$adr)
  v<-list(
    categoric=c('hotel','is_canceled', 'arrival_date_month','arrival_date_year', 'meal','market_segment','distribution_channel',
                'is_repeated_guest','reserved_room_type','assigned_room_type', 'room_coherence', 
                'is_company', 'is_agent', 'customer_type','deposit_type', 'if_prev_cancel','if_wait'),
    integer=c('lead_time', 'stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
              'booking_changes','required_car_parking_spaces','total_of_special_requests'),
    continua='adr')
  v$numeric <- c(v$integer,v$continua)
  
  for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
  for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
}
dd<-d.e

## PCA ## # TO INTERPRET THE RESULTS <-- https://www.researchgate.net/post/How_many_components_can_I_retrieve_in_principal_component_analysis
dd.pca <- dd[,v$numeric]
# Si queremos suprimir el outlier --> #dd.pca <- dd.pca[-c(173),]

res.pca <- PCA(dd.pca, graph = FALSE)
n<-which(res.pca$eig[,3]>80)[1]
n # Con las primeras 7 componentes se captura un 80% de la varianza

res.pca <- PCA(dd.pca, graph = FALSE,ncp=n)
print(res.pca)

eigenvalues <- res.pca$eig
eigenvalues[, 1:2]

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues),
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")

# Add connected line segments to the plot
lines(x = seq(0.7,10*1.2,by=1.2), eigenvalues[, 2],
      type="b", pch=19, col = "red")

fviz_screeplot(res.pca, ncp=10)

# Coordinates of variables on the principal components
res.pca$var$coord

# The quality of representation of the variables of the principal components are called the cos2.
res.pca$var$cos2

# Variable contributions in the determination of a given principal component are (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
taula<- res.pca$var$contrib


## DIMENSION 1VS2
fviz_pca_var(res.pca, col.var="contrib")

# DIMENSION 1VS3
fviz_pca_var(res.pca, axes=c(1,3), col.var="contrib")

# Contribution of the variables to each dimension
for(k in 1:(n)){ 
  print(fviz_contrib(res.pca, choice = "var", axes = k))
}

# Graph of individuals
# Coordinates of individuals on the principal components
head(res.pca$ind$coord)

# Cos2 : quality of representation of individuals on the principal components
head(res.pca$ind$cos2)

# Contribition of individuals to the princial components
head(res.pca$ind$contrib)

# Graph of individuals
# Control automatically the color of individuals using the cos2 values (the quality of the individuals on the factor map)
fviz_pca_ind(res.pca,  col.ind="cos2") +
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=0.50)+
  theme_minimal()
#If we look at this graph we see that individual 173 is clearly an outlier


# Make a biplot of individuals and variables :
fviz_pca_biplot(res.pca,  geom = "text")

# # Individual grouped by categoric variables (grouped by meal in this example)
# fviz_pca_ind(res.pca,  label="none", habillage=as.factor(dd$meal))
# 
# fviz_pca_ind(res.pca, label="none", habillage=as.factor(dd$meal),
#              addEllipses=TRUE, ellipse.level=0.95)
# 
# 
# fviz_pca_biplot(res.pca, 
#                 habillage = as.factor(dd$meal), addEllipses = TRUE,
#                 col.var = "red", alpha.var ="cos2",
#                 label = "var") +
#   scale_color_brewer(palette="Dark2")+
#   theme_minimal()

