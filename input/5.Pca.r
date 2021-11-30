###############################################
# Título: 5.PCA
# Autor: Aleix
# Fecha: 30/11/21

# Descripción: PRINCIPAL COMPONENT ANALYSIS (PCA)
###############################################

# TO INTERPRET THE RESULTS <-- https://www.researchgate.net/post/How_many_components_can_I_retrieve_in_principal_component_analysis

# Instalamos i cargamos los paquetes que usaremos
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("corrplot","PerformanceAnalytics", "FactoMineR", "factoextra")
ipak(packages)

setwd("C:/Users/garys/Desktop/MINERIA DE DADES")
dd <- read.table("datapreprocessed.csv", header=T, sep=",", fileEncoding = 'UTF-8-BOM');

# Declaración de variables, no incluimos la variables respuesta 'is_canceled'
v<-list(
  categoric=c('hotel','arrival_date_year','arrival_date_month','meal',
              'market_segment','distribution_channel','is_repeated_guest','room_coherence',
              'if_wait','if_prev_cancel','is_agent','is_company','customer_type','deposit_type'),
  integer=c('lead_time', 'stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
            'booking_changes','required_car_parking_spaces','total_of_special_requests'))

for(i in v$categoric) dd[[i]]<-as.factor(dd[[i]])
for(i in v$integer) dd[[i]]<-as.integer(dd[[i]])

# Veiem els missings de cada variable
sapply(dd, function(x) sum(is.na(x))) # --> NO MISSINGS

dd.pca <- dd[,v$integer]

# Si queremos suprimir el outlier
# dd.pca <- dd.pca[-c(173),]

# PCA

res.pca <- PCA(dd.pca, graph = FALSE)
print(res.pca)

eigenvalues <- res.pca$eig
eigenvalues[, 1:2]

# Con las primeras 5 componentes se captura un 66% de la varianza

barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues),
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")

# Add connected line segments to the plot
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2],
      type="b", pch=19, col = "red")

fviz_screeplot(res.pca, ncp=10)

# coordinates of variables on the principal components
res.pca$var$coord

# The quality of representation of the variables of the principal components are called the cos2.
res.pca$var$cos2

# Variable contributions in the determination of a given principal component are (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
res.pca$var$contrib

## DIMENSION 1VS2
fviz_pca_var(res.pca, col.var="contrib")

# DIMENSION 1VS3
fviz_pca_var(res.pca, axes=c(1,3), col.var="contrib")

# DIMENSION 1VS4
fviz_pca_var(res.pca, axes=c(1,4), col.var="contrib")

# DIMENSION 1VS5
fviz_pca_var(res.pca, axes=c(1,5), col.var="contrib")

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Contributions of variables to PC3
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)

# Contributions of variables to PC4
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)

# Contributions of variables to PC5
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)

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
# Si nos fijamos en este gráfico vemos que el individuo 173 es claramente un outlier

# Make a biplot of individuals and variables :
fviz_pca_biplot(res.pca,  geom = "text")


