###############################################
# Título: 3.Clustering
# Autor: Aleix
# Fecha: 13/12/21
# Descripción: Clustering: Es la tarea de dividir los
# datos en varios grupos de modo que los datos de los mismos
# grupos sean más similares a otros datos del mismo grupo que los de otros grupos.
# La idea es crear grupos con rasgos similares basandonos en una noción
# de similitud que definiremos. Se harán dos tipos de clustering:
#     - k-means (partitional clustering:  The algorithms require the analyst to specify the number of clusters to be generated.)
#     - hierarchical clustering
###############################################
# cargamos los paquetes que usaremos

packages <- c("tidyverse", "factoextra","NbClust","tidyr","cluster","lattice", "FactoMineR")
sapply(packages, require, character.only = TRUE)
getwd()
if(!exists('d.e')){
  path <-'../data'
  d.e <- read.csv2(paste0(path,'/',"hotel_bookings_proc.csv"), sep=",")
  d.e$adr<-as.numeric(d.e$adr)
  v<-list(
    categoric=c('hotel','is_canceled', 'arrival_date_month','arrival_date_year', 'meal','market_segment','distribution_channel',
                'is_repeated_guest','reserved_room_type','assigned_room_type', 'room_coherence', 
                'is_company', 'is_agent', 'customer_type','deposit_type', 'if_prev_cancel','if_wait'),
    integer=c('lead_time', 'stays_in_weekend_nights','stays_in_week_nights','adults','children','babies',
              'booking_changes','required_car_parking_spaces','total_of_special_requests'),
    continua='adr')
  v$numeric <- c(v$integer,v$continua)
  library(ggplot2)
  for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
  for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
}
dd<-d.e

# 3.1 k-means clustering
dd.num <- dd[,v$numeric]
# Elimino el individuo 173 ya que es un outlier y al plotear los clusters en
# el plano de los dos primeros componentes principales del PCA queda muy separado
# y altera visualmente el resultado de los clusters
dd.num <- dd.num[-c(173),]

# Normalize data
dd.num <- scale(dd.num)
head(dd.num)

# Distance matrix
# We can also use these methods: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
m.distancia <- get_dist(dd.num, method = "euclidean") 

# Estimate the number of clusters: Elbow & Silhouette methods
fviz_nbclust(dd.num, kmeans, method = "wss")
fviz_nbclust(dd.num, kmeans, method = "silhouette")

# We calculate the clusters with k=6
# nstart: if centers is a number, how many random sets should be chosen?
k6 <- kmeans(dd.num, centers = 6, nstart = 25) 
# Here we get all the information of the cluster, size, cluster of each individual
# cluster mean for each variable, cluster vector, within cluster sum of squares,
# between_SS / total_SS
k6

# Cluster plot
fviz_cluster(k6, data = dd.num)
fviz_cluster(k6, data = dd.num, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) 
fviz_cluster(k6, data = dd.num, ellipse.type = "norm")
fviz_cluster(k6, data = dd.num, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

# res2 <- hcut(dd.num, k = 6, stand = TRUE)
# Dendograma(demasiado pesado para ejecutar)
# fviz_dend(res2, rect = TRUE, cex = 0.5, show_labels = FALSE, k_colors = c("red","#2E9Fdd"))

# Mean of each cluster for every variable
dd.num <- dd[,v$numeric]
dd.num <- dd.num[-c(173),]
dd.num %>%
  mutate(Cluster = k6$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Pass the clusters to my dd.num dataframe to work with them

dd.num <- scale(dd.num)
dd.num<- as.data.frame(dd.num)
dd.num$clus<-as.factor(k6$cluster)
dd.num

dd.num$clus<-factor(dd.num$clus)
data_long <- gather(dd.num, caracteristica, value, v$numeric, factor_key=TRUE)
data_long

ggplot(data_long, aes(caracteristica, y = value,group = clus, colour = clus)) +
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  stat_summary(geom="line")
geom_point(aes(shape=clus))

# 3.2 Hierarchical clustering (manteniendo el outlier)

#dd[,v$categoric] <- lapply(dd[,v$categoric],as.factor);

sapply(dd,class)
actives<-c(1:length(dd))

# Clustering
# Dissimilarity Matrix (Gower as we have mixed data)
dissimMatrix <- daisy(dd[,actives], metric = "gower", stand=TRUE)
distMatrix <- dissimMatrix^2

# Clustering Ward Method, k=5
h1 <- hclust(distMatrix,method="ward.D2")
class(h1)
plot(h1, labels = F)
rect.hclust(h1, k = 5, border = rainbow(8)) 

# Cutting previous cluster
c2 <- cutree(h1,5)
dd$cluster<- as.factor(c2)

# Class sizes 
table(c2)
dd.pca <- dd[,v$numeric]
res.pca <- PCA(dd.pca, graph = FALSE)

# GROUP
fviz_pca_ind(res.pca,  label="none", habillage=as.factor(dd$cluster))

fviz_pca_ind(res.pca, label="none", habillage=as.factor(dd$cluster),
             addEllipses=TRUE, ellipse.level=0.95)
