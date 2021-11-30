###############################################
# Título: 0.BD_read
# Autor: Todos
# Fecha: 24/11/21

# Descripción: script que lee el base de datos
###############################################

if(!exists('filename') | !exists('path')){
  path<-strsplit(shell('dir TMD.Rmd',intern = T)[4],' ')[[1]][4]
  filename<- 'hotel_bookings.csv'
  setwd(path)
}

# Leer base de dada
data<-read.csv(paste0('Data/',filename))
# solo seleccionando clientes de España
d.e<-data[data$country=='ESP',names(data)!='country']
name<-names(d.e)
n<-list(total=prod(dim(d.e)),observation=nrow(d.e),variable=ncol(d.e))

library(ggplot2)
source('Function/Class_idx.r')

e<-new.env()
e$d<-d.e
