###############################################
# Título: 3.Preprocessing: Missing
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script hacemos el tractamiento de
# missings de la base de datos 
###############################################

if(!exists('e1.dec')) source('Input/1.2.Declaracion.r')
d.e<-e1.dec$d
v<-e1.dec$v

# Tractamiento de Company y agent
d.e$is_company<-factor(ifelse(is.na(d.e$company),0,1))
d.e$is_agent<-factor(ifelse(is.na(d.e$agent),0,1))
v.elimina<-which(names(d.e) %in% c('company','agent'))
d.e<-d.e[,-v.elimina]

# renovamos los indices
v<-cat.idx(d.e,names(d.e),time=v$times)

e3.mis<-new.env()
e3.mis$d<-d.e
e3.mis$v<-v
