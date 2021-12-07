###############################################
# Título: 1.Missings
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 24/11/21
# Descripción: Script donde se detectan los missings

# realmente este script no sirve de nada ya que 
# hemos visto que los missings que se detectan 
# no son realmente missings
###############################################

# Leer base de datos
if(!exists('path')) path<-strsplit(shell('dir TMD.Rmd',intern = T)[4],' ')[[1]][4] ; setwd(path)
if(!exists('e')) source('input/1.0.BD_read.r')
# sacar informacions desde environment
d.e<-e$d

# 2. Descripción de datos faltantes

# Missings (finalmente vimos en la metadata que no habían missingsj, de 
# hecho eran missings no aplicables, a diferencia de lo que habíamos dicho en las entregas D1 y D2).

d.e[d.e=='NULL']<-NA
mis<-sapply(d.e,function(x) sum(is.na(x)))
v.mis<-which(mis>0)
mis<-mis[mis>0]
n$missing<-sum(mis)
mis<-list(count=list(number=n$missing,forvar=mis),
          relative=list(forall=n$missing/n$total,formissing=mis/n$missing,
                        forvar=mis/n$observation))
mis$relative$formissing<-data.frame(prop=mis$relative$formissing, 
                                    variable=names(mis$relative$formissing))
# Guardar los resultados en un evironment nueva
e1.mis<-new.env()
e1.mis$d<-d.e
