---
title: "Trabajo Mineria de dada"
author: "Toto El Grupo"
date: "24/9/2021"
output: pdf_document
params:
  path: 'lo que sea'
  data.origin: 'hotel_bookings.csv'
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE , comment = '', warning = F)
```

```{r Data Read}
data<-read.csv(paste0(params$path,'/',params$data.origin))
```

```{r Data Description}
# n.observacions y n.variables
dim(data)
#nombre de cada variable
names(data)
```

```{r Data Selection}
# Seleccionamos solo los de España
d.e<-data[data$country=='ESP',names(data)!='country']

# Descrpcion por la nova bases de dadas
n<-list(total=prod(dim(d.e)))
n$observation<-nrow(d.e)
n$variable<-ncol(d.e)
n

# missings
d.e[d.e=='NULL']<-NA
mis<-sapply(d.e,function(x) sum(is.na(x)))
v.mis<-which(mis>0)
mis<-mis[mis>0]
n$missing<-sum(mis)
mis<-list(count=list(number=n$missing,forvar=mis),
          relative=list(forall=n$missing/n$total,formissing=mis/n$missing,
                        forvar=mis/n$observation))
mis
barplot(mis$count$forvar,col = c(2,3))
```

```{r Data Decralation}
# definir tipus de variables
v<-list(
  categoric=c(1,2,4,5,13:16,19,20,22:24,26,30),
  integer=c(3,6:12,17,18,21,25,28,29),
  continua=27,
  times=31
        )
v$numeric<-c(v$integer,v$continua,v$times)
v$withmissing<-v.mis

# decraracion de variables
for(i in v$categoric) d.e[[i]]<-as.factor(d.e[[i]])
for(i in v$integer) d.e[[i]]<-as.integer(d.e[[i]])
for(i in v$times) d.e[[i]]<-as.Date(d.e[[i]])

# Descripcion general
summary(d.e[,v$categoric])
summary(d.e[,v$numeric])
```
