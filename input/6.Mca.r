###############################################
# Título: 6.MCA
# Autor: Aleix
# Fecha: 31/11/21

# Descripción: MULTIPLE CORRESPONDENCE ANALYSIS (MCA)
###############################################

# Instalamos i cargamos los paquetes que usaremos
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("FactoMineR", "Matrix")
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
library(Matrix)


