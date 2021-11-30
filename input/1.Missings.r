###############################################
# Título: 1.Missings
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 24/11/21

# Descripción: Script donde se detectan los missings

# realmente este script no sirve de nada ya que 
# hemos visto que los missings que se detectan 
# no son realmente missings
###############################################

# LIBRERÍAS
library("ggplot2")

###############################################


# Leer base de datos
path <-'C:/Users/Irene/Downloads'
data.origin <- 'hotel_bookings.csv'
data <- read.csv(paste0(path,'/',data.origin))


# MISSINGS

# 1. Estructura de la base de datos

# n.observacions y n.variables
dim(data)

#nombre de cada variable
names(data)

# Filtro de la base de datos seleccionando solo clientes de España
d.e <- data[data$country=='ESP',names(data)!='country']
name <- c("Tipus Hotel", "Is Cancelled?", "Lead Time", "Arrival Date Year", "Arrival Date Month", "Arrival Date: Week Number", "Arrival Date: Day of Month", "Stays in Weekend Nights", "Stays in Week Nights", "Adults", "Children", "Babies", "Meal", "Market Segment", "Distribution Channel", "Is Repeated Guest?", "Previous Cancellations", "Previous Bookings not Cancelled", "Reserved Room Type", "Assigned Room Type", "Booking Changes", "Deposit Type", "Agent", "Company", "Days in Waiting List", "Customer Type", "ADR", "Required Car Parking Spaces", "Total of Special Requests", "Reservation Status", "Reservation Status Date")

# Descrpción de la nueva base de datos
dim(d.e)
names(d.e)

n<-list(total=prod(dim(d.e)))
n$observation<-nrow(d.e)
n$variable<-ncol(d.e)
n


# 2. Descripción de datos faltantes

# Missings (finalmente vimos en la metadata que no habían missings 
# a diferencia de lo que habíamos dicho en las entregas D1 y D2).

d.e[d.e=='NULL']<-NA
mis<-sapply(d.e,function(x) sum(is.na(x)))
hist(mis)

v.mis<-which(mis>0)
mis<-mis[mis>0]
n$missing<-sum(mis)
mis<-list(count=list(number=n$missing,forvar=mis),
          relative=list(forall=n$missing/n$total,formissing=mis/n$missing,
                        forvar=mis/n$observation))
mis

missing = data.frame(as.data.frame(mis$relative$formissing), names(mis$relative$formissing))
colnames(missing) = c("prop", "variable")

p<-ggplot(data=missing, aes(x=variable, y=prop)) +
  geom_bar(stat="identity", fill = "steelblue") +
  geom_text(aes(label=round(prop*100,2)), vjust=1.6, color="black", size=3.5) +
  theme_minimal() +
  labs(x = "Variable", y = "Proportion of missings", title = "Missings")
p

barplot(mis$relative$formissing,col = c(2,3),ylim=c(0,1),main = 'Proporción de missing')
text(c(0.7,2),mis$relative$formissing+0.05,
     paste0(round(mis$relative$formissing,2)*100,'%'),col='blue')