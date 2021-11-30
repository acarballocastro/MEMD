###############################################
# Título: 1.Missing
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script describimos 
# los datos faltantes del BD
###############################################

if(!exists('e1.mis')) source('Input/1.Missings.r')

# Taula
print(mis)

# Grafica
print(ggplot(data=mis$relative$formissing, aes(x=variable, y=prop)) +
  geom_bar(stat="identity", fill = "steelblue") +
  geom_text(aes(label=round(prop*100,2)), vjust=1.6, color="black", size=3.5) +
  theme_minimal() +
  labs(x = "Variable", y = "Proportion of missings", title = "Missings"))
