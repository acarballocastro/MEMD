
install.packages("arules")
install.packages("arulesViz")
library(arulesViz)
library(arules)

# se hacen las transformaciones necesarias para poder utilizar la función del paquete arules
dd <- d.e
dcat <- dd[, as.vector(sapply(dd, is.factor))]
trans <- as(dcat, "transactions")

# Se buscan las normas que tengan como resultado una cancelación de la reserva.
# Se filtan las que tengan longitud minima de 10, con un soporte mínimo de 0.2 y una confidence 
# mínima de 0.4
# al usar restricciones mas laxas, aparecen miles de reglas, y si se usan restricciones más 
# fuertes, no aparece ninguna regla.

rules <- apriori(dcat, parameter = list(support = 0.2, confidence = 0.4,  minlen = 10, target= "rules"), appearance = list(default="lhs", rhs="is_canceled=canceled"))

# La función da como resultado 8 reglas
inspect(rules)

# Esta gráfica muestra la norma con mayor confidence.
subrules <- head(rules, n = 1, by = "confidence")
plot(subrules, method = "graph",  engine = "htmlwidget")

# Esta gráfica muestra las 8 reglas obtenidas
plot(rules, method = "graph",  engine = "htmlwidget")
