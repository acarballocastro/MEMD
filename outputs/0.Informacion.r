###############################################
# Título: 1.Informacion bases de datos
# Autor: Alba, Aleix, Pol, Yuhang, Irene
# Fecha: 29/11/21

# Descripción: En este script describimos
# informacion general del base de datos
###############################################

if(!exists('n')) source('Input/1.BD_read.r')

# n.observacions y n.variables
cat('BD Dimencion:',dim(data))
#nombre de cada variable
cat('\n\nNombres de variables\n')
print(names(data))

# Descrpcion por la nova bases de dadas
cat('\nBD con solo espa?a\n')
print(n)
