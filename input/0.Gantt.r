###############################################
# Título: 0.Gantt
# Autor: Todos
# Fecha: 24/11/21

# Descripción: Script donde creamos el diagrama de Gantt 
# que representala división temporal del trabajo 
###############################################

# LIBRERÍAS
library(DiagrammeR)

###############################################

m <- mermaid("
gantt
dateFormat  DD-MM
title Diagrama de Gantt

section D1
Portada                  :done,      D1_1,   21-09,  28-09
Definir el proyecto      :done,      D1_2,   21-09,  28-09
Entrega D1               :crit,      D1,     21-09,  28-09

section D2
Descomposición de tareas  :done,    D2_1,    24-09,     05-10
Reparto de tareas   :done,    D2_3,    24-09,     05-10
Plan de riesgos           :done,    D2_4,    24-09,     05-10
Entrega D2                :crit,    D2,      after D1,  05-10

section D3
Depuración y descriptiva de datos :active,  D3_1,   after D1,     14-11
Motivación del trabajo            :         D3_11,  after D1,     06-10
Depuración de datos               :         D3_12,  after D1,     07-10
Análisis univariante inicial      :         D3_13,  after D3_12,  7d
Preprocessing                     :         D3_14,  after D3_12,  11-11
Análisis de datos procesados      :         D3_15,  after D3_14,  3d

Proceso de minería de datos       :active   D3_2,   after D3_14,  16-12
Diseño del proceso                :         D3_21,  after D3_14,  7d
Seguimiento del proceso           :         D3_22,  after D3_21,  01-12
Análisis comparativo              :         D3_3,   after D3_22,  7d
Conclusión                        :         F1,     after D3_3,   14-12
Plan de trabajo                   :         F2,     after D3_3,   14-12
Script R                          :active,  F3,     24-09,        16-12
Entrega D3                        :crit,    D3,     after D2,     16-12

Section Final
Preparación presentación  :       P1, 14-12,      16-12
Presentación oral         :crit,  PF, 15-12,      16-12
", width=1268 , height=628)

m$x$config = list(ganttConfig = list(
  axisFormatter = list(list(
    "%b %d, %Y" 
    ,htmlwidgets::JS(
      'function(d){ return d.getDay() == 1 }' 
    )
  ))
))
m
