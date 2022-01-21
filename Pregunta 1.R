# Librerías utlizadas:


# Pregunta 1
# (23 puntos) Lord Vader desea saber si los niveles de exigencia con que los
# distintos oficiales evaluadores (instructor, capitán, comandante y general) 
# califican a los lavatroopers son similares, por lo que le ha solicitado 
# estudiar si existe diferencias significativas en el promedio de la evaluación 
# realizada por cada uno de los oficiales. El Lord Sith ha sido muy claro al 
# solicitar un reporte de aquellos oficiales cuyas evaluaciones presenten 
# diferencias.

# Solución:

# Se pide estudiar si existen diferencias significativas en el promedio de la 
# evaluación realizada por cada uno de los oficiales evaluadores a los
# Lavatroopers.

# Como se trata de una muestra con más de dos medias y cada evaluación es
# realizada sobre un mismo Lavatrooper, se utilizará la prueba omni-bus ANOVA
# de una vía para muestras correlacionadas, para responder a la pregunta
# anterior, y posteriormente realizar un análisis post-hoc de los resultados.

# Para esto primero se cargan los datos del archivo csv:
datos <- read.csv2(file.choose(), header=TRUE)

# Se filtran los datos seleccionando a los reclutas "Lavatroopers" y las
# evaluaciones de sus oficiales:
datos <- datos %>% filter(division == "Lavatrooper") %>% select(eval_instructor, 
                                                                eval_capitan, 
                                                                eval_comandante, 
                                                                eval_general)


