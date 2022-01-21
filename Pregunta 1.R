# Librerías utlizadas:

if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}


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
# de una vía para muestras correlacionadas con un nivel de confianza de 95% para
# responder a la pregunta anterior, y posteriormente realizar un análisis
# post-hoc de los resultados.

# Para esto primero se cargan los datos del archivo csv:
datos_lavatroopers <- read.csv2(file.choose(), header=TRUE)

# Se filtran los datos seleccionando a los reclutas "Lavatroopers" y las
# evaluaciones de sus oficiales:
datos_lavatroopers <- datos_lavatroopers %>%filter(division == 
                                                     "Lavatrooper") %>% 
  select(eval_instructor, eval_capitan, eval_comandante, eval_general)

datos_lavatroopers <- as.data.frame(apply(datos_lavatroopers, 3, as.integer))
# La pregunta detrás de esta prueba es ¿se diferencias las medias poblacionales?
# lo cual nos permite plantear las siguientes hipótesis de acuerdo a lo
# solicitado:

# H0: la calificación promedio de la evaluación realizada por cada uno de los
# oficiales a los Lavatroopers es la misma.
# H1: la calificación promedio de la evaluación realizada por cada uno de los
# oficiales a los Lavatroopers es diferente para al menos un oficial.

# 1. La escala con que se mide la variable dependiente tiene las propiedades de
# una escala de intervalos iguales.
# Esto se cumple ya que los puntajes asignados a la calificación operan bajo
# esta condición.

# 2. Las mediciones son independientes al interior de cada grupo. 
# Se cumple ya que cada observación representa un oficial diferente, por lo
# tanto son independientes.

# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n)
# una distribución normal.

# Para verificar esto se utilizará la prueba de Shapiro de Normalidad, además se
# manipularan los datos para utilizar verificar la cuarta condición y utilizar
# el procedimiento ANOVA posteriormente:

#El dataframe es convertido a formato largo
datos_lavatroopers <- datos_lavatroopers %>% pivot_longer(c("eval_instructor",
                                                            "eval_capitan",
                                                            "eval_comandante",
                                                            "eval_general"),
                                  names_to = "oficiales", values_to = "puntaje")


# Se crea la variable categórica "instancia"
instancias_lavatroopers <- factor(1:100)
datos_lavatroopers <- data.frame(datos_lavatroopers, instancias_lavatroopers)

# Se convierte la variable "oficiales" a tipo categórica
datos_lavatroopers[["oficiales"]] <- factor(datos_lavatroopers[["oficiales"]])

# Se realiza la prueba de normalidad de Shapiro
prueba_shapiro <- shapiro.test(datos_lavatroopers$puntaje)
print(prueba_shapiro) 

# Como el p valor = 0.328 > 0.05 el nivel de significancia escogido, se puede
# concluir que los datos siguen una distribución normal, y para comprobar
# esta condición se realizan gráficos QQ:

gqq_lavatroopers <- ggqqplot(datos_lavatroopers, x = "puntaje", y = "oficiales",
                      color = "oficiales")
gqq_lavatroopers <- gqq_lavatroopers + facet_wrap(~ oficiales) + 
  rremove("x.ticks") + rremove("x.text") + 
  rremove("y.ticks") + rremove("y.text") + 
  rremove("axis.title")
print(gqq_lavatroopers)

# Donde no se observan valores atípicos en las observaciones, por lo tanto, 
# se puede suponer razonablemente que las distribuciones se asemejan a la normal.

# 4. La matriz de varianzas-covarianzas es esférica, es decir, las varianzas
# entre los diferentes niveles de las medidas repetidas deben ser iguales.
# Esta condición será comprobada al realizar el procedimiento ANOVA con
# ezANOVA().

#Procedimiento ANOVA
prueba_lavatroopers <- ezANOVA( data = datos_lavatroopers , dv = puntaje ,
                                within = oficiales ,
                                wid = instancias_lavatroopers,
                                return_aov = TRUE )
print(prueba_lavatroopers)