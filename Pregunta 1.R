# Librerías utlizadas:

if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(ez)){
  install.packages("ez", dependencies = TRUE )
  require (ez)
}

if (!require(datasets)){
  install.packages("datasets", dependencies = TRUE )
  require (datasets)
}

if (!require(DescTools)){
  install.packages("DescTools", dependencies = TRUE )
  require (DescTools)
}

if (!require(nlme)){
  install.packages("nlme", dependencies = TRUE )
  require (nlme)
}

if (!require(emmeans)){
  install.packages("emmeans", dependencies = TRUE )
  require (emmeans)
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
datos_lavatroopers <- datos_lavatroopers %>% filter(division == 
                                                     "Lavatrooper") %>% 
  select(eval_instructor, eval_capitan, eval_comandante, eval_general)

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

# El procedimiento ANOVA no se puede decretar debido a la falta de valores,
# por lo tanto la prueba cambia a la prueba no paramétrica de Kruskal, donde
# sus condiciones fueron confirmadas anteriormente.

kruskal <- kruskal.test(puntaje ~ oficiales, data = datos_lavatroopers)
print(kruskal)

# Lo cual nos da un p valor de 2.2e-16, por lo tanto se rechaza la hipótesis
# nula a favor de la alternativa, lo que quiere decir que la calificación promedio de la evaluación realizada por cada uno de los
# oficiales a los Lavatroopers es diferente para al menos un oficial.

# A partir de estos resultados se realiza un análisis post.hoc
post_hoc <- pairwise.wilcox.test(datos_lavatroopers$puntaje, datos_lavatroopers$oficiales, p.adjust.method = "holm",
                                 paired = FALSE)

print(post_hoc)

# Donde se puede observar que la diferencia más significativa se encuentra
# en los pares con la evaluación del comandante, debido a los p valores obtenidos
# con la prueba de Holm.
