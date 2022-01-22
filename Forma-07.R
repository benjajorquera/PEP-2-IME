# PEP 2
# Inf. y Modelos Estadísticos
# Benjamin Jorquera (19.182.719-8)
# Bryan Salas (19.316.410-2)


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

# Librerías Pregunta 2
if (!require(readxl) ) {
  install.packages("readxl", dependencies = TRUE )
  require (readxl)
}
if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}
if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}
if (!require(car)){
  install.packages("car", dependencies = TRUE )
  require (car)
}
if (!require(corrplot)){
  install.packages("corrplot", dependencies = TRUE )
  require (corrplot)
}
if (!require(caret)){
  install.packages("caret", dependencies = TRUE )
  require (caret)
}
if (!require(pROC)){
  install.packages("pROC", dependencies = TRUE )
  require (pROC)
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




# Pregunta 2
# (24 puntos) A fin de determinar si es necesario establecer programas de entrenamiento diferenciados para clones y
# reclutas, Lord Vader quiere saber si es posible distinguir entre ambas clases de soldados con los datos actuales. Para ello,
# ha solicitado evaluar un modelo clasificador que contemple entre 2 y 5 variables predictoras. Considere que, para ser
# aceptable, el modelo:
#   • Debe lograr una exactitud (accuracy) de al menos 0,8 en datos de prueba
# • No puede considerar casos con demasiada influencia (considerando la distancia de Cook)
# • No debe presentar autocorrelación (usando la prueba de Durbin-Watson para un retardo y un nivel de significación
#                                      α = .01)
# • No debe presentar multicolinealidad severa (considerando el factor de inflación de la varianza, con un VIF promedio
#                                               inferior a 1,03).
# Considere la semilla 2876 para obtener una muestra de 400 datos, 80% de los cuales serán empleados para ajustar el
# modelo y el 20% restante, para evaluarlo.



# semilla
set.seed(2876)

# Se cargan de datos archivo csv.
datos <- read.csv2(file.choose(), header=TRUE)

# Variables escogidas para ser posibles predictoras
variables <- c("estatura", "peso", "eval_instructor", "eval_general", "velocidad", "fuerza", "resistencia", "agilidad", "presicion")

#Se seleccionan al azar 8 varaibles predictoras
predictoras <- sample(variables, size = 8)
print(predictoras)

# Se convierten los datos es_clon a 1 si pertenece a la clase y 0 en caso contrario.
datos <- mutate(datos, es_clon = ifelse(es_clon == "S", 1, 0))
datos$es_clon <- factor(datos$es_clon)

# Se obtiene una muestra de 400 datos
datos_2 <- datos %>% sample_n(size=400, replace=FALSE)

# Separar conjuntos de entrenamiento y prueba.
n <- nrow(datos_2)
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos_2[muestra, ]
prueba <- datos_2[-muestra, ]


# Ajustar modelo nulo.
nulo <- glm(es_clon ~ 1, family = binomial(link = "logit"), data = entrenamiento)

# Ajustar modelo completo.
cat("\n\n")

f <- as.formula(paste("es_clon", paste(predictoras, collapse = "+"), sep = " ~ "))
print(f)

completo <- glm(f, family = binomial(link = "logit"), data = entrenamiento)


# Ajustar modelo con regresión escalonada.
cat("Modelo con regresión escalonada \n")
cat(" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
mejor <- step(nulo, scope = list(lower = nulo, upper = completo), direction = "both", trace = 0)
print(summary(mejor))


# Evaluar el modelo con el conjunto de prueba .
umbral <- 0.5

cat("Evaluación del modelo a partir del conjunto de prueba :\n")
probs_p <- predict(mejor, prueba, type = "response")

preds_p <- sapply(probs_p, function(p) ifelse(p >= umbral, "1", "0"))
preds_p <- factor(preds_p, levels = levels(prueba[["es_clon"]]))

ROC_p <- roc(prueba[["es_clon"]] , probs_p)
plot(ROC_p)

matriz_p <- confusionMatrix(preds_p, as.factor(prueba[["es_clon"]]))
print(matriz_p)


# Verificación de multicolinealidad .
cat("Verificación de colinealidad \n")
cat(" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
cat("\n VIF :\n")
vifs <- vif(mejor)
print(vifs)
cat("\n Promedio VIF: ")
print(mean(vifs))

# velocidad resistencia        peso      fuerza 
# 7.694569    7.851373    4.510058    3.331894

# Promedio VIF: > print(mean(vifs))
# 5.84697

# Podemos observar que ninguna de las variables presenta un VIF superior a 10
# pero el promedio es bastante superior a 1 por lo que el modelo presenta sesgo
# Y no es generalizable, por lo que no se recomienda usar el modelo.


# Independencia de los residuos .
cat("Verificación de independencia de los residuos \n")
cat(" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(mejor, max.lag = 5))

# lag Autocorrelation D-W Statistic p-value
# 1      0.04750226      1.904939   0.336
# 2     -0.03670683      2.073283   0.218
# 3      0.05230136      1.878862   0.262
# 4      0.08721694      1.800892   0.128
# 5     -0.02082599      2.016978   0.564

# Todas los residuos son independientes, ya que sus p-values son mucho mayor a 
# alfa = 0.01


# Obtener los residuos y las estadísticas .
output <- data.frame(predicted.probabilities = fitted (mejor))
output[["standardized.residuals"]] <- rstandard(mejor)
output[["studentized.residuals"]] <- rstudent(mejor)
output[["cooks.distance"]] <- cooks.distance(mejor)
output[["dfbeta"]] <- dfbeta(mejor)
output[["dffit"]] <- dffits(mejor)
output[["leverage"]] <- hatvalues(mejor)


# Evaluar residuos estandarizados que escapen a la normalidad.
# 95 % de los residuos estandarizados deber ían estar entre
# -1.96 y 1.96, y 99 % entre -2.58 y 2.58.
sospechosos1 <- which(abs(output[["standardized.residuals"]]) > 1.96)
sospechosos1 <- sort(sospechosos1)
cat("\n\n")
cat(" Residuos estandarizados fuera del 95 % esperado \n")
cat(" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos1, ]) )


# Revisar casos con distancia de Cook mayor a uno.
sospechosos2 <- which(output[["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat("\n\n")
cat("Residuales con una distancia de Cook alta \n")
cat(" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos2, ]))

# No hay caos con distancia Cook mayor a 1.







# Pregunta 3
# (9 puntos) Proponga un ejemplo novedoso (no mencionado en clase ni que aparezca en las lecturas dadas) en donde un
# estudio o experimento, relacionado con el sentir de los estudiantes de la Universidad de Santiago respecto al retorno a
# la presencialidad, necesite utilizar una prueba de Kruskal-Wallis debido a problemas con la escala de la variable
# dependiente en estudio. Indiqué cuáles serían las variables involucradas en su ejemplo (con sus respectivos niveles) y las
# hipótesis nula y alternativa a contrastar.

# Que tanto miedo tienes de volver a clase de manera presencial dado los nuevos casos de coronavirus omicron
# Casos: Mucho, Nada, Indiferente


# H0: La media no difiere en ningun caso.
# H1: La media difiere en almenos un caso.
