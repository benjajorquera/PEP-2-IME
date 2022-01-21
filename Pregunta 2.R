# Librerías
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

#Se hace una copia de los datos y se elimina la variable es_clon y la variable division
aux <- datos
aux$es_clon <- NULL
aux$division <- NULL

#Se obtienen los nombres de las varaibles
variables <- colnames(aux)

#Se seleccionan al azar 8 varaibles predictoras
predictoras <- sample(variables, size = 8)
print(predictoras)

# Se convierten los datos es_clon a 1 si pertenece a la clase y 0 en caso contrario.
datos <- mutate(datos, es_clon = ifelse(es_clon == "S", 1, 0))
datos$es_clon <- factor(datos$es_clon)

# Se obtiene una muestra de 400 datos
datos_2 <- datos %>% sample_n(size=400, replace=FALSE)
head(muestra)


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


# Independencia de los residuos .
cat("Verificación de independencia de los residuos \n")
cat(" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
print(durbinWatsonTest(mejor, max.lag = 5))


# Detectar posibles valores atípicos .
cat("Identificación de posibles valores atípicos \n")
cat("- - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - -\n")
plot(mejor)


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
sospechosos2 <- which(output [["cooks.distance"]] > 1)
sospechosos2 <- sort(sospechosos2)
cat("\n\n")
cat("Residuales con una distancia de Cook alta \n")
cat(" - - - - - - - - - - -- - - - - - - - - - - - - - - - - - - - -- - - - - - - - -\n")
print(rownames(entrenamiento[sospechosos2, ]) )























