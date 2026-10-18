source("R/00_config.R")
suppressPackageStartupMessages({library(DBI); library(RPostgres); library(sparklyr); library(dplyr)})
# Debe ejecutarse desde RStudio dentro de la red Docker.
# Confirmar spark://spark-master:7077 y versión 3.5.1 con el compose actual.
sc <- spark_connect(master="spark://spark-master:7077", version="3.5.1", config=list("spark.sql.shuffle.partitions"="4"))
con <- connect_pg()
datos <- DBI::dbReadTable(con, DBI::Id(schema="nova_analytics", table="modelo_estudiantes"))
DBI::dbDisconnect(con)
# SparklyR trabaja sobre datos sintéticos. Convertir factores/caracteres explícitamente.
datos <- datos %>% mutate(
  riesgo_desercion = as.numeric(riesgo_desercion),
  trabaja = as.numeric(trabaja),
  beca = as.numeric(beca),
  sexo = as.character(sexo),
  modalidad = as.character(modalidad),
  distrito = as.character(distrito)
)
sdf <- copy_to(sc, datos, name="nova_estudiantes", overwrite=TRUE)
# Selección de variables predictoras disponibles antes de cualquier uso real.

model_data <- sdf %>%
  select(
    riesgo_desercion, edad, trabaja, horas_estudio_semana,
    promedio_previo, asistencia_pct, cursos_desaprobados,
    beca, creditos_matriculados, deuda_pendiente
  ) %>%
  mutate(
    peso_clase = ifelse(riesgo_desercion == 1, 21.8, 1.0)
  )

splits <- sdf_random_split(model_data, training=0.8, testing=0.2, seed=2026)


modelo <- ml_logistic_regression(
  x = splits$training,
  response = "riesgo_desercion",
  features = c(
    "edad", "trabaja", "horas_estudio_semana",
    "promedio_previo", "asistencia_pct",
    "cursos_desaprobados", "beca",
    "creditos_matriculados", "deuda_pendiente"
  ),
  weight_col = "peso_clase",
  threshold = 0.5
)


pred <- ml_predict(modelo, splits$testing)
# Recuperar predicciones y probabilidades desde Spark
pred_resultados <- pred %>%
  select(riesgo_desercion, prediction, probability) %>%
  collect()

head(pred_resultados)

# Matriz de confusión y métricas calculadas en el conjunto de prueba.
evaluacion <- pred %>% select(riesgo_desercion, prediction) %>% collect()
evaluacion$y <- as.integer(evaluacion$riesgo_desercion)
evaluacion$pred <- as.integer(evaluacion$prediction)
cm <- table(Real=evaluacion$y, Predicho=evaluacion$pred)
accuracy <- mean(evaluacion$y == evaluacion$pred, na.rm=TRUE)
precision <- sum(evaluacion$y==1 & evaluacion$pred==1, na.rm=TRUE) /
  max(1, sum(evaluacion$pred==1, na.rm=TRUE))
recall <- sum(evaluacion$y==1 & evaluacion$pred==1, na.rm=TRUE) /
  max(1, sum(evaluacion$y==1, na.rm=TRUE))
cat("Matriz de confusión:\n"); print(cm)
cat(sprintf("Accuracy=%.3f | Precision=%.3f | Recall=%.3f\n", accuracy, precision, recall))
write.csv(as.data.frame(cm), "R/matriz_confusion.csv", row.names=FALSE)
write.csv(data.frame(metric=c("accuracy","precision","recall"), value=c(accuracy,precision,recall)),
          "R/metricas_modelo.csv", row.names=FALSE)
spark_disconnect(sc)

