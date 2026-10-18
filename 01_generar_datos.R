source("R/00_config.R")
suppressPackageStartupMessages({library(DBI); library(RPostgres)})
set.seed(20261017)
n <- 15000
estudiantes <- data.frame(
  estudiante_id = seq_len(n),
  edad = sample(17:55, n, replace=TRUE),
  sexo = sample(c("F","M","No especifica"), n, replace=TRUE, prob=c(.49,.49,.02)),
  distrito = sample(c("Lima Centro","Lima Norte","Lima Sur","Callao","Otra región"), n, replace=TRUE),
  modalidad = sample(c("Presencial","Virtual","Semipresencial"), n, replace=TRUE, prob=c(.55,.25,.20)),
  trabaja = sample(c(TRUE,FALSE), n, replace=TRUE, prob=c(.38,.62)),
  horas_estudio_semana = round(pmax(0, rnorm(n, 12, 5)),1),
  promedio_previo = round(pmin(20,pmax(0,rnorm(n,13.5,2.8))),2),
  asistencia_pct = round(pmin(100,pmax(35,rnorm(n,86,12))),1),
  cursos_desaprobados = pmin(8, rpois(n, .7)),
  beca = sample(c(TRUE,FALSE), n, replace=TRUE, prob=c(.18,.82))
)
# Etiqueta sintética para demostración, no es diagnóstico real.
logit <- -2.7 + .045*(70-estudiantes$asistencia_pct) +
  .25*estudiantes$cursos_desaprobados - .16*(estudiantes$promedio_previo-12) +
  .35*as.integer(estudiantes$trabaja)
p <- 1/(1+exp(-logit))
estudiantes$riesgo_desercion <- rbinom(n, 1, p)
matriculas <- data.frame(
  matricula_id = seq_len(n),
  estudiante_id = seq_len(n),
  periodo = "2026-II",
  creditos_matriculados = sample(c(12,15,18,20,22), n, replace=TRUE),
  deuda_pendiente = round(ifelse(runif(n)<.22, runif(n,100,2800),0),2),
  fecha_matricula = as.Date("2026-08-01") + sample(0:60,n,replace=TRUE)
)

con <- connect_pg()

tryCatch({
  DBI::dbExecute(
    con,
    "CREATE SCHEMA IF NOT EXISTS nova_raw"
  )

  DBI::dbWriteTable(
    con,
    DBI::Id(schema = "nova_raw", table = "estudiantes"),
    estudiantes,
    overwrite = TRUE
  )

  DBI::dbWriteTable(
    con,
    DBI::Id(schema = "nova_raw", table = "matriculas"),
    matriculas,
    overwrite = TRUE
  )

  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS idx_matriculas_estudiante
     ON nova_raw.matriculas(estudiante_id)"
  )

  cat(
    "Carga finalizada. Estudiantes:", nrow(estudiantes),
    "Matrículas:", nrow(matriculas), "\n"
  )

  cat(
    "Prevalencia sintética de riesgo:",
    round(mean(estudiantes$riesgo_desercion) * 100, 2),
    "%\n"
  )

}, finally = {
  if (DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }
})




cat("Carga finalizada. Estudiantes:", nrow(estudiantes), "Matrículas:", nrow(matriculas), "\n")
cat("Prevalencia sintética de riesgo:", round(mean(estudiantes$riesgo_desercion)*100,2), "%\n")
