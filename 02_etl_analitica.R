
source("R/00_config.R")

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
})

con <- connect_pg()

tryCatch({

  DBI::dbExecute(con, "
    CREATE SCHEMA IF NOT EXISTS nova_analytics
  ")

  DBI::dbExecute(con, "
    DROP TABLE IF EXISTS nova_analytics.modelo_estudiantes
  ")

  sql <- "
  CREATE TABLE nova_analytics.modelo_estudiantes AS
  SELECT
    e.estudiante_id,
    GREATEST(17, LEAST(100, e.edad)) AS edad,
    COALESCE(NULLIF(TRIM(e.sexo), ''), 'No especifica') AS sexo,
    COALESCE(NULLIF(TRIM(e.distrito), ''), 'No especificado') AS distrito,
    COALESCE(NULLIF(TRIM(e.modalidad), ''), 'No especificada') AS modalidad,
    COALESCE(e.trabaja, FALSE) AS trabaja,
    GREATEST(0, COALESCE(e.horas_estudio_semana, 0))
      AS horas_estudio_semana,
    LEAST(20, GREATEST(0, e.promedio_previo))
      AS promedio_previo,
    LEAST(100, GREATEST(0, e.asistencia_pct))
      AS asistencia_pct,
    GREATEST(0, e.cursos_desaprobados)
      AS cursos_desaprobados,
    COALESCE(e.beca, FALSE) AS beca,
    e.riesgo_desercion,
    m.periodo,
    m.creditos_matriculados,
    GREATEST(0, COALESCE(m.deuda_pendiente, 0))
      AS deuda_pendiente,
    m.fecha_matricula,
    CURRENT_TIMESTAMP AS fecha_proceso
  FROM nova_raw.estudiantes e
  LEFT JOIN nova_raw.matriculas m USING (estudiante_id)
  "

  DBI::dbExecute(con, sql)

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_modelo_estudiante
    ON nova_analytics.modelo_estudiantes(estudiante_id)
  ")

  DBI::dbExecute(con, "
    CREATE INDEX IF NOT EXISTS idx_modelo_riesgo
    ON nova_analytics.modelo_estudiantes(riesgo_desercion)
  ")

  resultado <- DBI::dbGetQuery(con, "
    SELECT
      COUNT(*) AS registros,
      COUNT(DISTINCT estudiante_id) AS estudiantes_unicos
    FROM nova_analytics.modelo_estudiantes
  ")

  cat("ETL completado correctamente.\n")
  print(resultado)

}, error = function(e) {

  message("Error en el ETL: ", conditionMessage(e))

}, finally = {

  if (DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con)
  }

})
