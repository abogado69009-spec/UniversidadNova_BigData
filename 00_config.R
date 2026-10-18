# Configuración para el docker-compose de sensors_big_data-main.
# No guardar contraseñas en Git. En este entorno didáctico se admiten
# valores predeterminados del compose local; cámbialos si tu configuración cambia.
get_pg_config <- function() {
  list(
    host = Sys.getenv("PGHOST", "postgres"),
    port = as.integer(Sys.getenv("PGPORT", "5432")),
    dbname = Sys.getenv("PGDATABASE", "postgres"),
    user = Sys.getenv("PGUSER", "postgres"),
    password = Sys.getenv("PGPASSWORD", "postgres")
  )
}
connect_pg <- function() {
  cfg <- get_pg_config()
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = cfg$host, port = cfg$port, dbname = cfg$dbname,
    user = cfg$user, password = cfg$password
  )
}
