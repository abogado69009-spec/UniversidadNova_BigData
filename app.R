suppressPackageStartupMessages({library(shiny); library(DBI); library(RPostgres); library(ggplot2); library(DT)})
source(file.path(dirname(getwd()), "R", "00_config.R"))
ui <- fluidPage(
  titlePanel("Universidad Nova | Gobierno y analítica de datos"),
  sidebarLayout(
    sidebarPanel(
      helpText("Tablero académico con datos sintéticos para demostración."),
      selectInput("modalidad", "Modalidad", choices=c("Todas","Presencial","Virtual","Semipresencial")),
      actionButton("actualizar", "Actualizar indicadores")
    ),
    mainPanel(
      fluidRow(column(4, wellPanel(h4("Estudiantes"), textOutput("n"))),
               column(4, wellPanel(h4("Riesgo sintético"), textOutput("riesgo"))),
               column(4, wellPanel(h4("Asistencia promedio"), textOutput("asistencia")))),
      plotOutput("grafico_riesgo", height="300px"),
      plotOutput("grafico_asistencia", height="300px"),
      h4("Resumen por modalidad"), DTOutput("tabla")
    )
  )
)
server <- function(input, output, session) {
  datos <- eventReactive(input$actualizar, {
    con <- connect_pg()
    on.exit(DBI::dbDisconnect(con))
    DBI::dbGetQuery(con, "SELECT estudiante_id, modalidad, asistencia_pct, promedio_previo, riesgo_desercion FROM nova_analytics.modelo_estudiantes")
  }, ignoreNULL=FALSE)
  filtrados <- reactive({
    d <- datos()
    if (input$modalidad != "Todas") d <- d[d$modalidad == input$modalidad, , drop=FALSE]
    d
  })
  output$n <- renderText(format(nrow(filtrados()), big.mark=","))
  output$riesgo <- renderText({
    d <- filtrados()
    if (!nrow(d)) "Sin datos" else paste0(round(mean(d$riesgo_desercion, na.rm=TRUE)*100,1), "%")
  })
  output$asistencia <- renderText({
    d <- filtrados()
    if (!nrow(d)) "Sin datos" else paste0(round(mean(d$asistencia_pct, na.rm=TRUE),1), "%")
  })
  output$grafico_riesgo <- renderPlot({
    d <- filtrados()
    validate(need(nrow(d)>0, "No hay registros para este filtro"))
    ggplot(d, aes(x=modalidad, fill=factor(riesgo_desercion))) +
      geom_bar(position="fill") + labs(x="Modalidad", y="Proporción",
      fill="Riesgo sintético", title="Distribución de la etiqueta sintética por modalidad") +
      theme_minimal()
  })
  output$grafico_asistencia <- renderPlot({
    d <- filtrados()
    validate(need(nrow(d)>0, "No hay registros para este filtro"))
    ggplot(d, aes(x=promedio_previo, y=asistencia_pct, color=modalidad)) +
      geom_point(alpha=.35) + labs(x="Promedio previo", y="Asistencia (%)",
      title="Asistencia y promedio previo") + theme_minimal()
  })
  output$tabla <- renderDT({
    d <- filtrados()
    datatable(aggregate(cbind(asistencia_pct, promedio_previo, riesgo_desercion) ~ modalidad,
      data=d, FUN=function(x) round(mean(x, na.rm=TRUE),2)),
      options=list(pageLength=5), rownames=FALSE)
  })
}
shinyApp(ui, server)
