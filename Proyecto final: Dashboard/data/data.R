# Este módulo se supone que lo que hará será:
#   - [x] Leer datos
#   - [ ] Filtrar datos
#   - [ ] Seleccionar una columna de los datos para 
#     hacer de insumo en la serie de tiempo
#   - [ ] Enseñarme a renderizar archivos md dentro
#     de mi módulo para presentarlo en la UI

library(shiny)
library(shinydashboard)
# library(bslib)
library(lubridate)
library(DT)


dataUI <- function(id) {
  ns <- NS(id)  # Genera un espacio de nombres único

  fluidPage(
    fluidRow(column(
      width = 12,
      includeMarkdown('data/dataRead.md')
    )),
    sidebarLayout(  # Layout con panel lateral y panel principal
      sidebarPanel(  # Panel lateral
        fileInput(ns("fileInput"), label = "Cargar archivo CSV", accept = ".csv")  # Cargar archivo CSV
      ),
      mainPanel(  # Panel principal
        # uiOutput('Intro'),
        DTOutput(ns("dataTable"))
        # Renderizar tabla con los datos del archivo CSV
      )
    ),
    fluidRow(column(
      width = 12,
      includeMarkdown('data/dataVisulization.md')
    )),
    
    sidebarLayout(
      sidebarPanel(
        selectInput(ns('TimeSerie'), label = 'Seleccione una columna', choices = NULL, selected = character(0)),
        numericInput(ns('Inicio'), label = 'Año de inicio de la serie', value = NULL,
                     min = 0, max = year(Sys.Date())),
        numericInput(ns('Recurrencia'), label = '¿Cuántas veces por año se midió la serie?', value = NULL,
                     min = 0),
        checkboxInput(ns('LineasV'),'¿Agregar líneas verticales en cada año?', value = FALSE)),
    mainPanel(plotOutput(ns('linePlot')))
    ),
    
    fluidRow(column(
      width = 12,
      includeMarkdown('data/dataNotes.md')
    ))
  )
}


# dataUI = function(id){
#   ns = NS(id)
#   fluidPage(
#     titlePanel('Lectura de datos')
#   )
# }


dataServer <- function(input, output, session) {
  
  data = reactive({
    req(input$fileInput)
    read.csv(input$fileInput$datapath)
  })
  
  observe({
    updateSelectInput(session, "TimeSerie", choices = colnames(data()), selected = character(0))
  })
  
  serie = reactive({
    req(input$TimeSerie)
    req(input$Inicio)
    req(input$Recurrencia)
    ts(data()[, input$TimeSerie],
       start = input$Inicio, 
       frequency = input$Recurrencia)
  })
  
  
    
  output$dataTable = renderDT({
    datatable(data())
  })
  
  output$linePlot <- renderPlot({
    req(input$TimeSerie)
    req(input$Inicio)
    req(input$Recurrencia)
    # req(input$LineasV)
    # tiempos = time(serie())
    # tiempos = tiempos[tiempos %% 1 == 0]
    plot.ts(serie(),
         main= input$fileInput$name,
         xlab = "Index",
         ylab = 'Value')
    if (input$LineasV){
      abline(v = time(serie())[time(serie()) %% 1 == 0],
             lty = 'dashed', col = '#FFA07A')
    }
    
  })
}
