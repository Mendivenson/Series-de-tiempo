#   - [ ] Filtrar datos

library(shiny)
library(shinydashboard)
library(lubridate)
library(markdown)
library(DT)
library(plotly)
library(TSstudio)


# **Nota importante:** Notése que siempre debemos llamar a los objetos de server
# dentro de un módulo con ns(). Pues como se llamará en una aplicación más grande
# ns() es la forma de indicar que se llama de dentro de este módulo.

# ======================== UI de lectura de datos ==============================
dataUI <- function(id) {
  ns <-
    NS(id)                            # NS: id único en la app principal
  
  # INTRODUCCIÓN DE LECTURA DE DATOS
  
  fluidPage(
    # shiny::fluidPage página adaptable al tamaño
    fluidRow(column(
      # shiny::fluidRow una sección adaptable al ancho
      width = 12,                         # width es una medida relativa (12 = 100%)
      includeMarkdown('data/dataRead.md') # htmltools:: Función para renderizar documentos externos en la sección
    )),
    
    # SELECCIÓN Y VISUALIZACIÓN DEL DATAFRAME
    # sidebarLayout(                        # shiny::sidebarLayout Crea un layour de dos columnas
    #   sidebarPanel(                       # LLenando una de las columnas (Menos ancha)
    #     fileInput(ns("fileInput"),        # Esta es una opción propia de shiny. Qué suerte XD
    #               label = "Cargar archivo CSV",
    #               accept = ".csv")),
    #   mainPanel(                          # Llenando el panel principal
    #     DTOutput(ns("dataTable"))         # Se dibujará una tabla solamente
    #   )
    # ),
    fluidRow(column(
      fileInput(ns('fileInput'),
                label = 'Cargar archivo csv',
                accept = '.csv'),
      width = 12
    )),
    fluidRow(DTOutput(ns('dataTable'))),
    # INTRODUCCIÓN VISUALIZACIÓN Y LECTURA COMO OBJETO TS
    
    fluidRow(column(
      width = 12,
      includeMarkdown('data/dataVisualization.md')
    )),
    
    # LECTURA COMO OBJETO TS Y VISUALIZACIÓN:
    sidebarLayout(sidebarPanel(
      fluidPage(
        fluidRow(
          selectInput(
            ns('TimeSerie'),
            # Un selector de columnas del dataframe. ¡Se actualiza en server!
            label = 'Seleccione una columna',
            choices = NULL,
            selected = character(0)
          ),
          numericInput(
            ns('Inicio'),
            # Un selector numérico para el momento en que inicia la serie
            label = 'Año de inicio de la serie',
            value = 2008,
            min = 0,
            max = year(Sys.Date())
          ),
          numericInput(
            ns('Recurrencia'),
            # Un selector numérico para pasar más adelante a frequency en ts
            label = '¿Cuántas veces por año se midió la serie?',
            value = 12,
            min = 1
          ),
          fluidRow(uiOutput(ns('serieInfo')))# Mostrar infomración de cuántas observaciones fueron leídas
        )
      )),
      mainPanel(plotlyOutput(ns('linePlot')))
    ),
    
    
    fluidRow(column(
      width = 12,                          # withMathJax renderiza ecuaciones de LaTeX
      withMathJax(includeMarkdown('data/dataNotes.md'))
    )))
}



# ====================== server de lectura de datos ============================

dataServer <- function(input, output, session) {
  # Leer los datos con la ruta dada en fileInput
  data = reactive({
    req(input$fileInput)
    read.csv(input$fileInput$datapath)
  })
  
  # Actualizar el menú de selección de las columnas
  observe({
    updateSelectInput(session,
                      "TimeSerie",
                      choices = colnames(data()),
                      selected = character(0))
  })
  
  # Generar la serie cuando se seleccionen la la columna, el inicio y la frecuencia de la serie
  serie = reactive({
    req(input$TimeSerie)
    req(input$Inicio)
    req(input$Recurrencia)
    ts(data()[, input$TimeSerie],
       start = input$Inicio,
       frequency = input$Recurrencia)
  })
  
  # Caja de información con cuántas observaciones tiene la serie
  output$serieInfo <- renderUI(valueBox(
    length(serie()),
    subtitle = 'Observaciones',
    icon = icon('hashtag'),
    width = 12
  ))
  
  # Renderizar el dataframe leído
  output$dataTable = renderDT({
    datatable(data())
  })
  
  # Renderizar el gráfico de líneas
  output$linePlot <- renderPlotly({
    req(input$TimeSerie)
    req(input$Inicio)
    req(input$Recurrencia)
    
    ts_plot(
      serie(),
      title = input$fileInput$name,
      Xtitle = "Index",
      Ytitle = 'Value',
      slider = T, Xgrid = T, Ygrid = T
    )
    
  })
  # Es importante este return. Llamaremos ese objeto para los otros módulos.
  return(serie)
  
}
