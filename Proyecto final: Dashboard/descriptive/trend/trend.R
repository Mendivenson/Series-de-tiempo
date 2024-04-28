library(shiny)
library(plotly)
library(TSstudio)
library(shinydashboard)
# library()

trendUI = function(id) {
  ns = NS(id)
  fluidPage(
    h1('Eliminación de la tendencia'),
    sidebarLayout(
      position = 'left',
      sidebarPanel(
        wisth = 2,
        # style = 'position: fixed; height: 100%; width: 25%; overflow-y: auto;',
        checkboxGroupInput(
          ns('metodos'),
          label = 'Seleccione a continuación algunos de los métodos que desea visualizar para eliminar la tendencia de la serie:',
          inline = F,
          width = NULL,
          choiceNames = c(
            'Regresión lineal',
            'Regresión lowess',
            'Suavizado kernel',
            'Suavizado splines',
            'Descomposición STL'
          ),
          choiceValues = c('lineal', 'lowess', 'kernel',
                           'splines', 'STL')
        ),
        selectInput(
          ns('metodo seleccionado'),
          label = 'Seleccione ahora el método con el que desea eliminar la tendencia:',
          choices = c(
            'Regresión lineal',
            'Regresión lowess',
            'Suavizado kernel',
            'Suavizado splines',
            'Descomposición STL',
            'Ninguno'
          ),
          multiple = F,
          selected = character(0)
        )
      ),
      mainPanel(
        # En este panel debe ir lo de regresión lineal
        conditionalPanel(
          ns = ns,
          condition = "input.metodos.indexOf('lineal') > -1",
          fluidPage(h2('Ajuste por regresión lineal'),
                    plotlyOutput(ns(
                      'RegresionLineal'
                    )))
        ),
        
        # En este panel debe ir lo de regresión lowess
        conditionalPanel(
          ns = ns,
          condition = "input.metodos.indexOf('lowess') > -1",
          fluidPage(
            h2('Regresión lowess'),
            plotlyOutput(ns('RegresionLowess')),
            fluidRow(
              column(numericInput(
                ns('LowessF'),
                label = 'Parámetro de suavizamiento (De 0 a 1)',
                value = 0.25,
                min = 0.00000001,
                max = 1
              ), width = 6),
              column(numericInput(
                ns('LowessIter'),
                label = 'Cantidad de iteraciones (De 1 a 100)',
                value = 3,
                min = 1,
                max = 1000
              ), width = 6))
          )
        ),
      
      # En este panel debe ir lo de suavizado kernel
      conditionalPanel(ns = ns,
                       condition = "input.metodos.indexOf('kernel') > -1",
                       fluidPage(
                         h2('Suavizado kernel'),
                         plotlyOutput(ns('RegresionKernel')), 
                         fluidRow(
                           column(width = 6,
                                  selectInput(ns('kernel'), label = 'Seleccione un kernel',
                                              choices = c('normal', 'box'), selected = 'normal')),
                           column(width = 6, 
                                  numericInput(ns('KernelBand'), label = 'Seleccione un ancho de banda (De 0 a 100):',
                                               min = 0.00000001, max = 100, value = 10))
                         ))),
      
      # En este panel debe ir lo de suavizado splines
      conditionalPanel(
        ns = ns,
        condition = "input.metodos.indexOf('splines') > -1",
        fluidPage(
          h2('Suavizamiento splines'),
          checkboxInput(ns('SplinesCv'), label = '¿Desea un ajuste automático?', value = TRUE),
          conditionalPanel(
            condition = 'input.SplinesCv == false',
            fluidRow(
              column(
                width = 6,
                numericInput(
                  ns('SplinesDf'), 
                  label =  '¿Cuántos grados de libertad desea usar?',
                  min = 1, max = 1000, value = 3
                )
              ),
              column(
                width = 6,
                numericInput(
                  ns('SplinesSpar'), 
                  label =  '¿Cuántos grados de libertad desea usar?', 
                  min = 0.000001, max = 2, value = NULL
                )
              )
            )
          )
        )
      ),
      
      
      # En este panel debe ir lo de descomposición STL -_- Pero este nos haría todo al tiempo
      # y el chiste es hacerlo a pedal ¿no?
      conditionalPanel(ns = ns,
                       condition = "input.metodos.indexOf('STL') > -1",
                       fluidPage(h2(
                         'Descomposición STL'
                       )))
      
    )
  )
  ) 
}



trendServer = function(input, output, session, serie) {
  output$RegresionLineal = renderPlotly({
    SerieNoVar = ts(serie())
    fitted = predict(lm(SerieNoVar ~ time(SerieNoVar)))
    tiempos = time(SerieNoVar)
    SerieNoVar = data.frame(SerieNoVar)
    
    p1 = plot_ly(
      SerieNoVar,
      type = 'scatter',
      mode = 'lines',
      color = '#1874CD'
    ) %>%
      add_trace(x = ~ tiempos,
                y = ~ SerieNoVar,
                name = 'Serie original', color = I('orange')) %>%
      add_trace(x = ~ tiempos,
                y = ~ fitted,
                name = 'Regresión lineal', color = I('darkgreen')) %>%
      layout(showlegend = F, title = '')
    
    residuos = SerieNoVar$SerieNoVar - fitted
    residuos = data.frame(residuos)
    p2 = plot_ly(
      residuos,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      add_trace(x = ~ tiempos,
                y = ~ residuos,
                name = 'Serie ajustada', color = I('orange')) %>%
      layout(showlegend = F, title = '')
    
    subplot(p1, p2, nrows = 2, shareX = T)
  })
  
  
  output$RegresionLowess = renderPlotly({
    req(input$LowessF); req(input$LowessIter)
    SerieNoVar = ts(serie())
    fitted = lowess(SerieNoVar, f = input$LowessF, iter = input$LowessIter)$y
    tiempos = time(SerieNoVar)
    SerieNoVar = data.frame(SerieNoVar)
    residuos = SerieNoVar$SerieNoVar - fitted
    residuos = data.frame(residuos)
    
    p1 = plot_ly(SerieNoVar, type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~ tiempos, y = ~ SerieNoVar, name = 'Serie original', color = I('orange')) %>%
      add_trace(x = ~ tiempos, y = ~ fitted, name = 'Regresión local o Lowess', color = I('darkgreen')) %>%
      layout(showlegend = F, title = '')
    
    
    p2 = plot_ly(
      residuos,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      add_trace(x = ~ tiempos,
                y = ~ residuos,
                name = 'Serie ajustada', color = I('orange')) %>%
      layout(showlegend = F, title = '')
    
    subplot(p1, p2, nrows = 2, shareX = T)
  })
  
  
  output$RegresionKernel = renderPlotly({
    req(input$kernel); req(input$KernelBand)
    SerieNoVar = ts(serie())
    fitted = ksmooth(x = time(SerieNoVar), y = SerieNoVar,
                     kernel = input$kernel, bandwidth = input$KernelBand)$y
    tiempos = time(SerieNoVar)
    SerieNoVar = data.frame(SerieNoVar)
    residuos = SerieNoVar$SerieNoVar - fitted
    residuos = data.frame(residuos)
    
    p1 = plot_ly(SerieNoVar, type = 'scatter', mode = 'lines') %>%
      add_trace(x = ~ tiempos, y = ~ SerieNoVar, name = 'Serie original', color = I('orange')) %>%
      add_trace(x = ~ tiempos, y = ~ fitted, name = 'Suavizamiento Kernel', color = I('darkgreen')) %>%
      layout(showlegend = F, title = '')
    
    
    p2 = plot_ly(
      residuos,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      add_trace(x = ~ tiempos,
                y = ~ residuos,
                name = 'Serie ajustada',color = I('orange')) %>%
      layout(showlegend = F, title = '')
    
    subplot(p1, p2, nrows = 2, shareX = T)
  })
  
  # output$RegresionSplines = renderPlotly({
  #   req(input$SplinesDf); req(input$SplinesSpar); req(input$SplinesCv)
  #   SerieNoVar = ts(serie())
  #   fitted = ksmooth(x = time(SerieNoVar), y = SerieNoVar,
  #                    kernel = input$kernel, bandwidth = input$KernelBand)$y
  #   tiempos = time(SerieNoVar)
  #   SerieNoVar = data.frame(SerieNoVar)
  #   residuos = SerieNoVar$SerieNoVar - fitted
  #   residuos = data.frame(residuos)
  #   
  #   p1 = plot_ly(SerieNoVar, type = 'scatter', mode = 'lines', color = '#1874CD') %>%
  #     add_trace(x = ~ tiempos, y = ~ SerieNoVar, name = 'Serie original') %>%
  #     add_trace(x = ~ tiempos, y = ~ fitted, name = 'Suavizamiento Kernel') %>%
  #     layout(showlegend = F, title = '')
  #   
  #   
  #   p2 = plot_ly(
  #     residuos,
  #     type = 'scatter',
  #     mode = 'lines',
  #     color = '#1874CD'
  #   ) %>%
  #     add_trace(x = ~ tiempos,
  #               y = ~ residuos,
  #               name = 'Serie ajustada') %>%
  #     layout(showlegend = F, title = '')
  #   
  #   subplot(p1, p2, nrows = 2, shareX = T)
  # })
  
}