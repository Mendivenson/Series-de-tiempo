library(shiny)
library(plotly)
library(TSstudio)
library(shinydashboard)

trendUI = function(id){
  ns = NS(id)
  fluidPage(
    h1('Eliminación de la tendencia'), 
    sidebarLayout(
      position = 'left',
      sidebarPanel(style = 'position: fixed; height: 100%; width: 25%; overflow-y: auto;',
        checkboxGroupInput(
          ns('metodos'),
          label = 'Seleccione a continuación algunos de los métodos que desea visualizar para eliminar la tendencia de la serie:',
          inline = F,
          width = NULL,
          choiceNames = c('Regresión lineal',
                          'Regresión lowess',
                          'Suavizado kernel',
                          'Suavizado splines',
                          'Descomposición STL'),
          choiceValues = c('lineal', 'lowess', 'kernel', 
                           'splines', 'STL')),
        selectInput(ns('metodo seleccionado'),
                    label = 'Seleccione ahora el método con el que desea eliminar la tendencia:', 
                    choices = c('Regresión lineal',
                                'Regresión lowess',
                                'Suavizado kernel',
                                'Suavizado splines',
                                'Descomposición STL'),
                    multiple = F)
        ),
      mainPanel(
        # En este panel debe ir lo de regresión lineal
        conditionalPanel(
          ns = ns,
          condition = "input.metodos.indexOf('lineal') > -1",
          fluidPage(
            h3('Ajuste por regresión lineal'),
            plotlyOutput(ns('RegresionLineal')))),
        
        # En este panel debe ir lo de regresión lowess
        conditionalPanel(
          ns = ns,
          condition = "input.metodos.indexOf('lowess') > -1",
          fluidPage(h3('Regresión lowess'))),
        
        # En este panel debe ir lo de suavizado kernel
        conditionalPanel(
          ns = ns,
          condition = "input.metodos.indexOf('kernel') > -1",
          fluidPage(h3('Suavizado kernel'))),
        
        # En este panel debe ir lo de suavizado splines
        conditionalPanel(
          ns = ns,
          condition = "input.metodos.indexOf('splines') > -1",
          fluidPage(h3('Regresión splines'))),
        
        # En este panel debe ir lo de descomposición STL -_- Pero este nos haría todo al tiempo
        # y el chiste es hacerlo a pedal ¿no?
        conditionalPanel(
          ns = ns,
          condition = "input.metodos.indexOf('STL') > -1",
          fluidPage(h3('Descomposición STL'))),
        
      )
    )
  )
}



trendServer = function(input, output, session, serie){

  output$RegresionLineal = renderPlotly({
    SerieNoVar = ts(serie())
    fitted = predict(lm(SerieNoVar ~ time(SerieNoVar)))
    tiempos = time(SerieNoVar)
    SerieNoVar = data.frame(SerieNoVar)
    
    p1 = plot_ly(SerieNoVar,type = 'scatter', mode = 'lines', color = '#1874CD') %>% 
      add_trace(x = ~tiempos, y = ~SerieNoVar, name = 'Serie 1') %>%
      add_trace(x = ~tiempos, y = ~fitted, name = 'Regresión lineall') %>% 
      layout(showlegend = F, title = '')
    
    residuos = SerieNoVar$SerieNoVar - fitted
    residuos = data.frame(residuos)
    p2 = plot_ly(residuos, type = 'scatter', mode = 'lines', color = '#1874CD') %>% 
      add_trace(x = ~tiempos, y = ~residuos, name = 'Serie ajustada') %>% 
      layout(showlegend = F,title = '')
      
    subplot(p1,p2, nrows = 2, shareX = T)
  })

}