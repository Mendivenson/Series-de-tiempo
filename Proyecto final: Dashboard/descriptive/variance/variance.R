library(easypackages)
libraries(c("shiny", "shinydashboard", "markdown", 
            "MASS", 'TSstudio', 'plotly'))


# ====================== UI de análisis de la varianza =========================

varianceUI <- function(id) {
  ns <- NS(id)
  pdf(NULL)
  fluidPage(
    includeMarkdown('descriptive/variance/varianceIntro.md'),
    h2('Box-Cox para la serie original'),
    fluidRow(
      column(numericInput(
        ns('Min'),label = HTML('Seleccione el menor valor posible para \\(\\lambda\\)'),min = -20,value = -3),
      width = 4),
      column(numericInput(
        ns('Max'),max = 20,value = 3,
        label = HTML('Seleccione el mayor valor posible para \\(\\lambda\\)')),
      width = 4),
      column(
        numericInput(
          ns('Iter'),min = 20,max = 1000,value = 200,
          label = '¿Cuántos valores de \\(\\lambda\\) desea probar?',),
        width = 4) ),
    sidebarLayout(
      mainPanel(plotOutput(ns('boxcox'))),
    sidebarPanel(
      fluidPage(
        fluidRow(
          uiOutput(ns('lambda1')) ),
        fluidRow(
          checkboxInput(ns('transformar'),
                        label = '¿Desea transformar la serie?',
                        value = FALSE) ),
        fluidRow(uiOutput(ns('lambda2')))
      )
    )),
    conditionalPanel(
      condition = 'input.transformar',
      # h2('Comparación serie original vs. serie transformada'),
      ns = ns,
      fluidPage(
        h2('Comparación serie original vs serie transformada'),
        fluidRow(
          column(width = 6,
                 plotlyOutput(ns('Comparativa1'))),
          
          column(width = 6,
                 plotlyOutput(ns('Comparativa2')))
        ),
        h2(HTML('Cálculo \\(\\lambda\\) para la serie transformada')),
        plotOutput(ns('boxcox2'))
      )
    ), 
    includeMarkdown('descriptive/variance/varianceNotes.md')
  )
  
}


# ==================== server de análisis de la varianza =======================

varianceServer <- function(input, output, session, serie) {
  # BOXCOX con datos originales
  BoxCox <- reactive({
    req(input$Max)
    req(input$Min)
    req(input$Iter)
    boxcox(
      serie() ~ 1,
      plotit = FALSE,
      lambda = seq(input$Min, input$Max, length = input$Iter)
    )
  })
  
  # LAMBDA para datos originales
  lambda = reactive({
    BoxCox()$x[which(BoxCox()$y == max(BoxCox()$y))]
  })
  
  # Caja de información LAMBDA para datos originales
  output$lambda1 <- renderUI({
    tags$style(
      HTML(
        "
        .custom-icon {font-size: 24px;display: inline-block;width: 30px;height: 30px;
        line-height: 30px;text-align: center;border-radius: 50%;background-color: #337ab7;
        color: white;}
        "
      )
    )
    icon_html <- tags$i(class = 'custom-icon', HTML("&lambda;"))
    valueBox(
      subtitle = 'Para la serie original',
      value = sprintf("%.2f", lambda()),
      icon = icon_html,
      width = 12
    )
  })
  
  
  # Renderizando la gráfica
  output$boxcox <- renderPlot({
    req(input$Max)
    req(input$Min)
    req(input$Iter)
    boxcox(serie() ~ 1,
           lambda = seq(input$Min, input$Max, length = input$Iter))
  })
  
  
  SerieNoVar = reactive({
    if (input$transformar) {
      if (lambda() == 0) {
        log(serie())
      }
      else {
        ((serie() ^ lambda()) - 1) / lambda()
      }
    }
  })
  
  BoxCox2 = reactive({
    if (input$transformar) {
      boxcox(SerieNoVar() ~ 1,seq(0,3, length = input$Iter))
    }
  })
  
  lambda2 = reactive({
    if (input$transformar) {
      BoxCox2()$x[which(BoxCox2()$y == max(BoxCox2()$y))]
    }
  })
  
  output$lambda2 <- renderUI({
    if (input$transformar) {
      tags$style(
        HTML(
          "
      .custom-icon {font-size: 24px;display: inline-block;width: 30px;height: 30px;
        line-height: 30px;text-align: center;border-radius: 50%;background-color: #337ab7;
        color: white;}
         "
        )
      )
      icon_html <- tags$i(class = 'custom-icon', HTML("&lambda;"))
      valueBox(
        subtitle = 'Para la serie transformada',
        value = sprintf("%.2f", lambda2()),
        icon = icon_html,
        width = 12
      )
    }
  })
  
  
   output$boxcox2 <- renderPlot({
     boxcox(SerieNoVar() ~ 1, seq(-1,3, length = input$Iter))
   })

   output$Comparativa1 = renderPlotly({
     if (input$transformar) {
       # par(mfrow = c(1, 2))
       ts_plot(serie(), title = 'Serie original', Xtitle = '', slider = T,
               Xgrid = T,Ygrid = T)
     }
   })
   
   output$Comparativa2 = renderPlotly({
     if (input$transformar){
       ts_plot(SerieNoVar(), title = 'Serie transformada', Xtitle = '', slider = T,
               Xgrid = T,Ygrid = T)
     }
   })
  
  Final = reactive({if (input$transformar){
    SerieNoVar()
  } else {serie}})
  
  return( list( serie = reactive({
    if (input$transformar){
      SerieNoVar()
    } else {
      serie()
    }
  }), check = reactive({input$transformar})))
}
