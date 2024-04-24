library(shiny)
library(shinydashboard)
library(markdown)
# library(shinydashboardPlus)
library(shinyjs)


# Vamos a crear nuestro primer módulo dentro de shiny. Lo que me conflictúa es
# que muy seguramente haya problemas con el sidebar. La aproximación correcta
# genera un menú automático o ir agregando los componentes manualmente.
# Define UI for application that draws a histogram


# **Nota:** Si una pestaña aparece como expandible aún cuando no debería probablemente
# tabname esté mal escrito XD


# ***************************** UI PRINCIPAL ***********************************

# =========================== MENÚS Y CUERPOS: =================================

# ---> PARA HOME <--- (Explicar la funcionalidad(?) No sé XD)
menuHome = menuItem('¿Cómo funciona esto?',
                    tabName = 'home',
                    icon = icon('lightbulb'))
bodyHome = tabItem(tabName = 'home', 
                   fluidPage(withMathJax(includeMarkdown('home.md'))))


# ---> PARA LECTURA DE DATOS <---

source('data/data.R')

menuData = menuItem('Datos',
                     tabName = 'data', 
                     icon = icon('database'))

bodyData = tabItem(tabName = 'data', 
                   # uiOutput('IntroData'),
                   dataUI('dataModule'))


# ---> ANÁLISIS DESCRIPTIVO <---

source('descriptive/variance/variance.R')

menuDescriptive = menuItem('Análisis descriptivo', tabName = 'descriptive',startExpanded = F,
                           icon = icon('chart-simple'),
                           menuSubItem('Análisis de varianza', 
                                       tabName = 'variance',
                                       icon = icon('crosshairs')),
                           menuSubItem('Tendencia', 
                                       tabName = 'trend',
                                       icon = icon('line-chart')),
                           menuSubItem('Estacionalidad', 
                                       tabName = 'season',
                                       icon = icon('calendar-alt')))

bodyDescriptive = list(tabItem(tabName = 'descriptive', 'Prueba'),
                       tabItem(tabName = 'variance', varianceUI('varianceModule')),
                       tabItem(tabName = 'trend', 'trend'),
                       tabItem(tabName = 'season', 'season'))


# ---> FILTRO EXPONENCIAL <---

menuExponential = menuItem('Filtro Exponencial',
                           tabName = 'exponential',
                           icon = icon('filter'))

bodyExponential = tabItem(tabName = 'exponential', 'exponential')


# ---> ÁRBOLES DE DECISIÓN <---

menuDecisionTree = menuItem('Árboles de decisión',
                            tabName = 'decisionTree',
                            icon = icon('diagram-project'))

bodyDecisionTree = tabItem('decisionTree', 'decisionTree')


# ---> Se debe incluir en este vector el cuerpo del módulo agregado para que  <---
# ---> aparezca en el cuerpo de la pestaña del dashboard                      <---
# DISCLAIMER: El orden es importante, básicamente es todo :v

# ¿Aquí se unen todas las renderizaciones del cuerpo en teoría? XD
bodyItems = tagList( 
  div(class = 'tab-content', 
      bodyHome, bodyData, bodyDescriptive,bodyExponential,bodyDecisionTree))
  
  
ui<- dashboardPage(
  header = dashboardHeader(title = 'Proyecto final. Series de tiempo univariadas',
                           tags$li(
                             class = "dropdown",
                             tags$a(
                               href = "https://github.com/Mendivenson/Series-de-tiempo/tree/main/Proyecto%20final%3A%20Dashboard",
                               target = "_blank",
                               tags$i(class = "fab fa-github", style = "font-size: 22px; line-height: 16px;")  # Adjust the font size as needed
                             )),
                           titleWidth = 350),
  sidebar = dashboardSidebar(width = 350, # minified = FALSE,
                             sidebarMenu(id = 'tabs',
                                         
                                    menuData,menuDescriptive,
                                    menuExponential,menuDecisionTree)),
  body = dashboardBody(bodyItems),
    useShinyjs()
)
# *************************** SERVER PRINCIPAL *********************************

server <- function(input, output) {
  # Código para no esconder los íconos
  runjs({'
        var el2 = document.querySelector(".skin-blue");
        el2.className = "skin-blue sidebar-mini";
        var clicker = document.querySelector(".sidebar-toggle");
        clicker.id = "switchState";
    '})
  
  onclick('switchState', runjs({'
        var title = document.querySelector(".logo")
        if (title.style.visibility == "hidden") {
          title.style.visibility = "visible";
        } else {
          title.style.visibility = "hidden";
        }
  '}))
  
  
  # ---> Módulos <---
  serie = callModule(dataServer, "dataModule")
  callModule(varianceServer, 'varianceModule', serie = serie)
}

# Función para correr la aplicación
shinyApp(ui = ui, server = server)
