library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "My Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home"), class = "sidebar-icon"),
      menuItem("Data", tabName = "data", icon = icon("database"), class = "sidebar-icon"),
      menuItem("Descriptive", tabName = "descriptive", icon = icon("chart-bar"), class = "sidebar-icon"),
      menuItem("Exponential", tabName = "exponential", icon = icon("calculator"), class = "sidebar-icon"),
      menuItem("Decision Tree", tabName = "decision_tree", icon = icon("tree"), class = "sidebar-icon")
    ),
    tags$script(
      HTML('
        $(document).ready(function() {
          $("#sidebarToggle").on("click", function() {
            $(".sidebar-icon").toggle();
          });
        });
      ')
    )
  ),
  dashboardBody(
    # Dashboard body content
  )
)

server <- function(input, output, session) {
  # Server logic
}

shinyApp(ui, server)
