library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)


counterButton <- function(id, label = "Counter") {
  ns <- NS(id)
  tagList(
    actionButton(ns("button"), label = label),
    verbatimTextOutput(ns("out"))
  )
}

counterServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      count <- reactiveVal(0)
      observeEvent(input$button, {
        count(count() + 1)
      })
      output$out <- renderText({
        count()
      })
      count
    }
  )
}

# ui <- fluidPage(
#   counterButton("counter1", "Counter #1")
# )


ui <- dashboardPage(skin = 'black',
        dashboardHeader(title = "Home"),
        dashboardSidebar(#width = 280,
        h2("Sidebar", style = "text-align:center"),
        sidebarMenu(
            menuItem("Counter", tabName = "Counter", icon = icon("tree")),
            menuItem("Cars", tabName = "Cars", icon = icon("car"))
        )
    ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "Counter",
                        counterButton("counter1", "Counter #1")
                )
            )
    )
)


server <- function(input, output, session) {
  counterServer("counter1")
}

shinyApp(ui, server)