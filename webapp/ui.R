SIDEWIDTH = 270

header <- dashboardHeader(
      title = "D-E-R analysis", titleWidth = SIDEWIDTH, disable = FALSE,
      dropdownMenu(type = "messages",
        messageItem(
          from = "Sales Dept",
          message = "Sales are steady this month."
        ),
        messageItem(
          from = "New User",
          message = "How do I register?",
          icon = icon("question"),
          time = "13:45"
        ),
        messageItem(
          from = "Support",
          message = "The new server is ready.",
          icon = icon("life-ring"),
          time = "2014-12-01"
        )
      ),
      dropdownMenu(type = "notifications",
        notificationItem(
          text = "5 new users today",
          icon("users")
        ),
        notificationItem(
          text = "12 items delivered",
          icon("truck"),
          status = "success"
        ),
        notificationItem(
          text = "Server load at 86%",
          icon = icon("exclamation-triangle"),
          status = "warning"
        )
      ),
      dropdownMenu(type = "tasks", badgeStatus = "success",
        taskItem(value = 90, color = "green",
          "Documentation"
        ),
        taskItem(value = 17, color = "aqua",
          "Project X"
        ),
        taskItem(value = 75, color = "yellow",
          "Server deployment"
        ),
        taskItem(value = 80, color = "red",
          "Overall project"
        )
      )
)

siderbar <- dashboardSidebar(
    width = SIDEWIDTH, disable = FALSE,
    sidebarMenu(
      menuItem("Simulate Data", tabName = "simulate", icon = icon("dashboard")),
      # actionButton("generate", "Generate", block = TRUE, icon = icon("play")),
      div(style = "margin: 10px 5px; width: 500px",  # Adjust the top and bottom margin as needed
        actionButton("generate", "Generate", icon = icon("play"), style = 'width: 25%;')
      ),
      menuItem("Upload Data", tabName = "realData", icon = icon("th")),
      p("Load data of type:", style = "margin: 10px 5px 0px;font-weight: bold;"),
      div(
        id = "sidebar_selection_container",
        selectInput("data_type", label = NULL, 
          choices = list("rds | rda | rdata" = "rdss", "csv" = "csv", "txt" = "txt"), 
          selected = "rds | rda | rdata")
      ),
      div(
        id = "upload_file_container",
        fileInput(inputId = "file1", label = NULL,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
          )
        ),
      ),
      p("Response Type:", style = "margin: 10px 5px 0px;font-weight: bold;"),
      div(
        id = "sidebar_selection_container",
        radioGroupButtons(
          inputId = "responseType",
          label = NULL, 
            choices = c("Continuous", "Binary"),
            # checkIcon = list(yes = icon("square-check"), no = icon("square")),
          status = "info", justified = TRUE, direction = "horizontal",
          width = '250px'
        ),
      ),
      p("Model:", style = "margin: 10px 5px 0px;font-weight: bold;"),
      div(
        id = "sidebar_selection_container",
        selectInput("model", label = NULL, # choices = c()
          choices = list("Sigmoid-Emax" = "sEmax", "Emax" = "emax", "Exponential" = "expo", 
                          "Beta" = "beta", "Linear" = "linear", "Linear-Log"="linearLog",
                          "Logistic" = "logistic", "Quadratic" = "quad"), 
          selected = "Sigmoid-Emax"
          )
      )
      # menuItem("Source code", icon = icon("file-code-o"), 
      #     href = "https://github.com/rstudio/shinydashboard/")
    )
  )

body <- dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = "simulate",
            div(class = "my-class", h1("Hello world"), p("This is a paragraph, which can be used for instructions.")),
            # fluidRow(
            #   tabBox(
            #     title = "Add Title Here", width = 12, height = 800,
            #     tabPanel(
            #       "Data", "Add Data content here.",
            #     ),
            #     tabPanel("Model", "Add Model content here."),
            #     tabPanel("Results", "Add Results content here.")
            #   )
            # ),
            fluidRow(
              box(
                title = "Source Data", status = "primary", 
                solidHeader = TRUE, width = 4, # height = 600,
                # div(id = "exposure_table_container", withSpinner(DTOutput("exposure_table")))
                withSpinner(DTOutput("exposure_table"))
                ),
              box(
                title = "Exposure-Response Quantile Plot", status = "warning", 
                solidHeader = TRUE, width = 8, # height = 500,
                "Add Box content here.", br(), "More content here.",
                # column(6, div(id = "plot1_container", withSpinner(plotOutput("plot1"))))
                withSpinner(plotOutput("plot1"))
              ),
              box(
                title = "Exposure-Response Logistic Regression Plot", status = "warning", 
                solidHeader = TRUE, width = 8,
                # div(id = "plot2_container", withSpinner(plotOutput("plot2")))
                withSpinner(plotOutput("plot2")),
                "Add Box content here.", br(), "More content here."
              ),
            ),
      ),
      tabItem(tabName = "realData",
            fluidRow(
                tabBox(
                  title = "Add Title Here", width = 12, height = 800,
                  tabPanel(
                    "Description",
                    h2("Data preview"),
                    fluidRow(
                      box(
                        width = 4,
                        tableOutput("data_preview")
                        ),
                      box(
                        width = 8,
                        verbatimTextOutput("data_description")
                      )
                    )
                  ),
                  tabPanel(
                    "View", "Add Data content here.",
                    box(
                      title = "Source Data", # status = "primary", 
                      solidHeader = TRUE, width = 6, # height = 600,
                      # div(id = "exposure_table_container", withSpinner(DTOutput("exposure_table")))
                      withSpinner(DTOutput("exposure_table_real"))
                    )
                  ),
                  tabPanel(
                    "Visualization", "Add Model content here.",
                      box(
                        title = "Exposure-Response Quantile Plot", # status = "warning", 
                        solidHeader = TRUE, width = 10, # height = 500,
                        "Add Box content here.", br(), "More content here.",
                        # column(6, div(id = "plot1_container", withSpinner(plotOutput("plot1"))))
                        withSpinner(plotOutput("plot1_real"))
                      ),
                    ),
                  tabPanel(
                    "Results", "Add Results content here.",
                      box(
                        title = "Exposure-Response Logistic Regression Plot", # status = "warning", 
                        solidHeader = TRUE, width = 10,
                        # div(id = "plot2_container", withSpinner(plotOutput("plot2")))
                        withSpinner(plotOutput("plot2_real")),
                        "Add Box content here.", br(), "More content here."
                      ),
                    )
                ),
              # column(6, div(id = "exposure_table_container", withSpinner(DTOutput("exposure_table_real")))),
              # column(6, div(id = "plot1_container", withSpinner(plotOutput("plot1_real")))),
              # column(10, div(id = "plot2_container", withSpinner(plotOutput("plot2_real"))))
            )
      )
    )
  )


ui <- dashboardPage(header, siderbar, body)