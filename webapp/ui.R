SIDEWIDTH = 270

header <- dashboardHeader(
  title = "DER analysis", titleWidth = SIDEWIDTH, disable = FALSE,
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
    # menuItem("Simulate Data", tabName = "simulate", icon = icon("dashboard")),
    # div(style = "margin: 10px 5px; width: 500px",  # Adjust the top and bottom margin as needed
    #   actionButton("generate", "Generate", icon = icon("play"), style = 'width: 25%;')
    # ),
    menuItem(HTML("<strong>&nbsp;&nbsp;Dose Finding</strong>"), tabName = "doseFinding", icon = icon("th")),
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
          # checkIcon = list(yes = icon("ok"), no = icon("square")),
          checkIcon = list(yes = icon("ok", lib = "glyphicon")),
          # status = "info", 
          justified = TRUE, direction = "horizontal",
          width = '250px'
        ),
      ),
      p("Model Selection:", style = "margin: 10px 5px 0px;font-weight: bold;"),
      div(
        id = "sidebar_selection_container",
        radioGroupButtons(
          inputId = "modelType",
          label = NULL, 
          choices = c("DR", "DER"),
          checkIcon = list(yes = icon("ok", lib = "glyphicon")),
          justified = TRUE, direction = "horizontal",
          width = '250px'
        ),
      ),
      conditionalPanel(
        condition = "input.modelType == 'DR'",        
        p("DR Model:", style = "margin: 10px 5px 0px;font-weight: bold;"),
        div(
          id = "sidebar_selection_container",
          selectInput("dr_model", label = NULL, # choices = c()
            choices = list("Sigmoid-Emax" = "sigEmax", "Emax" = "Emax", "Exponential" = "Expo", 
                            "Beta" = "Beta", "Linear" = "Linear", "Linear-Log"="LinearLog",
                            "Logistic" = "Logistic", "Quadratic" = "Quadratic"), 
            selected = "Sigmoid-Emax"
            )
        ),
      ),
      conditionalPanel(
        condition = "input.modelType == 'DER'",
        p("ER Model:", style = "margin: 10px 5px 0px;font-weight: bold;"),
        div(
          id = "sidebar_selection_container",
          selectInput("er_model", label = NULL, # choices = c()
            choices = list("Sigmoid-Emax" = "sigEmax", "Emax" = "Emax", "Exponential" = "Expo", 
                            "Beta" = "Beta", "Linear" = "Linear", "Linear-Log"="LinearLog",
                            "Logistic" = "Logistic", "Quadratic" = "Quadratic"), 
            selected = "Sigmoid-Emax"
            )
        ),
        p("DE Model:", style = "margin: 10px 5px 0px;font-weight: bold;"),
        div(
          id = "sidebar_selection_container",
          selectInput("de_model", label = NULL, # choices = c()
            choices = list("Power" = "power"), 
            selected = "Power"
            )
        ),
      ),
      div(
        id = "sidebar_selection_container",
        actionButton("upload", "Upload & Run", icon = icon("play"), style = 'width: 88%; margin-top: 40px; margin-bottom: 20px;')
      ),
    # ),  # TODO: not working
    menuItem(HTML("<strong>&nbsp;&nbsp;Tumor Growth Inhibition</strong>"), tabName = "tgi", icon = icon("dashboard"))
    # menuItem("Source code", icon = icon("file-code-o"), 
    # href = "https://github.com/rstudio/shinydashboard/"),
  )
)



body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"),  # 加载 MathJax
  ),
  tags$style(HTML("
    .content-wrapper, .right-side {
      background-color: white;
      overflow: auto;
    }
    /* 其他布局调整 */
  ")),
  tabItems(
    # tabItem(tabName = "simulate",
    #       div(class = "my-class", h1("Hello world"), p("This is a paragraph, which can be used for instructions.")),
    #       fluidRow(
    #         box(
    #           title = "Source Data", status = "primary", 
    #           solidHeader = TRUE, width = 4, # height = 600,
    #           # div(id = "exposure_table_container", withSpinner(DTOutput("exposure_table")))
    #           withSpinner(DTOutput("exposure_table"))
    #           ),
    #         box(
    #           title = "Exposure-Response Quantile Plot", status = "warning", 
    #           solidHeader = TRUE, width = 8, # height = 500,
    #           "Add Box content here.", br(), "More content here.",
    #           # column(6, div(id = "plot1_container", withSpinner(plotOutput("plot1"))))
    #           withSpinner(plotOutput("plot1"))
    #         ),
    #         box(
    #           title = "Exposure-Response Logistic Regression Plot", status = "warning", 
    #           solidHeader = TRUE, width = 8,
    #           # div(id = "plot2_container", withSpinner(plotOutput("plot2")))
    #           withSpinner(plotOutput("plot2")),
    #           "Add Box content here.", br(), "More content here."
    #         ),
    #       ),
    # ),
    tabItem(tabName = "doseFinding",
      h1(HTML("<span style='color: green; font-family: Arial, sans-serif;margin-bottom: 30px; display: block;'>Dose Finding Analysis</span>")),
      fluidRow(
        tabBox(
          title = "", width = 12, height = 800,
          tabPanel(
            "Description",
            # fluidRow(
              box(
                title = HTML("<b>Data Preview</b>"),
                solidHeader = TRUE, width = 10,
                tableOutput("data_preview")
                ),
              box(
                title = HTML("<b>Summary Statistics</b>"),
                solidHeader = TRUE, width = 10,
                verbatimTextOutput("data_description")
              ),
              box(
                title = HTML("<b>Source Data</b>"), # status = "primary", 
                solidHeader = TRUE, width = 10, # height = 600,
                withSpinner(DTOutput("exposure_table_real"))
              ),
          ),
          tabPanel(
            "Visualization", "Add Model content here.",
              fluidRow(
                box(
                  title = "Exposure-Response Quantile Plot", status = "warning", 
                  solidHeader = TRUE, width = 8, # height = 500,
                  "Add Box content here.", br(), "More content here.",
                  withSpinner(plotOutput("plot1_real"))
                ),
                box(
                  title = "Exposure-Response Logistic Regression Plot", status = "warning", 
                  solidHeader = TRUE, width = 8,
                  withSpinner(plotOutput("plot2_real")),
                  "Add Box content here.", br(), "More content here."
                ),
              ),
            ),
          tabPanel(
            "Results",
            # ! -------------------------------------------------------
            # ! Dose-Response Model
            # ! -------------------------------------------------------
            conditionalPanel(
              condition = "input.modelType == 'DR'",
              tabsetPanel(
                tabPanel(
                  "DR Model",
                  box(
                    title = "DR Model: Fitted Curve", status = "info", 
                    solidHeader = TRUE, width = 6, height = "475px",
                    withSpinner(plotOutput("DR_plot")),
                  ),
                  box(
                    title = "DR Model: Summary", status = "info", 
                    solidHeader = TRUE, width = 6, height = "475px",
                    verbatimTextOutput("DR_summary")
                  ),
                  # ! ----------------------
                  # ! Continuous Response
                  # ! ----------------------
                  conditionalPanel(
                    condition = "input.responseType == 'Continuous'",
                    box(
                      title = "DR Model: Q-Q Plot of Residuals", status = "info", 
                      solidHeader = TRUE, width = 6, height = "475px",
                      withSpinner(plotOutput("DR_qqplot")),
                      "Add Box content here.", br(), "More content here."
                    ),
                    box(
                      title = "DR Model: Residuals vs Fitted Values Plot", status = "info", 
                      solidHeader = TRUE, width = 6, height = "475px",
                      withSpinner(plotOutput("DR_ResFitplot")),
                      "Add Box content here.", br(), "More content here."
                    ),
                  ),
                  # ! ----------------------
                  # ! Binary Response
                  # ! ----------------------
                  conditionalPanel(
                    condition = "input.responseType == 'Binary'",
                    box(
                      title = "DR Model: ROC Curve", status = "info", 
                      solidHeader = TRUE, width = 6, # height = "520px",
                      withSpinner(plotOutput("DR_ROCplot")),
                      # textOutput("DR_AUC_text"),
                    ),
                  ),
                ),
                tabPanel(  # TODO: add content here
                  "DR bootstrap",
                  box(
                    title = NULL, solidHeader = TRUE,
                    width = 6, height = "160px",
                    sliderTextInput(
                      inputId = "n_bootstrap_dr",
                      label = "Bootstrap Replicates:", 
                      choices = c(100, 500, 1000, 5000, 10000),
                      grid = TRUE
                    ),
                    div(
                      style = "display: flex; align-items: center;",
                      prettyRadioButtons(
                        inputId = "sampling_method_dr",
                        label = "Resamling Method:", 
                        choices = list("Full" = "total", "By Dose" = "by_dose"),
                        inline = TRUE, 
                        fill = TRUE
                      ),
                      tags$i(
                        class = "fa fa-info-circle",
                        style = "margin-left: 10px; cursor: pointer;",
                        title = "Full: resample all data points.<br>By Dose: resample by dose level.",
                        'data-toggle' = "tooltip",
                        'data-placement' = "right",
                        'data-html' = "true"
                      )
                    ),
                  ),
                  box(
                    width = 4, height = "160px",
                    solidHeader = TRUE,
                    sliderTextInput(
                      inputId = "conf_lvl1_dr",
                      label = "Confidence Level:", 
                      choices = c(0.10, 0.25, 0.50, 0.75, 0.80, 0.90, 0.95),
                      selected = 0.95,
                      grid = TRUE
                    ),
                    actionButton("run_bootstrap_dr", "Run Bootstrap", icon = icon("play"), style = 'width: 88%;')
                  ),
                  fluidRow(
                    box(
                      solidHeader = TRUE, width = 4,
                      textInput("doi_dr", label = "Dose of Interest:", width = '50%'),
                    ),
                    conditionalPanel(
                      condition = "input.doi_dr != ''",
                      box(
                        solidHeader = TRUE, width = 6,
                        verbatimTextOutput("doi_res_dr"),
                      ),
                    )
                  ),
                  box(
                    title = "DR Model: Fitted Curve", status = "warning", 
                    solidHeader = TRUE, width = 10,
                    withSpinner(plotOutput("DR_bootstrapPlot")),
                  )
                ),
              )
            ),

            # ! -------------------------------------------------------
            # ! Dose-Exposure-Response Model
            # ! -------------------------------------------------------
            conditionalPanel(
              condition = "input.modelType == 'DER'",
              tabsetPanel(
                tabPanel(
                  "ER Model",
                  fluidRow(
                    box(
                      title = "ER Model: Fitted Curve with Data Points", status = "primary",
                      solidHeader = TRUE, width = 6, height = "475px",
                      withSpinner(plotOutput("ER_plot")),
                    ),
                    box(
                      title = "ER Model: Summary", status = "primary", 
                      solidHeader = TRUE, width = 6, # height = "475px",
                      verbatimTextOutput("ER_summary")
                    ),
                  ),
                  # ! ----------------------
                  # ! Continuous Response
                  # ! ----------------------
                  conditionalPanel(
                    condition = "input.responseType == 'Continuous'",
                    fluidRow(
                      box(
                        title = "ER Model: Q-Q Plot of Residuals", 
                        status = "primary", 
                        solidHeader = TRUE, width = 6, height = "475px",
                        withSpinner(plotOutput("ER_qqplot")),
                        "Add Box content here.", br(), "More content here."
                        ),
                      box(
                        title = "ER Model: Residuals vs Fitted Values Plot", 
                        status = "primary", 
                        solidHeader = TRUE, width = 6, height = "475px",
                        withSpinner(plotOutput("ER_ResFitplot")),
                        "Add Box content here.", br(), "More content here."
                        ),
                      ),
                    ),
                  # ! ----------------------
                  # ! Binary Response
                  # ! ----------------------
                  conditionalPanel(
                    condition = "input.responseType == 'Binary'",
                    fluidRow(
                      box(
                        title = "ER Model: ROC Curve", status = "primary", 
                        solidHeader = TRUE, width = 6, # height = "520px",
                        withSpinner(plotOutput("ER_ROCplot")),
                        # textOutput("ER_AUC_text"),
                        ),
                    )
                  ),
                ),
                tabPanel(
                  "DE Model",  # TODO: add model diagnostics here
                  box(
                    title = NULL, solidHeader = TRUE,
                    width = 10, # height = "150px",
                    uiOutput("select_covs"),
                    uiOutput("select_covs_type"),
                    div(
                      style = "display: flex; justify-content: right;",
                      actionButton("run_analysis", "Run model again", icon = icon("play"), style = 'width: 50%;'),
                    )
                  ),
                  fluidRow(
                    box(
                      title = "DE Model: Fitted Curve with Data Points", status = "success",
                      solidHeader = TRUE, width = 6, # height = "475px",
                      withSpinner(plotOutput("DE_plot"))
                    ),
                    box(
                      title = "DE Model: Summary", status = "success", 
                      solidHeader = TRUE, width = 6, # height = "475px",
                      verbatimTextOutput("DE_summary")
                    ),
                  ),
                  fluidRow(
                    box(
                      title = "DE Model: Q-Q Plot of Residuals", status = "success", 
                      solidHeader = TRUE, width = 6, # height = "475px",
                      withSpinner(plotOutput("DE_qqplot")),
                    ),
                    box(
                      title = "DE Model: Residuals vs Fitted Values Plot", status = "success", 
                      solidHeader = TRUE, width = 6, # height = "475px",
                      withSpinner(plotOutput("DE_ResFitplot")),
                    ),
                  ),
                ),
                tabPanel(
                  "DER",
                  fluidRow(
                    box(
                      title = "DER Model: Fitted Curve with Data Points", 
                      # status = "primary", 
                      solidHeader = TRUE, 
                      width = 10, 
                      withSpinner(plotOutput("DER_plot")),
                    ),
                  ),
                ),
                tabPanel(
                  "Bootstrap",
                  box(
                    title = NULL, solidHeader = TRUE,
                    width = 10, # height = "150px",
                    uiOutput("select_covs_bootstrap"),
                    uiOutput("select_covs_type_bootstrap"),
                  ),
                  fluidRow(
                    box(
                      title = NULL, solidHeader = TRUE,
                      width = 6, height = "200px",
                      sliderTextInput(
                        inputId = "n_bootstrap_der",
                        label = "Bootstrap Replicates:", 
                        choices = c(100, 500, 1000, 5000, 10000),
                        grid = TRUE
                      ),
                      div(
                        style = "display: flex; align-items: center;",
                        prettyRadioButtons(
                          inputId = "sampling_method_der",
                          label = "Resampling Method:", 
                          choices = list("Full" = "total", "By Dose" = "by_dose"),
                          inline = TRUE, 
                          fill = TRUE
                        ),
                        tags$i(
                          class = "fa fa-info-circle",
                          style = "margin-left: 10px; cursor: pointer;",
                          title = "Full: resample all data points.<br>By Dose: resample by dose level.",
                          'data-toggle' = "tooltip",
                          'data-placement' = "right",
                          'data-html' = "true"
                        )
                      ),
                    ),
                    box(
                      width = 4, height = "160px",
                      solidHeader = TRUE,
                      sliderTextInput(
                        inputId = "conf_lvl1_der",
                        label = "Confidence Level:", 
                        choices = c(0.10, 0.25, 0.50, 0.75, 0.80, 0.90, 0.95),
                        selected = 0.95,
                        grid = TRUE
                      ),
                      actionButton("run_bootstrap_der", "Run Bootstrap", icon = icon("play"), style = 'width: 88%;')
                    ),
                  ),
                  fluidRow(
                    box(
                      solidHeader = TRUE, width = 4,
                      textInput("doi_der", label = "Dose of Interest:", width = '50%'),
                    ),
                    conditionalPanel(
                      condition = "input.doi_der != ''",
                      box(
                        solidHeader = TRUE, width = 6,
                        verbatimTextOutput("doi_res"),
                      ),
                    )
                  ),
                  box(
                    title = "DER Model: Fitted Curve", status = "warning", 
                    solidHeader = TRUE, width = 10,
                    withSpinner(plotOutput("DER_bootstrapPlot")),
                  ),
                ),
              )
            ),
          ),
          tabPanel(
            "Documentation",
            includeMarkdown("doc.md")
          )
        ),
      )
    ),
    tabItem(tabName = "tgi",
      h1(HTML("<span style='color: green; font-family: Arial, sans-serif;margin-bottom: 10px; display: block;'>Tumor Growth Inhibition Model</span>")),
      fluidRow(
        box(
          solidHeader = TRUE, width = 4,
          p("Load data of type:", style = "margin: 0px 5px 0px;font-weight: bold; font-size: 18px;"),
          div(
            id = "sidebar_selection_container",
            selectInput("data_type_tgi", label = NULL, 
              choices = list("rds | rda | rdata" = "rdss", "csv" = "csv", "txt" = "txt"), 
              selected = "rds | rda | rdata")
          ),
          div(
            id = "upload_file_container",
            fileInput(inputId = "file_tgi", label = NULL,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
            ),
          ),
          div(
            id = "sidebar_selection_container",
            actionButton("upload_tgi", "Upload & Run", icon = icon("play"), style = 'width: 100%; margin-top: -20px; margin-bottom: 10px;')
          ),
        ),
      ),
      fluidRow(
        box(
          title = HTML("<strong>Data Preview</strong>"),
          solidHeader = TRUE, width = 6,
          tableOutput("tgi_data_preview")
        ),
        box(
          title = HTML("<strong>Model Summary</strong>"),
          solidHeader = TRUE, width = 6,
          verbatimTextOutput("tgi_fit_summary")
        )
      ),
      fluidRow(
        box(
          title = HTML("<strong>Model Fit</strong>"),
          solidHeader = TRUE, width = 6,
          plotOutput("tgi_fit_plot")
        ),
        box(
          title = HTML("<strong>Residuals</strong>"),
          solidHeader = TRUE, width = 6,
          plotOutput("tgi_resFitted")
        )
        
      )
    )
  )
)


ui <- dashboardPage(header, siderbar, body)