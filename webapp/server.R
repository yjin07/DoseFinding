server <- function(input, output, session) {
  # observeEvent(input$responseType, {
  #   if (input$responseType == 'Continuous') {
  #     updateSelectInput(session, "model", label = NULL,
  #                 choices = list("Sigmoid-Emax" = "sEmax", "Emax" = "emax", "Exponential" = "expo", "Beta" = "beta", "Linear" = "linear", "Linear-Log"="linearLog",
  #                 "Logistic" = "logistic", "Quadratic" = "quad"))
  #   } else {
  #     updateSelectInput(session, "model", label = NULL,
  #                 choices = list("Logistic" = "logistic"))
  #   }
  # })


  # ! ---------------------
  # ! Tab: simulate
  # ! ---------------------
  # generate_and_render_simulate_data(output)
  # observeEvent(input$generate, {
  #   generate_and_render_simulate_data(output)
  # })


  # ! ---------------------
  # ! Tab: real data
  # ! ---------------------
  generate_and_render_real_data(output)

  myData <- reactiveVal()

  observeEvent(input$upload, {
    log_info("Hit upload button...")
    myData(NULL)
    uploadChecking(input, myData)

    output$select_covs <- renderUI({
      df <- myData()
      if (is.null(df)) return(NULL)

      other_cols <- setdiff(names(df), c("Dose", "Exposure", "Response"))
      if (length(other_cols) == 0) return(NULL)

      tagList(
        awesomeCheckboxGroup(
          inputId = "selectCovs",
          label = "Select additional covariates to include in the Dose-Exposure model",
          choices = other_cols,
          inline = TRUE,
          status = "danger"
        ),
      )
    })

    output$select_covs_bootstrap <- renderUI({
      df <- myData()
      if (is.null(df)) return(NULL)

      other_cols <- setdiff(names(df), c("Dose", "Exposure", "Response"))
      if (length(other_cols) == 0) return(NULL)

      tagList(
        awesomeCheckboxGroup(
          inputId = "selectCovs_bootstrap",
          label = "Select additional covariates to include in the Dose-Exposure model",
          choices = other_cols,
          inline = TRUE,
          status = "danger"
        ),
      )
    })

  })

  observeEvent(input$modelType, {
    output$dynamic_tabs <- renderUI({
    log_info("Render dynamic tabs...")
      if (input$modelType == 'DER') {
        tagList(
          tabPanel("ER Model", value = "er", "ER Model Content"),
          tabPanel("DE Model", value = "de", "DE Model Content"),
        )
      } else {
        tabPanel("DR Model", value = "dr", "DR Model Content")
      }
    })
  })


  observe({
    cat("Selected covariates: ", input$selectCovs, "\n")
    selected_vars <- input$selectCovs

    output$select_covs_type <- renderUI({
      if (length(selected_vars) == 0) return(NULL)

      tagList(
        h5("Select type for variables:", style = "font-weight: bold;"),
        lapply(selected_vars, function(var) {
          var_type <- input[[paste0("type_", var)]] %||% "Continuous"

          div(
            style = "display: flex; align-items: center; margin-bottom: 10px;",
            div(style = "margin-right: 10px; align-items: center;", h5(paste("$", var))),
            div(style = "margin-right: 10px; align-items: center;",
              prettyRadioButtons(
                inputId = paste0("type_", var),
                label = NULL,
                choices = c("Continuous", "Categorical"),
                inline = TRUE,
                selected = var_type
              )
            ),
            div(style = "flex-grow: 1;",
              conditionalPanel(
                condition = sprintf("input['type_%s'] === 'Continuous'", var),
                textInput(inputId = paste0("value_", var, "_continuous"), 
                          label = NULL, 
                          placeholder = "Enter value")
              ),
              conditionalPanel(
                condition = sprintf("input['type_%s'] === 'Categorical'", var),
                selectInput(inputId = paste0("value_", var, "_categorical"), 
                            label = NULL, 
                            choices = c("Loading..." = ""))
              )
            )
          )
        })
      )
    })
  })

  # 更新分类变量的选项
  observe({
    req(input$selectCovs)
    for (var in input$selectCovs) {
      if (!is.null(input[[paste0("type_", var)]]) && input[[paste0("type_", var)]] == "Categorical") {
        choices <- tryCatch({
          levels(as.factor(myData()[[var]]))
        }, error = function(e) {
          c("Error loading choices" = "")
        })
        updateSelectInput(inputId = paste0("value_", var, "_categorical"), choices = choices)
      }
    }
  })


  # ! ---------------------
  # ! Tab: Bootstrap
  # ! ---------------------
  observe({
    cat("Selected covariates: ", input$selectCovs_bootstrap, "\n")
    selected_vars <- input$selectCovs_bootstrap

    output$select_covs_type_bootstrap <- renderUI({
      if (length(selected_vars) == 0) return(NULL)

      tagList(
        h5("Select type for variables:", style = "font-weight: bold;"),
        lapply(selected_vars, function(var) {
          var_type <- input[[paste0("Bootstrap_type_", var)]] %||% "Continuous"

          div(
            style = "display: flex; align-items: center; margin-bottom: 10px;",
            div(style = "margin-right: 10px; align-items: center;", h5(paste("$", var))),
            div(style = "margin-right: 10px; align-items: center;",
              prettyRadioButtons(
                inputId = paste0("Bootstrap_type_", var),
                label = NULL,
                choices = c("Continuous", "Categorical"),
                inline = TRUE,
                selected = var_type
              )
            ),
            div(style = "flex-grow: 1;",
              conditionalPanel(
                condition = sprintf("input['Bootstrap_type_%s'] === 'Continuous'", var),
                textInput(inputId = paste0("Bootstrap_value_", var, "_continuous"), 
                          label = NULL, 
                          placeholder = "Enter value")
              ),
              conditionalPanel(
                condition = sprintf("input['Bootstrap_type_%s'] === 'Categorical'", var),
                selectInput(inputId = paste0("Bootstrap_value_", var, "_categorical"), 
                            label = NULL, 
                            choices = c("Loading..." = ""))
              )
            )
          )
        })
      )
    })
  })

  # 更新分类变量的选项
  observe({
    req(input$selectCovs_bootstrap)
    for (var in input$selectCovs_bootstrap) {
      if (!is.null(input[[paste0("Bootstrap_type_", var)]]) && input[[paste0("Bootstrap_type_", var)]] == "Categorical") {
        choices <- tryCatch({
          levels(as.factor(myData()[[var]]))
        }, error = function(e) {
          c("Error loading choices" = "")
        })
        updateSelectInput(inputId = paste0("Bootstrap_value_", var, "_categorical"), choices = choices)
      }
    }
  })

  observeEvent(ignoreInit = TRUE, list(
    input$modelType,
    input$dr_model,
    input$er_model,
    input$de_model
  ),{
    log_info("Change model...")
    # myData(NULL)  # * Not necessary
  })

  observeEvent(myData(),{
    df <- myData()
    log_info("Data is uploaded...")

    # * Descriptive statistics in `Visualization` tab
    generate_and_render_real_data(output, data = df, input = input)

    if (input$modelType == 'DER') {
      get_der_results(df, input, output)
    } else {
      get_dr_results(df, input, output)
    }
  })

  observeEvent(input$run_analysis, {
    req(myData())
    df <- myData()

    if (is.null(input$selectCovs)) {
      showModal(modalDialog(
        title = "Error",
        "No covariates selected. Please select at least one covariate.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    empty_vars <- character(0)
    pred_data <- list()

    for (var in input$selectCovs) {
      var_type <- input[[paste0("type_", var)]]
      if (!is.null(var_type)) {
        if (var_type == "Continuous") {
          value <- input[[paste0("value_", var, "_continuous")]]
        } else {
          value <- input[[paste0("value_", var, "_categorical")]]
        }

        if (is.null(value) || value == "") {
          empty_vars <- c(empty_vars, var)
        } else {
          if (var_type == "Continuous") {
            pred_data[[var]] <- as.numeric(value)
          } else {
            pred_data[[var]] <- as.factor(value)
          }
        }
        # 在这里可以处理输入值的变化，例如更新其他反应式值或执行计算
        cat("Variable:", var, "Type:", var_type, "Value:", value, "\n")
      }
    }

    # 如果有空值，显示错误消息
    if (length(empty_vars) > 0) {
      formatted_vars <- paste0("$", empty_vars)

      showModal(modalDialog(
        title = "Error",
        HTML(paste0("The following variables have empty values:<br>",
                    paste(formatted_vars, collapse = "<br>"),
                    "<br><br>Please enter values for all selected variables.")),
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    pred_data_df <- as.data.frame(pred_data)

    # Construct the formula
    covars <- lapply(input$selectCovs, function(var) {
      var_type <- input[[paste0("type_", var)]]
      if (var_type == "Categorical") {
        return(paste0("factor(", var, ")"))
      } else {
        return(var)
      }
    })
    covars <- paste(covars, collapse = " + ")
    addCovar <- as.formula(paste("~", covars))

    cat("Generated formula: ", deparse(addCovar), "\n")
    
    get_der_results_withCovars(df, input, output, addCovar, pred_data_df)
    
  })

  observeEvent(input$run_bootstrap_dr, {
    if (is.null(myData())) {
      showModal(modalDialog(
        title = "Error",
        "No data uploaded. Please upload a file first.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    df <- myData()

    if (!input$doi_dr=="") {
      log_info("Dose of Interest:", input$doi_dr)

      is_numeric <- function(x) {
        !is.null(x) && !is.na(suppressWarnings(as.numeric(x)))
      }

      if (!is_numeric(input$doi_dr)) {
        showModal(modalDialog(
          title = "Error",
          "Please enter a valid number for Dose of Interest.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      doi <- suppressWarnings(as.numeric(input$doi_dr))
      max_dose <- max(df$Dose, na.rm = TRUE)

      if (doi < 0) {
        showModal(modalDialog(
          title = "Error",
          "Dose of Interest cannot be negative.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      } else if (doi > 2 * max_dose) {
        showModal(modalDialog(
          title = "Warning",
          HTML(paste0("Dose of Interest (", doi, ") exceeds twice the maximum dose in the dataset (", 2 * max_dose, ").<br>",
                      "Are you sure this is correct?")),
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
    }

    get_dr_bootstrap(df, input, output)
  })

  observe({
    runjs('$(document).ready(function(){
          $("[data-toggle=\'tooltip\']").tooltip(); 
          });')
  })

  observeEvent(input$run_bootstrap_der, {  # TODO: continue the work from here
    if (is.null(myData())) {
      showModal(modalDialog(
        title = "Error",
        "No data uploaded. Please upload a file first.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()
    }

    df <- myData()

    if (!input$doi_der=="") {
      log_info("Dose of Interest:", input$doi_der)

      is_numeric <- function(x) {
        !is.null(x) && !is.na(suppressWarnings(as.numeric(x)))
      }

      if (!is_numeric(input$doi_der)) {
        showModal(modalDialog(
          title = "Error",
          "Please enter a valid number for Dose of Interest.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      doi <- suppressWarnings(as.numeric(input$doi_der))
      max_dose <- max(df$Dose, na.rm = TRUE)

      if (doi < 0) {
        showModal(modalDialog(
          title = "Error",
          "Dose of Interest cannot be negative.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      } else if (doi > 2 * max_dose) {
        showModal(modalDialog(
          title = "Warning",
          HTML(paste0("Dose of Interest (", doi, ") exceeds twice the maximum dose in the dataset (", 2 * max_dose, ").<br>",
                      "Are you sure this is correct?")),
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
    }

    if (is.null(input$selectCovs_bootstrap)) {
      get_der_bootstrap(df, input, output)
    } else {
      empty_vars <- character(0)
      pred_data <- list()

      for (var in input$selectCovs_bootstrap) {
        var_type <- input[[paste0("Bootstrap_type_", var)]]
        if (!is.null(var_type)) {
          if (var_type == "Continuous") {
            value <- input[[paste0("Bootstrap_value_", var, "_continuous")]]
          } else {
            value <- input[[paste0("Bootstrap_value_", var, "_categorical")]]
          }

          if (is.null(value) || value == "") {
            empty_vars <- c(empty_vars, var)
          } else {
            if (var_type == "Continuous") {
              pred_data[[var]] <- as.numeric(value)
            } else {
              pred_data[[var]] <- as.factor(value)
            }
          }

          cat("Variable:", var, "Type:", var_type, "Value:", value, "\n")
        }
      }

      # 如果有空值，显示错误消息
      if (length(empty_vars) > 0) {
        formatted_vars <- paste0("$", empty_vars)

        showModal(modalDialog(
          title = "Error",
          HTML(paste0("The following variables have empty values:<br>",
                      paste(formatted_vars, collapse = "<br>"),
                      "<br><br>Please enter values for all selected variables.")),
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      pred_data_df <- as.data.frame(pred_data)

      # Construct the formula
      covars <- lapply(input$selectCovs_bootstrap, function(var) {
        var_type <- input[[paste0("Bootstrap_type_", var)]]
        if (var_type == "Categorical") {
          return(paste0("factor(", var, ")"))
        } else {
          return(var)
        }
      })
      covars <- paste(covars, collapse = " + ")
      addCovar <- as.formula(paste("~", covars))

      cat("Generated formula: ", deparse(addCovar), "\n")

      get_der_bootstrap_withCovars(df, input, output, addCovar, pred_data_df)
    }
  })


  # * ---------------------
  # * Menu: TGI
  # * ---------------------
  myTGIData <- reactiveVal()

  observeEvent(input$upload_tgi, {
    log_info("User trying to upload data of TGI...")
    myTGIData(NULL)
    uploadCheckingTGI(input, myTGIData)
  })

  observeEvent(myTGIData(), {
    df <- myTGIData()
    log_info("Data of TGI is uploaded...")

    get_tgi_results(df, input, output)
  })

}
