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
        actionButton("run_analysis", "Rerun with more covariates", icon = icon("play"), style = 'width: 50%;')
      )
    })
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
      get_dr_results(df, input, output)  # TODO: to be implemented
    }
  })

  observeEvent(input$run_bootstrap, {
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
    if (input$modelType == 'DER') {
      get_der_bootstrap(df, input, output)
    } else {
      get_dr_bootstrap(df, input, output)  # TODO: to be implemented
    }
  })
}
