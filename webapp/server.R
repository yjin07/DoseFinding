

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
  random_data <- generate_data()
  exposure_data <- random_data$exposure_data
  processed_data <- process_data(exposure_data)

  exposure_data_all <- processed_data$exposure_data_all
  er_summary <- processed_data$er_summary
  n_quantile <- processed_data$n_quantile

  output$exposure_table <- renderDT({
    datatable(exposure_data)
  })

  random_plot <- generate_plots(exposure_data, exposure_data_all, er_summary, n_quantile)
  output$plot1 <- renderPlot({
    print(random_plot$p1)
  })
  output$plot2 <- renderPlot({
    print(random_plot$p2)
  })
  
  observeEvent(input$generate, {
    random_data <- generate_data()
    exposure_data <- random_data$exposure_data
    processed_data <- process_data(exposure_data)

    exposure_data_all <- processed_data$exposure_data_all
    er_summary <- processed_data$er_summary
    n_quantile <- processed_data$n_quantile

    output$exposure_table <- renderDT({
      datatable(exposure_data) 
    })

    random_plot <- generate_plots(exposure_data, exposure_data_all, er_summary, n_quantile)
    output$plot1 <- renderPlot({
      print(random_plot$p1)
    })
    output$plot2 <- renderPlot({
      print(random_plot$p2)
    })
  })

  # ! ---------------------
  # ! Tab: real data
  # ! ---------------------
  random_data <- generate_data()
  exposure_data <- random_data$exposure_data
  processed_data <- process_data(exposure_data)

  exposure_data_all <- processed_data$exposure_data_all
  er_summary <- processed_data$er_summary
  n_quantile <- processed_data$n_quantile

  output$exposure_table_real <- renderDT({
    datatable(exposure_data)
  })

  output$data_preview <- renderTable({
    head(exposure_data, 10)
  })

  output$data_description <- renderPrint({
    paste("This is a description of the data.")
    exposure_data1 <- exposure_data
    exposure_data1$Dose <- factor(exposure_data1$Dose, levels = c("Placebo", "150 mg", "300 mg", "600 mg"))
    summary(exposure_data1)
  })

  random_plot <- generate_plots(exposure_data, exposure_data_all, er_summary, n_quantile)
  output$plot1_real <- renderPlot({
    print(random_plot$p1)
  })
  output$plot2_real <- renderPlot({
    print(random_plot$p2)
  })
  
  observeEvent(input$file1, {
    random_data <- generate_data()
    exposure_data <- random_data$exposure_data
    processed_data <- process_data(exposure_data)

    exposure_data_all <- processed_data$exposure_data_all
    er_summary <- processed_data$er_summary
    n_quantile <- processed_data$n_quantile

    output$exposure_table_real <- renderDT({
      datatable(exposure_data)
    })

    random_plot <- generate_plots(exposure_data, exposure_data_all, er_summary, n_quantile)
    output$plot1_real <- renderPlot({
      print(random_plot$p1)
    })
    output$plot2_real <- renderPlot({
      print(random_plot$p2)
    })
  })
}
