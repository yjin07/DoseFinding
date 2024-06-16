

server <- function(input, output) {
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
