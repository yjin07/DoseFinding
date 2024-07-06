

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
  })

  observeEvent(input$model, {
    log_info("Change model...")
    myData(NULL)
  })

  observeEvent(myData(),{
    df <- myData()
    log_info("Data is uploaded...")

    generate_and_render_real_data(output, data = df, input = input)

    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_er <- fitERMod(df$Exposure, df$Response, input$model, type = type)
    fit_de <- lm(log(df$Exposure) ~ log(df$Dose))
    sigma_c <- sqrt(sum(fit_de$residuals^2) / fit_de$df.residual)

    new_exposure <- seq(min(df$Exposure), max(df$Exposure), length.out = 100)
    fitted_values <- predict(fit_er, newdata = new_exposure)
    fit_df <- data.frame(Exposure = new_exposure, Fitted = fitted_values)

    output$de_summary <- renderPrint({
      summary(fit_de)
    })

    if (input$responseType == "Continuous") {
      p <- ggplot() +
        geom_point(data = df, aes(x = Exposure, y = Response), color = "blue", alpha = 0.5) +  # 数据点
        geom_line(data = fit_df, aes(x = Exposure, y = Fitted), color = "red", size = 1) +  # 拟合曲线
        labs( #title = "Fitted Curve with Data Points",
            x = "Exposure",
            y = "Response") +
        theme_minimal()


      output$ER_residuals <- renderPrint({
        print(round(quantile(fit_er$residuals), 3))
      })

      output$ER_AIC <- renderPrint({
        cat("AIC:", round(fit_er$AIC, 3))
      })

      output$ER_qqplot <- renderPlot({
        qqnorm(fit_er$residuals, main = "Q-Q plot of residuals")
        qqline(fit_er$residuals)
      })

      output$ER_ResFitplot <- renderPlot({
        plot(fit_er$fitted_values, fit_er$residuals, main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
        abline(h = 0, col = "red")
      })
    } else if (input$responseType == "Binary") {
      p <- ggplot() +
        geom_jitter(data = df, aes(x = Exposure, y = Response), color = "blue", alpha = 0.5) +  # 数据点
        geom_line(data = fit_df, aes(x = Exposure, y = Fitted), color = "red", size = 1) +  # 拟合曲线
        labs(title = "Fitted Curve with Data Points",
            x = "Exposure",
            y = "Response") +
        theme_minimal()


      roc_obj <- roc(df$Response, fit_er$fitted_values, percent = TRUE, direction = "<")
      output$ER_ROCplot <- renderPlot({
        plot(roc_obj, main = "ROC curve")
      })

      output$AUC_text <- renderText({
        paste("AUC:", round(auc(roc_obj), 3))
      })

      output$HL_test <- renderPrint({
        hl_test <- hoslem.test(df$Response, fit_er$fitted_values, g = 10)
        print(hl_test)
      })

      output$ER_devResiduals <- renderPrint({
        print(round(quantile(fit_er$devResiduals), 3))
      })

      output$ER_deviance <- renderPrint(
        cat("Residual deviance:", round(fit_er$deviance, 3), "on", fit_er$df.residual, "degrees of freedom.", "P-value:", round(1 - pchisq(fit_er$deviance, fit_er$df.residual), 3), "\nAIC:", round(fit_er$AIC, 3))
      )
    }

    output$ER_plot <- renderPlot({
      print(p)
    })
  })
}
