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
    fit_der <- fitDERMod(df$Dose, df$Exposure, df$Response, model = input$model, type = type)
    new_doses <- seq(min(df$Dose), max(df$Dose), by = 2)
    fitted_values2 <- predict(fit_der, newdata = new_doses, type = "response")
    # fit_df2 <- ifelse(
    #   input$responseType == "Continuous",
    #   data.frame(Dose = new_doses, Fitted = fitted_values2),
    #   data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))
    # )

    # log_info("Fitted values are calculated here 1...")

    if (input$responseType == "Continuous") {
      fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values2)
    } else {
      fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))
    }

    # log_info("Fitted values are calculated here 2...")

    fit_er <- fit_der$fit1
    fit_de <- fit_der$fit2
    sigma_c <- fit_der$sigma_c
    new_exposure <- seq(min(df$Exposure), max(df$Exposure), length.out = 100)
    fitted_values <- predict(fit_er, newdata = new_exposure)
    fit_df <- data.frame(Exposure = new_exposure, Fitted = fitted_values)


    # log_info("Fitted values are calculated here 2.5...")
    fitted_values1 <- predict(fit_de, newdata = data.frame(dose = new_doses))
    # log_info("Fitted values are calculated here 2.6...")
    fit_df1 <- data.frame(Dose = new_doses, Fitted = fitted_values1)

    # log_info("Fitted values are calculated here 3...")

    output$de_summary <- renderPrint({
      summary(fit_de)
    })

    p_de <- ggplot() +
      geom_point(data = df, aes(x = Dose, y = Exposure), color = "blue", alpha = 0.5) +
      geom_line(data = fit_df1, aes(x = Dose, y = exp(Fitted)), color = "red", size = 1) + 
      labs(
        x = "Dose",
        y = "Exposure") +
      theme_minimal()

    output$DE_plot <- renderPlot({
      print(p_de)
    })

    if (input$responseType == "Continuous") {
      p_der <- ggplot() +
        geom_point(data = df, aes(x = Dose, y = log(Response)), color = "blue", alpha = 0.5) +  # 数据点
        geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
        labs( #title = "Fitted Curve with Data Points",
            x = "Dose",
            y = "log(Response)") +
        theme_minimal()

      p_er <- ggplot() +
        geom_point(data = df, aes(x = Exposure, y = log(Response)), color = "blue", alpha = 0.5) +  # 数据点
        geom_line(data = fit_df, aes(x = Exposure, y = Fitted), color = "red", size = 1) +  # 拟合曲线
        labs( #title = "Fitted Curve with Data Points",
            x = "Exposure",
            y = "log(Response)") +
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
        plot(log(fit_er$fitted_values), fit_er$residuals, main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
        abline(h = 0, col = "red")
      })
    } else if (input$responseType == "Binary") {
      p_der <- ggplot() +
        geom_jitter(data = df, aes(x = Dose, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) +  # 数据点
        geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
        labs( #title = "Fitted Curve with Data Points",
            x = "Dose",
            y = "Probability of Response") +
        theme_minimal()

      p_er <- ggplot() +
        geom_jitter(data = df, aes(x = Exposure, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) +  # 数据点
        geom_line(data = fit_df, aes(x = Exposure, y = Fitted), color = "red", size = 1) +  # 拟合曲线
        labs(title = "Fitted Curve with Data Points",
            x = "Exposure",
            y = "Probability of Response") +
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
      print(p_er)
    })

    output$DER_plot <- renderPlot({
      print(p_der)
    })
  })

  observeEvent(input$run_bootstrap, {
    df <- myData()
    n_bootstrap <- input$n_bootstrap

    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_der <- fitDERMod(df$Dose, df$Exposure, df$Response, model = input$model, type = type)
    new_doses <- seq(min(df$Dose), max(df$Dose), by = 2)
    fitted_values2 <- predict(fit_der, newdata = new_doses, type = "response")

    if (input$responseType == "Continuous") {
      fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values2)
    } else {
      fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))
    }

    log_info("Bootstrap started...")
    fitted_vals_bootstrap <- matrix(NA, nrow = n_bootstrap, ncol = length(new_doses))

    withProgress(message = "Run Bootstrap replicates", value = 0, {
      for (jj in 1:n_bootstrap) {
        ind <- sample(1:nrow(df), nrow(df), replace = TRUE)
        fit_der0 <- fitDERMod(df$Dose[ind], df$Exposure[ind], df$Response[ind], model = input$model, type = type)
        if (input$responseType == "Continuous") {
          fitted_vals_bootstrap[jj, ] <- predict(fit_der0, newdata = new_doses, type = "response")
        } else {
          fitted_vals_bootstrap[jj, ] <- inv_logit(predict(fit_der0, newdata = new_doses, type = "response"))
        }

        incProgress(1/n_bootstrap, detail = paste("\nBootstrap sample", jj))
      }
    })


    fitted_vals_bootstrap <- na.omit(fitted_vals_bootstrap)
    log_info("Bootstrap finished...")
    log_info("Valid bootstrap samples:", nrow(fitted_vals_bootstrap))

    q_lower <- (1 - input$conf_lvl1) / 2
    ci_low <- apply(fitted_vals_bootstrap, 2, function(x) quantile(x, q_lower))
    ci_high <- apply(fitted_vals_bootstrap, 2, function(x) quantile(x, 1 - q_lower))

    # 将置信区间添加到拟合数据框
    fit_df2$CI_low <- ci_low
    fit_df2$CI_high <- ci_high

    if (input$responseType == "Continuous") {
      df_summary <- df %>%
        group_by(Dose) %>%
        summarise(
          n = n(),
          mean_log_response = mean(log(Response)),
          sd_log_response = sd(log(Response)),
          se_log_response = sd_log_response / sqrt(n),
          ci_low = mean_log_response - qt(0.975, df = n - 1) * se_log_response,
          ci_high = mean_log_response + qt(0.975, df = n - 1) * se_log_response
        )

      p_der0 <- ggplot() +
        geom_errorbar(data = df_summary, aes(x = Dose, ymin = ci_low, ymax = ci_high), width = 0.2) +  # Error bars
        geom_point(data = df, aes(x = Dose, y = log(Response)), color = "blue", alpha = 0.5) +  # 数据点
        geom_point(data = df_summary, aes(x = Dose, y = mean_log_response), color = "black", size = 2) +  # 数据点
        geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
        geom_ribbon(data = fit_df2, aes(x = Dose, ymin = CI_low, ymax = CI_high), alpha = 0.2, fill = "grey") +  # 置信区间
        labs(
          x = "Dose",
          y = "log(Response)"
        ) +
        theme_minimal()
    } else {
      df_summary <- df %>%
        group_by(Dose) %>%
        summarise(
          n = n(),
          resp = sum(Response),
          prob = mean(Response),
          ci_low = binom.confint(resp, n, methods = "wilson")$lower,
          ci_high = binom.confint(resp, n, methods = "wilson")$upper
        )

      p_der0 <- ggplot() +
        geom_jitter(data = df, aes(x = Dose, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) + 
        geom_point(data = df_summary, aes(x = Dose, y = prob), color = "black", size = 2) +  # 数据点
        geom_errorbar(data = df_summary, aes(x = Dose, ymin = ci_low, ymax = ci_high), width = 0.2) +  # Error bars
        geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
        geom_ribbon(data = fit_df2, aes(x = Dose, ymin = CI_low, ymax = CI_high), alpha = 0.2, fill = "grey") + 
        labs(
          x = "Dose",
          y = "Probability of Response"
        ) +
        theme_minimal()
    }


    output$DER_bootstrapPlot <- renderPlot({
      print(p_der0)
    })

  })
}
