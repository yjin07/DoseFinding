get_der_results <- function(df, input, output) {
    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_der <- fitDERMod(df$Dose, df$Exposure, df$Response, model = input$er_model, type = type)
    new_doses <- seq(min(df$Dose), max(df$Dose), by = 2)
    fitted_values2 <- predict(fit_der, newdata = new_doses, type = "response")

    if (input$responseType == "Continuous") {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values2)
    } else {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))
    }

    fit_er <- fit_der$fit1
    fit_de <- fit_der$fit2
    sigma_c <- fit_der$sigma_c
    new_exposure <- seq(min(df$Exposure), max(df$Exposure), length.out = 100)
    fitted_values <- predict(fit_er, newdata = new_exposure)
    fit_df <- data.frame(Exposure = new_exposure, Fitted = fitted_values)

    fitted_values1 <- predict(fit_de, newdata = data.frame(dose = new_doses))
    fit_df1 <- data.frame(Dose = new_doses, Fitted = fitted_values1)

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
}