get_dr_results <- function(df, input, output) {
    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_dr <- fitERMod(df$Dose, df$Response, model = input$dr_model, type = type)
    new_doses <- seq(min(df$Dose), max(df$Dose), by = 2)
    fitted_values <- predict(fit_dr, newdata = new_doses, type = "response")

    if (input$responseType == "Continuous") {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values)
    } else {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values))
    }


    if (input$responseType == "Continuous") {
        p_dr <- ggplot() +
            geom_point(data = df, aes(x = Dose, y = log(Response)), color = "blue", alpha = 0.5) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs( #title = "Fitted Curve with Data Points",
                x = "Dose",
                y = "log(Response)") +
            theme_minimal()

        output$DR_residuals <- renderPrint({
            print(round(quantile(fit_dr$residuals), 3))
        })

        output$DR_AIC <- renderPrint({
            cat("AIC:", round(fit_dr$AIC, 3))
        })

        output$DR_qqplot <- renderPlot({
            qqnorm(fit_dr$residuals, main = "Q-Q plot of residuals")
            qqline(fit_dr$residuals)
        })

        output$DR_ResFitplot <- renderPlot({
            plot(log(fit_dr$fitted_values), fit_dr$residuals, main = "Residuals vs Fitted", xlab = "Fitted values", ylab = "Residuals")
            abline(h = 0, col = "red")
        })
    } else if (input$responseType == "Binary") {
        p_dr <- ggplot() +
            geom_jitter(data = df, aes(x = Dose, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs( #title = "Fitted Curve with Data Points",
                x = "Dose",
                y = "Probability of Response") +
            theme_minimal()

        roc_obj <- roc(df$Response, fit_dr$fitted_values, percent = TRUE, direction = "<")
        output$DR_ROCplot <- renderPlot({
            plot(roc_obj, main = "ROC curve")
        })

        output$DR_AUC_text <- renderText({
            paste("AUC:", round(auc(roc_obj), 3))
        })

        output$DR_HL_test <- renderPrint({
            hl_test <- hoslem.test(df$Response, fit_dr$fitted_values, g = 10)
            print(hl_test)
        })

        output$DR_devResiduals <- renderPrint({
            print(round(quantile(fit_dr$devResiduals), 3))
        })

        output$DR_deviance <- renderPrint(
            cat("Residual deviance:", round(fit_dr$deviance, 3), "on", fit_dr$df.residual, "degrees of freedom.", "P-value:", round(1 - pchisq(fit_dr$deviance, fit_dr$df.residual), 3), "\nAIC:", round(fit_dr$AIC, 3))
            )
    }

    output$DR_plot <- renderPlot({
        print(p_dr)
    })
}