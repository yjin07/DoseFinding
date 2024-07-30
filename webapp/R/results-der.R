get_der_results <- function(df, input, output) {
    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_der <- fitDERMod(df$Dose, df$Exposure, df$Response, model = input$er_model, type = type)

    valid_dose <- complete.cases(df$Dose)
    new_doses <- seq(min(df$Dose[valid_dose]), max(df$Dose[valid_dose]), by = 2)
    fitted_values2 <- predict(fit_der, newdata = new_doses, type = "response")

    if (input$responseType == "Continuous") {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values2)
    } else {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))
    }

    fit_er <- fit_der$fit1
    fit_de <- fit_der$fit2
    sigma_c <- fit_der$sigma_c

    valid_exposure <- complete.cases(df$Exposure)
    new_exposure <- seq(min(df$Exposure[valid_exposure]), max(df$Exposure[valid_exposure]), length.out = 100)
    fitted_values <- predict(fit_er, newdata = new_exposure)
    fit_df <- data.frame(Exposure = new_exposure, Fitted = fitted_values)

    fitted_values1 <- predict(fit_de, newdata = data.frame(dose = new_doses))
    fit_df1 <- data.frame(Dose = new_doses, Fitted = fitted_values1)

    output$ER_summary <- renderPrint({
        summary(fit_er)
    })

    output$DE_summary <- renderPrint({
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
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model, "\nDE Model: ", input$de_model,  "\nNo Covariates"),
                x = "Dose",
                y = "log(Response)") +
            theme_minimal()

        p_er <- ggplot() +
            geom_point(data = df, aes(x = Exposure, y = log(Response)), color = "blue", alpha = 0.5) +  # 数据点
            geom_line(data = fit_df, aes(x = Exposure, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model),
                x = "Exposure",
                y = "log(Response)") +
            theme_minimal()


        output$ER_qqplot <- renderPlot({
            qqnorm(fit_er$residuals, 
            main = paste0("Q-Q plot of residuals (ER: ", input$er_model, ")")
            )
            qqline(fit_er$residuals)
        })

        output$ER_ResFitplot <- renderPlot({
            plot(log(fit_er$fitted_values), fit_er$residuals, 
            main = paste0("Residuals vs Fitted (ER: ", input$er_model, ")"), 
            xlab = "Fitted values", ylab = "Residuals")
            abline(h = 0, col = "red")
        })



    } else if (input$responseType == "Binary") {
        p_der <- ggplot() +
            geom_jitter(data = df, aes(x = Dose, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model, "\nDE Model: ", input$de_model,  "\nNo Covariates"),
                x = "Dose",
                y = "Probability of Response") +
            theme_minimal()

        p_er <- ggplot() +
            geom_jitter(data = df, aes(x = Exposure, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) +  # 数据点
            geom_line(data = fit_df, aes(x = Exposure, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model),
                x = "Exposure",
                y = "Probability of Response") +
            theme_minimal()


        roc_obj <- roc(df$Response, fit_er$fitted_values, percent = TRUE, direction = "<")

        output$ER_AUC_text <- renderText({
            paste("AUC:", round(auc(roc_obj), 3))
        })

        output$ER_ROCplot <- renderPlot({
            plot(roc_obj, 
            main = paste0("ROC curve", " (ER: ", input$er_model, ")")
            )
        })
    }


    output$DE_qqplot <- renderPlot({
        qqnorm(fit_de$residuals,
        main = paste0("Q-Q plot of residuals (DE: ", input$de_model, ")"),
        )
        qqline(fit_de$residuals)
    })

    output$DE_ResFitplot <- renderPlot({
        plot(fitted(fit_de), fit_de$residuals,
        main = paste0("Residuals vs Fitted (DE: ", input$de_model, ")"),
        xlab = "Fitted values", ylab = "Residuals")
        abline(h = 0, col = "red")
    })

    output$ER_plot <- renderPlot({
        print(p_er)
    })

    output$DER_plot <- renderPlot({
        print(p_der)
    })
}



get_der_results_withCovars <- function(df, input, output, addCovars, pred_data_df) {
    log_info("Running DER model with covariates...")
    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_der <- fitDERMod(df$Dose, df$Exposure, df$Response, data = df, model = input$er_model, type = type, addCovars = addCovars)
    
    log_info("Fitting DER model finished...")

    valid_dose <- complete.cases(df$Dose)
    new_doses <- seq(min(df$Dose[valid_dose]), max(df$Dose[valid_dose]), by = 2)

    fit_er <- fit_der$fit1
    fit_de <- fit_der$fit2

    output$ER_summary <- renderPrint({
        summary(fit_er)
    })

    output$DE_summary <- renderPrint({
        summary(fit_de)
    })
    

    valid_exposure <- complete.cases(df$Exposure)
    new_exposure <- seq(min(df$Exposure[valid_exposure]), max(df$Exposure[valid_exposure]), length.out = 100)
    fitted_values <- predict(fit_er, newdata = new_exposure)
    fit_df <- data.frame(Exposure = new_exposure, Fitted = fitted_values)


    pred_df <- pred_data_df[rep(1, length(new_doses)), , drop = FALSE]
    pred_df$dose <- new_doses
    fitted_values1 <- predict(fit_de, newdata = pred_df)
    fit_df1 <- data.frame(Dose = new_doses, Fitted = fitted_values1)

    fitted_values2 <- predict(fit_der, newdata = pred_df, type = "response")

    if (input$responseType == "Continuous") {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values2)
    } else {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))
    }

    print(head(fit_df2))

    p_de <- ggplot() +
        geom_point(data = df, aes(x = Dose, y = Exposure), color = "blue", alpha = 0.5) +
        geom_line(data = fit_df1, aes(x = Dose, y = exp(Fitted)), color = "red", size = 1) + 
        labs(title = paste0("DE Model: ", input$de_model, "\nCovariates: ", deparse(addCovars)),
            x = "Dose",
            y = "Exposure") +
        theme_minimal()

    annotation_text <- paste(names(pred_data_df), "=", sapply(pred_data_df, as.character), collapse = "\n")

    p_de <- p_de +
        annotate("text", x = -Inf, y = Inf, label = annotation_text,
                hjust = 0, vjust = 1, size = 4, color = "darkgreen") +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # 增加边距以容纳注释

    output$DE_plot <- renderPlot({
        print(p_de)
    })

    if (input$responseType == "Continuous") {
        p_der <- ggplot() +
            geom_point(data = df, aes(x = Dose, y = log(Response)), color = "blue", alpha = 0.5) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model, "\nDE Model: ", input$de_model,  "\nCovariates: ", deparse(addCovars)),
                x = "Dose",
                y = "log(Response)") +
            theme_minimal()

        p_er <- ggplot() +
            geom_point(data = df, aes(x = Exposure, y = log(Response)), color = "blue", alpha = 0.5) +  # 数据点
            geom_line(data = fit_df, aes(x = Exposure, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model),
                x = "Exposure",
                y = "log(Response)") +
            theme_minimal()


        output$ER_qqplot <- renderPlot({
            qqnorm(fit_er$residuals, 
            main = paste0("Q-Q plot of residuals (ER: ", input$er_model, ")")
            )
            qqline(fit_er$residuals)
        })

        output$ER_ResFitplot <- renderPlot({
            plot(log(fit_er$fitted_values), fit_er$residuals, 
            main = paste0("Residuals vs Fitted (ER: ", input$er_model, ")"), 
            xlab = "Fitted values", ylab = "Residuals")
            abline(h = 0, col = "red")
        })
    } else if (input$responseType == "Binary") {
        p_der <- ggplot() +
            geom_jitter(data = df, aes(x = Dose, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model, "\nDE Model: ", input$de_model,  "\nCovariates: ", deparse(addCovars)),
                x = "Dose",
                y = "Probability of Response") +
            theme_minimal()

        p_er <- ggplot() +
            geom_jitter(data = df, aes(x = Exposure, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) +  # 数据点
            geom_line(data = fit_df, aes(x = Exposure, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model),
                x = "Exposure",
                y = "Probability of Response") +
            theme_minimal()


        roc_obj <- roc(df$Response, fit_er$fitted_values, percent = TRUE, direction = "<")

        output$ER_AUC_text <- renderText({
            paste("AUC:", round(auc(roc_obj), 3))
        })

        output$ER_ROCplot <- renderPlot({
            plot(roc_obj, 
            main = paste0("ROC curve", " (ER: ", input$er_model, ")")
            )
        })
    }

    p_der <- p_der +
        annotate("text", x = -Inf, y = Inf, label = annotation_text,
                hjust = 0, vjust = 1, size = 4, color = "darkgreen") +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # 增加边距以容纳注释


    output$DE_qqplot <- renderPlot({
        qqnorm(fit_de$residuals,
        main = paste0("Q-Q plot of residuals (DE: ", input$de_model, ")\nCovariates: ", deparse(addCovars)),
        )
        qqline(fit_de$residuals)
    })

    output$DE_ResFitplot <- renderPlot({
        plot(fitted(fit_de), fit_de$residuals,
        main = paste0("Residuals vs Fitted (DE: ", input$de_model, ")\nCovariates: ", deparse(addCovars)),
        xlab = "Fitted values", ylab = "Residuals")
        abline(h = 0, col = "red")
    })


    output$ER_plot <- renderPlot({
        print(p_er)
    })

    output$DER_plot <- renderPlot({
        print(p_der)
    })
}

