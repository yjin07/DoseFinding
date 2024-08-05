get_der_bootstrap <- function(df, input, output) {
    n_bootstrap <- input$n_bootstrap_der
    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_der <- fitDERMod(df$Dose, df$Exposure, df$Response, model = input$er_model, type = type)
    new_doses <- seq(min(df$Dose), max(df$Dose), by = 2)

    if (!input$doi_der=="") {
        doi <- as.numeric(input$doi_der)
        if (!doi %in% new_doses) {
            new_doses <- sort(c(new_doses, doi))
        }
    }


    fitted_values2 <- predict(fit_der, newdata = new_doses, type = "response")

    if (input$responseType == "Continuous") {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values2)
    } else {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))
    }

    log_info("Bootstrap started with sampling method:", input$sampling_method_der)
    fitted_vals_bootstrap <- matrix(NA, nrow = n_bootstrap, ncol = length(new_doses))

    withProgress(message = "Run Bootstrap replicates", value = 0, {
        for (jj in 1:n_bootstrap) {
            ind <- bootstrap_indices(df, method = input$sampling_method)
            fit_der0 <- fitDERMod(df$Dose[ind], df$Exposure[ind], df$Response[ind], model = input$er_model, type = type)
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

    q_lower <- (1 - input$conf_lvl1_der) / 2
    ci_low <- apply(fitted_vals_bootstrap, 2, function(x) quantile(x, q_lower))
    ci_high <- apply(fitted_vals_bootstrap, 2, function(x) quantile(x, 1 - q_lower))

    # 将置信区间添加到拟合数据框
    fit_df2$CI_low <- ci_low
    fit_df2$CI_high <- ci_high

    if (!input$doi_der=="") {
        doi_ind <- which(new_doses == doi)
        doi_mean <- fit_df2$Fitted[doi_ind]
        doi_ci_low <- fit_df2$CI_low[doi_ind]
        doi_ci_high <- fit_df2$CI_high[doi_ind]
        log_info("DOI:", doi, "Mean:", doi_mean, "CI_low:", doi_ci_low, "CI_high:", doi_ci_high)
        output$doi_res <- renderText({
            paste("Selected dose level:", doi, "\nMean:", round(doi_mean, 2), "\nCI lower:", round(doi_ci_low, 2), "\nCI upper:", round(doi_ci_high, 2))
        })
    }

    valid_dr <- complete.cases(df$Dose, df$Response)
    df_valid <- df[valid_dr, ]


    if (input$responseType == "Continuous") {
        df_summary <- df_valid %>%
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
            geom_point(data = df, aes(x = Dose, y = log(Response)), color = "orange", alpha = 0.5) +  # 数据点
            geom_point(data = df_summary, aes(x = Dose, y = mean_log_response), color = "black", size = 2) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            geom_ribbon(data = fit_df2, aes(x = Dose, ymin = CI_low, ymax = CI_high), alpha = 0.2, fill = "grey") +  # 置信区间
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model, "\nDE Model: ", input$de_model,  "\nNo Covariates"),
                x = "Dose",
                y = "log(Response)"
            ) +
            theme_minimal()
    } else {
        df_summary <- df_valid %>%
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
            labs(title = paste0("Response: ", type, "\nER Model: ", input$er_model, "\nDE Model: ", input$de_model,  "\nNo Covariates"),
                x = "Dose",
                y = "Probability of Response"
            ) +
            theme_minimal()
    }

    output$DER_bootstrapPlot <- renderPlot({
        print(p_der0)
    })
}



get_der_bootstrap_withCovars <- function(df, input, output, addCovars, pred_data_df) {
    n_bootstrap <- input$n_bootstrap_der
    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_der <- fitDERMod(df$Dose, df$Exposure, df$Response, data = df, model = input$er_model, type = type, addCovars = addCovars)

    valid_dose <- complete.cases(df$Dose)
    new_doses <- seq(min(df$Dose[valid_dose]), max(df$Dose[valid_dose]), by = 2)

    if (!input$doi_der=="") {
        doi <- as.numeric(input$doi_der)
        if (!doi %in% new_doses) {
            new_doses <- sort(c(new_doses, doi))
        }
    }

    pred_df <- pred_data_df[rep(1, length(new_doses)), , drop = FALSE]
    pred_df$dose <- new_doses
    fitted_values2 <- predict(fit_der, newdata = pred_df, type = "response")

    if (input$responseType == "Continuous") {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values2)
    } else {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))
    }

    log_info("Bootstrap started with sampling method:", input$sampling_method_der)
    fitted_vals_bootstrap <- matrix(NA, nrow = n_bootstrap, ncol = length(new_doses))

    withProgress(message = "Run Bootstrap replicates", value = 0, {
        for (jj in 1:n_bootstrap) {
            ind <- bootstrap_indices(df, method = input$sampling_method)
            fit_der0 <- fitDERMod(df$Dose[ind], df$Exposure[ind], df$Response[ind], model = input$er_model, type = type)
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

    q_lower <- (1 - input$conf_lvl1_der) / 2
    ci_low <- apply(fitted_vals_bootstrap, 2, function(x) quantile(x, q_lower))
    ci_high <- apply(fitted_vals_bootstrap, 2, function(x) quantile(x, 1 - q_lower))

    # 将置信区间添加到拟合数据框
    fit_df2$CI_low <- ci_low
    fit_df2$CI_high <- ci_high

    if (!input$doi_der=="") {
        doi_ind <- which(new_doses == doi)
        doi_mean <- fit_df2$Fitted[doi_ind]
        doi_ci_low <- fit_df2$CI_low[doi_ind]
        doi_ci_high <- fit_df2$CI_high[doi_ind]
        log_info("DOI:", doi, "Mean:", doi_mean, "CI_low:", doi_ci_low, "CI_high:", doi_ci_high)
        output$doi_res <- renderText({
            paste("Selected dose level:", doi, "\nMean:", round(doi_mean, 2), "\nCI lower:", round(doi_ci_low, 2), "\nCI upper:", round(doi_ci_high, 2))
        })
    }

    valid_dr <- complete.cases(df$Dose, df$Response)
    df_valid <- df[valid_dr, ]

    annotation_text <- paste(names(pred_data_df), "=", sapply(pred_data_df, as.character), collapse = "\n")

    if (input$responseType == "Continuous") {
        df_summary <- df_valid %>%
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
            geom_point(data = df, aes(x = Dose, y = log(Response)), color = "orange", alpha = 0.5) +  # 数据点
            geom_point(data = df_summary, aes(x = Dose, y = mean_log_response), color = "black", size = 2) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            geom_ribbon(data = fit_df2, aes(x = Dose, ymin = CI_low, ymax = CI_high), alpha = 0.2, fill = "grey") +  # 置信区间
            labs(
                title = paste0("Response: ", type, 
                                "\nER Model: ", input$er_model, 
                                "\nDE Model: ", input$de_model,  "\nCovariates: ", deparse(addCovars),
                                "\nConfidence Level: ", input$conf_lvl1_der),
                x = "Dose",
                y = "log(Response)"
            ) +
            theme_minimal()
    } else {
        df_summary <- df_valid %>%
        group_by(Dose) %>%
        summarise(
            n = n(),
            resp = sum(Response),
            prob = mean(Response),
            ci_low = binom.confint(resp, n, methods = "wilson")$lower,
            ci_high = binom.confint(resp, n, methods = "wilson")$upper
        )

        p_der0 <- ggplot() +
            geom_errorbar(data = df_summary, aes(x = Dose, ymin = ci_low, ymax = ci_high), width = 0.2) +  # Error bars
            geom_jitter(data = df_valid, aes(x = Dose, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) + 
            geom_point(data = df_summary, aes(x = Dose, y = prob), color = "black", size = 2) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            geom_ribbon(data = fit_df2, aes(x = Dose, ymin = CI_low, ymax = CI_high), alpha = 0.2, fill = "grey") + 
            labs(
                title = paste0("Response: ", type, 
                                "\nER Model: ", input$er_model, 
                                "\nDE Model: ", input$de_model,  "\nCovariates: ", deparse(addCovars),
                                "\nConfidence Level: ", input$conf_lvl1_der),
                x = "Dose",
                y = "Probability of Response"
            ) +
            theme_minimal()
    }

    p_der0 <- p_der0 +
        annotate("text", x = -Inf, y = Inf, label = annotation_text,
                hjust = 0, vjust = 1, size = 4, color = "darkgreen") +
        theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))  # 增加边距以容纳注释

    output$DER_bootstrapPlot <- renderPlot({
        print(p_der0)
    })
}