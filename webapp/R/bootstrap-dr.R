get_dr_bootstrap <- function(df, input, output) {
    n_bootstrap <- input$n_bootstrap_dr
    valid_dr <- complete.cases(df$Dose, df$Response)
    df_valid <- df[valid_dr, ]
    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_dr <- fitERMod(df_valid$Dose, df_valid$Response, model = input$dr_model, type = type)
    new_doses <- seq(min(df_valid$Dose), max(df_valid$Dose), by = 2)
    
    if (!input$doi_dr=="") {
        doi <- as.numeric(input$doi_dr)
        if (!doi %in% new_doses) {
            new_doses <- sort(c(new_doses, doi))
        }
    }

    fitted_values <- predict(fit_dr, newdata = new_doses, type = "response")

    if (input$responseType == "Continuous") {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values)
    } else {
        fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values))
    }

    log_info("Bootstrap started with sampling method:", input$sampling_method_dr)
    fitted_vals_bootstrap <- matrix(NA, nrow = n_bootstrap, ncol = length(new_doses))

    withProgress(message = "Run Bootstrap replicates", value = 0, {
        for (jj in 1:n_bootstrap) {
            ind <- bootstrap_indices(df_valid, method = input$sampling_method)
            fit_dr0 <- fitERMod(df_valid$Dose[ind], df_valid$Response[ind], model = input$dr_model, type = type)
            if (input$responseType == "Continuous") {
                fitted_vals_bootstrap[jj, ] <- predict(fit_dr0, newdata = new_doses, type = "response")
            } else {
                fitted_vals_bootstrap[jj, ] <- inv_logit(predict(fit_dr0, newdata = new_doses, type = "response"))
            }

            incProgress(1/n_bootstrap, detail = paste("\nBootstrap sample", jj))
        }
    })


    fitted_vals_bootstrap <- na.omit(fitted_vals_bootstrap)
    log_info("Bootstrap finished...")
    log_info("Valid bootstrap samples:", nrow(fitted_vals_bootstrap))

    q_lower <- (1 - input$conf_lvl1_dr) / 2
    ci_low <- apply(fitted_vals_bootstrap, 2, function(x) quantile(x, q_lower))
    ci_high <- apply(fitted_vals_bootstrap, 2, function(x) quantile(x, 1 - q_lower))

    # 将置信区间添加到拟合数据框
    fit_df2$CI_low <- ci_low
    fit_df2$CI_high <- ci_high


    if (!input$doi_dr=="") {
        doi_ind <- which(new_doses == doi)
        doi_mean <- fit_df2$Fitted[doi_ind]
        doi_ci_low <- fit_df2$CI_low[doi_ind]
        doi_ci_high <- fit_df2$CI_high[doi_ind]
        log_info("DOI:", doi, " Mean:", round(doi_mean, 5), " CI_low:", round(doi_ci_low, 5), " CI_high:", round(doi_ci_high, 5))
        output$doi_res_dr <- renderText({
            paste("Selected dose level:", doi, "\nMean:", round(doi_mean, 2), "\nCI lower:", round(doi_ci_low, 2), "\nCI upper:", round(doi_ci_high, 2))
        })
    }

    if (input$responseType == "Continuous") {
        df_summary <- df_valid %>%
            group_by(Dose) %>%
            summarise(
                n = n(),
                mean_response = mean(Response),
                sd_response = sd(Response),
                se_response = sd_response / sqrt(n),
            ci_low = mean_response - qt(0.975, df = n - 1) * se_response,
            ci_high = mean_response + qt(0.975, df = n - 1) * se_response
            )

        p_dr0 <- ggplot() +
            geom_errorbar(data = df_summary, aes(x = Dose, ymin = ci_low, ymax = ci_high), width = 0.2) +  # Error bars
            geom_point(data = df_valid, aes(x = Dose, y = Response), color = "orange", alpha = 0.5) +  # 数据点
            geom_point(data = df_summary, aes(x = Dose, y = mean_response), color = "black", size = 2) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            geom_ribbon(data = fit_df2, aes(x = Dose, ymin = CI_low, ymax = CI_high), alpha = 0.2, fill = "grey") +  # 置信区间
            labs(title = paste0("Reponse: ", type, "\nDR Model: ", input$dr_model, "\nNo Covariates"),
                x = "Dose",
                y = "Response"
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

        p_dr0 <- ggplot() +
            geom_errorbar(data = df_summary, aes(x = Dose, ymin = ci_low, ymax = ci_high), width = 0.2) +  # Error bars
            geom_jitter(data = df_valid, aes(x = Dose, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) + 
            geom_point(data = df_summary, aes(x = Dose, y = prob), color = "black", size = 2) +  # 数据点
            geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
            geom_ribbon(data = fit_df2, aes(x = Dose, ymin = CI_low, ymax = CI_high), alpha = 0.2, fill = "grey") + 
            labs(title = paste0("Reponse: ", type, "\nDR Model: ", input$dr_model, "\nNo Covariates"),
                x = "Dose",
                y = "Probability of Response"
            ) +
            theme_minimal()
    }

    output$DR_bootstrapPlot <- renderPlot({
        print(p_dr0)
    })
}