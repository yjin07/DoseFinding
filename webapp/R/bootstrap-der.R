get_der_bootstrap <- function(df, input, output) {
    n_bootstrap <- input$n_bootstrap
    type <- ifelse(input$responseType == "Continuous", "gaussian", "binomial")
    fit_der <- fitDERMod(df$Dose, df$Exposure, df$Response, model = input$er_model, type = type)
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
            geom_point(data = df, aes(x = Dose, y = log(Response)), color = "orange", alpha = 0.5) +  # 数据点
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
}