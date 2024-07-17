
# ? ----------------------------------
# ? Real Data Section
# ? ----------------------------------
generate_data_real <- function() {
  e0 <- logit(0.1)
  eMax <- 3
  h <- 4
  EC50 <- 5
  sigma_c <- 0.5
  sigma_y <- 0.2
  TVCL <- 5                                       # ? Typical Value of Clearance
  beta0 <- -log(TVCL)
  beta1 <- 0.85
  doses <- c(20, 30, 48, 60, 80, 100, 110)        # ? Do not consider the placebo
  reps <- c(3, 3, 6, 8, 12, 18, 10)
  ds <- rep(doses, reps)

  logC <- beta0 + beta1 * log(ds) + rnorm(sum(reps), 0, sigma_c)
  CC <- round(exp(logC), 3)
  logitP <- e0 + eMax / (1 + (EC50 / CC)^h)       # ? True model: sigEmax
  Ps <- inv_logit(logitP)
  Y <- rbinom(sum(reps), 1, Ps)

  df <- data.frame(Dose = ds, Exposure = CC, Response = Y)
}

process_data_bin <- function(der_data, n_quantile = 4) {
  der_data_placebo <- filter(der_data, Dose == 0)
  der_data_active <- filter(der_data, Dose != 0)

  n_quantile <- length(levels(factor(der_data$Dose)))
  
  step <- 1 / n_quantile
  quantiles <- seq(0, 1, step)
  breaks <- quantile(der_data_active$Exposure, quantiles)
  
  der_data_active <- der_data_active %>%
    mutate(quantile = cut(Exposure, breaks, include.lowest = TRUE))
  
  der_data_placebo <- der_data_placebo %>%
    mutate(quantile = "Placebo")
  
  der_data_all <- rbind(der_data_placebo, der_data_active)
  
  der_data_all$quantile <- factor(der_data_all$quantile,
                                      levels = c("Placebo", levels(cut(der_data_active$Exposure, breaks, include.lowest = TRUE))))
  
  er_summary <- der_data_all %>%
    group_by(quantile) %>%
    summarise(median_AUC = median(Exposure),
              n = n(),
              resp_n = sum(Response),
              resp_perc = sum(Response) / n() * 100,
              placebo = quantile[1] == "Placebo",
              ci = binom.confint(sum(Response), n(), conf.level = 0.95, methods = "exact"))
  
  list(der_data_all = der_data_all, er_summary = er_summary, n_quantile = n_quantile)
}

process_data_cont <- function(der_data, n_quantile = 4) {
  der_data_placebo <- filter(der_data, Dose == 0)
  der_data_active <- filter(der_data, Dose != 0)

  n_quantile <- length(levels(factor(der_data$Dose)))
  
  step <- 1 / n_quantile
  quantiles <- seq(0, 1, step)
  breaks <- quantile(der_data_active$Exposure, quantiles)
  
  der_data_active <- der_data_active %>%
    mutate(quantile = cut(Exposure, breaks, include.lowest = TRUE))
  
  der_data_placebo <- der_data_placebo %>%
    mutate(quantile = "Placebo")
  
  der_data_all <- rbind(der_data_placebo, der_data_active)
  
  der_data_all$quantile <- factor(der_data_all$quantile,
                                      levels = c("Placebo", levels(cut(der_data_active$Exposure, breaks, include.lowest = TRUE))))

  er_summary <- der_data_all %>%
    group_by(quantile) %>%
    summarise(median_AUC = median(Exposure),
              n = n(),
              mean_resp = mean(Response),           # 计算 Response 的平均值
              sd_resp = sd(Response),               # 计算 Response 的标准差
              se_resp = sd(Response) / sqrt(n()),   # 计算标准误
              lower_ci = mean_resp - qt(0.975, n() - 1) * se_resp, # 计算95%置信区间的下限
              upper_ci = mean_resp + qt(0.975, n() - 1) * se_resp, # 计算95%置信区间的上限
              placebo = quantile[1] == "Placebo")

  list(der_data_all = der_data_all, er_summary = er_summary, n_quantile = n_quantile)
}


generate_plots_bin <- function(der_data, der_data_all, er_summary, n_quantile) {
    # Exposure Response Quantile Plot
    p1 <- ggplot(er_summary, aes(x = quantile, y = resp_perc)) +
      geom_col(aes(fill = placebo), alpha = 0.6) +
      geom_text(aes(label = paste0("(",resp_n,"/", n, ")")), vjust = -0.5, color = "grey30", size = 3,
                position = position_dodge(width = 0.9)) +
      geom_text(aes(label = paste0(round(resp_perc,1),"%")), vjust = -1.9, color = "grey30", size = 3,
                position = position_dodge(width = 0.9)) +
      labs(x = "AUC Exposure Quantile (ng/ml*hr)",
          y = "Percent Responders (%)",
          # title = "Exposure-Response Quantile Plot",
          subtitle = paste(n_quantile, "Bins") ) +
      scale_y_continuous(limits = c(0, 100)) +
      scale_fill_manual(values = c( "purple4", "grey20")) +
      guides(fill = "none")
    
    # Exposure Response Logistic Regression Plot
    er_plot <- ggplot(er_summary, aes(x = median_AUC, y = resp_perc/100)) +
      geom_point() +
      geom_errorbar(aes(ymax = ci$upper, ymin = ci$lower)) +
      geom_jitter(data = der_data_all,
                  aes(x = Exposure,y = Response, color = factor(Dose)),
                  height = 0.05,
                  alpha = 0.5) +
      geom_smooth(data = der_data_all,
                  aes(x = Exposure,y = Response),
                  color = "grey10",
                  method = "glm",
                  method.args = list(family = "binomial")) +
      labs(x= "AUC (ng/ml*hr)", y = "Probability of Response", color = "Dose Level") +
      scale_x_continuous(limits= c(-1, ceiling(max(der_data$Exposure) * 1.5 / 10) * 10))
    
    # Summary plot of exposures
    exposure_plot <- ggplot(der_data,
                            aes(x = Exposure, y = factor(Dose), fill = factor(Dose), color = factor(Dose))) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      geom_boxplot(outlier.colour = NA, width = 0.7, alpha = 0.5) +
      scale_x_continuous(limits= c(-1, ceiling(max(der_data$Exposure) * 1.5 / 10) * 10)) +
      labs(x= "AUC (ng/ml*hr)", y = "Dose Group") +
      guides(color = "none", fill = "none")
    
    # Combining ER Plot and exposure summary plot
    p2 <- er_plot / exposure_plot + plot_layout(heights = c(4, 3))
    list(p1 = p1, p2 = p2)
}

generate_plots_cont <- function(der_data, der_data_all, er_summary, n_quantile) {
    # Exposure Response Quantile Plot
    p1 <- ggplot(er_summary, aes(x = quantile, y = mean_resp)) +
      geom_col(aes(fill = placebo), alpha = 0.6) +
      geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci), width = 0.2) +
      geom_text(aes(label = paste0("n=", n)), vjust = -0.5, color = "grey30", size = 3,
                position = position_dodge(width = 0.9)) +
      geom_text(aes(label = round(mean_resp, 2)), vjust = -1.9, color = "grey30", size = 3,
                position = position_dodge(width = 0.9)) +
      labs(x = "AUC Exposure Quantile (ng/ml*hr)",
          y = "Mean Response",
          subtitle = paste(n_quantile, "Bins")) +
      scale_y_continuous() +
      scale_fill_manual(values = c("purple4", "grey20")) +
      guides(fill = "none")
    
    # Exposure Response Plot with Continuous Response
    er_plot <- ggplot(er_summary, aes(x = median_AUC, y = mean_resp)) +
      geom_point() +
      geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci), width = 0.2) +
      geom_point(data = der_data_all,
                  aes(x = Exposure, y = Response, color = factor(Dose)),
                  alpha = 0.5) +
      geom_smooth(data = der_data_all,
                  aes(x = Exposure, y = Response),
                  color = "grey10",
                  method = "lm") +
      labs(x = "AUC (ng/ml*hr)", y = "Response", color = "Dose Level") +
      scale_x_continuous(limits = c(-1, ceiling(max(der_data$Exposure) * 1.5 / 10) * 10))
    
    # Summary plot of exposures
    exposure_plot <- ggplot(der_data,
                            aes(x = Exposure, y = factor(Dose), fill = factor(Dose), color = factor(Dose))) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      geom_boxplot(outlier.colour = NA, width = 0.7, alpha = 0.5) +
      scale_x_continuous(limits = c(-1, ceiling(max(der_data$Exposure) * 1.5 / 10) * 10)) +
      labs(x = "AUC (ng/ml*hr)", y = "Dose Group") +
      guides(color = "none", fill = "none")
    
    # Combining ER Plot and exposure summary plot
    p2 <- er_plot / exposure_plot + plot_layout(heights = c(4, 3))
    list(p1 = p1, p2 = p2)
}


generate_and_render_real_data <- function(output, data = NULL, input = NULL) {
  if (is.null(data)) {
    set.seed(1000)
    der_data <- generate_data_real()    # Generate data with Binary response by default
  } else {
    der_data <- data
  }
  
  output$exposure_table_real <- renderDT({
    datatable(der_data)
  })

  output$data_preview <- renderTable({
    head(der_data, 10)
  })

  output$data_description <- renderPrint({
    paste("This is a description of the data.")
    der_data1 <- der_data
    der_data1$Dose <- factor(der_data1$Dose)
    summary(der_data1)
  })

  if (is.null(input) || input$responseType == "Binary") {
    processed_data <- process_data_bin(der_data)
    der_data_all <- processed_data$der_data_all
    er_summary <- processed_data$er_summary
    n_quantile <- processed_data$n_quantile
    descript_plot <- generate_plots_bin(der_data, der_data_all, er_summary, n_quantile)
  } else {
    processed_data <- process_data_cont(der_data)
    der_data_all <- processed_data$der_data_all
    er_summary <- processed_data$er_summary
    n_quantile <- processed_data$n_quantile
    descript_plot <- generate_plots_cont(der_data, der_data_all, er_summary, n_quantile)
  }
  
  output$plot1_real <- renderPlot({
    print(descript_plot$p1)
  })
  output$plot2_real <- renderPlot({
    print(descript_plot$p2)
  })
}