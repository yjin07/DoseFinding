library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(binom)
library(patchwork)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
source("../utils/fitERMod.R")


# ? ----------------------------------
# ? Simulate Data Section
# ? ----------------------------------
generate_data <- function() {
  placebo <- data.frame(Dose = "Placebo", Exposure = rep(0, 200))
  dose1 <- data.frame(Dose = "150 mg", Exposure = rlnorm(200, meanlog = log(10), sdlog = log(1.5)))
  dose2 <- data.frame(Dose = "300 mg", Exposure = rlnorm(200, meanlog = log(20), sdlog = log(1.5)))
  dose3 <- data.frame(Dose = "600 mg", Exposure = rlnorm(200, meanlog = log(40), sdlog = log(1.5)))
  
  exposure_data <- rbind(placebo, dose1, dose2, dose3) %>%
    mutate(z = .035 * Exposure, pr = 1 / (1 + exp(-z)) - 0.3, Response = rbinom(800, 1, pr))

  exposure_data <- exposure_data[, -c(3:4)]
  colnames(exposure_data) <- c("Dose", "Exposure", "Response")
  list(exposure_data = exposure_data)
}

process_data <- function(exposure_data, levels = c("Placebo", "150 mg", "300 mg", "600 mg"), n_quantile = 4) {
  exposure_data$Dose <- factor(exposure_data$Dose, levels = levels)
  
  exposure_data_placebo <- filter(exposure_data, Dose == "Placebo")
  exposure_data_active <- filter(exposure_data, Dose != "Placebo")
  
  step <- 1 / n_quantile
  quantiles <- seq(0, 1, step)
  
  breaks <- quantile(exposure_data_active$Exposure, quantiles)
  
  exposure_data_active <- exposure_data_active %>%
    mutate(quantile = cut(Exposure, breaks, include.lowest = TRUE))
  
  exposure_data_placebo <- exposure_data_placebo %>%
    mutate(quantile = "Placebo")
  
  exposure_data_all <- rbind(exposure_data_placebo, exposure_data_active)
  
  exposure_data_all$quantile <- factor(exposure_data_all$quantile,
                                      levels = c("Placebo", levels(cut(exposure_data_active$Exposure, breaks, include.lowest = TRUE))))
  
  er_summary <- exposure_data_all %>%
    group_by(quantile) %>%
    summarise(median_AUC = median(Exposure),
              n = n(),
              resp_n = sum(Response),
              resp_perc = sum(Response) / n() * 100,
              placebo = quantile[1] == "Placebo",
              ci = binom.confint(sum(Response), n(), conf.level = 0.95, methods = "exact"))
  
  list(exposure_data_all = exposure_data_all, er_summary = er_summary, n_quantile = n_quantile)
}


generate_plots <- function(exposure_data, exposure_data_all, er_summary, n_quantile) {
    # Exposure Response Quantile Plot
    p1 <- ggplot(er_summary, aes(x = quantile, y = resp_perc )) +
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
      geom_jitter(data = exposure_data_all,
                  aes(x = Exposure,y =Response, color = Dose ),
                  height = 0.05,
                  alpha = 0.5) +
      geom_smooth(data = exposure_data_all,
                  aes(x = Exposure,y =Response ),
                  color = "grey10",
                  method = "glm",
                  method.args = list(family = "binomial")) +
      labs(x= "AUC (ng/ml*hr)", y = "Probability of Response") +
      scale_x_continuous(limits= c(-1, 125))
    
    # Summary plot of exposures
    exposure_plot <- ggplot(exposure_data,
                            aes(x = Exposure, y = Dose, fill = Dose, color = Dose)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      geom_boxplot(outlier.colour = NA, width = 0.7, alpha = 0.5) +
      scale_x_continuous(limits= c(-1, 125)) +
      labs(x= "AUC (ng/ml*hr)", y = "Dose Group") +
      guides(color = "none", fill = "none")
    
    # Combining ER Plot and exposure summary plot
    p2 <- er_plot / exposure_plot + plot_layout(heights = c(4, 3))
    list(p1 = p1, p2 = p2)
}

generate_and_render_simulate_data <- function(output) {
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
}


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

process_data_real <- function(der_data, n_quantile = 4) {
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


generate_plots_real <- function(der_data, der_data_all, er_summary, n_quantile) {
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

generate_and_render_real_data <- function(output, data = NULL) {
  if (is.null(data)) {
    set.seed(1000)
    der_data <- generate_data_real()
  } else {
    der_data <- data
  }
  processed_data <- process_data_real(der_data)
  der_data_all <- processed_data$der_data_all
  er_summary <- processed_data$er_summary
  n_quantile <- processed_data$n_quantile
  
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


  random_plot <- generate_plots_real(der_data, der_data_all, er_summary, n_quantile)
  output$plot1_real <- renderPlot({
    print(random_plot$p1)
  })
  output$plot2_real <- renderPlot({
    print(random_plot$p2)
  })
}


uploadChecking <- function(input, myData) {
  if (is.null(input$file1)) {
    showModal(modalDialog(
      title = "Error",
      "No file uploaded. Please upload a file first.",
      easyClose = TRUE,
      footer = NULL
    ))
  } else {
    req(input$file1)
    ext <- tools::file_ext(input$file1$datapath)
    data_type <- input$data_type

    if (!((data_type == "csv" && ext == "csv") || 
          (data_type == "txt" && ext == "txt") ||
          (data_type == "rdss" && ext %in% c("rds", "rda", "rdata")))) {
        showModal(modalDialog(
          title = "Error",
          "File type does not match the selected data type. Please upload a file with the correct extension.",
          easyClose = TRUE,
          footer = NULL
        ))
    } 
    
    df <- NULL
    if (data_type == "csv" && ext == "csv") {
      df <- read.csv(input$file1$datapath, header = TRUE)
    } else if (data_type == "txt" && ext == "txt") {
      df <- read.table(input$file1$datapath, header = TRUE, sep = "\t")
    } else if (data_type == "rdss" && ext %in% c("rds", "rda", "rdata")) {
      if (ext == "rds") {
        df <- readRDS(input$file1$datapath)
      } else if (ext %in% c("rda", "rdata")) {
        load(input$file1$datapath)
        df <- get(ls()[ls() != "file1"])  # 假设只加载一个对象
      }
    }

    if (all(df$Response %in% c(0,1)) && input$responseType != "Binary" || 
        !all(df$Response %in% c(0,1)) && input$responseType != "Continuous") {
      showModal(modalDialog(
        title = "Error",
        "Response type does not match the uploaded data. Please select the correct response type.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Success",
        "File uploaded successfully.",
        easyClose = TRUE,
        footer = NULL
      ))
      myData(df)
    }
  }
}
