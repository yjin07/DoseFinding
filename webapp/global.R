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


generate_data <- function() {
  placebo <- data.frame(Dose = "Placebo", AUC24 = rep(0, 200))
  dose1 <- data.frame(Dose = "150 mg", AUC24 = rlnorm(200, meanlog = log(10), sdlog = log(1.5)))
  dose2 <- data.frame(Dose = "300 mg", AUC24 = rlnorm(200, meanlog = log(20), sdlog = log(1.5)))
  dose3 <- data.frame(Dose = "600 mg", AUC24 = rlnorm(200, meanlog = log(40), sdlog = log(1.5)))
  
  exposure_data <- rbind(placebo, dose1, dose2, dose3) %>%
    mutate(z = .035 * AUC24, pr = 1 / (1 + exp(-z)) - 0.3, resp = rbinom(800, 1, pr))

  exposure_data <- exposure_data[, -c(3:4)]
  list(exposure_data = exposure_data)
}

process_data <- function(exposure_data, levels = c("Placebo", "150 mg", "300 mg", "600 mg"), n_quantile = 4) {
  exposure_data$Dose <- factor(exposure_data$Dose, levels = levels)
  
  exposure_data_placebo <- filter(exposure_data, Dose == "Placebo")
  exposure_data_active <- filter(exposure_data, Dose != "Placebo")
  
  step <- 1 / n_quantile
  quantiles <- seq(0, 1, step)
  
  breaks <- quantile(exposure_data_active$AUC24, quantiles)
  
  exposure_data_active <- exposure_data_active %>%
    mutate(quantile = cut(AUC24, breaks, include.lowest = TRUE))
  
  exposure_data_placebo <- exposure_data_placebo %>%
    mutate(quantile = "Placebo")
  
  exposure_data_all <- rbind(exposure_data_placebo, exposure_data_active)
  
  exposure_data_all$quantile <- factor(exposure_data_all$quantile,
                                       levels = c("Placebo", levels(cut(exposure_data_active$AUC24, breaks, include.lowest = TRUE))))
  
  er_summary <- exposure_data_all %>%
    group_by(quantile) %>%
    summarise(median_AUC = median(AUC24),
              n = n(),
              resp_n = sum(resp),
              resp_perc = sum(resp) / n() * 100,
              placebo = quantile[1] == "Placebo",
              ci = binom.confint(sum(resp), n(), conf.level = 0.95, methods = "exact"))
  
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
                  aes(x = AUC24,y =resp, color = Dose ),
                  height = 0.05,
                  alpha = 0.5) +
      geom_smooth(data = exposure_data_all,
                  aes(x = AUC24,y =resp ),
                  color = "grey10",
                  method = "glm",
                  method.args = list(family = "binomial")) +
      labs(x= "AUC (ng/ml*hr)", y = "Probability of Response") +
      scale_x_continuous(limits= c(-1, 125))
    
    # Summary plot of exposures
    exposure_plot <- ggplot(exposure_data,
                            aes(x = AUC24, y = Dose, fill = Dose, color = Dose)) +
      stat_boxplot(geom = "errorbar", width = 0.5) +
      geom_boxplot(outlier.colour = NA, width = 0.7,
                   alpha = 0.5) +
      scale_x_continuous(limits= c(-1, 125)) +
      labs(x= "AUC (ng/ml*hr)", y = "Dose Group") +
      guides(color = "none", fill = "none")
    
    # Combining ER Plot and exposure summary plot
    p2 <- er_plot / exposure_plot + plot_layout(heights = c(4, 3))
    list(p1 = p1, p2 = p2)
}