get_tgi_results <- function(df, input, output, selected = NULL) {
    output$tgi_data_preview <- renderTable({
        head(df, 10)
    })

    fit <- nlme(
        Y ~ Y0 * (exp(g * TIME) + exp(-d * TIME) - 1) + bv,
        data = df,
        fixed = list(g ~ 1, d ~ 1),
        random = bv ~ 1 | SUBJID,
        start = c(g = 0.1, d = 1)
    )
    
    output$tgi_fit_summary <- renderPrint({
        summary(fit)
    })

    output$download_tgi_summary <- downloadHandler(
        filename = function() {
            paste("tgi_fit_summary", ".txt", sep = "")
        },
        content = function(file) {
            writeLines(capture.output(summary(fit)), file)
        }
    )

    output$tgi_fit_plot <- renderPlot({
        if (is.null(selected)) {
            selected <- sample(1:length(unique(df$SUBJID)), 5)
        }

        # 提取所选数据点的Y值
        selected_Y_values <- unlist(lapply(selected, function(id) subset(df, SUBJID == id)$Y))

        # 计算Y轴范围
        y_range <- range(selected_Y_values)

        col_vals <- 1:length(selected)

        ii <- 1
        plot(subset(df, SUBJID == selected[ii])$TIME, subset(df, SUBJID == selected[ii])$Y, type = "p", xlab = "Time", ylab = "Y", pch = 16, col = ii, ylim = y_range,
        main = "Selected subjects with data points and fitted lines")
        lines(subset(df, SUBJID == selected[ii])$TIME, predict(fit, newdata = subset(df, SUBJID == selected[ii])), col = col_vals[ii])


        for (ii in 2:length(selected)) {
            points(subset(df, SUBJID == selected[ii])$TIME, subset(df, SUBJID == selected[ii])$Y, type = "p", pch = 16, col = col_vals[ii])
            lines(subset(df, SUBJID == selected[ii])$TIME, predict(fit, newdata = subset(df, SUBJID == selected[ii])), col = col_vals[ii])
        }

        if (length(selected) < 10) {
            legend("topleft", legend = paste("SUBJID", selected), pch = 16, col = col_vals)
        }
        
    })

    output$tgi_resFitted <- renderPlot({
        plot(fit, main = "Standardized residuals v.s. Fitted values", col = "black", pch = 16)
    })

    output$tgi_qqplot <- renderPlot({
        qqnorm(fit$residuals, main = "Normal Q-Q plot", pch = 16)
        qqline(fit$residuals)
    })

}