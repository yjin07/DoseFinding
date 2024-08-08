get_tgi_results <- function(df, input, output) {
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

    output$tgi_fit_plot <- renderPlot({
        selected <- sample(1:length(unique(df$SUBJID)), 5)

        # 提取所选数据点的Y值
        selected_Y_values <- unlist(lapply(selected, function(id) subset(df, SUBJID == id)$Y))

        # 计算Y轴范围
        y_range <- range(selected_Y_values)

        ii <- 1
        plot(subset(df, SUBJID == selected[ii])$TIME, subset(df, SUBJID == selected[ii])$Y, type = "p", xlab = "Time", ylab = "Y", pch = 15, col = ii, ylim = y_range,
        main = "Selected subjects with data points and fitted lines")
        lines(subset(df, SUBJID == selected[ii])$TIME, predict(fit, newdata = subset(df, SUBJID == selected[ii])), col = ii)


        for (ii in 2:length(selected)) {
            points(subset(df, SUBJID == selected[ii])$TIME, subset(df, SUBJID == selected[ii])$Y, type = "p", pch = 14 + ii, col = ii)
            lines(subset(df, SUBJID == selected[ii])$TIME, predict(fit, newdata = subset(df, SUBJID == selected[ii])), col = ii)
        }
        legend("topleft", legend = paste("SUBJID", selected), pch = 15:19, col = 1:5)
    })

    output$tgi_resFitted <- renderPlot({
        plot(fit, main = "Standardized residuals v.s. Fitted values", col = "black", pch = 16)
    })
    
}