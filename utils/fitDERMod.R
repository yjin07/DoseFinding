fitDERMod <- function(dose, exposure, resp, model = NULL, data = NULL,
                    type = c("gaussian", "binomial"), 
                    addCovar = ~1, addArgs = NULL) {

    if (!model %in% names(model_info)) {
        stop("Invalid model specified.")
    }

    if (!inherits(addCovar, "formula")) {
        stop("addCovar must be a formula.")
    }

    if (addCovar != ~1) {
        covar_vars <- all.vars(addCovar)
        if (!all(covar_vars %in% names(data))) {
            stop("Some variables in addCovar are not present in the data.")
        }
    }

    fit1 <- fitERMod(exposure, resp, model = model, type = type, addArgs = addArgs)

    if (addCovar != ~1) {
        formula <- as.formula(paste("log(exposure) ~ log(dose) + ", as.character(addCovar)[2]))
        fit2 <- lm(formula, data = data)
    } else {
        fit2 <- lm(log(exposure) ~ log(dose))
    }

    sigma_c <- sqrt(sum(fit2$residuals^2) / fit2$df.residual)
    output <- list(fit1 = fit1, fit2 = fit2, sigma_c = sigma_c, type = type)

    class(output) <- "DERMod"
    return(output)
}


# * -------------------------------------------------
# * In this predict method, the response of continuous
# * case is already in log scale because of the mean 
# * are calculated in log scale.
# * -------------------------------------------------
predict.DERMod <- function(object, newdata, n = 1e4, type = "response") {
    fit1 <- object$fit1   # * ERMod object
    fit2 <- object$fit2   # * lm object

    # 确保 newdata 是一个数据框
    newdata <- data.frame(dose = newdata)

    # 初始化响应值的向量
    resp <- numeric(nrow(newdata))
    
    # 对每个 newdata 逐一处理
    for (i in 1:nrow(newdata)) {
        logC <- predict(fit2, newdata = newdata[i, , drop = FALSE])
        logCs <- logC + object$sigma_c * rnorm(n)
        CCs <- exp(logCs)

        if (object$fit1$type == "binomial") {
            resp[i] <- mean(logit(predict(fit1, newdata = CCs)))
        } else if (object$fit1$type == "gaussian") {
            resp[i] <- mean(predict(fit1, newdata = CCs))
        }
    }

    return(resp)
}
