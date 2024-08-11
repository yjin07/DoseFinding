fitDERMod <- function(dose, exposure, resp, data = NULL, model = NULL, 
                    type = c("gaussian", "binomial"), 
                    addCovars = ~1, addArgs = NULL) {

    if (!model %in% names(model_info)) {
        stop("Invalid model specified.")
    }

    if (!inherits(addCovars, "formula")) {
        stop("addCovars must be a formula.")
    }

    valid_er <- complete.cases(exposure, resp)
    exposure_valid <- exposure[valid_er]
    resp_valid <- resp[valid_er]

    fit1 <- fitERMod(exposure_valid, resp_valid, model = model, type = type, addArgs = addArgs)    

    if (addCovars != ~1) {
        covar_vars <- all.vars(addCovars)
        if (!all(covar_vars %in% names(data))) {
            stop("Some variables in addCovars are not present in the data.")
        }
    }


    if (addCovars != ~1) {
        covar_vars <- all.vars(addCovars)
        if (!all(covar_vars %in% names(data))) {
            stop("Some variables in addCovars are not present in the data.")
        }
        valid_de <- complete.cases(dose, exposure, data[, covar_vars, drop = FALSE])
    } else {
        valid_de <- complete.cases(dose, exposure)
    }

    dose <- dose[valid_de]
    exposure <- exposure[valid_de]

    if (addCovars != ~1) {
        formula <- as.formula(paste("log(exposure) ~ log(dose) + ", as.character(addCovars)[2]))
        fit2 <- lm(formula, data = data[valid_de, , drop = FALSE])
    } else {
        fit2 <- lm(log(exposure) ~ log(dose))
    }

    sigma_c <- sqrt(sum(fit2$residuals^2) / fit2$df.residual)
    output <- list(fit1 = fit1, fit2 = fit2, sigma_c = sigma_c, type = type)

    class(output) <- "DERMod"
    return(output)
}


predict.DERMod <- function(object, newdata, n = 1e4, type = "response") {
    fit1 <- object$fit1   # * ERMod object
    fit2 <- object$fit2   # * lm object

    # 确保 newdata 是一个数据框
    if (!is.data.frame(newdata)) {
        newdata <- data.frame(dose = newdata)
    } else {
        if (!"dose" %in% names(newdata)) {
            stop("newdata must contain a 'dose' column.")
        }
    }
    

    # 初始化响应值的向量
    resp <- numeric(nrow(newdata))
    
    # 对每个 newdata 逐一处理
    for (i in 1:nrow(newdata)) {
        logC <- predict(fit2, newdata = newdata[i, , drop = FALSE])

        # Monte Carlo simulation to approximate the conditional expectation
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
