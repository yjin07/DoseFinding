source("utils/ermodels.R")

# 通用拟合函数
fitERMod <- function(exposure, resp, model = NULL, type = c("gaussian", "binomial")) {
    data <- data.frame(exposure = exposure, resp = resp)

    if (!model %in% names(model_info)) {
        stop("Invalid model specified.")
    }

    model_params <- model_info[[model]]
    start <- model_params$start
    lower <- model_params$lower
    loglik <- model_params$loglik
    ols <- model_params$ols
    param_names <- model_params$param_names


    if (type == "binomial") {
        fit <- nlminb(start = start, objective = loglik, data = data, lower = lower)$par
    } else if (type == "gaussian") {
        fit <- nlminb(start = start, objective = ols, data = data, lower = lower)$par
    } else {
    stop("Invalid type specified. Choose 'gaussian' or 'binomial'.")
    }

    names(fit) <- param_names
    print(fit)

    fit_list <- list(model = model, coeffs = as.list(fit))

    fitted_values <- predict.ERMod(fit_list, exposure)
    rediduals <- resp - fitted_values
    fit_list$residuals <- rediduals
    
    class(fit_list) <- "ERMod"
    return(fit_list)
}

predict.ERMod <- function(object, newdata, ...) {
    if (missing(newdata)) {
        stop("newdata must be provided for type 'response'")
    }

    model <- object$model
    coef <- object$coeffs

    # pred <- switch(model,
    #     sigEmax = sigEmax(newdata, coef$E0, coef$Emax, coef$EC50, coef$h),
    #     Emax = Emax(newdata, coef$E0, coef$Emax, coef$EC50),
    #     Expo = Expo(newdata, coef$E0, coef$E1, coef$Delta),
    #     Beta = Beta(newdata, coef$E0, coef$Emax, coef$Delta1, coef$Delta2, coef$Scale),
    #     Linear = Linear(newdata, coef$E0, coef$Delta),
    #     LinearLog = LinearLog(newdata, coef$E0, coef$Delta, coef$Offset),
    #     Logistic = Logistic(newdata, coef$E0, coef$Emax, coef$EC50, coef$Delta),
    #     Quadratic = Quadratic(newdata, coef$E0, coef$B1, coef$B2))

    # 获取模型函数和参数名称
    model_func <- model_functions[[model]]$func
    param_names <- model_functions[[model]]$params
    # 提取参数并调用模型函数
    params <- as.list(coef)
    names(params) <- param_names[-1]  # 去掉第一个参数的名称
    pred <- do.call(model_func, c(list(cc = newdata), params))


    return(pred)
}