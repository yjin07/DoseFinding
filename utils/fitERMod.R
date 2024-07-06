library(here)

source(here("utils", "ermodels.R"))
source(here("utils", "Mods.R"))

# General function to fit an exposure-response model
fitERMod <- function(exposure, resp, model = NULL, type = c("gaussian", "binomial"), addArgs = NULL) {
    data <- data.frame(exposure = exposure, resp = resp)

    if (!model %in% names(model_info)) {
        stop("Invalid model specified.")
    }

    scale <- off <- NULL
    if (model %in% c("LinearLog", "Beta")) {
        aPar <- getAddArgs(addArgs, exposure)
        if (model == "LinearLog") {
            off <- aPar$off
        }
        if (model == "Beta") {
            scale <- aPar$scal
        }
    }


    model_params <- model_info[[model]]
    start <- model_params$start
    lower <- model_params$lower
    param_names <- model_params$param_names

    if (model == "Beta") {
        loglik <- function(para, data) {
            return(loglik_Beta(para, data, scale))
        }
        ols <- function(para, data) {
            return(ols_Beta(para, data, scale))
        }
    } else if (model == "LinearLog") {
        loglik <- function(para, data) {
            return(loglik_LinearLog(para, data, off))
        }
        ols <- function(para, data) {
            return(ols_LinearLog(para, data, off))
        }
    } else {
        loglik <- model_params$loglik
        ols <- model_params$ols
    } 


    if (type == "binomial") {
        fit <- nlminb(start = start, objective = loglik, data = data, lower = lower)$par
    } else if (type == "gaussian") {
        fit <- nlminb(start = start, objective = ols, data = data, lower = lower)$par
    } else {
        stop("Invalid type specified. Choose 'gaussian' or 'binomial'.")
    }

    names(fit) <- param_names
    print(fit)

    fit_list <- list(model = model, coeffs = as.list(fit), scale = scale, off = off, type = type)

    n <- length(resp)
    fitted_values <- predict.ERMod(fit_list, exposure)

    fit_list$fitted_values <- fitted_values
    fit_list$df.residual <- n - length(fit_list$coeffs)

    if (type == 'binomial') {
        residuals <- resp - fitted_values
        devResiduals <- sign(residuals) * sqrt(-2 * (resp * log(fitted_values) + (1 - resp) * log(1 - fitted_values)))

        fit_list$devResiduals <- devResiduals
        fit_list$deviance <- sum(devResiduals^2)
        fit_list$logLike <- sum(resp * log(fitted_values) + (1 - resp) * log(1 - fitted_values))
    } else if (type == 'gaussian') {
        residuals <- log(resp) - log(fitted_values)
        residuals_sq <- sum(residuals^2)
        sigma_sq <- residuals_sq / fit_list$df.residual

        fit_list$residuals <- residuals
        fit_list$logLike <- -0.5 * (n * log(2 * pi) + n * log(sigma_sq) + residuals_sq / sigma_sq)
    }

    fit_list$AIC <- -2 * fit_list$logLike + 2 * length(fit_list$coeffs)

    
    class(fit_list) <- "ERMod"
    return(fit_list)
}

predict.ERMod <- function(object, newdata, type = 'response', ...) {
    if (missing(newdata)) {
        stop("newdata must be provided")
    }

    model <- object$model
    coef <- object$coeffs
    scale <- object$scale
    off <- object$off

    # 获取模型函数和参数名称
    model_func <- model_functions[[model]]$func
    param_names <- model_functions[[model]]$params
    # 提取参数并调用模型函数
    params <- as.list(coef)
    names(params) <- param_names[-1]  # 去掉第一个参数的名称
    # pred <- do.call(model_func, c(list(cc = newdata), params))

    if (model == "Beta") {
        pred <- do.call(model_func, c(list(cc = newdata), params, list(scale = scale)))
    } else if (model == "LinearLog") {
        pred <- do.call(model_func, c(list(cc = newdata), params, list(off = off)))
    } else {
        pred <- do.call(model_func, c(list(cc = newdata), params))
    }

    if (object$type == "binomial") {
        if (type == 'class') {
            pred <- ifelse(inv_logit(pred) > 0.5, 1, 0)
        } else {
            pred <- inv_logit(pred)
        }
    }

    return(pred)
}