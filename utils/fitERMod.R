source("utils/ermodels.R")
source("utils/Mods.R")

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

    fitted_values <- predict.ERMod(fit_list, exposure)
    rediduals <- resp - fitted_values
    fit_list$residuals <- rediduals
    
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