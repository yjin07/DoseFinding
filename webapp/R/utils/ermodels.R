# * ---------------------------
# * E-R Models
# * ---------------------------
sigEmax <- function(cc, e0, eMax, ec50, h) {
    return(e0 + eMax * 1 / (1 + (ec50 / cc)^h))
}

Emax <- function(cc, e0, eMax, ec50) {
    return(e0 + eMax * cc / (ec50 + cc))
}

Expo <- function(cc, e0, e1, delta) {
    return(e0 + e1 * (exp(cc / delta) - 1))
}

Beta <- function(cc, e0, eMax, delta1, delta2, scale){
  xlogx <- function(x) {if(x == 0) 0 else x * log(x)} # will not be called with vector x
  logMaxDens <- xlogx(delta1) + xlogx(delta2) - xlogx(delta1 + delta2)
  cc <- cc/scale
  return(e0 + eMax/exp(logMaxDens) * (cc^delta1) * (1 - cc)^delta2)
}

Linear <- function(cc, e0, delta) {
    return(e0 + delta * cc)
}

LinearLog <- function(cc, e0, delta, off) {
    return(e0 + delta * log(cc + off))
}

Logistic <- function(cc, e0, eMax, ec50, delta) {
    return(e0 + eMax / (1 + exp((ec50 - cc) / delta)))
}

Quadratic <- function(cc, e0, b1, b2) {
    return(e0 + b1 * cc + b2 * cc^2)
}


# * ---------------------------
# * Log-likelihood functions
# * ---------------------------
logit <- function(p) {log(p / (1 - p))}
inv_logit <- function(y) {1 / (1 + exp(-y))}


# -----------------------------
# data[, 1]: exposure
# data[, 2]: response
# -----------------------------
loglik_sigEmax <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]
  h <- para[4]
  
  # log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(sigEmax(data[i, 1], e0, emax, ec50, h))))
  # log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(sigEmax(data[i, 1], e0, emax, ec50, h))))
  # loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)

  p <- pmin(pmax(inv_logit(sigEmax(data[,1], e0, emax, ec50, h)), 1e-10), 1-1e-10)
  loglik <- sum(data[,2] * log(p) + (1 - data[,2]) * log(1 - p))
  return(-loglik)
}


loglik_Emax <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]
  
  # log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Emax(data[i, 1], e0, emax, ec50))))
  # log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Emax(data[i, 1], e0, emax, ec50))))
  # loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)

  p <- pmin(pmax(inv_logit(Emax(data[,1], e0, emax, ec50)), 1e-10), 1-1e-10)
  loglik <- sum(data[,2] * log(p) + (1 - data[,2]) * log(1 - p))
  return(-loglik)
}

loglik_Expo <- function(para, data) {
  e0 <- para[1]
  e1 <- para[2]
  delta <- para[3]
  
  # log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Expo(data[i, 1], e0, e1, delta))))
  # log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Expo(data[i, 1], e0, e1, delta))))  
  # loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)

  p <- pmin(pmax(inv_logit(Expo(data[,1], e0, e1, delta)), 1e-10), 1-1e-10)
  loglik <- sum(data[,2] * log(p) + (1 - data[,2]) * log(1 - p))

  return(-loglik)
}

# loglik_Beta <- function(para, data) {
#   e0 <- para[1]
#   emax <- para[2]
#   delta1 <- para[3]
#   delta2 <- para[4]
#   scale <- para[5]
  
#   log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Beta(data[i, 1], e0, emax, delta1, delta2, scale))))
#   log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Beta(data[i, 1], e0, emax, delta1, delta2, scale))))
  
#   loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
#   return(-loglik)
# }

loglik_Beta <- function(para, data, scale) {
  e0 <- para[1]
  emax <- para[2]
  delta1 <- para[3]
  delta2 <- para[4]
  
  # log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Beta(data[i, 1], e0, emax, delta1, delta2, scale))))
  # log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Beta(data[i, 1], e0, emax, delta1, delta2, scale))))
  # loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)

  p <- pmin(pmax(inv_logit(Beta(data[,1], e0, emax, delta1, delta2, scale)), 1e-10), 1-1e-10)
  loglik <- sum(data[,2] * log(p) + (1 - data[,2]) * log(1 - p))
  return(-loglik)
}

loglik_Linear <- function(para, data) {
  e0 <- para[1]
  delta <- para[2]
  
  # log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Linear(data[i, 1], e0, delta))))
  # log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Linear(data[i, 1], e0, delta))))
  # loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)

  p <- pmin(pmax(inv_logit(Linear(data[,1], e0, delta)), 1e-10), 1-1e-10)
  loglik <- sum(data[,2] * log(p) + (1 - data[,2]) * log(1 - p))
  return(-loglik)
}

loglik_LinearLog <- function(para, data, off) {
  e0 <- para[1]
  delta <- para[2]
  
  # log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(LinearLog(data[i, 1], e0, delta, off))))
  # log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(LinearLog(data[i, 1], e0, delta, off))))
  # loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)

  p <- pmin(pmax(inv_logit(LinearLog(data[,1], e0, delta, off)), 1e-10), 1-1e-10)
  loglik <- sum(data[,2] * log(p) + (1 - data[,2]) * log(1 - p))
  return(-loglik)
}

loglik_Logistic <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]
  delta <- para[4]
  
  # log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Logistic(data[i, 1], e0, emax, ec50, delta))))
  # log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Logistic(data[i, 1], e0, emax, ec50, delta))))
  # loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)

  p <- pmin(pmax(inv_logit(Logistic(data[,1], e0, emax, ec50, delta)), 1e-10), 1-1e-10)
  loglik <- sum(data[,2] * log(p) + (1 - data[,2]) * log(1 - p))
  return(-loglik)
}

loglik_Quadratic <- function(para, data) {
  e0 <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]
  
  # log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Quadratic(data[i, 1], e0, beta1, beta2))) + 1e-8)
  # log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Quadratic(data[i, 1], e0, beta1, beta2))) + 1e-8)
  # loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)

  p <- pmin(pmax(inv_logit(Quadratic(data[,1], e0, beta1, beta2)), 1e-10), 1-1e-10)
  loglik <- sum(data[,2] * log(p) + (1 - data[,2]) * log(1 - p))

  return(-loglik)
}


# -----------------------------
# data[, 1]: exposure
# data[, 2]: response
# -----------------------------
ols_sigEmax <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]
  h <- para[4]

  hat_logy <- sigEmax(data[, 1], e0, emax, ec50, h)
  sse <- sum((log(data[, 2]) - hat_logy)^2)
  return(sse)
}

ols_Emax <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]

  hat_logy <- Emax(data[, 1], e0, emax, ec50)
  sse <- sum((log(data[, 2]) - hat_logy)^2)
  return(sse)
}

ols_Expo <- function(para, data) {
  e0 <- para[1]
  e1 <- para[2]
  delta <- para[3]

  hat_logy <- Expo(data[, 1], e0, e1, delta)
  sse <- sum((log(data[, 2]) - hat_logy)^2)
  return(sse)
}

ols_Beta <- function(para, data, scale) {
  e0 <- para[1]
  emax <- para[2]
  delta1 <- para[3]
  delta2 <- para[4]

  hat_logy <- Beta(data[, 1], e0, emax, delta1, delta2, scale)
  sse <- sum((log(data[, 2]) - hat_logy)^2)
  return(sse)
}

ols_Linear <- function(para, data) {
  e0 <- para[1]
  delta <- para[2]

  hat_logy <- Linear(data[, 1], e0, delta)
  sse <- sum((log(data[, 2]) - hat_logy)^2)
  return(sse)
}

ols_LinearLog <- function(para, data, off) {
  e0 <- para[1]
  delta <- para[2]

  hat_logy <- LinearLog(data[, 1], e0, delta, off)
  sse <- sum((log(data[, 2]) - hat_logy)^2)
  return(sse)
}

ols_Logistic <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]
  delta <- para[4]

  hat_logy <- Logistic(data[, 1], e0, emax, ec50, delta)
  sse <- sum((log(data[, 2]) - hat_logy)^2)
  return(sse)
}

ols_Quadratic <- function(para, data) {
  e0 <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]

  hat_logy <- Quadratic(data[, 1], e0, beta1, beta2)
  sse <- sum((log(data[, 2]) - hat_logy)^2)
  return(sse)
}


model_info = list(
    sigEmax = list(start = c(1, 1, 1, 1), lower = c(-Inf, -Inf, 0, 0), loglik = loglik_sigEmax, ols = ols_sigEmax, param_names = c("E0", "Emax", "EC50", "h")),
    Emax = list(start = c(1, 1, 1), lower = c(-Inf, -Inf, 0), loglik = loglik_Emax, ols = ols_Emax, param_names = c("E0", "Emax", "EC50")),
    Expo = list(start = c(1, 1, 1), lower = c(-Inf, -Inf, 0), loglik = loglik_Expo, ols = ols_Expo, param_names = c("E0", "E1", "Delta")),
    # Beta = list(start = c(1, 1, 1, 1, 1), lower = c(-Inf, -Inf, 1e-8, 1e-8, 1e-8), loglik = loglik_Beta, ols = ols_Beta, param_names = c("E0", "Emax", "Delta1", "Delta2", "Scale")),
    Beta = list(start = c(1, 1, 1, 1), lower = c(-Inf, -Inf, 1e-8, 1e-8), loglik = loglik_Beta, ols = ols_Beta, param_names = c("E0", "Emax", "Delta1", "Delta2")),
    Linear = list(start = c(1, 1), lower = c(-Inf, -Inf), loglik = loglik_Linear, ols = ols_Linear, param_names = c("E0", "Delta")),
    LinearLog = list(start = c(1, 1), lower = c(-Inf, -Inf), loglik = loglik_LinearLog, ols = ols_LinearLog, param_names = c("E0", "Delta")),
    Logistic = list(start = c(1, 1, 1, 1), lower = c(-Inf, -Inf, 0, 0), loglik = loglik_Logistic, ols = ols_Logistic, param_names = c("E0", "Emax", "EC50", "Delta")),
    Quadratic = list(start = c(1, 1, 1), lower = c(-Inf, -Inf, -Inf), loglik = loglik_Quadratic, ols = ols_Quadratic, param_names = c("E0", "B1", "B2"))
)


model_functions <- list(
    sigEmax = list(func = sigEmax, params = c("cc", "e0", "eMax", "ec50", "h")),
    Emax = list(func = Emax, params = c("cc", "e0", "eMax", "ec50")),
    Expo = list(func = Expo, params = c("cc", "e0", "e1", "delta")),
    Beta = list(func = Beta, params = c("cc", "e0", "eMax", "delta1", "delta2")),
    Linear = list(func = Linear, params = c("cc", "e0", "delta")),
    LinearLog = list(func = LinearLog, params = c("cc", "e0", "delta")),
    Logistic = list(func = Logistic, params = c("cc", "e0", "eMax", "ec50", "delta")),
    Quadratic = list(func = Quadratic, params = c("cc", "e0", "b1", "b2"))
)