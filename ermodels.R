# * ---------------------------
# * E-R Models
# * ---------------------------
sigEmax <- function(cc, e0, eMax, ec50, h) {
    return(e0 + eMax * 1 / (1 + (ec50 / cc)^h))
}

Emax <- function(cc, e0, eMax, ec50) {
    return(e0 + eMax * (cc / (ec50 + cc)))
}

Expo <- function(cc, e0, e1, delta) {
    return(e0 + e1 * (exp(cc / delta) - 1))
}

Beta <- function(cc, e0, eMax, delta1, delta2, scale){
  xlogx <- function(x) if(x == 0) 0 else x * log(x) # will not be called with vector x
  logMaxDens <- xlogx(delta1) + xlogx(delta2) - xlogx(delta1 + delta2)
  cc <- cc/scale
  return(e0 + eMax/exp(logMaxDens) * (cc^delta1) * (1 - cc)^delta2)
}

Linear <- function(cc, e0, delta) {
    return(e0 + delta * cc)
}

LinearLog <- function(cc, e0, delta, offset = 1) {
    return(e0 + delta * log(cc + offset))
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

loglik_sigEmax <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]
  h <- para[4]
  
  log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(sigEmax(data[i, 1], e0, emax, ec50, h))))
  log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(sigEmax(data[i, 1], e0, emax, ec50, h))))
  
  loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
  return(-loglik)
}


loglik_Emax <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]
  
  log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Emax(data[i, 1], e0, emax, ec50))))
  log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Emax(data[i, 1], e0, emax, ec50))))
  
  loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
  return(-loglik)
}

loglik_Expo <- function(para, data) {
  e0 <- para[1]
  e1 <- para[2]
  delta <- para[3]
  
  log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Expo(data[i, 1], e0, e1, delta))))
  log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Expo(data[i, 1], e0, e1, delta))))
  
  loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
  return(-loglik)
}

loglik_Beta <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  scale <- para[3]
  delta1 <- para[4]
  delta2 <- para[5]
  
  log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Beta(data[i, 1], e0, emax, delta1, delta2, scale))))
  log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Beta(data[i, 1], e0, emax, delta1, delta2, scale))))
  
  loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
  return(-loglik)
}

loglik_Linear <- function(para, data) {
  e0 <- para[1]
  delta <- para[2]
  
  log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Linear(data[i, 1], e0, delta))))
  log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Linear(data[i, 1], e0, delta))))
  
  loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
  return(-loglik)
}

loglik_LinearLog <- function(para, data) {
  e0 <- para[1]
  delta <- para[2]
  offset <- para[3]
  
  log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(LinearLog(data[i, 1], e0, delta, offset))))
  log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(LinearLog(data[i, 1], e0, delta, offset))))
  
  loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
  return(-loglik)
}

loglik_Logistic <- function(para, data) {
  e0 <- para[1]
  emax <- para[2]
  ec50 <- para[3]
  delta <- para[4]
  
  log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Logistic(data[i, 1], e0, emax, ec50, delta))))
  log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Logistic(data[i, 1], e0, emax, ec50, delta))))
  
  loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
  return(-loglik)
}

loglik_Quadratic <- function(para, data) {
  e0 <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]
  
  log_p_y1 <- sapply(1:nrow(data), function(i) log(inv_logit(Quadratic(data[i, 1], e0, beta1, beta2))))
  log_p_y0 <- sapply(1:nrow(data), function(i) log(1 - inv_logit(Quadratic(data[i, 1], e0, beta1, beta2))))
  
  loglik <- sum(data[, 2] * log_p_y1 + (1 - data[, 2]) * log_p_y0)
  return(-loglik)
}



