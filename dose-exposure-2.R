# rm(list=ls())

source("ermodels.R")
library(dplyr)


#===========================================================================
# For different models, please refer to: ??DoseFinding::drmodels
#===========================================================================

#---------------------------------------------------------------------------
# Sigmoid Emax Model
#---------------------------------------------------------------------------

# Model: logit(P(Y=1)) = E_0 + (E_max * d^h) / (ED50^h + d^h)

sigEmax <- function(d, e0, emax, ed50, h){
  theta <- e0 + emax*(d^h/(ed50^h + d^h))
  return(theta)
}

# specify model parameter
e0 <- logit(0.1)
emax <- 3
ed50 <- 0.4
h <- 3

# plot the model curve
dose <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
logit_resp <- mapply(sigEmax, dose, e0, emax, ed50, h)
resp <- inv_logit(logit_resp)
# plot(dose, resp)
# lines(dose, resp) 

# generate binary data using the model
v_dose <- 1:7
dose_seq <- dose[v_dose]
p_true <- resp[v_dose]
n <- c(3,3,6,8,12,18,10,6,3,3)[v_dose]   

ds <- NULL
rspd <- NULL
rspd_obs <- NULL

for(i in 1:length(n)){
  ds <- c(ds, rep(dose_seq[i], n[i]))
  tmp <- rbinom(n[i], 1, p_true[i])
  # tmp <- rep(0, n[i])
  # for(j in 1:n[i]){
  #   if(j<=n[i]*p_true[i]) tmp[j] <-1
  #   else break
  # }
  rspd <- c(rspd, tmp)
  rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
}

# plot the observed ORR
df = data.frame(dose=ds, Response = rspd)
obs.sigEmax.orr <- rspd_obs


# * ----------------------------------------
# set.seed(9981)
# cc <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
# n <- c(3,3,6,8,12,18,10,6,3,3)
# selected <- 1:7

# ds <- cc[selected]
# nn <- n[selected]

# ds <- rep(ds, nn) + rnorm(sum(nn), 0, 0.01)
# pr <- inv_logit(sigEmax(ds, e0, emax, ed50, h))
# df <- data.frame(dose=ds, Response = rbinom(sum(nn), 1, pr)) %>% arrange(dose)
# head(df)
# * ----------------------------------------

log-likelihood function for Sigmoid Emax Model
loglik_sigEmax <- function(para, data){
  e0 <- para[1]
  emax <- para[2]
  ed50 <- para[3]
  h <- para[4]
  
  log_p_y1 <- NULL
  log_p_y0 <- NULL
  
  for(i in 1:dim(data)[1]){
    logit_y1 <- sigEmax(data[i,1], e0, emax, ed50, h)
    p_y1 <- inv_logit(logit_y1)
    p_y0 <- 1-p_y1
    log_p_y1 <- c(log_p_y1, log(p_y1))
    log_p_y0 <- c(log_p_y0, log(p_y0))
  }
  
  loglik <- as.numeric(data[,2]%*%log_p_y1)+as.numeric((1-data[,2])%*%log_p_y0)
  
  return(-loglik)
}

# maximize log-likelihood function i.e. minimize -log-likelihood function
fit.sigEmax.par <- nlminb(start=c(1, 1, 1, 1), objective=loglik_sigEmax, data=df, lower=c(-Inf, -Inf, 0, 0))$par
fit.sigEmax.par

fit.sigEmax.e0 <- fit.sigEmax.par[1]
fit.sigEmax.emax <- fit.sigEmax.par[2]
fit.sigEmax.ed50 <- fit.sigEmax.par[3]
fit.sigEmax.h <- fit.sigEmax.par[4]

# the fitted ORR for different dose levels
fit.sigEmax.orr <- inv_logit(sigEmax(dose_seq, fit.sigEmax.e0, fit.sigEmax.emax, fit.sigEmax.ed50, fit.sigEmax.h))

plot(dose_seq, obs.sigEmax.orr)
lines(dose_seq, obs.sigEmax.orr)
lines(dose_seq, fit.sigEmax.orr, type='o', col='red')



#---------------------------------------------------------------------------
# Emax Model
#---------------------------------------------------------------------------

# Model: logit(P(Y=1)) = E_0 + (E_max * d) / (ED50 + d)

Emax <- function(d, e0, emax, ed50){
  theta <- e0 + emax*(d/(ed50 + d))
  return(theta)
}

# specify model parameter
e0 <- logit(0.1)
emax <- 4
ed50 <- 0.4

# plot the model curve
dose <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
logit_resp <- mapply(Emax, dose, e0, emax, ed50)
resp <- inv_logit(logit_resp)
plot(dose, resp)
lines(dose, resp)

# generate binary data using the model
v_dose <- 1:7
dose_seq <- dose[v_dose]
p_true <- resp[v_dose]
n <- c(3,3,6,8,12,18,10,6,3,3)[v_dose]*10

ds <- NULL
rspd <- NULL
rspd_obs <- NULL

for(i in 1:length(n)){
  ds <- c(ds, rep(dose_seq[i], n[i]))
  tmp <- rbinom(n[i], 1, p_true[i])
  # tmp <- rep(0, n[i])
  # for(j in 1:n[i]){
  #   if(j<=n[i]*p_true[i]) tmp[j] <-1
  #   else break
  # }
  rspd <- c(rspd, tmp)
  rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
}

# plot the observed ORR
df = data.frame(dose=ds, Response = rspd)
obs.Emax.orr <- rspd_obs


# log-likelihood function for Emax Model
loglik_Emax <- function(para, data){
  e0 <- para[1]
  emax <- para[2]
  ed50 <- para[3]
  
  log_p_y1 <- NULL
  log_p_y0 <- NULL
  
  for(i in 1:dim(data)[1]){
    logit_y1 <- Emax(data[i,1], e0, emax, ed50)
    p_y1 <- inv_logit(logit_y1)
    p_y0 <- 1-p_y1
    log_p_y1 <- c(log_p_y1, log(p_y1))
    log_p_y0 <- c(log_p_y0, log(p_y0))
  }
  
  loglik <- as.numeric(data[,2]%*%log_p_y1)+as.numeric((1-data[,2])%*%log_p_y0)
  
  return(-loglik)
}

# maximize log-likelihood function i.e. minimize -log-likelihood function
fit.Emax.par <- nlminb(start=c(1, 1, 1), objective=loglik_Emax, data=df, lower=c(-Inf, -Inf, 0))$par
fit.Emax.e0 <- fit.Emax.par[1]
fit.Emax.emax <- fit.Emax.par[2]
fit.Emax.ed50 <- fit.Emax.par[3]

# the fitted ORR for different dose levels
fit.Emax.orr <- inv_logit(Emax(dose_seq, fit.Emax.e0, fit.Emax.emax, fit.Emax.ed50))

plot(dose_seq, obs.Emax.orr)
lines(dose_seq, obs.Emax.orr)
lines(dose_seq, fit.Emax.orr, type='o', col='red')


#---------------------------------------------------------------------------
# Exponential Model
#---------------------------------------------------------------------------

# Model: logit(P(Y=1)) = E_0 + E_1*(exp(d/delta)-1)

Expo <- function(d, e0, e1, delta){
  theta <- e0 + e1*(exp(d/delta)-1)
  return(theta)
}

# specify model parameter
e0 <- logit(0.1)
e1 <- 0.8
delta <- 0.4

# plot the model curve
dose <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
logit_resp <- mapply(Expo, dose, e0, e1, delta)
resp <- inv_logit(logit_resp)
plot(dose, resp)
lines(dose, resp)

# generate binary data using the model
v_dose <- 1:7
dose_seq <- dose[v_dose]
p_true <- resp[v_dose]
n <- c(3,3,6,8,12,18,10,6,3,3)[v_dose]*10

ds <- NULL
rspd <- NULL
rspd_obs <- NULL

for(i in 1:length(n)){
  ds <- c(ds, rep(dose_seq[i], n[i]))
  tmp <- rbinom(n[i], 1, p_true[i])
  # tmp <- rep(0, n[i])
  # for(j in 1:n[i]){
  #   if(j<=n[i]*p_true[i]) tmp[j] <-1
  #   else break
  # }
  rspd <- c(rspd, tmp)
  rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
}

# plot the observed ORR
df = data.frame(dose=ds, Response = rspd)
obs.Expo.orr <- rspd_obs


# log-likelihood function for Expo Model
loglik_Expo <- function(para, data){
  e0 <- para[1]
  e1 <- para[2]
  delta <- para[3]
  
  log_p_y1 <- NULL
  log_p_y0 <- NULL
  
  for(i in 1:dim(data)[1]){
    logit_y1 <- Expo(data[i,1], e0, e1, delta)
    p_y1 <- inv_logit(logit_y1)
    p_y0 <- 1-p_y1
    log_p_y1 <- c(log_p_y1, log(p_y1))
    log_p_y0 <- c(log_p_y0, log(p_y0))
  }
  
  loglik <- as.numeric(data[,2]%*%log_p_y1)+as.numeric((1-data[,2])%*%log_p_y0)
  
  return(-loglik)
}

# maximize log-likelihood function i.e. minimize -log-likelihood function
fit.Expo.par <- nlminb(start=c(1, 1, 1), objective=loglik_Expo, data=df, lower=c(-Inf, -Inf, 0))$par
fit.Expo.e0 <- fit.Expo.par[1]
fit.Expo.e1 <- fit.Expo.par[2]
fit.Expo.delta <- fit.Expo.par[3]

# the fitted ORR for different dose levels
fit.Expo.orr <- inv_logit(Expo(dose_seq, fit.Expo.e0, fit.Expo.e1, fit.Expo.delta))
plot(dose_seq, obs.Expo.orr)
lines(dose_seq, obs.Expo.orr)
lines(dose_seq, fit.Expo.orr, type='o', col='red')



#---------------------------------------------------------------------------
# Beta Model
#---------------------------------------------------------------------------

# Model: logit(P(Y=1)) = E_0 + E_max*B(delta1, delta2)*(d/scale)^delta1*(1-d/scale)^delta2
# in which: B(delta1, delta2) = (delta1 + delta2)^(delta1 + delta2)/(delta1^delta1 * delta2^delta2)

Beta <- function(d, e0, emax, scale, delta1, delta2){
  Bdelta12 <- (delta1+delta2)^(delta1+delta2)/(delta1^delta1*delta2^delta2)
  theta <- e0 + emax*Bdelta12*(d/scale)^delta1*(1-d/scale)^delta2
  return(theta)
}

# specify model parameter
e0 <- logit(0.1)
emax <- 3
scale <- 0.2
delta1 <- -1
delta2 <- 1


# plot the model curve
dose <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
logit_resp <- mapply(Beta, dose, e0, emax, scale, delta1, delta2)
resp <- inv_logit(logit_resp)
plot(dose, resp)
lines(dose, resp)

# generate binary data using the model
v_dose <- 1:7
dose_seq <- dose[v_dose]
p_true <- resp[v_dose]
n <- c(3,3,6,8,12,18,10,6,3,3)[v_dose]*10

ds <- NULL
rspd <- NULL
rspd_obs <- NULL

for(i in 1:length(n)){
  ds <- c(ds, rep(dose_seq[i], n[i]))
  tmp <- rbinom(n[i], 1, p_true[i])
  # tmp <- rep(0, n[i])
  # for(j in 1:n[i]){
  #   if(j<=n[i]*p_true[i]) tmp[j] <-1
  #   else break
  # }
  rspd <- c(rspd, tmp)
  rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
}

# plot the observed ORR
df = data.frame(dose=ds, Response = rspd)
obs.Beta.orr <- rspd_obs


# log-likelihood function for Beta Model
loglik_Beta <- function(para, data){
  e0 <- para[1]
  emax <- para[2]
  scale <- para[3]
  delta1 <- para[4]
  delta2 <- para[5]
  
  log_p_y1 <- NULL
  log_p_y0 <- NULL
  
  for(i in 1:dim(data)[1]){
    logit_y1 <- Beta(data[i,1], e0, emax, scale, delta1, delta2)
    p_y1 <- inv_logit(logit_y1)
    p_y0 <- 1-p_y1
    log_p_y1 <- c(log_p_y1, log(p_y1))
    log_p_y0 <- c(log_p_y0, log(p_y0))
  }
  
  loglik <- as.numeric(data[,2]%*%log_p_y1)+as.numeric((1-data[,2])%*%log_p_y0)
  
  return(-loglik)
}

# TODO: why this lower bound?
# maximize log-likelihood function i.e. minimize -log-likelihood function
fit.Beta.par <- nlminb(start=c(1, 1, 1, 1, 1), objective=loglik_Beta, data=df, lower=c(-Inf, -Inf, 0, -Inf, -Inf))$par
fit.Beta.e0 <- fit.Beta.par[1]
fit.Beta.emax <- fit.Beta.par[2]
fit.Beta.scale <- fit.Beta.par[3]
fit.Beta.delta1 <- fit.Beta.par[4]
fit.Beta.delta2 <- fit.Beta.par[5]

# the fitted ORR for different dose levels
fit.Beta.orr <- inv_logit(Beta(dose_seq, fit.Beta.e0, fit.Beta.emax, fit.Beta.scale, fit.Beta.delta1, fit.Beta.delta2))

plot(dose_seq, obs.Beta.orr)
lines(dose_seq, obs.Beta.orr)
lines(dose_seq, fit.Beta.orr, type='o', col='red')


#---------------------------------------------------------------------------
# Linear Model
#---------------------------------------------------------------------------

# Model: logit(P(Y=1)) = E_0 + delta*d

Linear <- function(d, e0, delta){
  theta <- e0 + delta*d
  return(theta)
}

# specify model parameter
e0 <- logit(0.1)
delta <- 3


# plot the model curve
dose <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
logit_resp <- mapply(Linear, dose, e0, delta)
resp <- inv_logit(logit_resp)
plot(dose, resp)
lines(dose, resp)

# generate binary data using the model
v_dose <- 1:7
dose_seq <- dose[v_dose]
p_true <- resp[v_dose]
n <- c(3,3,6,8,12,18,10,6,3,3)[v_dose]*10

ds <- NULL
rspd <- NULL
rspd_obs <- NULL

for(i in 1:length(n)){
  ds <- c(ds, rep(dose_seq[i], n[i]))
  tmp <- rbinom(n[i], 1, p_true[i])
  # tmp <- rep(0, n[i])
  # for(j in 1:n[i]){
  #   if(j<=n[i]*p_true[i]) tmp[j] <-1
  #   else break
  # }
  rspd <- c(rspd, tmp)
  rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
}

# plot the observed ORR
df = data.frame(dose=ds, Response = rspd)
obs.Linear.orr <- rspd_obs
plot(dose_seq, obs.Linear.orr)
lines(dose_seq, obs.Linear.orr)

# log-likelihood function for Linear Model
loglik_Linear <- function(para, data){
  e0 <- para[1]
  delta <- para[2]
  
  log_p_y1 <- NULL
  log_p_y0 <- NULL
  
  for(i in 1:dim(data)[1]){
    logit_y1 <- Linear(data[i,1], e0, delta)
    p_y1 <- inv_logit(logit_y1)
    p_y0 <- 1-p_y1
    log_p_y1 <- c(log_p_y1, log(p_y1))
    log_p_y0 <- c(log_p_y0, log(p_y0))
  }
  
  loglik <- as.numeric(data[,2]%*%log_p_y1)+as.numeric((1-data[,2])%*%log_p_y0)
  
  return(-loglik)
}

# TODO: why this lower bound?
# maximize log-likelihood function i.e. minimize -log-likelihood function
fit.Linear.par <- nlminb(start=c(1, 1), objective=loglik_Linear, data=df, lower=c(-Inf, 0))$par
fit.Linear.e0 <- fit.Linear.par[1]
fit.Linear.delta <- fit.Linear.par[2]


# the fitted ORR for different dose levels
fit.Linear.orr <- inv_logit(Linear(dose_seq, fit.Linear.e0, fit.Linear.delta))




#---------------------------------------------------------------------------
# Linear in log Model
#---------------------------------------------------------------------------

# Model: logit(P(Y=1)) = E_0 + delta*log(d+offset)

LinearLog <- function(d, e0, delta, offset){  # FIXME:
  theta <- e0 + delta*d
  return(theta)
}

# specify model parameter
e0 <- logit(0.1)
delta <- 3
offset <- exp(0.1)


# plot the model curve
dose <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
logit_resp <- mapply(LinearLog, dose, e0, delta, offset)
resp <- inv_logit(logit_resp)
plot(dose, resp)
lines(dose, resp)

# generate binary data using the model
v_dose <- 1:7
dose_seq <- dose[v_dose]
p_true <- resp[v_dose]
n <- c(3,3,6,8,12,18,10,6,3,3)[v_dose]*10

ds <- NULL
rspd <- NULL
rspd_obs <- NULL

for(i in 1:length(n)){
  ds <- c(ds, rep(dose_seq[i], n[i]))
  tmp <- rbinom(n[i], 1, p_true[i])
  # tmp <- rep(0, n[i])
  # for(j in 1:n[i]){
  #   if(j<=n[i]*p_true[i]) tmp[j] <-1
  #   else break
  # }
  rspd <- c(rspd, tmp)
  rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
}

# plot the observed ORR
df = data.frame(dose=ds, Response = rspd)
obs.LinearLog.orr <- rspd_obs
plot(dose_seq, obs.LinearLog.orr)
lines(dose_seq, obs.LinearLog.orr)

# log-likelihood function for LinearLog Model
loglik_LinearLog <- function(para, data){
  e0 <- para[1]
  delta <- para[2]
  offset <- para[3]
  
  log_p_y1 <- NULL
  log_p_y0 <- NULL
  
  for(i in 1:dim(data)[1]){
    logit_y1 <- LinearLog(data[i,1], e0, delta, offset)
    p_y1 <- inv_logit(logit_y1)
    p_y0 <- 1-p_y1
    log_p_y1 <- c(log_p_y1, log(p_y1))
    log_p_y0 <- c(log_p_y0, log(p_y0))
  }
  
  loglik <- as.numeric(data[,2]%*%log_p_y1)+as.numeric((1-data[,2])%*%log_p_y0)
  
  return(-loglik)
}

# TODO: why this lower bound?
# maximize log-likelihood function i.e. minimize -log-likelihood function
fit.LinearLog.par <- nlminb(start=c(1, 1, 1), objective=loglik_LinearLog, data=df, lower=c(-Inf, 0, 0))$par
fit.LinearLog.e0 <- fit.LinearLog.par[1]
fit.LinearLog.delta <- fit.LinearLog.par[2]
fit.LinearLog.offset <- fit.LinearLog.par[2]


# the fitted ORR for different dose levels
fit.LinearLog.orr <- inv_logit(LinearLog(dose_seq, fit.LinearLog.e0, fit.LinearLog.delta, fit.LinearLog.offset))




#---------------------------------------------------------------------------
# Logistic Model
#---------------------------------------------------------------------------

# Model: logit(P(Y=1)) = E_0 + E_max / (1 + exp(ED50-d)/delta)

Logistic <- function(d, e0, emax, ed50, delta){ # FIXME:
  theta <- e0 + emax/(1+exp(ed50-d)/delta)
  return(theta)
}

# specify model parameter
e0 <- logit(0.1)
emax <- 3
ed50 <- 0.4
delta <- 1.5


# plot the model curve
dose <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
logit_resp <- mapply(Logistic, dose, e0, emax, ed50, delta)
resp <- inv_logit(logit_resp)
plot(dose, resp)
lines(dose, resp)

# generate binary data using the model
v_dose <- 1:7
dose_seq <- dose[v_dose]
p_true <- resp[v_dose]
n <- c(3,3,6,8,12,18,10,6,3,3)[v_dose]*10

ds <- NULL
rspd <- NULL
rspd_obs <- NULL

for(i in 1:length(n)){
  ds <- c(ds, rep(dose_seq[i], n[i]))
  tmp <- rbinom(n[i], 1, p_true[i])
  # tmp <- rep(0, n[i])
  # for(j in 1:n[i]){
  #   if(j<=n[i]*p_true[i]) tmp[j] <-1
  #   else break
  # }
  rspd <- c(rspd, tmp)
  rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
}

# plot the observed ORR
df = data.frame(dose=ds, Response = rspd)
obs.Logistic.orr <- rspd_obs
plot(dose_seq, obs.Logistic.orr)
lines(dose_seq, obs.Logistic.orr)

# log-likelihood function for Logistic Model
loglik_Logistic <- function(para, data){
  e0 <- para[1]
  emax <- para[2]
  ed50 <- para[3]
  delta <- para[4]
  
  log_p_y1 <- NULL
  log_p_y0 <- NULL
  
  for(i in 1:dim(data)[1]){
    logit_y1 <- Logistic(data[i,1], e0, emax, ed50, delta)
    p_y1 <- inv_logit(logit_y1)
    p_y0 <- 1-p_y1
    log_p_y1 <- c(log_p_y1, log(p_y1))
    log_p_y0 <- c(log_p_y0, log(p_y0))
  }
  
  loglik <- as.numeric(data[,2]%*%log_p_y1)+as.numeric((1-data[,2])%*%log_p_y0)
  
  return(-loglik)
}

# maximize log-likelihood function i.e. minimize -log-likelihood function
fit.Logistic.par <- nlminb(start=c(1, 1, 1, 1), objective=loglik_Logistic , data=df, lower=c(-Inf, -Inf, 0, 0))$par
fit.Logistic.e0 <- fit.Logistic.par[1]
fit.Logistic.emax <- fit.Logistic.par[2]
fit.Logistic.ed50 <- fit.Logistic.par[3]
fit.Logistic.delta <- fit.Logistic.par[4]


# the fitted ORR for different dose levels
fit.Logistic.orr <- inv_logit(Logistic(dose_seq, fit.Logistic.e0, fit.Logistic.emax, fit.Logistic.ed50, fit.Logistic.delta))




#---------------------------------------------------------------------------
# Quadratic Model
#---------------------------------------------------------------------------

# Model: logit(P(Y=1)) = E_0 + beta_1*d + beta_2*d^2

Quadratic <- function(d, e0, beta1, beta2){
  theta <- e0 + beta1*d + beta2*d^2
  return(theta)
}

# specify model parameter
e0 <- logit(0.1)
beta1 <- 2
beta2 <- 0.5


# plot the model curve
dose <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
logit_resp <- mapply(Quadratic, dose, e0, beta1, beta2)
resp <- inv_logit(logit_resp)
plot(dose, resp)
lines(dose, resp)

# generate binary data using the model
v_dose <- 1:7
dose_seq <- dose[v_dose]
p_true <- resp[v_dose]
n <- c(3,3,6,8,12,18,10,6,3,3)[v_dose]*10

ds <- NULL
rspd <- NULL
rspd_obs <- NULL

for(i in 1:length(n)){
  ds <- c(ds, rep(dose_seq[i], n[i]))
  tmp <- rbinom(n[i], 1, p_true[i])
  # tmp <- rep(0, n[i])
  # for(j in 1:n[i]){
  #   if(j<=n[i]*p_true[i]) tmp[j] <-1
  #   else break
  # }
  rspd <- c(rspd, tmp)
  rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
}

# plot the observed ORR
df = data.frame(dose=ds, Response = rspd)
obs.Quadratic.orr <- rspd_obs
plot(dose_seq, obs.Logistic.orr)
lines(dose_seq, obs.Logistic.orr)

# log-likelihood function for Quadratic Model
loglik_Quadratic <- function(para, data){
  e0 <- para[1]
  beta1 <- para[2]
  beta2 <- para[3]
  
  log_p_y1 <- NULL
  log_p_y0 <- NULL
  
  for(i in 1:dim(data)[1]){
    logit_y1 <- Quadratic(data[i,1], e0, beta1, beta2)
    p_y1 <- inv_logit(logit_y1)
    p_y0 <- 1-p_y1
    log_p_y1 <- c(log_p_y1, log(p_y1))
    log_p_y0 <- c(log_p_y0, log(p_y0))
  }
  
  loglik <- as.numeric(data[,2]%*%log_p_y1)+as.numeric((1-data[,2])%*%log_p_y0)
  
  return(-loglik)
}


# TODO: why this lower bound?
# maximize log-likelihood function i.e. minimize -log-likelihood function
fit.Quadratic.par <- nlminb(start=c(1, 1, 1), objective=loglik_Quadratic , data=df, lower=c(-Inf, 0, 0))$par
fit.Quadratic.e0 <- fit.Quadratic.par[1]
fit.Quadratic.beta1 <- fit.Quadratic.par[2]
fit.Quadratic.beta2 <- fit.Quadratic.par[3]


# the fitted ORR for different dose levels
fit.Quadratic.orr <- inv_logit(Quadratic(dose_seq, fit.Quadratic.e0, fit.Quadratic.beta1, fit.Quadratic.beta2))


