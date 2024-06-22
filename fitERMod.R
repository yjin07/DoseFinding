source("ermodels.R")

# 通用拟合函数
fitERMod <- function(exposure, resp, model = NULL, type = c("gaussian", "binomial")) {
    data <- data.frame(exposure = exposure, resp = resp)

    start <- switch(model,
        sigEmax = c(1, 1, 1, 1),
        Emax = c(1, 1, 1),
        Expo = c(1, 1, 1),
        Beta = c(1, 1, 1, 1, 1),
        Linear = c(1, 1),
        LinearLog = c(1, 1, 1),
        Logistic = c(1, 1, 1, 1),
        Quadratic = c(1, 1, 1))

    lower <- switch(model,
        sigEmax = c(-Inf, -Inf, 0, 0),
        Emax = c(-Inf, -Inf, 0),
        Expo = c(-Inf, -Inf, 0),
        Beta = c(-Inf, -Inf, 0, -Inf, -Inf),
        Linear = c(-Inf, 0),
        LinearLog = c(-Inf, 0, 0),
        Logistic = c(-Inf, -Inf, 0, 0),
        Quadratic = c(-Inf, 0, 0))

    loglik <- switch(model,
        sigEmax = loglik_sigEmax,
        Emax = loglik_Emax,
        Expo = loglik_Expo,
        Beta = loglik_Beta,
        Linear = loglik_Linear,
        LinearLog = loglik_LinearLog,
        Logistic = loglik_Logistic,
        Quadratic = loglik_Quadratic)

    ols <- switch(model,
        sigEmax = ols_sigEmax,
        Emax = ols_Emax,
        Expo = ols_Expo,
        Beta = ols_Beta,
        Linear = ols_Linear,
        LinearLog = ols_LinearLog,
        Logistic = ols_Logistic,
        Quadratic = ols_Quadratic)

    if (type == "binomial") {
        fit <- nlminb(start = start, objective = loglik, data = data, lower = lower)$par
    } else if (type == "gaussian") {
        fit <- nlminb(start = start, objective = ols, data = data, lower = lower)$par
    } else {
    stop("Invalid type specified. Choose 'gaussian' or 'binomial'.")
    }

    return(fit)
}


# ----------------------------------
# Example (Exposure-Response Model)
# ----------------------------------
library(dplyr)     # for data manipulation

e0 <- logit(0.1)
emax <- 3
ec50 <- 0.4
h <- 3

# # ----------------------------------
# # ? Data generating model 1
# # ----------------------------------
# # dose0 <- data.frame(Dose = "75 mg", AUC24 =  rlnorm(3, meanlog = log(0.1), sdlog = 0.05))
# # dose1 <- data.frame(Dose = "150 mg", AUC24 =  rlnorm(3, meanlog = log(0.2), sdlog = 0.05))
# # dose2 <- data.frame(Dose = "300 mg", AUC24 =  rlnorm(3, meanlog = log(0.4), sdlog = 0.05))
# # dose3 <- data.frame(Dose = "450 mg", AUC24 =  rlnorm(6, meanlog = log(0.6), sdlog = 0.05))
# # dose4 <- data.frame(Dose = "600 mg", AUC24 =  rlnorm(6, meanlog = log(0.8), sdlog = 0.05))
# # dose5 <- data.frame(Dose = "900 mg", AUC24 =  rlnorm(6, meanlog = log(1.2), sdlog = 0.05))

# # exposure_data_bin <- rbind(dose0, dose1, dose2, dose3, dose4, dose5) %>%
# #   mutate(pr = inv_logit(sigEmax(AUC24, e0, emax, ec50, h)), resp = rbinom(n(),1,pr)) %>%
# #   arrange(AUC24)

# # print(exposure_data_bin)

# # ----------------------------------
# # ? Data generating model 2 (coinci-
# # ? dent with the d-e-2.R)
# # ----------------------------------
# set.seed(123456)

# cc <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
# logit_resp <- mapply(sigEmax, cc, e0, emax, ec50, h)
# resp <- inv_logit(logit_resp)

# selected <- 1:7
# cc_seq <- cc[selected]
# p_true <- resp[selected]
# n <- c(3,3,6,8,12,18,10,6,3,3)[selected]   


# ds <- NULL
# rspd <- NULL
# rspd_obs <- NULL

# for(i in 1:length(n)){
#     ds <- c(ds, rep(cc_seq[i], n[i]))
#     tmp <- rbinom(n[i], 1, p_true[i])
#     rspd <- c(rspd, tmp)
#     rspd_obs <- c(rspd_obs, sum(tmp)/n[i])
# }

# exposure_data_bin <- data.frame(AUC24 = ds, resp = rspd) %>%
#     arrange(AUC24)

# # ----------------------------------
# # ? Data generating model 3
# # ----------------------------------
set.seed(9981)
cc <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
n <- c(3,3,6,8,12,18,10,6,3,3)
selected <- 1:7

ds <- cc[selected]
nn <- n[selected]

ds <- rep(ds, nn) + rnorm(sum(nn), 0, 0.01)
pr <- inv_logit(sigEmax(ds, e0, emax, ec50, h))

exposure_data_bin <- data.frame(AUC24 = ds, pr = pr) %>% 
    mutate(resp = rbinom(sum(nn), 1, pr)) %>% arrange(AUC24)
head(exposure_data_bin)
# # .....................................
# # Check the shape of the true ER curve
# # .....................................
# # cc <- seq(0.01, 1, length.out = 100)
# # pr_true <- inv_logit(sigEmax(cc, e0, emax, ec50, h))
# # plot(cc, pr_true, type = 'o', col = "red", xlab = "AUC24", ylab = "Response", ylim = c(-0.05, 1))


# # ----------------------------------
# # ? Fitting the model
# # ----------------------------------
fit.sigEmax.par <- fitERMod(exposure_data_bin$AUC24, exposure_data_bin$resp, model = "sigEmax", type = "binomial")
fit.sigEmax.par

# # fit.sigEmax.e0 <- fit.sigEmax.par[1]
# # fit.sigEmax.emax <- fit.sigEmax.par[2]
# # fit.sigEmax.ed50 <- fit.sigEmax.par[3]
# # fit.sigEmax.h <- fit.sigEmax.par[4]

# # fit.sigEmax.orr <- inv_logit(sigEmax(exposure_data_bin$AUC24, fit.sigEmax.e0, fit.sigEmax.emax, fit.sigEmax.ed50, fit.sigEmax.h))

# # true.sigEmax.orr <- exposure_data_bin$pr

# # cbind(exposure_data_bin, fit.sigEmax.orr)

# # plot(exposure_data_bin$AUC24, fit.sigEmax.orr, type = 'o', col = "blue", pch = 16, xlab = "AUC24", ylab = "Response")
# # lines(exposure_data_bin$AUC24, true.sigEmax.orr, type = 'o', col = "red", pch = 16)


# # aucs <- seq(min(exposure_data_bin$AUC24), max(exposure_data_bin$AUC24), length.out = 100)
# # pr <- inv_logit(sigEmax(aucs, fit.sigEmax.e0, fit.sigEmax.emax, fit.sigEmax.ed50, fit.sigEmax.h))
# # pr.true <- inv_logit(sigEmax(aucs, e0, emax, ec50, h))
# # plot(aucs, pr, type = 'l', col = "blue", xlab = "AUC24", ylab = "Response", ylim = c(-0.05, 1))
# # lines(aucs, pr.true, type = 'l', col = "red")



# # TODO: too small difference between the responses
# # dose1 <- data.frame(Dose = "150 mg", AUC24 =  rlnorm(3, meanlog = log(10), sdlog = log(1.5)))
# # dose2 <- data.frame(Dose = "300 mg", AUC24 =  rlnorm(3, meanlog = log(20), sdlog = log(1.5)))
# # dose3 <- data.frame(Dose = "600 mg", AUC24 =  rlnorm(6, meanlog = log(40), sdlog = log(1.5)))
# # exposure_data_cont <- rbind(dose1, dose2, dose3) %>%
# #   mutate(resp = exp(sigEmax(AUC24, e0, eMax, ec50, h)))

# # print(exposure_data_cont)
