source("utils/fitERMod.R")

# ----------------------------------
# Example (Exposure-Response Model)
# ----------------------------------
library(dplyr)     # for data manipulation

# e0 <- logit(0.1)
# emax <- 3
# ec50 <- 0.4
# h <- 3

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
# set.seed(9981)
# cc <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
# n <- c(3,3,6,8,12,18,10,6,3,3)
# selected <- 1:7

# ds <- cc[selected]
# nn <- n[selected]

# ds <- rep(ds, nn) + rnorm(sum(nn), 0, 0.01)
# pr <- inv_logit(sigEmax(ds, e0, emax, ec50, h))

# exposure_data_bin <- data.frame(AUC24 = ds, pr = pr) %>% 
#     mutate(resp = rbinom(sum(nn), 1, pr)) %>% arrange(AUC24)
# head(exposure_data_bin)
# # .....................................
# # Check the shape of the true ER curve
# # .....................................
# # cc <- seq(0.01, 1, length.out = 100)
# # pr_true <- inv_logit(sigEmax(cc, e0, emax, ec50, h))
# # plot(cc, pr_true, type = 'o', col = "red", xlab = "AUC24", ylab = "Response", ylim = c(-0.05, 1))


# # ----------------------------------
# # ? Fitting the model
# # ----------------------------------

# models <- c("sigEmax", "Emax", "Expo", "Beta", "Linear", "LinearLog", "Logistic", "Quadratic")
# for (iii in 1:length(models)) {
#     cat("Model: ", models[iii], "\n")
#     model <- models[iii]
#     fit <- fitERMod(exposure_data_bin$AUC24, exposure_data_bin$resp, model = model, type = "binomial")
#     print(fit)
# }

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



# * ==================================
# * Continuous response case
# * ==================================
source("utils/fitERMod.R")
set.seed(1000)
e0 <- 20
eMax <- 100
h <- 4
EC50 <- 5
sigma_c <- 0.5
sigma_y <- 0.2
TVCL <- 5                                       # ? Typical Value of Clearance
beta0 <- -log(TVCL)
beta1 <- 0.85

doses <- c(20, 30, 48, 60, 80, 100, 110)        # ? Do not consider the placebo
reps <- c(3, 3, 6, 8, 12, 18, 10)
cat("Total number of observations: ", sum(reps), "\n")
ds <- rep(doses, reps)

logC <- beta0 + beta1 * log(ds) + rnorm(sum(reps), 0, sigma_c)
CC <- exp(logC)
logY <- log(e0 + eMax / (1 + (EC50 / CC)^h)) + rnorm(sum(reps), 0, sigma_y)
Y <- exp(logY)

df <- data.frame(Dose = ds, Exposure = CC, Response = Y)

model <- "sigEmax"


models <- c("sigEmax", "Emax", "Expo", "Beta", "Linear", "LinearLog", "Logistic", "Quadratic")
for (iii in 1:length(models)) {
    cat("Model: ", models[iii], "\n")
    model <- models[iii]
    fit <- fitERMod(df$Exposure, df$Response, model = model, type = "gaussian")
    print(all.equal(fit$residuals, Y - predict(fit, newdata = df$Exposure)))
}
# fit <- fitERMod(df$Exposure, df$Response, model = model, type = "gaussian")
# pred <- predict(fit, newdata = df$Exposure)
# print(pred)
# Y - pred