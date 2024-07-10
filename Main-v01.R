source("utils/fitERMod.R")
library(ggplot2)
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
# * Continuous response case (ER)
# * ==================================
source("utils/fitERMod.R")
set.seed(1000)
e0 <- log(20) / 2
eMax <- log(100) / 2
h <- 4
EC50 <- 5
sigma_c <- 0.5
sigma_y <- exp(0.2)
TVCL <- 5                                       # ? Typical Value of Clearance
beta0 <- -log(TVCL)
beta1 <- 0.85

doses <- c(20, 30, 48, 60, 80, 100, 110)        # ? Do not consider the placebo
reps <- c(3, 3, 6, 8, 12, 18, 10)
cat("Total number of observations: ", sum(reps), "\n")
ds <- rep(doses, reps)

logC <- beta0 + beta1 * log(ds) + rnorm(sum(reps), 0, sigma_c)
CC <- round(exp(logC), 3)
logY <- e0 + eMax / (1 + (EC50 / CC)^h) + rnorm(sum(reps), 0, sigma_y)   # ? True model: sigEmax
Y <- exp(logY)

df <- data.frame(Dose = ds, Exposure = CC, Response = Y)


saveRDS(df, "data/DER_cont.rds")
write.csv(df, "data/DER_cont.csv", row.names = FALSE)
write.table(df, "data/DER_cont.txt", row.names = FALSE, sep = "\t")

model <- "sigEmax"
fit <- fitERMod(df$Exposure, df$Response, model = model, type = "gaussian")
pred <- predict(fit, newdata = df$Exposure)
print(pred)
log(Y) - pred



# models <- c("sigEmax", "Emax", "Expo", "Beta", "Linear", "LinearLog", "Logistic", "Quadratic")
# for (iii in 1:length(models)) {
#     cat("Model: ", models[iii], "\n")
#     model <- models[iii]
#     fit <- fitERMod(df$Exposure, df$Response, model = model, type = "gaussian")
#     print(all.equal(fit$residuals, Y - predict(fit, newdata = df$Exposure)))
# }


fit <- fitERMod(df$Exposure, df$Response, model = "sigEmax", type = "gaussian")


# * ==================================
# * Binary response case (ER)
# * ==================================
source("utils/fitERMod.R")
set.seed(1024)
e0 <- logit(0.1)
eMax <- 3
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
CC <- round(exp(logC), 3)
logitP <- e0 + eMax / (1 + (EC50 / CC)^h)       # ? True model: sigEmax
Ps <- inv_logit(logitP)
Y <- rbinom(sum(reps), 1, Ps)

df <- data.frame(Dose = ds, Exposure = CC, Response = Y)

saveRDS(df, "data/DER_bin.rds")
write.csv(df, "data/DER_bin.csv", row.names = FALSE)
write.table(df, "data/DER_bin.txt", row.names = FALSE, sep = "\t")

model <- "sigEmax"
fit <- fitERMod(df$Exposure, df$Response, model = model, type = "binomial")
pred <- predict(fit, newdata = df$Exposure, type = 'class')
print(pred)
pred <- predict(fit, newdata = df$Exposure)
print(pred)


fit2 <- lm(log(CC) ~ log(ds))

newdoses <- data.frame(ds = c(20))
pred2 <- predict(fit2, newdata = newdoses)























# * ==================================
# * Continuous response case (DER)
# * ==================================
source("utils/fitDERMod.R")
df <- readRDS("data/DER_cont.rds")

model <- "sigEmax"
fit <- fitDERMod(df$Dose, df$Exposure, df$Response, model = model, type = "gaussian")
pred <- predict(fit, newdata = unique(df$Dose))
print(pred)


new_doses <- seq(min(df$Dose), max(df$Dose), by = 2)
fit_de <- fit$fit2
logC <- predict(fit_de, newdata = data.frame(dose = new_doses))

plot(df$Dose, df$Exposure)
lines(new_doses, logC, col = "red")


ds0 <- unique(df$Dose)
CC0 <- exp(beta0 + beta1 * log(ds0))
logY0 <- log(e0 + eMax / (1 + (EC50 / CC0)^h))
Y0 <- exp(logY0)
Y0 - pred



source("utils/fitDERMod.R")
fit.der <- fitDERMod(df$Dose, df$Exposure, df$Response, model = "sigEmax", type = "gaussian")


new_doses <- seq(min(df$Dose), max(df$Dose), by = 2)
fitted_values2 <- predict(fit.der, newdata = new_doses, type = "response")
fit_df2 <- data.frame(Dose = new_doses, Fitted = fitted_values2)

p <- ggplot() +
    geom_point(data = df, aes(x = Dose, y = log(Response)), color = "blue", alpha = 0.5) +  # 数据点
    geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
    labs( #title = "Fitted Curve with Data Points",
        x = "Dose",
        y = "log(Response)") +
    theme_minimal()

p


# models <- c("sigEmax", "Emax", "Expo", "Beta", "Linear", "LinearLog", "Logistic", "Quadratic")
# for (iii in 1:length(models)) {
#     cat("Model: ", models[iii], "\n")
#     model <- models[iii]
#     fit <- fitERMod(df$Exposure, df$Response, model = model, type = "gaussian")
#     print(all.equal(fit$residuals, Y - predict(fit, newdata = df$Exposure)))
# }


# * ==================================
# * Binary response case (DER)
# * ==================================
source("utils/fitERMod.R")
df <- readRDS("data/DER_bin.rds")

model <- "sigEmax"
fit <- fitERMod(df$Exposure, df$Response, model = model, type = "binomial")
pred <- predict(fit, newdata = df$Exposure, type = 'class')
print(pred)
pred <- predict(fit, newdata = df$Exposure)
print(pred)


fit2 <- lm(log(CC) ~ log(ds))
fit2$coefficients

# newdoses <- data.frame(ds = c(20))
# pred2 <- predict(fit2, newdata = newdoses)    # * log(CC)
# CC.pred <- exp(pred2)                         # * CC

# predict_DER <- function(dose, fit_exp, fit_resp, type = 'response') {
#     newdoses <- data.frame(ds = dose)
#     log_CC_pred <- predict(fit_exp, newdata = newdoses)         # 预测log(CC)
#     CC_pred <- exp(log_CC_pred)                                 # 转换为CC
#     response_pred <- predict(fit_resp, newdata = CC_pred, type = type)
#     return(response_pred)
# }

# predict_DER(110, fit2, fit)

source("utils/fitDERMod.R")
fit.der <- fitDERMod(df$Dose, df$Exposure, df$Response, model = "sigEmax", type = "binomial")


new_doses <- seq(min(df$Dose), max(df$Dose), by = 2)
fitted_values2 <- predict(fit.der, newdata = new_doses, type = "response")
fit_df2 <- data.frame(Dose = new_doses, Fitted = inv_logit(fitted_values2))

p <- ggplot() +
    geom_jitter(data = df, aes(x = Dose, y = Response), color = "blue", alpha = 0.5, width = 0, height = 0.05) +  # 数据点
    geom_line(data = fit_df2, aes(x = Dose, y = Fitted), color = "red", size = 1) +  # 拟合曲线
    labs( #title = "Fitted Curve with Data Points",
        x = "Dose",
        y = "Response") +
    theme_minimal()

p


