library(dplyr)     # for data manipulation
library(ggplot2)   # for plotting
library(binom)     # for calculation of confidence interval
library(patchwork) # for combining plots

#Setting ggplot theme
theme_set(theme_bw())

set.seed(1000)

# Generating Exposure-Response Dataset with AUC (log normal) as exposure metric
placebo <- data.frame(Dose = "Placebo", AUC24 =  rep(0, 200))
dose1 <- data.frame(Dose = "150 mg", AUC24 =  rlnorm(200, meanlog = log(10), sdlog = log(1.5)))
dose2 <- data.frame(Dose = "300 mg", AUC24 =  rlnorm(200, meanlog = log(20), sdlog = log(1.5)))
dose3 <- data.frame(Dose = "600 mg", AUC24 =  rlnorm(200, meanlog = log(40), sdlog = log(1.5)))

exposure_data <- rbind(placebo, dose1, dose2, dose3) %>%
  mutate(z = .035*AUC24, pr = 1/(1+exp(-z)) - 0.3, resp = rbinom(800,1,pr))

print(head(exposure_data,10))

# ! --------------------------------------------------------
# ! P(Y = 1) = 1/(1+exp(-.035*AUC24)) - 0.3
# ! AUC24 ~ lognormal(meanlog = log(d), sdlog = log(1.5))
# ! --------------------------------------------------------

# Setting Dose as a factor will ensure Dose levels show up in the correct order when plotting.
exposure_data$Dose <- factor(exposure_data$Dose, levels = c("Placebo", "150 mg", "300 mg", "600 mg"))


# Separate placebo and active treatment groups
exposure_data_placebo <- filter(exposure_data, Dose == "Placebo")
exposure_data_active <- filter(exposure_data, Dose != "Placebo")


# Update n_quantile and rerun to view ER plot with different number of quantiles
n_quantile <- 4

step <- 1/n_quantile
quantiles <- seq(0, 1, step)
print(quantiles)
## [1] 0.00 0.25 0.50 0.75 1.00


# Define exposure cutoffs
breaks <- quantile(exposure_data_active$AUC24, quantiles)
print(breaks)
##         0%        25%        50%        75%       100%
##   2.900026  12.002059  19.683045  33.981775 111.558171

# Use cut function to place each subjects exposure into the correct bin
exposure_data_active <- exposure_data_active %>%
  mutate(quantile = cut(AUC24, breaks, include.lowest = TRUE))

# Setting Placebo group quantile to "Placebo" for plotting purposes
exposure_data_placebo <- exposure_data_placebo %>%
  mutate(quantile = "Placebo")

# Combine placebo and active treatment groups
exposure_data_all <- rbind(exposure_data_placebo, exposure_data_active)

# Set quantile factors to ensure quantile labels show up in the correct
#   order when plotting.
exposure_data_all$quantile <- factor(exposure_data_all$quantile,
                                     levels = c("Placebo", levels(cut(exposure_data_active$AUC24, breaks,  include.lowest = TRUE))))

er_summary <- exposure_data_all %>%
  group_by(quantile) %>%
  summarise(median_AUC = median(AUC24),
            n = n(),
            resp_n = sum(resp),
            resp_perc = sum(resp)/n()*100,
            placebo = quantile[1] == "Placebo",
            ci = binom.confint(sum(resp), n(),
                               conf.level = 0.95, methods = "exact"))
print(er_summary)

## # A tibble: 5 x 7
##   quantile median_AUC     n resp_n resp_perc placebo ci$method    $x    $n $mean
##   <fct>         <dbl> <int>  <int>     <dbl> <lgl>   <chr>     <int> <int> <dbl>
## 1 Placebo        0      200     51      25.5 TRUE    exact        51   200 0.255
## 2 [2.9,12]       8.27   150     46      30.7 FALSE   exact        46   150 0.307
## 3 (12,19.~      15.8    150     41      27.3 FALSE   exact        41   150 0.273
## 4 (19.7,3~      25.7    150     64      42.7 FALSE   exact        64   150 0.427
## 5 (34,112]      44.5    150     87      58   FALSE   exact        87   150 0.58


# Exposure Response Quantile Plot
p1 <- ggplot(er_summary, aes(x = quantile, y = resp_perc )) +
  geom_col( aes(fill = placebo), alpha = 0.6) +
  geom_text(aes(label = paste0("(",resp_n,"/", n, ")")), vjust = -0.5, color = "grey30", size = 3,
            position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(resp_perc,1),"%")), vjust = -1.9, color = "grey30", size = 3,
            position = position_dodge(width = 0.9)) +
  labs(x = "AUC Exposure Quantile (ng/ml*hr)",
       y = "Percent Responders (%)",
       title = "Exposure-Response Quantile Plot",
       subtitle = paste(n_quantile, "Bins") ) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_fill_manual(values = c( "purple4", "grey20")) +
  guides(fill = "none")


print(p1)

# Exposure Response Logistic Regression Plot
er_plot <- ggplot(er_summary, aes(x = median_AUC, y = resp_perc/100)) +
  geom_point() +
  geom_errorbar(aes(ymax = ci$upper, ymin = ci$lower)) +
  geom_jitter(data = exposure_data_all,
              aes(x = AUC24,y =resp, color = Dose ),
              height = 0.05,
              alpha = 0.5) +
  geom_smooth(data = exposure_data_all,
              aes(x = AUC24,y =resp ),
              color = "grey10",
              method = "glm",
              method.args = list(family = "binomial")) +
  labs(x= "AUC (ng/ml*hr)", y = "Probability of Response") +
  scale_x_continuous(limits= c(-1, 125))

# Summary plot of exposures
exposure_plot <- ggplot(exposure_data,
                        aes(x = AUC24, y = Dose, fill = Dose, color = Dose)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.colour = NA, width = 0.7,
               alpha = 0.5) +
  scale_x_continuous(limits= c(-1, 125)) +
  labs(x= "AUC (ng/ml*hr)", y = "Dose Group") +
  guides(color = "none", fill = "none")


# Combining  ER Plot and exposure summary plot
p2 <- er_plot/exposure_plot  + plot_layout(heights = c(2, 1))
print(p2)

