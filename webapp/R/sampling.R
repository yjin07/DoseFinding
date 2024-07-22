# Bootstrap function
bootstrap_indices <- function(df, method = c("total", "by_dose")) {
    method <- match.arg(method)

    if (method == "total") {
        # Completely random bootstrap
        indices <- sample(1:nrow(df), nrow(df), replace = TRUE)
    } else if (method == "by_dose") {
        # Bootstrap by Dose
        indices <- unlist(lapply(split(1:nrow(df), df$Dose), function(indices) {
            sample(indices, length(indices), replace = TRUE)
        }))
    } else {
        stop("Invalid method specified. Choose 'random' or 'by_dose'.")
    }

    return(indices)
}

# # Example usage:
# # Assuming df is your dataframe with columns Dose, Exposure, Response
# set.seed(123)  # For reproducibility
# df <- data.frame(
#     Dose = c(rep(10, 5), rep(20, 5), rep(30, 5)),
#     Exposure = rnorm(15),
#     Response = rnorm(15)
# )

# # Completely random bootstrap
# indices_random <- bootstrap_indices(df, method = "random")
# print(indices_random)

# # Bootstrap by Dose
# indices_by_dose <- bootstrap_indices(df, method = "by_dose")
# print(indices_by_dose)

# df[indices_by_dose,]
