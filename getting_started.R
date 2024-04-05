library(DiceKriging)
library(ggplot2)
library(patchwork)
library(httpgd)
library(purrr)
set.seed(1226)
fct_test <- function(x) {
    return(x^2 * sin(x))
}

x_plot <- seq(0, 10, 1)

x_samples <- runif(5, 0, 10)
model <- km(
    formula = ~1, design = data.frame(x_samples), response = fct_test(x_samples)
)
truth <- data.frame(x = x_plot, truth = fct_test(x_plot))
prediction <- predict(model, newdata = data.frame(x = x_plot), type = "UK", checkNames = FALSE)

predict_df <- data.frame(
    x = x_plot, pred = prediction$mean,
    lower = prediction$mean - prediction$sd, upper = prediction$mean + prediction$sd
)
observations <- data.frame(x = x_samples, obs = fct_test((x_samples)))



km_1d <- ggplot() +
    geom_point(data = observations, mapping = aes(x = x, y = obs)) +
    geom_line(data = predict_df, mapping = aes(x = x, y = pred)) +
    geom_ribbon(data = predict_df, mapping = aes(x = x, ymin = lower, ymax = upper), linetype = 2, alpha = 0.1) +
    geom_line(data = truth, mapping = aes(x = x, y = truth), color = "red")

km_1d
##
fct_2d <- function(x, y) {
    return(cos(x) * log((x - 5)^2) * sin(y))
}
x_plot <- seq(0, 10, 0.1)
y_plot <- seq(0, 10, 0.1)
df <- data.frame(expand.grid(x = x_plot, y = y_plot))
df <- cbind(df, t(data.frame(out = pmap(df, fct_2d)))[, 1])
rownames(df) <- NULL
colnames(df) <- c("x", "y", "f")
ggplot() +
    geom_contour_filled(data = df, mapping = aes(x = x, y = y, z = f))
