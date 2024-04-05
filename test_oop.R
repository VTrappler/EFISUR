library(R6)
library(ggplot2)
library(purrr)
library(patchwork)

estimate_pareto_front <- function(df_res) {
    D <- df_res[order(df_res$f1, df_res$f2, decreasing = FALSE), ]
    front <- D[which(!duplicated(cummin(D$f2))), ]
    return(front)
}

MultiObj <- R6Class("MultiObj", list(
    f = list(),
    dim_in = NULL,
    input_names = NULL,
    output_names = NULL,
    initialize = function(dim_in) {
        self$dim_in <- dim_in
        self$input_names <- c()
        for (i in 1:self$dim_in) {
            self$input_names <- c(self$input_names, paste0("x", i))
        }
    },
    add_objective = function(closure) {
        self$f[[length(self$f) + 1]] <- closure
    },
    dim_out = function() {
        self$output_names <- c()
        for (i in seq_along(self$f)) {
            self$output_names <- c(self$output_names, paste0("f", i))
        }
        return(length(self$f))
    },
    evaluate = function(input_df) {
        df_res <- input_df
        for (i in 1:self$dim_out()) {
            out <- unlist(pmap(input_df, (self$f[[i]])))
            df_res <- cbind(df_res, data.frame(out))
        }
        colnames(df_res) <- c(self$input_names, self$output_names)
        return(df_res)
    }
))


.f1 <- function(x1, x2) {
    b1 <- 15 * x1 - 5
    b2 <- 15 * x2
    lhs <- (b2 - (5.1 / (4 * pi^2)) * b1^2 + 5 * b1 / pi - 6)^2
    rhs <- 10 * ((1 - (1 / (8 * pi))) * cos(b1) + 1)
    return(lhs + rhs)
}
.f2 <- function(x1, x2) {
    b1 <- 15 * x1 - 5
    b2 <- 15 * x2
    lhs <- -sqrt((10.5 - b1) * (b1 + 5.5) * (b2 + 0.5))
    cen <- -(1 / 30) * (b2 - (5.1 / (4 * pi^2)) * b1^2 + -6)^2
    rhs <- -(1 / 3) * ((1 - 1 / (8 * pi)) * cos(b1) + 1)
    return(lhs + cen + rhs)
}


branin <- MultiObj$new(dim_in = 2)
branin$add_objective(.f1)
branin$add_objective(.f2)

x_plot <- seq(0, 1, 0.05)
y_plot <- seq(0, 1, 0.05)
df <- data.frame(expand.grid(x1 = x_plot, x2 = y_plot))
df_res <- branin$evaluate(df)
fc1 <- ggplot() +
    geom_contour_filled(data = df_res, mapping = aes(x = x1, y = x2, z = f1)) +
    coord_fixed()
fc2 <- ggplot() +
    geom_contour_filled(data = df_res, mapping = aes(x = x1, y = x2, z = f2)) +
    coord_fixed()

fc1 + fc2 + plot_annotation("Contour plots of f_1 and f_2")


df_samples <- data.frame(expand.grid(x1 = x_plot, x2 = y_plot))
df_res <- branin$evaluate(df_samples)
pareto <- estimate_pareto_front(df_res)
pareto_front_true <- ggplot() +
    geom_step(data = pareto, mapping = aes(x = f1, y = f2), alpha = 1, colour = "red")
pareto_front <- pareto_front_true
pareto_set_true <- ggplot() +
    geom_point(data = pareto, mapping = aes(x = x1, y = x2), colour = "red")
pareto_set <- pareto_set_true
for (i in 1:50) {
    df_samples <- data.frame(x1 = runif(10), x2 = runif(10))
    df_res <- branin$evaluate(df_samples)
    pareto <- estimate_pareto_front(df_res)
    pareto_front <- pareto_front + geom_step(data = pareto, mapping = aes(x = f1, y = f2), alpha = 0.1)
    pareto_set <- pareto_set + geom_point(data = pareto, mapping = aes(x = x1, y = x2))
}
(pareto_front + ggtitle("Pareto Front")) +
    (pareto_set + ggtitle("Pareto set")) +
    plot_annotation(title = "Pareto under different sampling points")







library(DiceKriging)
n_training <- 10
df_samples <- data.frame(x1 = runif(n_training), x2 = runif(n_training))
df_res <- branin$evaluate(df_samples)
model1 <- km(
    formula = ~1, design = df_samples, response = df_res$f1
)
model2 <- km(
    formula = ~1, design = df_samples, response = df_res$f2
)
prediction <- predict(model1, newdata = df, type = "UK", checkNames = FALSE)
predict_df <- data.frame(
    x1 = df$x1, x2 = df$x2, pred = prediction$mean, pred_sd = (prediction$sd),
    lower = prediction$mean - prediction$sd, upper = prediction$mean + prediction$sd
)
k_fc <- ggplot() +
    geom_contour_filled(data = predict_df, mapping = aes(x = x1, y = x2, z = pred)) +
    geom_point(data = df_samples, mapping = aes(x = x1, y = x2, color = "Sampled points"), color = "red") +
    coord_fixed()
k_sd <- ggplot() +
    geom_contour_filled(data = predict_df, mapping = aes(x = x1, y = x2, z = pred_sd)) +
    geom_point(data = df_samples, mapping = aes(x = x1, y = x2, color = "Sampled points"), color = "red") +
    coord_fixed()

k_fc + k_sd + plot_annotation("GP mean and GP sd of f1")





df_simulated <- data.frame(df,
    f1 = t(simulate(model1, newdata = df)),
    f2 = t(simulate(model2, newdata = df))
)
fc1_sim <- ggplot() +
    geom_contour_filled(data = df_simulated, mapping = aes(x = x1, y = x2, z = f1)) +
    coord_fixed()
fc2_sim <- ggplot() +
    geom_contour_filled(data = df_simulated, mapping = aes(x = x1, y = x2, z = f2)) +
    coord_fixed()

fc1 + fc2 + fc1_sim + fc2_sim + plot_annotation("Contour plots of simulated f_1 and f_2")

df_simulations <- data.frame(df, f1 = double(length(df$x1)), f2 = double(length(df$x1)), r = factor(length(df$x1)))
for (i in 1:200) {
    df_simulations <- rbind(df_simulations, data.frame(df,
        f1 = t(simulate(model1, newdata = df)),
        f2 = t(simulate(model2, newdata = df)),
        r = as.factor(i)
    ))
}
paretos <- do.call(rbind, by(df_simulations, df_simulations["r"], estimate_pareto_front))
ggplot() +
    geom_line(data = paretos, aes(x = f1, y = f2, color = r))


dom <- data.frame(f1 = c(1, 2, 3, 4), f2 = c(4, 3, 5, 1))


is_dominated <- function(dom, x) {
    for (i in seq_along(dom)) {
        dom[[i]] > xt[[i]]
    }
}


pareto <- estimate_pareto_front(df_res_sim)
pareto_front <- pareto_front_true +
    geom_step(data = pareto, mapping = aes(x = f1, y = f2), alpha = 0.1)
pareto_set <- ggplot() +
    geom_point(data = pareto, mapping = aes(x = x1, y = x2))

for (i in 1:100) {
    df_res_sim <- data.frame(df,
        f1 = t(simulate(model1, newdata = df)),
        f2 = t(simulate(model2, newdata = df))
    )
    pareto <- estimate_pareto_front(df_res_sim)
    pareto_front <- pareto_front +
        geom_point(data = pareto, mapping = aes(x = f1, y = f2), alpha = 0.1)
    pareto_set <- pareto_set +
        geom_point(data = pareto, mapping = aes(x = x1, y = x2))
}
pareto_front + pareto_set
