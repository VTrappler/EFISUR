fct_1 <- function(x, y) {
    b1 <- 15 * x - 5
    b2 <- 15 * y
    lhs <- (b2 - (5.1 / (4 * pi^2)) * b1^2 + 5 * b1 / pi - 6)^2
    rhs <- 10 * ((1 - (1 / (8 * pi))) * cos(b1) + 1)
    return(lhs + rhs)
}

fct_2 <- function(x, y) {
    b1 <- 15 * x - 5
    b2 <- 15 * y
    lhs <- -sqrt((10.5 - b1) * (b1 + 5.5) * (b2 + 0.5))
    cen <- -(1 / 30) * (b2 - (5.1 / (4 * pi^2)) * b1^2 + -6)^2
    rhs <- -(1 / 3) * ((1 - 1 / (8 * pi)) * cos(b1) + 1)
    return(lhs + cen + rhs)
}
library(DiceKriging)
library(ggplot2)
library(patchwork)
library(purrr)

x_plot <- seq(0, 1, 0.01)
y_plot <- seq(0, 1, 0.01)
df <- data.frame(expand.grid(x = x_plot, y = y_plot))
df1 <- cbind(df, t(data.frame(out = pmap(df, fct_1)))[, 1])
df2 <- cbind(df, t(data.frame(out = pmap(df, fct_2)))[, 1])

rownames(df1) <- NULL
colnames(df1) <- c("x", "y", "f1")
rownames(df1) <- NULL
colnames(df2) <- c("x", "y", "f2")
fc1 <- ggplot() +
    geom_contour_filled(data = df1, mapping = aes(x = x, y = y, z = f1))
fc2 <- ggplot() +
    geom_contour_filled(data = df2, mapping = aes(x = x, y = y, z = f2))

fc1 + fc2

d <- data.frame(x = df1$f1, y = df2$f2)
D <- d[order(d$x, d$y, decreasing = FALSE), ]
front <- D[which(!duplicated(cummin(D$y))), ]

ggplot(data = front) +
    geom_point(aes(x = x, y = y))
