## ---- assign1-init
library(ggplot2)

myspline <- function(X, y, knots) {
    n <- length(X)
    m <- length(knots)
    df <- m + 2

    H <- matrix(0, nrow=n, ncol=df)
    H[, 1] <- 1
    H[, 2] <- X

    for (i in 3:df) {
        H[, i] <- pmax(X - knots[i - 2], 0)
    }

    data <- data.frame(y=y, H)
    ## Removes the intercept term (have it already)
    lmfit <- lm(y ~ 0 + ., data=data)
    coefficients <- as.numeric(coef(lmfit))
    yhat <- H %*% coefficients

    yhat
}

data <- read.csv2("../data/cube.csv", header=TRUE, sep=";")
## ---- end-of-assign1-init

## 2
## ---- assign1-2
knots <- c(2, 4)
yhat <- myspline(data$x, data$y, knots)

plot_data <- data.frame(x=data$x, y=data$y, yhat=yhat)

ggplot(plot_data) +
    geom_point(aes(x, y), color="red") +
    geom_point(aes(x, yhat), color="blue")
## ---- end-of-assign1-2

## 3
## ---- assign1-3
smooth_fit <- smooth.spline(x=data$x, y=data$y)
yhat <- fitted(smooth_fit)

## plot(smooth_fit, col="blue")
## points(data$x, data$y, col="red")

plot_data <- data.frame(x=data$x, y=data$y, yhat=yhat)

ggplot(plot_data) +
    geom_point(aes(x, y), color="red") +
    geom_point(aes(x, yhat), color="blue")
## ---- end-of-assign1-3
