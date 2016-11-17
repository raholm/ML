## ---- assign1-init
library(ggplot2)

data <- read.csv("../data/australian-crabs.csv", sep=",")
## ---- end-of-assign1-init

## 1
## ---- assign1-1-plot
ggplot(data) +
    geom_point(aes(x=RW, y=CL, color=sex))
## ---- end-of-assign1-1-plot

## 2
LDA <- function(X, y) {
    n <- nrow(X)
    p <- ncol(X)

    labels <- unique(y)
    priors <- table(y) / length(y)

    means <- aggregate(X, list(y), mean)
    means <- as.matrix(means[, -1], ncol=p)

    lengths <- by(X, list(y), nrow)

    cov_mats <- by(X, list(y), cov)
    cov_mats <- lapply(1:length(lengths), function(i) {
        cov_mats[[i]] * lengths[[i]]
    })

    sigma <- as.matrix(Reduce("+", cov_mats) / sum(lengths), nrow=p)
    sigma_inv <- solve(sigma)

    w0 <- sapply(1:length(labels), function(i) {
        -(1 / 2) * t(means[i,]) %*% sigma_inv %*% means[i,] + log(priors[i])
    })

    w1 <- sapply(1:length(labels), function(i) {
        sigma_inv %*% means[i, ]
    })

    list(w0=w0, w1=w1, sigma=sigma)
}


X <- cbind(data$RW, data$CL)
y <- data$sex

result <- LDA(X, y)

w1 <- result$w1[, 2] - result$w1[, 1]
w0 <- result$w0[2] - result$w0[1]

intercept <- -w0 / w1[2]
slope <- -w1[1] / w1[2]
predicted <- factor(as.numeric((w0 + w1 %*% t(X)) > 0), levels=c(0, 1), labels=c("Female", "Male"))

## Actual Data
plot_data <- data.frame(RW=data$RW, CL=data$CL, class=data$sex)
line_data <- data.frame(intercept=intercept, slope=slope)

ggplot() +
    geom_point(data=plot_data, aes(x=RW, y=CL, color=class)) +
    geom_abline(data=line_data, intercept=intercept, slope=slope,
                color="black", linetype="dotted", size=1)

## Predicted Data
plot_data <- data.frame(RW=data$RW, CL=data$CL, class=predicted)
line_data <- data.frame(intercept=intercept, slope=slope)

ggplot() +
    geom_point(data=plot_data, aes(x=RW, y=CL, color=class)) +
    geom_abline(data=line_data, intercept=intercept, slope=slope,
                color="black", linetype="dotted", size=1)

## 3

## 4
