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
plda <- function(X, y) {
    n <- nrow(X)
    labels <- unique(y)
    priors <- table(y) / length(y)

    mean <- colMeans(X[which(y == labels[1]),])

    result <- list()
    sigmahat <- matrix(0, nrow=length(labels), ncol=length(labels))

    for (i in 1:length(labels)) {
        data <- X[which(y == labels[i]),]
        mean <- colMeans(data)
        sigma <- cov(data)

        result[[i]] <- list(data=data, label=labels[i], mean=mean, sigma=sigma, priors=priors[[labels[i]]])

        sigmahat <- sigmahat + sigma * nrow(data)
    }

    sigmahat <- sigmahat / nrow(X)

    list(result, sigmahat)
}

X <- cbind(data$CL, data$RW)
y <- data$sex

fit <- plda(X, y)
result <- fit[[1]]


## 3

## 4
