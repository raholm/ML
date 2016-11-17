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
    p <- ncol(X)

    labels <- unique(y)
    priors <- table(y) / length(y)

    mean <- colMeans(X[which(y == labels[1]),])

    result <- list()
    sigmahat <- matrix(0, nrow=length(labels), ncol=length(labels))

    ## means <- aggregate(X, list(y), mean)
    ## covs <- by(X, list(y), cov)
    ## lengths <- by(X, list(y), nrow)

    ## cov_mats <- lapply(1:length(lengths), function(i) covs[[i]] * lengths[[i]])
    ## covhat <- Reduce("+", mats) / sum(lengths)


    for (i in 1:length(labels)) {
        data <- X[which(y == labels[i]),]
        mean <- as.matrix(colMeans(data), nrow=p)
        sigma <- as.matrix(cov(data), nrow=p)

        result[[i]] <- list(data=data, label=labels[i],
                            mean=as.matrix(mean, nrow=length(mean)),
                            sigma=as.matrix(sigma, nrow=length(mean)),
                            priors=priors[[labels[i]]])

        sigmahat <- sigmahat + sigma * nrow(data)
    }

    sigmahat <- sigmahat / nrow(X)

    ## Calculate discriminant function
    for (i in 1:length(result)) {
        class <- result[[i]]

        w0 <- as.numeric(-(1 / 2) * t(class$mean) %*% solve(sigmahat) %*% class$mean + log(class$priors))
        w1 <- solve(sigmahat) %*% class$mean

        result[[i]]$discriminant <- w0 + class$data %*% w1
        result[[i]]$w0 <- w0
        result[[i]]$w1 <- w1
    }

    list(result, sigmahat)
}

X <- cbind(data$CL, data$RW)
y <- data$sex

fit <- plda(X, y)
result <- fit[[1]]
sigma <- fit[[2]]

result[[1]]$w0
result[[1]]$w1
result[[1]]$discriminant

solve(sigma) %*% (result[[1]]$mean - result[[2]]$mean)


a <- list(w10 = result[[1]]$w0, w11=result[[1]]$w1, w20 = result[[2]]$w0, w21=result[[2]]$w1)

b <- a$w10 - a$w20
w <- a$w11 - a$w21
b
w

intercept <- -b / w[2]
slope <- -w[1] / w[2]

b
s

solution <- as.numeric(t(w) %*% t(X) + b)
solution

yfit <- solution > 0
yfit

plot(cbind(X[,1], X[, 2]))
abline(intercept, slope)

## 3

## 4
